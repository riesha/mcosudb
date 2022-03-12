use std::ffi::{CStr, CString};
pub use serde_derive::{Deserialize, Serialize};


use nom::{
    bytes::complete::{tag, take, take_while, take_while1},
    combinator::{cond, map, map_opt, map_res, opt},
    error::{Error as NomError, ErrorKind as NomErrorKind},
    multi::{length_count, length_data, many0},
    Err as NomErr, IResult, Needed, number::complete::float,
};
use std::{
    fmt,
    fs::{self, File},
    io::{self, BufWriter, Write},
    ops,
    path::Path,
};
#[derive(Debug)]
pub enum Error {
    
    
    Io(io::Error),
    ParseError(NomErrorKind),
    ParseIncomplete(Needed),
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            #[cfg(feature = "compression")]
            Error::Compression(_err) => f.write_str("failed to compress/decompress replay data"),
            Error::Io(_err) => f.write_str("failed to read osu .db file"),
            Error::ParseError(kind) => {
                write!(f, "failed to parse osu file: {}", kind.description())
            }
            Error::ParseIncomplete(Needed::Size(u)) => write!(
                f,
                "failed to parse osu file: parsing requires {} bytes/chars",
                u
            ),
            Error::ParseIncomplete(Needed::Unknown) => {
                f.write_str("failed to parse osu file: parsing requires more data")
            }
        }
    }
}
impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            #[cfg(feature = "compression")]
            Error::Compression(err) => Some(err as &dyn std::error::Error),
            Error::Io(err) => Some(err as &dyn std::error::Error),
            Error::ParseError(_kind) => None,
            Error::ParseIncomplete(_needed) => None,
        }
    }
}
impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}
impl From<NomErr<NomError<&[u8]>>> for Error {
    fn from(err: NomErr<NomError<&[u8]>>) -> Self {
        match err {
            NomErr::Incomplete(needed) => Self::ParseIncomplete(needed),
            NomErr::Error(err) | NomErr::Failure(err) => Self::ParseError(err.code),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[derive(Debug, Clone, PartialEq)]
pub struct Score {
    
    pub is_legacy: u8,
   
    pub version: i32,
 
    pub unix_timestamp: i64,
  
    pub player_name: Option<String>,
   
    pub count_300: i16,
    pub count_100: i16,
    pub count_50: i16,
    pub count_geki: i16,
    pub count_katsu: i16,
    pub count_miss: i16,

    pub score: i64,
    pub max_combo: i16,
    pub mods: i32,

    pub slider_breaks: i16,

    pub pp: f32,
    pub ur: f32,

    pub hit_min: f32,
    pub hit_max: f32,

    pub stars_total: f32,
    pub stars_aim: f32,
    pub stars_speed: f32,

    pub speed_multiplier: f32,
    pub cs: f32,
    pub ar: f32,
    pub od: f32,
    pub hp: f32,

    pub max_possible_combo: i32,
    pub num_objects: i32,
    pub num_circles: i32,

    pub exp_mods: Option<String>,
}
#[derive(Serialize, Deserialize)]
pub struct BeatmapScores {

    pub hash: Option<String>,
    pub num_scores: i32,
    pub scores: Vec<Score>,
}
#[derive(Serialize, Deserialize)]
pub struct ScoreList {
    pub version: i32,
    pub num_beatmaps: i32,
    pub beatmaps: Vec<BeatmapScores>,
}
use nom::number::complete::le_f32 as single;
use nom::number::complete::le_i16 as short;
use nom::number::complete::le_i32 as int;
use nom::number::complete::le_i64 as long;
use nom::number::complete::le_u8 as byte;

trait Bit {
    fn bit(&self, pos: u32) -> bool;
    fn bit_range(&self, pos: ops::Range<u32>) -> Self;
    fn set_bit(&mut self, pos: u32, val: bool);
    fn set_bit_range(&mut self, pos: ops::Range<u32>, val: Self);
}
macro_rules! impl_bit {
    (@ $ty:ty) => {
        impl Bit for $ty {
            fn bit(&self, pos: u32) -> bool {
                (*self & 1 << pos) != 0
            }
            fn bit_range(&self, pos: ops::Range<u32>) -> Self {
                (*self & ((1<<pos.end)-1)) >> pos.start
            }
            fn set_bit(&mut self, pos: u32, val: bool) {
                *self = (*self & !(1<<pos)) | ((val as Self)<<pos);
            }
            fn set_bit_range(&mut self, pos: ops::Range<u32>, val: Self) {
                let mask = ((1<<(pos.end-pos.start))-1) << pos.start;
                *self = (*self & !mask) | ((val<<pos.start)&mask);
            }
        }
    };
    ($($ty:ty),*) => {
        $(
            impl_bit!(@ $ty);
        )*
    }
}
impl_bit!(u8, u16, u32, u64);

fn uleb(bytes: &[u8]) -> IResult<&[u8], usize> {
    let (rem, prelude) = take_while(|b: u8| b.bit(7))(bytes)?;
    let (rem, finalizer) = byte(rem)?;

    let mut out = 0;
    let mut offset = 0;

    for byte in prelude {
        out |= (byte.bit_range(0..7) as usize) << offset;
        offset += 7;
    }

    out |= (finalizer as usize) << offset;

    Ok((rem, out))
}
fn opt_string(bytes: &[u8]) -> IResult<&[u8], Option<String>> {
    let (rem, first_byte) = byte(bytes)?;

    match first_byte {
        0x00 => Ok((rem, None)),
        0x0b => {
            let (rem, len) = uleb(rem)?;
            let (rem, string) = map_res(take(len), std::str::from_utf8)(rem)?;

            Ok((rem, Some(string.to_owned())))
        }
        _ => Err(NomErr::Error(NomError::new(bytes, NomErrorKind::Switch))),
    }
}

pub fn score(bytes: &[u8]) -> Result<(&[u8], Score), Error> {
    let (rem, is_legacy) = byte(bytes)?;
    let (rem, version) = int(rem)?;

    let (rem, unix_timestamp) = long(rem)?;
    let (rem, player_name) = opt_string(rem)?;

    let (rem, count_300) = short(rem)?;
    let (rem, count_100) = short(rem)?;
    let (rem, count_50) = short(rem)?;
    let (rem, count_geki) = short(rem)?;
    let (rem, count_katsu) = short(rem)?;
    let (rem, count_miss) = short(rem)?;

    let (rem, score) = long(rem)?;
    let (rem, max_combo) = short(rem)?;
    let (rem, mods) = int(rem)?;

    let (rem, slider_breaks) = short(rem)?;
    let (rem, pp) = single(rem)?;
    let (rem, ur) = single(rem)?;

    let (rem, hit_min) = single(rem)?;
    let (rem, hit_max) = single(rem)?;
    let (rem, stars_total) = single(rem)?;
    let (rem, stars_aim) = single(rem)?;
    let (rem, stars_speed) = single(rem)?;

    let (rem, speed_multiplier) = single(rem)?;
    let (rem, cs) = single(rem)?;
    let (rem, ar) = single(rem)?;
    let (rem, od) = single(rem)?;
    let (rem, hp) = single(rem)?;
    let (rem, max_possible_combo) = int(rem)?;
    let (rem, num_objects) = int(rem)?;
    let (rem, num_circles) = int(rem)?;

    let (rem, exp_mods) = opt_string(rem)?;

    let score = Score {
        is_legacy,
       
        version,
        unix_timestamp,
        player_name,
        count_300,
        count_100,
        count_50,
        count_geki,
        count_katsu,
        count_miss,
        score,
        max_combo,
        mods,
        slider_breaks,
        pp,
        ur,
        hit_min,
        hit_max,
        stars_total,
        stars_aim,
        stars_speed,
        speed_multiplier,
        cs,
        ar,
        od,
        hp,
        max_possible_combo,
        num_objects,
        num_circles,
        exp_mods,
    };

    Ok((rem, score))
}
impl ScoreList {
    
    pub fn from_bytes(bytes: &[u8]) -> Result<ScoreList, Error> {
        scores(bytes).map(|(_rem, scores)| scores)
    }

   
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<ScoreList, Error> {
        Self::from_bytes(&fs::read(path)?)
    }

}

fn scores(bytes: &[u8]) -> Result<(&[u8], ScoreList), Error> {
    let (rem, version) = int(bytes)?;
    let (mut rem, num_beatmaps) = int(rem)?;
    let mut beatmaps = Vec::with_capacity(num_beatmaps as usize);

    for _ in 0..num_beatmaps {
        let (rem_, beatmap_scores) = beatmap_scores(rem)?;
        beatmaps.push(beatmap_scores);
        rem = rem_;
    }

    let list = ScoreList { version,num_beatmaps, beatmaps };

    Ok((rem, list))
}

fn beatmap_scores(bytes: &[u8]) -> Result<(&[u8], BeatmapScores), Error> {
    let (rem, hash) = opt_string(bytes)?;
    let (mut rem, num_scores) = int(rem)?;
    let mut scores = Vec::with_capacity(num_scores as usize);

    for _ in 0..num_scores {
        let (rem_, replay) = score(rem)?;
        rem = rem_;
        scores.push(replay);
    }

    let scores = BeatmapScores { hash,num_scores, scores };

    Ok((rem, scores))
}
