pub mod literal;

use crate::grammar::Literal;
use nom::{named, complete};

named!(pub parse<&str, Literal>, complete!(literal::literal));
