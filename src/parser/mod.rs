pub mod literal;

use crate::grammar::Literal;
use nom::{named, call, complete};

named!(pub Parser<&str, Literal>, complete!(literal::LITERAL));
