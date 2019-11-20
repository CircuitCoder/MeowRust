// TODO: support MacroItem

use super::attr::Attr;
use super::SimplePath;
use super::*;
use super::r#type::Type;

#[derive(Debug, Clone)]
pub struct Item<'a> {
  pub attr: Option<Attr<'a>>,
  pub value: ItemValue<'a>,
}

#[derive(Debug, Clone)]
pub enum ItemValue<'a> {
  /**
   * Module scope
   */
  Module(&'a str, Option<Vec<Item<'a>>>),
  // We don't have a crate system, so no extern crate

  /**
   * use foo::{bar, a::b::c};
   */
  UseDecl(Vec<(SimplePath<'a>, Option<&'a str>)>),

  /**
   * Function decl/impl
   */
  Func(Func<'a>),

  /**
   * Type alias
   * 
   * Currently only supports solid types
   * 
   * TODO: supports where clause in type alises
   * TODO: generics
   */
  TypeAlias(TypeAlias<'a>),
}

#[derive(Debug, Clone)]
pub struct Func<'a> {
  pub name: &'a str,
  // TODO: supports qualifiers and ABI

  pub params: Vec<(Pat<'a>, Type<'a>)>,
  pub ret: Type<'a>,

  pub generics: Vec<GenericParam<'a>>,
  pub r#where: Vec<(Type<'a>, Vec<TypePath<'a>>)>,

  pub body: Expr<'a>, // Asserts to be an block expression
}

#[derive(Debug, Clone)]
pub struct TypeParam<'a> {
  pub name: &'a str,
  pub trait_bounds: Vec<TypePath<'a>>,
  pub default: Option<Type<'a>>,
}

#[derive(Debug, Clone)]
pub enum Lifetime<'a> {
  Named(&'a str),
  Static,
  Unnamed,
}

#[derive(Debug, Clone)]
pub struct LifetimeParam<'a> {
    pub name: &'a str,
    pub lifetime_bounds: Vec<Lifetime<'a>>,
}

#[derive(Debug, Clone)]
pub enum GenericParam<'a> {
  Lifetime(LifetimeParam<'a>),
  Type(TypeParam<'a>),
}

#[derive(Debug, Clone)]
pub struct TypeAlias<'a> {
  pub name: &'a str,

  pub generics: Vec<GenericParam<'a>>,
  pub r#where: Vec<(Type<'a>, Vec<TypePath<'a>>)>,

  pub value: Type<'a>,
}
