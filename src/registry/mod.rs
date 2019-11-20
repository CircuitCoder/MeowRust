use im_rc::hashmap::HashMap;
use crate::grammar::item::*;
use failure::Error;
use failure::format_err;

#[derive(Clone)]
pub struct RegistryNode<'a> {
  item: &'a Item<'a>,
  children: HashMap<&'a str, RegistryNode<'a>>,
}

impl<'a> RegistryNode<'a> {
  fn create<'f>(item: &'a Item<'a>) -> Result<(&'a str, Self), Error> {
    let (name, children) = match item.value {
      ItemValue::Module(name, ref items) => {
        let mut children = HashMap::new();
        if let Some(inner) = items {
          for item in inner.iter() {
            let (cname, cnode) = RegistryNode::create(item)?;
            children.insert(cname, cnode);
          }
        }
        (name, children)
      },
      ItemValue::Func(ref f) => (f.name, HashMap::new()),
      ItemValue::TypeAlias(ref t) => (t.name, HashMap::new()),
      // TODO: supports use decl

      _ => return Err(format_err!("Item not supported")),
    };

    Ok((name, RegistryNode::<'a> {
      item,
      children,
    }))
  }
}
