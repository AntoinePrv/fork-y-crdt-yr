use extendr_api::prelude::*;

pub trait IntoExtendr<T> {
    fn extendr(self) -> extendr_api::Result<T>;
}

impl<T, E: ToString> IntoExtendr<T> for Result<T, E> {
    fn extendr(self) -> extendr_api::Result<T> {
        self.map_err(|e| Error::Other(e.to_string()))
    }
}

impl<T: IntoExtendr<Robj>> IntoExtendr<Robj> for Option<T> {
    fn extendr(self) -> extendr_api::Result<Robj> {
        match self {
            None => Ok(Robj::from(())),
            Some(v) => v.extendr(),
        }
    }
}

impl IntoExtendr<Robj> for yrs::Any {
    fn extendr(self) -> extendr_api::Result<Robj> {
        Ok(match self {
            yrs::Any::Null | yrs::Any::Undefined => Robj::from(()),
            yrs::Any::Bool(v) => Robj::from(v),
            yrs::Any::Number(v) => Robj::from(v),
            // R has no native i64; use i32 if it fits, otherwise error
            yrs::Any::BigInt(v) => {
                let v = i32::try_from(v)
                    .map_err(|_| Error::Other(format!("{v} does not fit in i32")))?;
                Robj::from(v)
            }
            yrs::Any::String(v) => Robj::from(v.as_ref()),
            yrs::Any::Buffer(v) => Raw::from_bytes(v.as_ref()).into(),
            yrs::Any::Array(v) => {
                let values: Vec<Robj> = v
                    .iter()
                    .map(|e| e.clone().extendr())
                    .collect::<extendr_api::Result<_>>()?;
                List::from_values(values).into()
            }
            yrs::Any::Map(v) => {
                let n = v.len();
                let mut keys = Strings::new(n);
                let mut values = List::new(n);
                for (i, (k, v)) in v.iter().enumerate() {
                    keys.set_elt(i, k.as_str().into());
                    values.set_elt(i, v.clone().extendr()?)?;
                }
                if n > 0 {
                    values.set_names(keys.as_slice())?;
                }
                values.into()
            }
        })
    }
}

impl IntoExtendr<Robj> for yrs::Out {
    fn extendr(self) -> extendr_api::Result<Robj> {
        match self {
            yrs::Out::Any(v) => v.extendr(),
            yrs::Out::YText(v) => Ok(crate::TextRef::from(v).into()),
            yrs::Out::YArray(v) => Ok(crate::ArrayRef::from(v).into()),
            yrs::Out::YMap(v) => Ok(crate::MapRef::from(v).into()),
            yrs::Out::YDoc(v) => Ok(crate::Doc::from(v).into()),
            yrs::Out::YXmlElement(_) => {
                Err(Error::Other("YXmlElement is not yet supported".to_string()))
            }
            yrs::Out::YXmlFragment(_) => Err(Error::Other(
                "YXmlFragment is not yet supported".to_string(),
            )),
            yrs::Out::YXmlText(_) => Err(Error::Other("YXmlText is not yet supported".to_string())),
            yrs::Out::UndefinedRef(_) => {
                Err(Error::Other("UndefinedRef is not supported".to_string()))
            }
        }
    }
}

impl IntoExtendr<Robj> for &yrs::types::Attrs {
    fn extendr(self) -> extendr_api::Result<Robj> {
        let n = self.len();
        let mut keys = Strings::new(n);
        let mut values = List::new(n);
        for (i, (k, v)) in self.iter().enumerate() {
            keys.set_elt(i, k.as_ref().into());
            values.set_elt(i, v.clone().extendr()?)?;
        }
        if n > 0 {
            values.set_names(keys.as_slice())?;
        }
        Ok(values.into())
    }
}

pub trait FromExtendr<T>: Sized {
    fn from_extendr(value: T) -> extendr_api::Result<Self>;
}

impl FromExtendr<Robj> for yrs::Any {
    fn from_extendr(robj: Robj) -> extendr_api::Result<Self> {
        if robj.is_null() {
            Ok(yrs::Any::Null)
        } else if let Some(v) = robj.as_bool() {
            Ok(yrs::Any::Bool(v))
        } else if let Some(v) = robj.as_integer() {
            Ok(yrs::Any::BigInt(v as i64))
        } else if let Some(v) = robj.as_real() {
            Ok(yrs::Any::Number(v))
        } else if let Some(v) = robj.as_str() {
            Ok(yrs::Any::String(std::sync::Arc::from(v)))
        } else if robj.is_raw() {
            let raw = Raw::try_from(robj).unwrap();
            Ok(yrs::Any::Buffer(std::sync::Arc::from(raw.as_slice())))
        } else if robj.is_list() {
            let list = robj.as_list().unwrap();
            if robj.names().is_some() {
                let map = std::collections::HashMap::<String, Robj>::try_from(list)
                    .unwrap()
                    .into_iter()
                    .map(|(k, v)| Ok((k, yrs::Any::from_extendr(v)?)))
                    .collect::<extendr_api::Result<_>>()?;
                Ok(yrs::Any::Map(std::sync::Arc::new(map)))
            } else {
                let arr = list
                    .values()
                    .map(yrs::Any::from_extendr)
                    .collect::<extendr_api::Result<Vec<_>>>()?;
                Ok(yrs::Any::Array(std::sync::Arc::from(arr.as_slice())))
            }
        } else {
            Err(Error::Other(format!(
                "Cannot convert {:?} to yrs::Any",
                robj.rtype()
            )))
        }
    }
}

impl FromExtendr<Robj> for yrs::types::Attrs {
    fn from_extendr(robj: Robj) -> extendr_api::Result<Self> {
        if !robj.is_list() {
            return Err(Error::Other(format!(
                "Expected a named list for Attrs, got {:?}",
                robj.rtype()
            )));
        }
        let list = robj.as_list().unwrap();
        // In R, a partially named list has names() return Some but with empty
        // strings for unnamed elements.
        let fully_named = list
            .names()
            .map(|mut ns| ns.all(|n| !n.is_empty()))
            .unwrap_or(false);
        if !fully_named {
            return Err(Error::Other(
                "Expected a fully named list for Attrs".to_string(),
            ));
        }
        list.names()
            .unwrap()
            .zip(list.values())
            .map(|(k, v)| Ok((std::sync::Arc::from(k), yrs::Any::from_extendr(v)?)))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::sync::Arc;

    use super::*;

    #[test]
    fn test_to_any_null() {
        extendr_api::test! {
            assert_eq!(yrs::Any::Null.extendr().unwrap(), r!(NULL));
            assert_eq!(yrs::Any::Undefined.extendr().unwrap(), r!(NULL));
        }
    }

    #[test]
    fn test_to_any_bool() {
        extendr_api::test! {
            assert_eq!(yrs::Any::Bool(true).extendr().unwrap(), r!(true));
            assert_eq!(yrs::Any::Bool(false).extendr().unwrap(), r!(false));
        }
    }

    #[test]
    fn test_to_any_number() {
        extendr_api::test! {
            assert_eq!(yrs::Any::Number(1.5).extendr().unwrap(), r!(1.5));
        }
    }

    #[test]
    fn test_to_any_bigint() {
        extendr_api::test! {
            assert_eq!(yrs::Any::BigInt(42).extendr().unwrap(), r!(42i32));
            assert!(yrs::Any::BigInt(i64::MAX).extendr().is_err());
        }
    }

    #[test]
    fn test_to_any_string() {
        extendr_api::test! {
            assert_eq!(yrs::Any::String(Arc::from("hello")).extendr().unwrap(), r!("hello"));
        }
    }

    #[test]
    fn test_to_any_buffer() {
        extendr_api::test! {
            let buf: Arc<[u8]> = Arc::from([1u8, 2, 3].as_slice());
            let robj = yrs::Any::Buffer(buf).extendr().unwrap();
            assert!(robj.is_raw());
            assert_eq!(robj.len(), 3);
        }
    }

    #[test]
    fn test_to_any_array() {
        extendr_api::test! {
            let arr: Arc<[yrs::Any]> = Arc::from([yrs::Any::Bool(true), yrs::Any::Number(1.0)].as_slice());
            let robj = yrs::Any::Array(arr).extendr().unwrap();
            assert!(robj.is_list());
            assert_eq!(robj.len(), 2);
        }
    }

    #[test]
    fn test_to_any_map() {
        extendr_api::test! {
            let map: Arc<HashMap<String, yrs::Any>> =
                Arc::new(HashMap::from([("x".to_string(), yrs::Any::Number(1.0))]));
            let robj = yrs::Any::Map(map).extendr().unwrap();
            assert!(robj.is_list());
            assert_eq!(robj.len(), 1);
        }
    }

    #[test]
    fn test_to_out_any() {
        extendr_api::test! {
            assert_eq!(yrs::Out::Any(yrs::Any::Null).extendr().unwrap(), r!(NULL));
            assert_eq!(yrs::Out::Any(yrs::Any::Number(1.5)).extendr().unwrap(), r!(1.5));
        }
    }

    #[test]
    fn test_to_out_ytext() {
        extendr_api::test! {
            let doc = yrs::Doc::new();
            let text_ref = doc.get_or_insert_text("test");
            let robj = yrs::Out::YText(text_ref).extendr().unwrap();
            assert!(robj.is_external_pointer());
        }
    }

    #[test]
    fn test_to_out_yarray() {
        extendr_api::test! {
            let doc = yrs::Doc::new();
            let array_ref = doc.get_or_insert_array("test");
            let robj = yrs::Out::YArray(array_ref).extendr().unwrap();
            assert!(robj.is_external_pointer());
        }
    }

    #[test]
    fn test_to_out_ymap() {
        extendr_api::test! {
            let doc = yrs::Doc::new();
            let map_ref = doc.get_or_insert_map("test");
            let robj = yrs::Out::YMap(map_ref).extendr().unwrap();
            assert!(robj.is_external_pointer());
        }
    }

    #[test]
    fn test_to_out_ydoc() {
        extendr_api::test! {
            let subdoc = yrs::Doc::new();
            let robj = yrs::Out::YDoc(subdoc).extendr().unwrap();
            assert!(robj.is_external_pointer());
        }
    }

    #[test]
    fn test_to_attrs() {
        extendr_api::test! {
            let attrs: yrs::types::Attrs = HashMap::from([
                (Arc::from("bold"), yrs::Any::Bool(true)),
                (Arc::from("color"), yrs::Any::String(Arc::from("red"))),
            ]);
            let robj = attrs.extendr().unwrap();
            assert!(robj.is_list());
            assert_eq!(robj.len(), 2);
            assert!(robj.names().is_some());
        }
    }

    #[test]
    fn test_to_attrs_empty() {
        extendr_api::test! {
            let attrs: yrs::types::Attrs = HashMap::new();
            let robj = attrs.extendr().unwrap();
            assert!(robj.is_list());
            assert_eq!(robj.len(), 0);
        }
    }

    #[test]
    fn test_from_any_null() {
        extendr_api::test! {
            assert!(matches!(yrs::Any::from_extendr(r!(NULL)).unwrap(), yrs::Any::Null));
        }
    }

    #[test]
    fn test_from_any_bool() {
        extendr_api::test! {
            assert!(matches!(yrs::Any::from_extendr(r!(true)).unwrap(), yrs::Any::Bool(true)));
            assert!(matches!(yrs::Any::from_extendr(r!(false)).unwrap(), yrs::Any::Bool(false)));
        }
    }

    #[test]
    fn test_from_any_integer() {
        extendr_api::test! {
            assert!(matches!(yrs::Any::from_extendr(r!(42i32)).unwrap(), yrs::Any::BigInt(42)));
        }
    }

    #[test]
    fn test_from_any_number() {
        extendr_api::test! {
            assert!(matches!(yrs::Any::from_extendr(r!(1.5)).unwrap(), yrs::Any::Number(v) if v == 1.5));
        }
    }

    #[test]
    fn test_from_any_string() {
        extendr_api::test! {
            assert!(matches!(yrs::Any::from_extendr(r!("hello")).unwrap(), yrs::Any::String(ref s) if s.as_ref() == "hello"));
        }
    }

    #[test]
    fn test_from_any_buffer() {
        extendr_api::test! {
            let robj: Robj = Raw::from_bytes(&[1, 2, 3]).into();
            assert!(matches!(yrs::Any::from_extendr(robj).unwrap(), yrs::Any::Buffer(ref b) if b.len() == 3));
        }
    }

    #[test]
    fn test_from_any_array() {
        extendr_api::test! {
            let robj: Robj = List::from_values([r!(true), r!(1.5)]).into();
            assert!(matches!(yrs::Any::from_extendr(robj).unwrap(), yrs::Any::Array(ref a) if a.len() == 2));
        }
    }

    #[test]
    fn test_from_any_map() {
        extendr_api::test! {
            let robj: Robj = List::from_names_and_values(["x"], [r!(1.5)]).unwrap().into();
            assert!(matches!(yrs::Any::from_extendr(robj).unwrap(), yrs::Any::Map(ref m) if m.len() == 1));
        }
    }

    #[test]
    fn test_from_attrs() {
        extendr_api::test! {
            let robj: Robj = List::from_names_and_values(["bold", "size"], [r!(true), r!(12.0)]).unwrap().into();
            let attrs = yrs::types::Attrs::from_extendr(robj).unwrap();
            assert_eq!(attrs.len(), 2);
            assert!(matches!(attrs.get("bold"), Some(yrs::Any::Bool(true))));
            assert!(matches!(attrs.get("size"), Some(yrs::Any::Number(v)) if v == &12.0));
        }
    }

    #[test]
    fn test_from_attrs_empty() {
        extendr_api::test! {
            let robj: Robj = List::from_names_and_values([] as [&str; 0], [] as [Robj; 0]).unwrap().into();
            let attrs = yrs::types::Attrs::from_extendr(robj).unwrap();
            assert_eq!(attrs.len(), 0);
        }
    }

    #[test]
    fn test_from_attrs_not_a_list() {
        extendr_api::test! {
            assert!(yrs::types::Attrs::from_extendr(r!(42.0)).is_err());
        }
    }

    #[test]
    fn test_from_attrs_unnamed_list() {
        extendr_api::test! {
            let robj: Robj = List::from_values([r!(true)]).into();
            assert!(yrs::types::Attrs::from_extendr(robj).is_err());
        }
    }

    #[test]
    fn test_from_attrs_partially_named_list() {
        extendr_api::test! {
            // "bold" is named but second element is not (name = "")
            let robj: Robj = List::from_names_and_values(["bold", ""], [r!(true), r!(1.0)]).unwrap().into();
            assert!(yrs::types::Attrs::from_extendr(robj).is_err());
        }
    }
}
