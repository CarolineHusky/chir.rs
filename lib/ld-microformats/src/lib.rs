//! Crate for converting microformats into JSON-LD

use std::{cell::RefCell, collections::HashMap, io::Read, rc::Rc};

use microformats::types::{self, temporal};
use serde_json::{json, Map, Value};
use thiserror::Error;

#[cfg(test)]
mod tests;

/// Errors that can occur while serializing to JSON-LD
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum SerializeError {
    /// Returned when the microformats data does not contain the URL of the page it was on
    #[error("Microformats data does not include the URL")]
    MissingURL,
    /// Returned when the microformats parser returns an error
    #[error("Microformats parsing error: {0}")]
    Microformats(#[from] microformats::Error),
    /// Returned when serde_json fails to encode the value
    #[error("Serde_json serialization error: {0}")]
    SerdeJson(#[from] serde_json::Error),
}

/// Shorthand for `Result<T, SerializeError>`
type Result<T, E = SerializeError> = std::result::Result<T, E>;

/// Internal serializer for generating JSON-LD data
trait LDSerializer {
    /// Serializes an object into a JSON-LD value
    fn serialize(&self) -> Result<Value>;
    /// Returns a string key to sort by
    fn sort_key(&self) -> String;
}

/// Returns the context used for microformats
fn get_context(base: &str) -> Value {
    json!({
        "@version": 1.1,
        "@base": base,
        "vcard": "http://www.w3.org/2006/vcard/ns#",
        "relation": "http://purl.org/vocab/relationship/",
        "xhtml": "http://www.w3.org/1999/xhtml/vocab/",
        "mf": "http://microformats.org/profile#",
        "schema": "https://schema.org/"
    })
}

impl LDSerializer for String {
    fn serialize(&self) -> Result<Value> {
        Ok(json!(self))
    }
    #[cfg_attr(test, mutants::skip)]
    fn sort_key(&self) -> String {
        self.clone()
    }
}

impl LDSerializer for types::Class {
    fn serialize(&self) -> Result<Value> {
        format!("mf2:{}", self.to_string()).serialize()
    }
    #[cfg_attr(test, mutants::skip)]
    fn sort_key(&self) -> String {
        self.to_string()
    }
}

impl<T: LDSerializer> LDSerializer for Vec<T> {
    fn serialize(&self) -> Result<Value> {
        if self.len() == 1 {
            return self[0].serialize();
        }
        let mut sorted = self.iter().collect::<Vec<_>>();
        sorted.sort_unstable_by_key(|v| v.sort_key());
        Ok(Value::Array(
            sorted
                .into_iter()
                .map(LDSerializer::serialize)
                .collect::<Result<Vec<_>>>()?,
        ))
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.get(0).map_or_else(String::new, LDSerializer::sort_key)
    }
}

impl LDSerializer for types::PropertyValue {
    fn serialize(&self) -> Result<Value> {
        match self {
            Self::Plain(ref v) => v.serialize(),
            Self::Url(ref v) => v.serialize(),
            Self::Temporal(ref v) => match v {
                types::temporal::Value::Duration(ref v) => v.serialize(),
                types::temporal::Value::Timestamp(ref v) => v.serialize(),
            },
            Self::Fragment(ref v) => v.serialize(),
            Self::Image(ref v) => v.serialize(),
            Self::Item(ref v) => v.serialize(),
        }
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        match self {
            Self::Plain(ref v) => v.sort_key(),
            Self::Url(ref v) => v.sort_key(),
            Self::Temporal(ref v) => match v {
                types::temporal::Value::Duration(ref v) => v.sort_key(),
                types::temporal::Value::Timestamp(ref v) => v.sort_key(),
            },
            Self::Fragment(ref v) => v.sort_key(),
            Self::Image(ref v) => v.sort_key(),
            Self::Item(ref v) => v.sort_key(),
        }
    }
}

impl LDSerializer for temporal::Duration {
    fn serialize(&self) -> Result<Value> {
        Ok(json!({
            "@value": self.to_string(),
            "@type": "schema:Duration"
        }))
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.to_string()
    }
}

impl LDSerializer for temporal::Stamp {
    fn serialize(&self) -> Result<Value> {
        Ok(json!({
            "@value": self.to_string(),
            "@type": "schema:DateTime"
        }))
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.to_string()
    }
}

impl LDSerializer for types::Fragment {
    fn serialize(&self) -> Result<Value> {
        Ok(self.lang.as_ref().map_or_else(
            || Value::String(self.html.clone()),
            |lang| {
                json!({
                    "@value": &self.html,
                    "@language": lang
                })
            },
        ))
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.html.clone()
    }
}

impl LDSerializer for types::Image {
    fn serialize(&self) -> Result<Value> {
        Ok(json!({
            "@type": "schema:ImageObject",
            "schema:contentUrl": self.src,
            "schema:caption": self.alt
        }))
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.src.to_string()
    }
}

impl<T: LDSerializer> LDSerializer for RefCell<T> {
    fn serialize(&self) -> Result<Value> {
        self.borrow().serialize()
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.borrow().sort_key()
    }
}

impl<T: LDSerializer> LDSerializer for Rc<T> {
    fn serialize(&self) -> Result<Value> {
        (**self).serialize()
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        (**self).sort_key()
    }
}

impl LDSerializer for types::ValueKind {
    fn serialize(&self) -> Result<Value> {
        match self {
            Self::Url(ref v) => v.serialize(),
            Self::Plain(ref v) => v.serialize(),
        }
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        match self {
            Self::Url(ref v) => v.sort_key(),
            Self::Plain(ref v) => v.sort_key(),
        }
    }
}

impl<T: LDSerializer> LDSerializer for HashMap<String, T> {
    fn serialize(&self) -> Result<Value> {
        let hm = self
            .iter()
            .map(|(k, v)| {
                let key = format!("mf2:{}", k);
                let value = v.serialize()?;
                Ok((key, value))
            })
            .collect::<Result<Map<_, _>>>()?;
        Ok(Value::Object(hm))
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.keys().min().cloned().unwrap_or_default()
    }
}

impl LDSerializer for types::Item {
    fn serialize(&self) -> Result<Value> {
        let mut properties = match self.properties.serialize()? {
            Value::Object(hm) => hm,
            _ => unreachable!(),
        };
        properties.insert("@type".to_owned(), self.r#type.serialize()?);
        if !self.children.is_empty() {
            properties.insert("mf2:child".to_owned(), self.children.serialize()?);
        }
        if let Some(ref id) = self.id {
            properties.insert("@id".to_owned(), id.serialize()?);
        }
        if let Some(ref value) = self.value {
            properties.insert("mf2:value".to_owned(), value.serialize()?);
        }
        Ok(Value::Object(properties))
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.properties.sort_key()
    }
}

impl LDSerializer for types::Url {
    fn serialize(&self) -> Result<Value> {
        Ok(json!({
            "@id": self.to_string()
        }))
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.to_string()
    }
}

/// Temporary struct for mapping Relations to URLs
#[derive(Copy, Clone, Debug)]
struct UrlMap<'a> {
    /// The URL
    url: &'a types::Url,
    /// The language of the link target
    hreflang: Option<&'a String>,
    /// The media type associated with the link
    media: Option<&'a String>,
    /// The title of the link
    title: Option<&'a String>,
    /// The content type of the link
    r#type: Option<&'a String>,
    /// Text label associated with the link
    text: Option<&'a String>,
}

impl LDSerializer for UrlMap<'_> {
    fn serialize(&self) -> Result<Value> {
        let mut hm = Map::new();
        hm.insert("@id".to_owned(), self.url.to_string().serialize()?);
        if let Some(hreflang) = self.hreflang {
            hm.insert("@language".to_owned(), hreflang.serialize()?);
        }
        // TODO: figure out how to best encode media
        if let Some(media) = self.media {
            hm.insert("xhtml:media".to_owned(), media.serialize()?);
        }
        if let Some(title) = self.title {
            hm.insert("schema:name".to_owned(), title.serialize()?);
        }
        if let Some(mime) = self.r#type {
            hm.insert("schema:encodingFormat".to_owned(), mime.serialize()?);
        }
        if let Some(text) = self.text {
            hm.insert("schema:description".to_owned(), text.serialize()?);
        }
        Ok(Value::Object(hm))
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.url.sort_key()
    }
}

impl LDSerializer for types::Relations {
    fn serialize(&self) -> Result<Value> {
        let mut relations: HashMap<String, Vec<UrlMap<'_>>> = HashMap::new();
        for (url, relation) in &self.items {
            let map = UrlMap {
                url,
                hreflang: relation.hreflang.as_ref(),
                media: relation.media.as_ref(),
                title: relation.title.as_ref(),
                r#type: relation.r#type.as_ref(),
                text: relation.text.as_ref(),
            };
            for rel in &relation.rels {
                #[allow(clippy::option_if_let_else)] // Spurious
                if let Some(v) = relations.get_mut(rel) {
                    v.push(map);
                } else {
                    relations.insert(rel.clone(), vec![map]);
                }
            }
        }
        relations.serialize()
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.by_rels().sort_key()
    }
}

impl LDSerializer for types::Document {
    fn serialize(&self) -> Result<Value> {
        let mut graph = Vec::with_capacity(self.items.len() + 1);
        let mut sorted_items = self.items.clone();
        sorted_items.sort_unstable_by_key(&LDSerializer::sort_key);
        for item in sorted_items {
            graph.push(item.serialize()?);
        }
        if !self.rels.items.is_empty() {
            graph.push(self.rels.serialize()?);
        }
        let mut data = if graph.len() == 1 {
            match graph.get(0) {
                Some(Value::Object(ref v)) => v.clone(),
                _ => unreachable!(),
            }
        } else {
            let mut hm = Map::new();
            hm.insert("@graph".to_owned(), Value::Array(graph));
            hm
        };
        let url = self
            .url
            .as_ref()
            .map(ToString::to_string)
            .ok_or(SerializeError::MissingURL)?;
        data.insert("@context".to_owned(), get_context(&url));
        if let Some(ref lang) = self.lang {
            data.insert("@language".to_owned(), lang.serialize()?);
        }
        Ok(Value::Object(data))
    }
    #[cfg_attr(test, mutants::skip)]

    fn sort_key(&self) -> String {
        self.items.sort_key()
    }
}

/// The Microformats converter
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MicroformatsConverter {
    /// The decoded value
    json: Value,
}

impl MicroformatsConverter {
    /// Parses the microformats document
    ///
    /// # Errors
    /// This function returns an error if the document is invalid
    pub fn from_document(mut document: types::Document, url: types::Url) -> Result<Self> {
        if document.url.is_none() {
            document.url = Some(url);
        }
        Ok(Self {
            json: document.serialize()?,
        })
    }
    /// Parses the provided HTML
    ///
    /// # Errors
    /// This function returns an error if the parsing fails, or returns invalid data
    pub fn from_html(html: &str, url: types::Url) -> Result<Self> {
        Self::from_document(microformats::from_html(html, url.clone())?, url)
    }

    /// Reads the HTML from a reader and parses that
    ///
    /// # Errors
    /// This function returns an error if reading fails, the parsing fails, or the parsing returns invalid data
    pub fn from_reader(reader: impl Read, url: types::Url) -> Result<Self> {
        Self::from_document(microformats::from_reader(reader, url.clone())?, url)
    }

    /// Returns the JSON-LD as a [`Value`]
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // cannot evaluate destructors
    pub fn to_value(self) -> Value {
        self.json
    }

    /// Encodes the JSON-LD into its textual representation
    ///
    /// # Errors
    /// This function returns an error if the serialization fails.
    pub fn to_str(self) -> Result<String> {
        Ok(serde_json::to_string(&self.to_value())?)
    }
}
