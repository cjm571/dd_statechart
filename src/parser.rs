/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : parser.rs

Copyright (C) 2020 CJ McAllister
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software Foundation,
    Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

Purpose:
    This module leverages the roxmltree crate to parse a well-formed SCXML
    document into a StateChart object.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

extern crate roxmltree;

use std::{
    error::Error,
    fmt,
    fs,
    path::Path,
};


///////////////////////////////////////////////////////////////////////////////
//  Named Constants
///////////////////////////////////////////////////////////////////////////////

const VALID_SCXML_NAMESPACE: &str = "http://www.w3.org/2005/07/scxml";
const VALID_SCXML_VERSION: &str = "1.0";
const VALID_DATAMODEL: &str = "null";


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

pub struct Parser {

}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    InvalidScxmlNamespace(String),
    InvalidScxmlVersion(String),
    InvalidDataModel(String),
}
//TODO: Would rather use &'static str above, but having trouble doing this with roxmltree::Document

///////////////////////////////////////////////////////////////////////////////
//  Object Implementation
///////////////////////////////////////////////////////////////////////////////

impl Parser {
    pub fn parse<P: AsRef<Path>>(path: P) -> Result<(), ParserError> {
        // Read the file into a string for use by the XML parser
        let content = fs::read_to_string(path).unwrap();
        let parsed_content = roxmltree::Document::parse(content.as_str()).unwrap();

        // Ensure the document is properly structured
        Self::validate_structure(&parsed_content)?;

        //TODO: Gotta figure out how to deal with datamodels...

        Ok(())
    }

    
    /*  *  *  *  *  *  *  *\
     *  Helper Methods    *
    \*  *  *  *  *  *  *  */

    fn validate_structure(document: &roxmltree::Document) -> Result<(), ParserError> {
        let root_node = document.root_element();

        // Verify namespace
        if let Some(namespace) = root_node.tag_name().namespace() {
            if namespace != VALID_SCXML_NAMESPACE {
                return Err(ParserError::InvalidScxmlNamespace(String::from(namespace)));
            }
        }
        else {
            // Namespace was not specified
            return Err(ParserError::InvalidScxmlNamespace(String::new()))
        }

        // Verify version
        if let Some(version) = root_node.attribute("version") {
            if version != VALID_SCXML_VERSION {
                return Err(ParserError::InvalidScxmlVersion(String::from(version)));
            }
        }
        else {
            // Version not specified
            return Err(ParserError::InvalidScxmlVersion(String::new()))
        }

        // Verify datamodel
        if let Some(datamodel) = root_node.attribute("datamodel") {
            if datamodel != VALID_DATAMODEL {
                return Err(ParserError::InvalidDataModel(String::from(datamodel)));
            }
        }
        // NOTE: Not specifying datamodel is allowable - Null will be assumed.

        //TODO: More structural checks?
        
        Ok(())
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *     ParserError    *
\*  *  *  *  *  *  *  */

impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidScxmlNamespace(namespace) => {
                write!(f, "Invalid SCXML Namespace '{}', expected '{}'", namespace, VALID_SCXML_NAMESPACE)
            },
            Self::InvalidScxmlVersion(version) => {
                write!(f, "Invalid SCXML Version '{}', expected '{}'", version, VALID_SCXML_VERSION)
            },
            Self::InvalidDataModel(datamodel) => {
                write!(f, "Invalid SCXML Datamodel '{}', expected '{}'", datamodel, VALID_DATAMODEL)
            },
        }
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {

    use crate::parser::{
        Parser,
        ParserError,
    };


    #[test]
    fn namespace() {
        // Verify that the empty namespace is caught
        assert_eq!(
            Parser::parse("res/test_cases/namespace_empty.scxml"),
            Err(ParserError::InvalidScxmlNamespace(String::new())),
            "Failed to detect empty SCXML namespace"
        );

        // Verify that an invalid namespace is caught
        assert_eq!(
            Parser::parse("res/test_cases/namespace_invalid.scxml"),
            Err(ParserError::InvalidScxmlNamespace(String::from("http://invalid.namespace"))),
            "Failed to detect invalid SCXML namespace"
        );
    }

    #[test]
    fn version() {
        // Verify that the empty version is caught
        assert_eq!(
            Parser::parse("res/test_cases/version_empty.scxml"),
            Err(ParserError::InvalidScxmlVersion(String::new())),
            "Failed to detect empty SCXML version"
        );

        // Verify that an invalid version is caught
        assert_eq!(
            Parser::parse("res/test_cases/version_invalid.scxml"),
            Err(ParserError::InvalidScxmlVersion(String::from("2.0"))),
            "Failed to detect invalid SCXML version"
        );
    }

    #[test]
    fn datamodel() {
        // Verify that the empty datamodel is allowed
        assert_eq!(
            Parser::parse("res/test_cases/datamodel_empty.scxml"),
            Ok(()),
            "Failed to accept empty SCXML datamodel"
        );

        // Verify that an invalid datamodel is caught
        assert_eq!(
            Parser::parse("res/test_cases/datamodel_invalid.scxml"),
            Err(ParserError::InvalidDataModel(String::from("ecmascript"))),
            "Failed to detect invalid SCXML datamodel"
        );
    }
}
