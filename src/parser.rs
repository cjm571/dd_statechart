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
extern crate uuid;

use std::{
    error::Error,
    fmt,
    fs,
    path::Path,
};

use crate::{
    StateChart,
    StateChartBuilder,
    event::{
        Event,
        EventError,
    },
    state::{
        StateBuilder,
        StateId,
    },
    transition::{
        Transition,
        TransitionBuilder,
        TransitionBuilderError,
    },
};

use uuid::Uuid;


///////////////////////////////////////////////////////////////////////////////
//  Named Constants
///////////////////////////////////////////////////////////////////////////////

const VALID_SCXML_NAMESPACE: &str = "http://www.w3.org/2005/07/scxml";
const VALID_SCXML_VERSION: &str = "1.0";
const VALID_DATAMODEL: &str = "ecmascript";


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

//TODO: Investigate whether it's necessary for this to be an object
pub struct Parser {
    statechart_builder: StateChartBuilder,
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    // Primary
    InvalidScxmlNamespace(String),
    InvalidScxmlVersion(String),
    InvalidDataModel(String),
    StateHasNoId(roxmltree::NodeId),

    // Wrappers
    EventError(EventError),
    TransitionBuilderError(TransitionBuilderError),
}

pub enum ValidElementClass {
    State,
    DataModel,
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementation
///////////////////////////////////////////////////////////////////////////////

impl Parser {
    pub fn parse<P: AsRef<Path>>(&mut self, path: P) -> Result<StateChart, ParserError> {
        // Read the file into a string for use by the XML parser
        //OPT: *DESIGN* Do something smarter than unwrap here
        let content = fs::read_to_string(path).unwrap();
        let parsed_content = roxmltree::Document::parse(content.as_str()).unwrap();

        // Ensure the document is properly structured
        Self::validate_structure(&parsed_content)?;

        //TODO: Handle attributes of <scxml> root element

        // Get StateChart name from <scxml>, if specified
        if let Some(chart_name) = parsed_content.root_element().attribute("name") {
            self.statechart_builder = StateChartBuilder::new(String::from(chart_name));
        }
        else {
            // Name was not specified, generate a UUID
            self.statechart_builder = StateChartBuilder::new(Uuid::new_v4().to_string());
        }

        // Begin iterating through the parsed content
        for child in parsed_content.root_element().children() {
            // Skip text and comment nodes
            if !child.is_comment() && !child.is_text() {
                // Make the appropriate callback for the type of element
                Self::parse_element(child, &mut self.statechart_builder)?;
            }
        }

        //OPT: *DESIGN* Do something smarter than unwrap here
        Ok(self.statechart_builder.build().unwrap())
    }


    /*  *  *  *  *  *  *  *\
        *  Helper Methods    *
    \*  *  *  *  *  *  *  */

    fn validate_structure(document: &roxmltree::Document) -> Result<(), ParserError> {
        /* <scxml> Root Element Checks */
        let root_node = document.root_element();

        // Verify namespace
        if let Some(namespace) = root_node.tag_name().namespace() {
            if namespace != VALID_SCXML_NAMESPACE {
                return Err(ParserError::InvalidScxmlNamespace(String::from(namespace)));
            }
        }
        else {
            // Namespace was not specified
            return Err(ParserError::InvalidScxmlNamespace(String::new()));
        }

        // Verify version
        if let Some(version) = root_node.attribute("version") {
            if version != VALID_SCXML_VERSION {
                return Err(ParserError::InvalidScxmlVersion(String::from(version)));
            }
        }
        else {
            // Version not specified
            return Err(ParserError::InvalidScxmlVersion(String::new()));
        }

        // Verify datamodel
        if let Some(datamodel) = root_node.attribute("datamodel") {
            if datamodel != VALID_DATAMODEL {
                return Err(ParserError::InvalidDataModel(String::from(datamodel)));
            }
        }
        // NOTE: Not specifying datamodel is allowable - ECMAScript will be assumed.

        //TODO: More structural checks?
        
        Ok(())
    }

    fn parse_element(element: roxmltree::Node, statechart_builder: &mut StateChartBuilder) -> Result<(), ParserError> {
        // Match the element class to the appropriate handling function
        match ValidElementClass::from(element.tag_name().name()) {
            ValidElementClass::DataModel    => Self::parse_datamodel(element, statechart_builder),
            ValidElementClass::State        => Self::parse_state(element, statechart_builder)?,
        }

        Ok(())
    }

    fn parse_datamodel(element: roxmltree::Node, statechart_builder: &mut StateChartBuilder) {
        // Collect all <data> children
        for child in element.children() {
            // Skip text and comment nodes
            if !child.is_comment() && !child.is_text() {
                // Add data item to the System Variables of the StateChart
                let id = String::from(child.attribute("id").unwrap());
                let value = String::from(child.attribute("expr").unwrap());
                //TODO: Need smart handling of all possible 'expr' values
                //      Maybe some fancy enum shenanigans?
                if let Ok(converted_value) = value.parse::<u32>() {
                    statechart_builder.sys_vars.set_data_member(
                        id,
                        converted_value);
                }
                else if value == "true" {
                    statechart_builder.sys_vars.set_data_member(
                        id,
                        1);
                }
                else if value == "false" {
                    statechart_builder.sys_vars.set_data_member(
                        id,
                        0);
                }
                else {
                    //TODO: Come up with better marker for unrecognized expr type
                    statechart_builder.sys_vars.set_data_member(
                        id,
                        0xDEADBEEF);
                }
            }
        }
    }

    //TODO: Do something smarter than these unwrap()s
    fn parse_state(element: roxmltree::Node, statechart_builder: &mut StateChartBuilder) -> Result<(), ParserError> {
        let mut initial_specified = false;

        // Get ID attribute to create StateBuilder
        let state_id;
        if let Some(id) = element.attribute("id") {
            state_id = id;
        }
        else {
            return Err(ParserError::StateHasNoId(element.id()));
        }
        let mut state_builder = StateBuilder::new(String::from(state_id));

        // Check for initial attribute
        if let Some(initial) = element.attribute("initial") {
            state_builder = state_builder.initial(String::from(initial));
            initial_specified = true;
        }

        // Iterate through children, adding transitions or parsing substates
        for child in element.children() {
            // Skip text and comment nodes
            if !child.is_comment() && !child.is_text() {
                //TODO: Handle <onentry>

                //TODO: Handle <onexit>

                // Handle <transition>
                if child.tag_name().name() == "transition" {
                    state_builder = state_builder.transition(Self::parse_transition(child, String::from(state_id)).unwrap()).unwrap();
                }

                // Handle <initial>, if not already specified
                if !initial_specified {

                }

                //TODO: Handle <state>

                //FEAT: Handle <parallel>

                //FEAT: Handle <history>

                //FEAT: Handle <datamodel>
            }
        }

        let state = state_builder.build().unwrap();
        statechart_builder.state(state).unwrap();

        Ok(())
    }

    //TODO: Do something smarter than these unwrap()s
    fn parse_transition(element: roxmltree::Node, parent_id: StateId) -> Result<Transition, ParserError> {
        let mut transition_builder = TransitionBuilder::new(parent_id);

        // Collect Events
        if let Some(event_ids) = element.attribute("event") {
            // Tokenize event list
            let event_id_list = event_ids.split_whitespace();
            for event_id in event_id_list {
                // Create an event and add it to the builder
                let event = Event::from(event_id)?;
                transition_builder = transition_builder.event(event)?;
            }
        }

        //TODO: Parse guard condition
        //TODO: Parse conditional statements into functions

        // Set target State
        if let Some(target_id) = element.attribute("target") {
            transition_builder = transition_builder.target_id(String::from(target_id)).unwrap();
        }
        

        Ok(transition_builder.build())
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////


impl Default for Parser {
    fn default() -> Self {
        Self {
            statechart_builder: StateChartBuilder::new(String::from("")),
        }
    }
}

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
            Self::StateHasNoId(node_id) => {
                write!(f, "Node ID {:?} is a <state> element with no 'id' attribute", node_id)
            },
            Self::EventError(event_error) => {
                write!(f, "EventError '{:?}' encountered while parsing", event_error)
            },
            Self::TransitionBuilderError(tb_error) => {
                write!(f, "TransitionBuilderError '{:?}' encountered while parsing", tb_error)
            }
        }
    }
}

impl From<EventError> for ParserError {
    fn from(src: EventError) -> Self {
        Self::EventError(src)
    }
}

impl From<TransitionBuilderError> for ParserError {
    fn from(src: TransitionBuilderError) -> Self {
        Self::TransitionBuilderError(src)
    }
}


/*  *  *  *  *  *  *  *\
 *  ValidElementClass *
\*  *  *  *  *  *  *  */

impl From<&str> for ValidElementClass {
    fn from(src: &str) -> Self {
        match src {
            "datamodel" => Self::DataModel,
            "state"     => Self::State,
            _           => panic!("Invalid ValidElementClass conversion, '{}'", src),
            //OPT: *DESIGN* Probably do something smarter than panic here
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
        ParserError
    };


    #[test]
    fn namespace() {
        let mut parser_a = Parser::default();
        let mut parser_b = Parser::default();

        // Verify that the empty namespace is caught
        assert_eq!(
            parser_a.parse("res/test_cases/namespace_empty.scxml"),
            Err(ParserError::InvalidScxmlNamespace(String::new())),
            "Failed to detect empty SCXML namespace"
        );

        // Verify that an invalid namespace is caught
        assert_eq!(
            parser_b.parse("res/test_cases/namespace_invalid.scxml"),
            Err(ParserError::InvalidScxmlNamespace(String::from("http://invalid.namespace"))),
            "Failed to detect invalid SCXML namespace"
        );
    }

    #[test]
    fn version() {
        let mut parser_a = Parser::default();
        let mut parser_b = Parser::default();

        // Verify that the empty version is caught
        assert_eq!(
            parser_a.parse("res/test_cases/version_empty.scxml"),
            Err(ParserError::InvalidScxmlVersion(String::new())),
            "Failed to detect empty SCXML version"
        );

        // Verify that an invalid version is caught
        assert_eq!(
            parser_b.parse("res/test_cases/version_invalid.scxml"),
            Err(ParserError::InvalidScxmlVersion(String::from("2.0"))),
            "Failed to detect invalid SCXML version"
        );
    }

    #[test]
    fn datamodel() {
        let mut parser_a = Parser::default();
        let mut parser_b = Parser::default();

        // Verify that the empty datamodel is allowed
        assert_eq!(
            parser_a.parse("res/test_cases/datamodel_empty.scxml").is_ok(),
            true,
            "Failed to accept empty SCXML datamodel"
        );

        // Verify that an invalid datamodel is caught
        assert_eq!(
            parser_b.parse("res/test_cases/datamodel_invalid.scxml"),
            Err(ParserError::InvalidDataModel(String::from("someotherscript"))),
            "Failed to detect invalid SCXML datamodel"
        );
    }

    #[test]
    fn no_id_state() {
        let mut parser = Parser::default();

        assert_eq!(
            parser.parse("res/test_cases/state_no_id.scxml"),
            Err(ParserError::StateHasNoId(roxmltree::NodeId::new(3))),
            "Failed to detect <state> with no `id`"
        );
    }

    #[test]
    fn microwave() {
        let mut parser = Parser::default();

        // Parse microwave example scxml doc
        parser.parse("res/examples/01_microwave.scxml").unwrap();
    }
}
