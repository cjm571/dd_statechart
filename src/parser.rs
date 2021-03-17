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
    io::Read,
};

use crate::{
    StateChart,
    StateChartBuilder,
    StateChartBuilderError,
    datamodel::{
        DataModelError,
        SystemVariables,
    },
    event::{
        Event,
        EventError,
    },
    executable_content::ExecutableContent,
    interpreter::{
        Interpreter,
        InterpreterError,
    },
    registry::RegistryError,
    state::{
        State,
        StateBuilder,
        StateBuilderError,
        StateId,
    },
    transition::{
        Transition,
        TransitionBuilder,
        TransitionBuilderError,
    }
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

#[derive(Debug, PartialEq)]
pub struct Parser {
    path:               String,
    content:            String,
    statechart_builder: StateChartBuilder,
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    AssignWithoutLocation(
        String  /* Stringified XML Tree Node */
    ),
    AssignWithoutExpr(
        String  /* Stringified XML Tree Node */
    ),
    InitialNodeChildless(
        String  /* Stringified XML Tree Node */
    ),
    InitialTransitionTargetless(
        String  /* Stringified XML Tree Node */
    ),
    InvalidScxmlNamespace(
        String  /* Invalid namespace */
    ),
    InvalidScxmlVersion(
        String  /* Invalid version */
    ),
    InvalidDataModel(
        String  /* Invalid data model name */
    ),
    InvalidDataItem(
        String  /* Stringified XML Tree Node */
    ),
    StateHasNoId(
        String  /* Stringified XML Tree Node */
    ),

    // Wrappers
    DataModelError(DataModelError),
    EventError(EventError),
    InterpreterError(InterpreterError),
    IoError(std::io::ErrorKind),
    RegistryError(RegistryError),
    RoxmlTreeError(roxmltree::Error),
    StateBuilderError(StateBuilderError),
    StateChartBuilderError(StateChartBuilderError),
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
    /// Creates a new Parser object for the SCXML file at the given path.
    pub fn new(path: &str) -> Result<Self, ParserError> {
        // Attempt to open the file at the given path
        let mut file = fs::File::open(path)?;
        let mut file_contents = String::new();
        file.read_to_string(&mut file_contents)?;
        
        Ok(
            Self {
                path:               String::from(path),
                content:            file_contents,
                statechart_builder: StateChartBuilder::default(),
            }
        )
    }

    pub fn parse(&mut self) -> Result<StateChart, ParserError> {
        // Parse contents of the SCXML file into an roxmltree::Document
        let parsed_content = roxmltree::Document::parse(self.content.as_str())?;

        // Ensure the document is properly structured
        Self::validate_structure(&parsed_content)?;

        // Get Initial State from <scxml>, if specified
        if let Some(initial) = parsed_content.root_element().attribute("initial") {
            self.statechart_builder.initial(String::from(initial));
        }

        // Get StateChart name from <scxml>, if specified
        if let Some(chart_name) = parsed_content.root_element().attribute("name") {
            self.statechart_builder.name(chart_name);
        }
        else {
            // Name was not specified, generate a UUID
            self.statechart_builder.name(Uuid::new_v4().to_string().as_str());
        }

        // Begin iterating through the parsed content
        for child in parsed_content.root_element().children() {
            // Skip text and comment nodes
            if !child.is_comment() && !child.is_text() {
                // Make the appropriate callback for the type of element
                Self::parse_element(child, &mut self.statechart_builder)?;
            }
        }

        Ok(self.statechart_builder.build()?)
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
        
        Ok(())
    }

    fn parse_element(element: roxmltree::Node, statechart_builder: &mut StateChartBuilder) -> Result<(), ParserError> {
        // Match the element class to the appropriate handling function
        match ValidElementClass::from(element.tag_name().name()) {
            ValidElementClass::DataModel => {
                Self::parse_datamodel(element, &mut statechart_builder.sys_vars)?;
            },
            ValidElementClass::State => {
                statechart_builder.state(Self::parse_state(element)?)?;
            },
        }

        Ok(())
    }

    fn parse_datamodel(element: roxmltree::Node, sys_vars: &mut SystemVariables) -> Result<(), ParserError> {
        // Collect all <data> children
        for child in element.children() {
            // Skip text and comment nodes
            if !child.is_comment() && !child.is_text() {
                if let (Some(id), Some(expr_str)) = (child.attribute("id"), child.attribute("expr")) {
                    // Interpret value expression and insert into data map
                    let interpreter = Interpreter::new(expr_str);
                    let expr_value = interpreter.interpret(sys_vars)?;
                    sys_vars.set_data_member(id.to_string(), expr_value);
                }
                else {
                    return Err(ParserError::InvalidDataItem(format!("{:?}", element)));
                }
            }
        }

        Ok(())
    }

    fn parse_state(element: roxmltree::Node) -> Result<State, ParserError> {
        let mut initial_specified = false;

        // Get ID attribute to create StateBuilder
        let state_id;
        if let Some(id) = element.attribute("id") {
            state_id = id;
        }
        else {
            return Err(ParserError::StateHasNoId(format!("{:?}", element)));
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
                    state_builder = state_builder.transition(
                        Self::parse_transition(
                            child, 
                            String::from(state_id),
                        )?
                    )?;
                }

                // Handle <initial>, if not already specified
                if !initial_specified && child.tag_name().name() == "initial" {
                    // Get the target of the internal <transition>'s target
                    // If the internal <transition> does not exist, the document is not well-formed
                    for grandchild in child.children() {
                        // Skip text and comment nodes
                        if !grandchild.is_comment() && !grandchild.is_text() {
                            if let Some(initial) = grandchild.attribute("target") {
                                state_builder = state_builder.initial(String::from(initial));
                                initial_specified = true;
                            }
                            else {
                                return Err(ParserError::InitialTransitionTargetless(format!("{:?}", element)));
                            }
                        }
                    }
                    // Ensure that a <transition> element was found, otherwise the document is not well-formed
                    if !initial_specified {
                        return Err(ParserError::InitialNodeChildless(format!("{:?}", element)));
                    }
                }

                // Handle <state>
                if child.tag_name().name() == "state" {
                    state_builder = state_builder.substate(Self::parse_state(child)?)?;
                }

                //FEAT: Handle <parallel>

                //FEAT: Handle <history>

                //FEAT: Handle <datamodel>
            }
        }

        Ok(state_builder.build()?)
    }

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

        // Parse guard condition
        if let Some(cond_str) = element.attribute("cond") {            
            transition_builder = transition_builder.cond(cond_str)?;
        }

        // Set target State
        if let Some(target_id) = element.attribute("target") {
            transition_builder = transition_builder.target_id(String::from(target_id))?;
        }

        //TODO: Refactor all child-check loops to use .filter()
        // Check for Executable Content children, skipping comments and text
        for child in element.children().filter(|v| !v.is_comment() && !v.is_text()) {
            // Handle <assign>
            if child.tag_name().name() == "assign" {
                if let Some(location) = child.attribute("location") {
                    // 'expr' may be either an attribute of 'assign', or a child
                    if let Some(expr) = child.attribute("expr") {    
                        let assignment = ExecutableContent::Assign(location.to_string(), expr.to_string());
                        transition_builder = transition_builder.executable_content(assignment)?;
                    }
                    else if child.has_children() {
                        unimplemented!("Multi-line expression are not yet supported");
                    }
                    // 'expr' was not specified, this is an error
                    else {
                        return Err(ParserError::AssignWithoutExpr(format!("{:?}", child)));
                    }
                }
                // 'location' was not specified, this is an error
                else {
                    return Err(ParserError::AssignWithoutLocation(format!("{:?}", child)));
                }
            }
        }

        Ok(transition_builder.build())
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
            Self::AssignWithoutLocation(parent_assign) => {
                write!(f, "Assignment '{}' is missing a 'location'", parent_assign)
            },
            Self::AssignWithoutExpr(parent_assign) => {
                write!(f, "Assignment '{}' is missing an 'expr'", parent_assign)
            },
            Self::InitialNodeChildless(parent_state) => {
                write!(f, "State '{}' contains a childless <initial> element", parent_state)
            },
            Self::InitialTransitionTargetless(parent_state) => {
                write!(f, "State '{}' contains an <initial> element with a targetless <transition>", parent_state)
            },
            Self::InvalidScxmlNamespace(namespace) => {
                write!(f, "Invalid SCXML Namespace '{}', expected '{}'", namespace, VALID_SCXML_NAMESPACE)
            },
            Self::InvalidScxmlVersion(version) => {
                write!(f, "Invalid SCXML Version '{}', expected '{}'", version, VALID_SCXML_VERSION)
            },
            Self::InvalidDataModel(datamodel) => {
                write!(f, "Invalid SCXML Datamodel '{}', expected '{}'", datamodel, VALID_DATAMODEL)
            },
            Self::InvalidDataItem(node_id) => {
                write!(f, "Invalid Data Item '{:?}'", node_id)
            },
            Self::StateHasNoId(node_id) => {
                write!(f, "Node ID {:?} is a <state> element with no 'id' attribute", node_id)
            },

            //FIXME: Align other modules with this style
            // Wrappers
            Self::DataModelError(data_error) => {
                write!(f, "DataModelError '{:?}' encountered while parsing", data_error)
            },
            Self::EventError(event_error) => {
                write!(f, "EventError '{:?}' encountered while parsing", event_error)
            },
            Self::InterpreterError(interp_error) => {
                write!(f, "InterpreterError '{:?}' encountered while parsing", interp_error)
            },
            Self::IoError(io_err_kind) => {
                write!(f, "IoError of kind '{:?}' encountered when attempting to create Parser object", io_err_kind)
            },
            Self::RegistryError(reg_error) => {
                write!(f, "RegistryError '{:?}' encountered while parsing", reg_error)
            },
            Self::RoxmlTreeError(roxmltree_error) => {
                write!(f, "roxmltree::Error '{:?}' encountered while parsing", roxmltree_error)
            },
            Self::StateBuilderError(sb_error) => {
                write!(f, "StateBuilderError '{:?}' encountered while parsing", sb_error)
            },
            Self::StateChartBuilderError(scb_error) => {
                write!(f, "StateChartBuilderError '{:?}' encountered while parsing", scb_error)
            },
            Self::TransitionBuilderError(tb_error) => {
                write!(f, "TransitionBuilderError '{:?}' encountered while parsing", tb_error)
            },
        }
    }
}

impl From<DataModelError> for ParserError {
    fn from(src: DataModelError) -> Self {
        Self::DataModelError(src)
    }
}
impl From<EventError> for ParserError {
    fn from(src: EventError) -> Self {
        Self::EventError(src)
    }
}
impl From<InterpreterError> for ParserError {
    fn from(src: InterpreterError) -> Self {
        Self::InterpreterError(src)
    }
}
impl From<std::io::Error> for ParserError {
    fn from(src: std::io::Error) -> Self {
        Self::IoError(src.kind())
    }
}
impl From<RegistryError> for ParserError {
    fn from(src: RegistryError) -> Self {
        Self::RegistryError(src)
    }
}
impl From<roxmltree::Error> for ParserError {
    fn from(src: roxmltree::Error) -> Self {
        Self::RoxmlTreeError(src)
    }
}
impl From<StateBuilderError> for ParserError {
    fn from(src: StateBuilderError) -> Self {
        Self::StateBuilderError(src)
    }
}
impl From<StateChartBuilderError> for ParserError {
    fn from(src: StateChartBuilderError) -> Self {
        Self::StateChartBuilderError(src)
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

    use std::error::Error;

    use crate::{
        StateChartBuilderError,
        event::{
            Event,
            EventError,
        },
        parser::{
            Parser,
            ParserError
        },
        registry::RegistryError,
        state::StateBuilderError,
        transition::TransitionBuilderError,
    };


    type TestResult = Result<(), Box<dyn Error>>;


    #[test]
    fn assign_without_location() -> TestResult {
        let assign_string = "Element { tag_name: {http://www.w3.org/2005/07/scxml}assign, attributes: [Attribute { name: expr, value: \"true\" }], namespaces: [Namespace { name: None, uri: \"http://www.w3.org/2005/07/scxml\" }] }".to_string();
        let mut parser = Parser::new("res/test_cases/assign_without_location.scxml")?;

        assert_eq!(
            parser.parse(),
            Err(ParserError::AssignWithoutLocation(assign_string))
        );

        Ok(())
    }

    #[test]
    fn assign_without_expr() -> TestResult {
        let assign_string = "Element { tag_name: {http://www.w3.org/2005/07/scxml}assign, attributes: [Attribute { name: location, value: \"door_closed\" }], namespaces: [Namespace { name: None, uri: \"http://www.w3.org/2005/07/scxml\" }] }".to_string();
        let mut parser = Parser::new("res/test_cases/assign_without_expr.scxml")?;

        assert_eq!(
            parser.parse(),
            Err(ParserError::AssignWithoutExpr(assign_string))
        );

        Ok(())
    }

    #[test]
    fn initial_node_errors() -> TestResult {
        let state_string = String::from("Element { tag_name: {http://www.w3.org/2005/07/scxml}state, attributes: [Attribute { name: id, value: \"on\" }], namespaces: [Namespace { name: None, uri: \"http://www.w3.org/2005/07/scxml\" }] }");

        let mut parser_childless = Parser::new("res/test_cases/initial_node_childless.scxml")?;
        let mut parser_targetless = Parser::new("res/test_cases/initial_transition_targetless.scxml")?;

        assert_eq!(
            parser_childless.parse(),
            Err(ParserError::InitialNodeChildless(state_string.clone()))
        );

        assert_eq!(
            parser_targetless.parse(),
            Err(ParserError::InitialTransitionTargetless(state_string))
        );

        Ok(())
    }

    #[test]
    fn namespace() -> TestResult {
        let mut parser_a = Parser::new("res/test_cases/invalid_scxml_namespace_empty.scxml")?;
        let mut parser_b = Parser::new("res/test_cases/invalid_scxml_namespace_invalid.scxml")?;
        // Verify that the empty namespace is caught
        assert_eq!(
            parser_a.parse(),
            Err(ParserError::InvalidScxmlNamespace(String::new()))
        );

        // Verify that an invalid namespace is caught
        assert_eq!(
            parser_b.parse(),
            Err(ParserError::InvalidScxmlNamespace(String::from("http://invalid.namespace")))
        );

        Ok(())
    }

    #[test]
    fn version() -> TestResult {
        let mut parser_a = Parser::new("res/test_cases/invalid_scxml_version_empty.scxml")?;
        let mut parser_b = Parser::new("res/test_cases/invalid_scxml_version_invalid.scxml")?;

        // Verify that the empty version is caught
        assert_eq!(
            parser_a.parse(),
            Err(ParserError::InvalidScxmlVersion(String::new()))
        );

        // Verify that an invalid version is caught
        assert_eq!(
            parser_b.parse(),
            Err(ParserError::InvalidScxmlVersion(String::from("2.0")))
        );

        Ok(())
    }

    #[test]
    fn datamodel() -> TestResult {
        let mut parser_a = Parser::new("res/test_cases/datamodel_empty.scxml")?;
        let mut parser_b = Parser::new("res/test_cases/datamodel_invalid.scxml")?;

        // Verify that the empty datamodel is allowed
        assert_eq!(
            parser_a.parse().is_ok(),
            true
        );

        // Verify that an invalid datamodel is caught
        assert_eq!(
            parser_b.parse(),
            Err(ParserError::InvalidDataModel(String::from("someotherscript")))
        );

        Ok(())
    }

    #[test]
    fn data_item_invalid() -> TestResult {
        let state_string = String::from("Element { tag_name: {http://www.w3.org/2005/07/scxml}datamodel, attributes: [], namespaces: [Namespace { name: None, uri: \"http://www.w3.org/2005/07/scxml\" }] }");
        let mut parser = Parser::new("res/test_cases/data_item_invalid.scxml")?;

        assert_eq!(
            parser.parse(),
            Err(ParserError::InvalidDataItem(state_string))
        );

        Ok(())
    }

    #[test]
    fn no_id_state() -> TestResult {
        let state_string = String::from("Element { tag_name: {http://www.w3.org/2005/07/scxml}state, attributes: [], namespaces: [Namespace { name: None, uri: \"http://www.w3.org/2005/07/scxml\" }] }");
        let mut parser = Parser::new("res/test_cases/state_no_id.scxml")?;

        assert_eq!(
            parser.parse(),
            Err(ParserError::StateHasNoId(state_string))
        );

        Ok(())
    }

    #[test]
    fn event_error() -> TestResult {
        let mut parser = Parser::new("res/test_cases/event_invalid_id.scxml")?;

        assert_eq!(
            parser.parse(),
            Err(ParserError::EventError(EventError::IdContainsDuplicates(String::from("turn.on.on"))))
        );

        Ok(())
    }

    #[test]
    fn io_error() -> TestResult {
        // Attempt to create a parser for a file that does not exist
        assert_eq!(
            Parser::new("does_not_exist.scxml"),
            Err(ParserError::IoError(std::io::ErrorKind::NotFound))
        );

        Ok(())
    }

    #[test]
    fn registry_error() -> TestResult {
        let mut parser = Parser::new("res/test_cases/registry_invalid_dup_state.scxml")?;

        assert_eq!(
            parser.parse(),
            Err(
                ParserError::StateChartBuilderError(
                    StateChartBuilderError::RegistryError(
                        RegistryError::StateAlreadyRegistered(
                            String::from("duplicate")
                        )
                    )
                )
            )
        );

        Ok(())
    }

    #[test]
    fn roxmltree_error() -> TestResult {
        let mut parser = Parser::new("res/test_cases/completely_empty.scxml")?;

        assert_eq!(
            parser.parse(),
            Err(ParserError::RoxmlTreeError(roxmltree::Error::NoRootNode))
        );

        Ok(())
    }

    #[test]
    fn state_builder_error() -> TestResult {
        let mut parser = Parser::new("res/test_cases/state_invalid_initial.scxml")?;

        assert_eq!(
            parser.parse(),
            Err(ParserError::StateBuilderError(StateBuilderError::InitialIsNotChild(String::from("dne"))))
        );

        Ok(())
    }

    #[test]
    fn statechart_builder_error() -> TestResult {
        let mut parser = Parser::new("res/test_cases/statechart_invalid_no_states.scxml")?;

        assert_eq!(
            parser.parse(),
            Err(ParserError::StateChartBuilderError(StateChartBuilderError::NoStatesRegistered))
        );

        Ok(())
    }

    #[test]
    fn transition_builder_error() -> TestResult {
        let mut parser = Parser::new("res/test_cases/transition_invalid_dup_event.scxml")?;

        assert_eq!(
            parser.parse(),
            Err(ParserError::TransitionBuilderError(TransitionBuilderError::DuplicateEventId(Event::from("turn.on")?)))
        );

        Ok(())
    }

    #[test]
    fn microwave() -> TestResult {
        let mut parser = Parser::new("res/examples/01_microwave.scxml")?;

        // Parse microwave example scxml doc
        let mut statechart = parser.parse()?;
        eprintln!("*** Initial Active State(s):\n{:#?}", statechart.active_state_ids());
        eprintln!("*** Initial Data Model:\n{:#?}", statechart.sys_vars);

        // Send turn-on event
        eprintln!("*** Sending 'turn.on' Event...");
        let turn_on = Event::from("turn.on")?;
        statechart.process_external_event(&turn_on)?;

        eprintln!("*** Active State(s):\n{:#?}", statechart.active_state_ids());
        assert_eq!(
            statechart.active_state_ids(),
            vec![String::from("on"), String::from("cooking")],
        );

        // Send a door-open event
        eprintln!("*** Sending 'door.open' Event...");
        let door_open = Event::from("door.open")?;
        statechart.process_external_event(&door_open)?;

        eprintln!("*** Active State(s):\n{:#?}", statechart.active_state_ids());
        assert_eq!(
            statechart.active_state_ids(),
            vec![String::from("on"), String::from("idle")],
        );

        // Send a door-close event
        eprintln!("*** Sending 'door.close' Event...");
        let door_close = Event::from("door.close")?;
        statechart.process_external_event(&door_close)?;

        eprintln!("*** Active State(s):\n{:#?}", statechart.active_state_ids());
        assert_eq!(
            statechart.active_state_ids(),
            vec![String::from("on"), String::from("cooking")],
        );

        // Send time event 6x
        let time = Event::from("time")?;
        for _ in 0..5 {
            eprintln!("*** Sending 'time' Event...");
            statechart.process_external_event(&time)?;
            eprintln!("*** Current Active State(s):\n{:#?}", statechart.active_state_ids());
        }
        eprintln!("*** Current Data Model:\n{:#?}", statechart.sys_vars);

        Ok(())
    }
}
