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

use std::io::{Read, Write};
use std::{error::Error, fmt, fs};

use crate::{
    datamodel::{DataModelError, SystemVariables},
    event::{Event, EventError},
    executable_content::{BranchTableEntry, ExecutableContent},
    interpreter::{Interpreter, InterpreterError},
    registry::RegistryError,
    state::{State, StateBuilder, StateBuilderError},
    transition::{Transition, TransitionBuilder, TransitionBuilderError},
    StateChart, StateChartBuilder, StateChartBuilderError,
};

use roxmltree::Node;

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
pub struct Parser<'w, W: 'w + Write> {
    path: String,
    content: String,
    statechart_builder: StateChartBuilder<'w, W>,
}

#[derive(Debug, PartialEq)]
pub enum ParserError {
    AssignWithoutLocation(String /* Stringified XML Tree Node */),
    AssignWithoutExpr(String /* Stringified XML Tree Node */),
    ElseIfWithoutCond(String /* Stringified XML Tree Node */),
    IfWithoutCond(String /* Stringified XML Tree Node */),
    InitialNodeChildless(String /* Stringified XML Tree Node */),
    InitialTransitionTargetless(String /* Stringified XML Tree Node */),
    InvalidScxmlChild(String /* Name of child element */),
    InvalidScxmlNamespace(String /* Invalid namespace */),
    InvalidScxmlVersion(String /* Invalid version */),
    InvalidDataModel(String /* Invalid data model name */),
    InvalidDataItem(String /* Stringified XML Tree Node */),
    StateHasNoId(String /* Stringified XML Tree Node */),

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

impl<'w, W: 'w + Write> Parser<'w, W> {
    /// Creates a new Parser object for the SCXML file at the given path.
    pub fn new(path: &str, writer: &'w mut W) -> Result<Self, ParserError> {
        // Attempt to open the file at the given path
        let mut file = fs::File::open(path)?;
        let mut file_contents = String::new();
        file.read_to_string(&mut file_contents)?;

        Ok(Self {
            path: path.to_string(),
            content: file_contents,
            statechart_builder: StateChartBuilder::new(writer),
        })
    }

    pub fn parse(mut self) -> Result<StateChart<'w, W>, ParserError> {
        // Parse contents of the SCXML file into an roxmltree::Document
        let parsed_content = roxmltree::Document::parse(self.content.as_str())?;

        // Ensure the document is properly structured
        Self::validate_structure(&parsed_content)?;

        // Get Initial State from <scxml>, if specified
        if let Some(initial) = parsed_content.root_element().attribute("initial") {
            self.statechart_builder = self.statechart_builder.initial(initial.to_string());
        }

        // Get StateChart name from <scxml>, if specified
        if let Some(chart_name) = parsed_content.root_element().attribute("name") {
            self.statechart_builder = self.statechart_builder.name(chart_name);
        } else {
            // Name was not specified, generate a UUID
            self.statechart_builder = self
                .statechart_builder
                .name(Uuid::new_v4().to_string().as_str());
        }

        // Begin iterating through the parsed content, skipping comment and text nodes
        for child in parsed_content
            .root_element()
            .children()
            .filter(|v| v.is_element())
        {
            // Parse element based on its tag name
            match child.tag_name().name() {
                "datamodel" => {
                    Self::parse_datamodel(child, &mut self.statechart_builder.sys_vars)?;
                }
                "final" => {
                    unimplemented!("Final states are not supported.")
                }
                "parallel" => {
                    unimplemented!("Parallel States not yet supported.")
                }
                "script" => {
                    unimplemented!("Scripting is not yet supported.")
                }
                "state" => {
                    self.statechart_builder =
                        self.statechart_builder.state(Self::parse_state(child)?)?;
                }
                unexpected => {
                    return Err(ParserError::InvalidScxmlChild(unexpected.to_string()));
                }
            }
        }

        self.statechart_builder
            .build()
            .map_err(ParserError::StateChartBuilderError)
    }


    /*  *  *  *  *  *  *  *\
     *   Helper Methods   *
    \*  *  *  *  *  *  *  */

    fn validate_structure(document: &roxmltree::Document) -> Result<(), ParserError> {
        /* <scxml> Root Element Checks */
        let root_node = document.root_element();

        // Verify namespace
        if let Some(namespace) = root_node.tag_name().namespace() {
            if namespace != VALID_SCXML_NAMESPACE {
                return Err(ParserError::InvalidScxmlNamespace(namespace.to_string()));
            }
        } else {
            // Namespace was not specified
            return Err(ParserError::InvalidScxmlNamespace(String::new()));
        }

        // Verify version
        if let Some(version) = root_node.attribute("version") {
            if version != VALID_SCXML_VERSION {
                return Err(ParserError::InvalidScxmlVersion(version.to_string()));
            }
        } else {
            // Version not specified
            return Err(ParserError::InvalidScxmlVersion(String::new()));
        }

        // Verify datamodel
        if let Some(datamodel) = root_node.attribute("datamodel") {
            if datamodel != VALID_DATAMODEL {
                return Err(ParserError::InvalidDataModel(datamodel.to_string()));
            }
        }
        // NOTE: Not specifying datamodel is allowable - ECMAScript will be assumed.

        Ok(())
    }

    fn parse_datamodel(
        element: roxmltree::Node,
        sys_vars: &mut SystemVariables,
    ) -> Result<(), ParserError> {
        // Collect all <data> children
        for child in element.children().filter(|v| v.is_element()) {
            if let (Some(id), Some(expr_str)) = (child.attribute("id"), child.attribute("expr")) {
                // Interpret value expression and insert into data map
                let interpreter = Interpreter::new(expr_str);
                let expr_value = interpreter.interpret(sys_vars)?;
                sys_vars.set_data_member(id, expr_value);
            } else {
                return Err(ParserError::InvalidDataItem(format!("{:?}", element)));
            }
        }

        Ok(())
    }

    fn parse_state(element: roxmltree::Node) -> Result<State, ParserError> {
        let mut initial_specified = false;

        // // Get ID attribute to create StateBuilder
        let state_id = element
            .attribute("id")
            .ok_or_else(|| ParserError::StateHasNoId(format!("{:?}", element)))?;
        let mut state_builder = StateBuilder::new(state_id.to_string());

        // Check for initial attribute
        if let Some(initial) = element.attribute("initial") {
            state_builder = state_builder.initial(initial.to_string());
            initial_specified = true;
        }

        // Iterate through children, adding transitions or parsing substates
        for child in element.children().filter(|v| v.is_element()) {
            //FEAT: Handle <onentry>

            //FEAT: Handle <onexit>

            // Handle <transition>
            if child.tag_name().name() == "transition" {
                state_builder =
                    state_builder.transition(Self::parse_transition(child, state_id)?)?;
            }

            // Handle <initial>, if not already specified
            if !initial_specified && child.tag_name().name() == "initial" {
                // Get the target of the internal <transition>'s target
                // If the internal <transition> does not exist, the document is not well-formed
                for grandchild in child.children().filter(|v| v.is_element()) {
                    if let Some(initial) = grandchild.attribute("target") {
                        state_builder = state_builder.initial(initial.to_string());
                        initial_specified = true;
                    } else {
                        return Err(ParserError::InitialTransitionTargetless(format!(
                            "{:?}",
                            element
                        )));
                    }
                }
                // Ensure that a <transition> element was found, otherwise the document is not well-formed
                if !initial_specified {
                    return Err(ParserError::InitialNodeChildless(format!("{:?}", element)));
                }
            }

            // Handle sub <state>
            if child.tag_name().name() == "state" {
                state_builder = state_builder.substate(Self::parse_state(child)?)?;
            }

            //FEAT: Handle <parallel>

            //FEAT: Handle <history>

            //FEAT: Handle <datamodel>
        }

        state_builder
            .build()
            .map_err(ParserError::StateBuilderError)
    }

    fn parse_transition(
        element: roxmltree::Node,
        parent_id: &str,
    ) -> Result<Transition, ParserError> {
        let mut transition_builder = TransitionBuilder::new(parent_id);

        // Tokenize Events
        if let Some(event_ids) = element.attribute("event").map(|v| v.split_whitespace()) {
            for event_id in event_ids {
                // Create an event and add it to the builder
                let event = Event::from(event_id)?;
                transition_builder = transition_builder.event(&event)?;
            }
        }

        // Parse guard condition
        if let Some(cond_str) = element.attribute("cond") {
            transition_builder = transition_builder.cond(cond_str)?;
        }

        // Set target State
        if let Some(target_id) = element.attribute("target") {
            transition_builder = transition_builder.target_id(target_id)?;
        }

        // Check for Executable Content children, skipping comments and text
        for child in element.children().filter(|v| v.is_element()) {
            transition_builder =
                transition_builder.executable_content(Self::parse_for_executable_content(child)?);
        }

        transition_builder
            .build()
            .map_err(ParserError::TransitionBuilderError)
    }

    fn parse_for_executable_content(element: Node) -> Result<ExecutableContent, ParserError> {
        // Handle <assign>
        if element.tag_name().name() == "assign" {
            return Self::parse_assign(element);
        }
        // Handle <if>
        if element.tag_name().name() == "if" {
            return Self::parse_if(element);
        }
        // Handle <log>
        if element.tag_name().name() == "log" {
            return Ok(Self::parse_log(element));
        } else {
            unimplemented!(
                "Parsing of this Executable Content element '{}' is not yet supported",
                element.tag_name().name()
            )
        }

        //FEAT: Handle other executable content
    }

    fn parse_assign(element: Node) -> Result<ExecutableContent, ParserError> {
        if let Some(location) = element.attribute("location") {
            // 'expr' may be either an attribute of 'assign', or its child(ren)
            if let Some(expr) = element.attribute("expr") {
                // 'expr' was in-line, create it and return
                Ok(ExecutableContent::Assign(
                    location.to_string(),
                    expr.to_string(),
                ))
            } else if element.has_children() {
                // When the 'expr' is not specified as part of the assign tag, it can be one
                // or more child elements. This constitutes a multi-line expression, which
                // is not yet supported by the ECMAScript Interpreter
                unimplemented!("Multi-line ECMAScript expression are not yet supported");
            }
            // 'expr' was not specified, this is an error
            else {
                Err(ParserError::AssignWithoutExpr(format!("{:?}", element)))
            }
        }
        // 'location' was not specified, this is an error
        else {
            Err(ParserError::AssignWithoutLocation(format!("{:?}", element)))
        }
    }

    fn parse_if(element: Node) -> Result<ExecutableContent, ParserError> {
        if let Some(cond) = element.attribute("cond") {
            // Create vector to hold children. Note that there may not be any
            let mut branch_table = Vec::new();

            let mut cur_entry = BranchTableEntry::new(cond.to_string(), Vec::new());
            for child in element.children().filter(|v| v.is_element()) {
                // Handle <elseif>
                if child.tag_name().name() == "elseif" {
                    // Current entry has concluded, push it into the table
                    branch_table.push(cur_entry);

                    // <elseif>s must contain a "cond" attribute, if they do not it is an error
                    if let Some(child_cond) = child.attribute("cond") {
                        // Create a new entry and move on to the next child element
                        cur_entry = BranchTableEntry::new(child_cond.to_string(), Vec::new());
                        continue;
                    } else {
                        return Err(ParserError::ElseIfWithoutCond(format!("{:?}", child)));
                    }
                }

                // Handle <else>
                if child.tag_name().name() == "else" {
                    // Current entry has concluded, push it into the table
                    branch_table.push(cur_entry);

                    // <else>s are effectively <if>s with a cond that always evaluates to 'true'
                    // Create an always-true entry and move on to the next child element
                    cur_entry = BranchTableEntry::new("true".to_string(), Vec::new());
                    continue;
                }

                // Executable Content child, push onto entry's vector
                cur_entry.push_exec_content(Self::parse_for_executable_content(child)?);
            }

            // Add current (final) entry to the table
            branch_table.push(cur_entry);

            Ok(ExecutableContent::BranchTable(branch_table))
        }
        // 'cond' not specified, this is an error
        else {
            Err(ParserError::IfWithoutCond(format!("{:?}", element)))
        }
    }

    fn parse_log(element: Node) -> ExecutableContent {
        let mut label = String::new();
        let mut expr = String::new();

        // <log> can contain a label, expression, both or neither
        if let Some(label_str) = element.attribute("label") {
            label.push_str(label_str);
        }
        if let Some(expr_str) = element.attribute("expr") {
            expr.push_str(expr_str);
        }

        // Create the Executable Content and return it
        ExecutableContent::Log(label, expr)
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
            }
            Self::AssignWithoutExpr(parent_assign) => {
                write!(f, "Assignment '{}' is missing an 'expr'", parent_assign)
            }
            Self::ElseIfWithoutCond(parent_elseif) => {
                write!(f, "ElseIf '{}' is missing a 'cond'", parent_elseif)
            }
            Self::IfWithoutCond(parent_if) => {
                write!(f, "If '{}' is missing a 'cond'", parent_if)
            }
            Self::InitialNodeChildless(parent_state) => {
                write!(
                    f,
                    "State '{}' contains a childless <initial> element",
                    parent_state
                )
            }
            Self::InitialTransitionTargetless(parent_state) => {
                write!(
                    f,
                    "State '{}' contains an <initial> element with a targetless <transition>",
                    parent_state
                )
            }
            Self::InvalidScxmlChild(name) => {
                write!(f, "Invalid Child '{}' of top-level SCXML element", name)
            }
            Self::InvalidScxmlNamespace(namespace) => {
                write!(
                    f,
                    "Invalid SCXML Namespace '{}', expected '{}'",
                    namespace, VALID_SCXML_NAMESPACE
                )
            }
            Self::InvalidScxmlVersion(version) => {
                write!(
                    f,
                    "Invalid SCXML Version '{}', expected '{}'",
                    version, VALID_SCXML_VERSION
                )
            }
            Self::InvalidDataModel(datamodel) => {
                write!(
                    f,
                    "Invalid SCXML Datamodel '{}', expected '{}'",
                    datamodel, VALID_DATAMODEL
                )
            }
            Self::InvalidDataItem(node_id) => {
                write!(f, "Invalid Data Item '{:?}'", node_id)
            }
            Self::StateHasNoId(node_id) => {
                write!(
                    f,
                    "Node ID {:?} is a <state> element with no 'id' attribute",
                    node_id
                )
            }

            // Wrappers
            Self::DataModelError(data_error) => {
                write!(
                    f,
                    "DataModelError '{:?}' encountered while parsing",
                    data_error
                )
            }
            Self::EventError(event_error) => {
                write!(
                    f,
                    "EventError '{:?}' encountered while parsing",
                    event_error
                )
            }
            Self::InterpreterError(interp_error) => {
                write!(
                    f,
                    "InterpreterError '{:?}' encountered while parsing",
                    interp_error
                )
            }
            Self::IoError(io_err_kind) => {
                write!(
                    f,
                    "IoError of kind '{:?}' encountered when attempting to create Parser object",
                    io_err_kind
                )
            }
            Self::RegistryError(reg_error) => {
                write!(
                    f,
                    "RegistryError '{:?}' encountered while parsing",
                    reg_error
                )
            }
            Self::RoxmlTreeError(roxmltree_error) => {
                write!(
                    f,
                    "roxmltree::Error '{:?}' encountered while parsing",
                    roxmltree_error
                )
            }
            Self::StateBuilderError(sb_error) => {
                write!(
                    f,
                    "StateBuilderError '{:?}' encountered while parsing",
                    sb_error
                )
            }
            Self::StateChartBuilderError(scb_error) => {
                write!(
                    f,
                    "StateChartBuilderError '{:?}' encountered while parsing",
                    scb_error
                )
            }
            Self::TransitionBuilderError(tb_error) => {
                write!(
                    f,
                    "TransitionBuilderError '{:?}' encountered while parsing",
                    tb_error
                )
            }
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


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {

    use std::error::Error;
    use std::io;

    use crate::{
        event::{Event, EventError},
        parser::{Parser, ParserError},
        registry::RegistryError,
        state::StateBuilderError,
        transition::TransitionBuilderError,
        StateChartBuilderError,
    };


    type TestResult = Result<(), Box<dyn Error>>;


    #[test]
    fn assign_without_location() -> TestResult {
        let mut dev_null = io::sink();
        let assign_string = "Element { tag_name: {http://www.w3.org/2005/07/scxml}assign, attributes: [Attribute { name: expr, value: \"true\" }], namespaces: [Namespace { name: None, uri: \"http://www.w3.org/2005/07/scxml\" }] }".to_string();
        let parser = Parser::new(
            "res/test_cases/assign_without_location.scxml",
            &mut dev_null,
        )?;

        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::AssignWithoutLocation(assign_string)
        );

        Ok(())
    }

    #[test]
    fn assign_without_expr() -> TestResult {
        let mut dev_null = io::sink();
        let assign_string = "Element { tag_name: {http://www.w3.org/2005/07/scxml}assign, attributes: [Attribute { name: location, value: \"door_closed\" }], namespaces: [Namespace { name: None, uri: \"http://www.w3.org/2005/07/scxml\" }] }".to_string();
        let parser = Parser::new("res/test_cases/assign_without_expr.scxml", &mut dev_null)?;

        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::AssignWithoutExpr(assign_string)
        );

        Ok(())
    }

    #[test]
    fn if_without_cond() -> TestResult {
        todo!()
    }

    #[test]
    fn initial_node_errors() -> TestResult {
        let mut dev_null_a = io::sink();
        let mut dev_null_b = io::sink();
        let state_string = "Element { tag_name: {http://www.w3.org/2005/07/scxml}state, attributes: [Attribute { name: id, value: \"on\" }], namespaces: [Namespace { name: None, uri: \"http://www.w3.org/2005/07/scxml\" }] }".to_string();

        let parser_childless = Parser::new(
            "res/test_cases/initial_node_childless.scxml",
            &mut dev_null_a,
        )?;
        let parser_targetless = Parser::new(
            "res/test_cases/initial_transition_targetless.scxml",
            &mut dev_null_b,
        )?;

        assert_eq!(
            parser_childless.parse().unwrap_err(),
            ParserError::InitialNodeChildless(state_string.clone())
        );

        assert_eq!(
            parser_targetless.parse().unwrap_err(),
            ParserError::InitialTransitionTargetless(state_string)
        );

        Ok(())
    }

    #[test]
    fn invalid_scxml_child() -> TestResult {
        let mut dev_null = io::sink();
        let parser = Parser::new("res/test_cases/invalid_scxml_child.scxml", &mut dev_null)?;

        // Verify that the invalid child element is caught
        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::InvalidScxmlChild("invalid".to_string())
        );

        Ok(())
    }

    #[test]
    fn namespace() -> TestResult {
        let mut dev_null_a = io::sink();
        let mut dev_null_b = io::sink();
        let parser_a = Parser::new(
            "res/test_cases/invalid_scxml_namespace_empty.scxml",
            &mut dev_null_a,
        )?;
        let parser_b = Parser::new(
            "res/test_cases/invalid_scxml_namespace_invalid.scxml",
            &mut dev_null_b,
        )?;
        // Verify that the empty namespace is caught
        assert_eq!(
            parser_a.parse().unwrap_err(),
            ParserError::InvalidScxmlNamespace(String::new())
        );

        // Verify that an invalid namespace is caught
        assert_eq!(
            parser_b.parse().unwrap_err(),
            ParserError::InvalidScxmlNamespace("http://invalid.namespace".to_string())
        );

        Ok(())
    }

    #[test]
    fn version() -> TestResult {
        let mut dev_null_a = io::sink();
        let mut dev_null_b = io::sink();
        let parser_a = Parser::new(
            "res/test_cases/invalid_scxml_version_empty.scxml",
            &mut dev_null_a,
        )?;
        let parser_b = Parser::new(
            "res/test_cases/invalid_scxml_version_invalid.scxml",
            &mut dev_null_b,
        )?;

        // Verify that the empty version is caught
        assert_eq!(
            parser_a.parse().unwrap_err(),
            ParserError::InvalidScxmlVersion(String::new())
        );

        // Verify that an invalid version is caught
        assert_eq!(
            parser_b.parse().unwrap_err(),
            ParserError::InvalidScxmlVersion("2.0".to_string())
        );

        Ok(())
    }

    #[test]
    fn datamodel() -> TestResult {
        let mut dev_null_a = io::sink();
        let mut dev_null_b = io::sink();
        let parser_a = Parser::new("res/test_cases/datamodel_empty.scxml", &mut dev_null_a)?;
        let parser_b = Parser::new("res/test_cases/datamodel_invalid.scxml", &mut dev_null_b)?;

        // Verify that the empty datamodel is allowed
        assert_eq!(parser_a.parse().is_ok(), true);

        // Verify that an invalid datamodel is caught
        assert_eq!(
            parser_b.parse().unwrap_err(),
            ParserError::InvalidDataModel("someotherscript".to_string())
        );

        Ok(())
    }

    #[test]
    fn data_item_invalid() -> TestResult {
        let mut dev_null = io::sink();
        let state_string = "Element { tag_name: {http://www.w3.org/2005/07/scxml}datamodel, attributes: [], namespaces: [Namespace { name: None, uri: \"http://www.w3.org/2005/07/scxml\" }] }".to_string();
        let parser = Parser::new("res/test_cases/data_item_invalid.scxml", &mut dev_null)?;

        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::InvalidDataItem(state_string)
        );

        Ok(())
    }

    #[test]
    fn no_id_state() -> TestResult {
        let mut dev_null = io::sink();
        let state_string = "Element { tag_name: {http://www.w3.org/2005/07/scxml}state, attributes: [], namespaces: [Namespace { name: None, uri: \"http://www.w3.org/2005/07/scxml\" }] }".to_string();
        let parser = Parser::new("res/test_cases/state_no_id.scxml", &mut dev_null)?;

        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::StateHasNoId(state_string)
        );

        Ok(())
    }

    #[test]
    fn event_error() -> TestResult {
        let mut dev_null = io::sink();
        let parser = Parser::new("res/test_cases/event_invalid_id.scxml", &mut dev_null)?;

        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::EventError(EventError::IdContainsDuplicates("turn.on.on".to_string()))
        );

        Ok(())
    }

    #[test]
    fn io_error() -> TestResult {
        // Attempt to create a parser for a file that does not exist
        assert_eq!(
            Parser::new("does_not_exist.scxml", &mut io::sink()).unwrap_err(),
            ParserError::IoError(std::io::ErrorKind::NotFound)
        );

        Ok(())
    }

    #[test]
    fn registry_error() -> TestResult {
        let mut dev_null = io::sink();
        let parser = Parser::new(
            "res/test_cases/registry_invalid_dup_state.scxml",
            &mut dev_null,
        )?;

        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::StateChartBuilderError(StateChartBuilderError::RegistryError(
                RegistryError::StateAlreadyRegistered("duplicate".to_string())
            ))
        );

        Ok(())
    }

    #[test]
    fn roxmltree_error() -> TestResult {
        let mut dev_null = io::sink();
        let parser = Parser::new("res/test_cases/completely_empty.scxml", &mut dev_null)?;

        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::RoxmlTreeError(roxmltree::Error::NoRootNode)
        );

        Ok(())
    }

    #[test]
    fn state_builder_error() -> TestResult {
        let mut dev_null = io::sink();
        let parser = Parser::new("res/test_cases/state_invalid_initial.scxml", &mut dev_null)?;

        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::StateBuilderError(StateBuilderError::InitialSetWithoutChildStates(
                "dne".to_string()
            ))
        );

        Ok(())
    }

    #[test]
    fn statechart_builder_error() -> TestResult {
        let mut dev_null = io::sink();
        let parser = Parser::new(
            "res/test_cases/statechart_invalid_no_states.scxml",
            &mut dev_null,
        )?;

        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::StateChartBuilderError(StateChartBuilderError::NoStatesRegistered)
        );

        Ok(())
    }

    #[test]
    fn transition_builder_error() -> TestResult {
        let mut dev_null = io::sink();
        let parser = Parser::new(
            "res/test_cases/transition_invalid_dup_event.scxml",
            &mut dev_null,
        )?;

        assert_eq!(
            parser.parse().unwrap_err(),
            ParserError::TransitionBuilderError(TransitionBuilderError::DuplicateEventId(
                Event::from("turn.on")?
            ))
        );

        Ok(())
    }

    #[test]
    fn microwave() -> TestResult {
        let mut dev_null = io::sink();
        let parser = Parser::new("res/examples/01_microwave.scxml", &mut dev_null)?;

        // Parse microwave example scxml doc
        let mut statechart = parser.parse()?;
        eprintln!(
            "*** Initial Active State(s):\n{:#?}",
            statechart.active_state_ids()
        );
        eprintln!("*** Initial Data Model:\n{:#?}", statechart.sys_vars);

        // Send turn-on event
        eprintln!("*** Sending 'turn.on' Event...");
        let turn_on = Event::from("turn.on")?;
        statechart.process_external_event(&turn_on)?;

        eprintln!("*** Active State(s):\n{:#?}", statechart.active_state_ids());
        assert_eq!(
            statechart.active_state_ids(),
            vec!["on".to_string(), "cooking".to_string()],
        );

        // Send a door-open event
        eprintln!("*** Sending 'door.open' Event...");
        let door_open = Event::from("door.open")?;
        statechart.process_external_event(&door_open)?;

        eprintln!("*** Active State(s):\n{:#?}", statechart.active_state_ids());
        assert_eq!(
            statechart.active_state_ids(),
            vec!["on".to_string(), "idle".to_string()],
        );

        // Send a door-close event
        eprintln!("*** Sending 'door.close' Event...");
        let door_close = Event::from("door.close")?;
        statechart.process_external_event(&door_close)?;

        eprintln!("*** Active State(s):\n{:#?}", statechart.active_state_ids());
        assert_eq!(
            statechart.active_state_ids(),
            vec!["on".to_string(), "cooking".to_string()],
        );

        // Send time event 6x
        let time = Event::from("time")?;
        for _ in 0..5 {
            eprintln!("*** Sending 'time' Event...");
            statechart.process_external_event(&time)?;
            eprintln!(
                "*** Current Active State(s):\n{:#?}",
                statechart.active_state_ids()
            );
        }
        eprintln!("*** Current Data Model:\n{:#?}", statechart.sys_vars);

        Ok(())
    }
}
