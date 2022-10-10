/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : event.rs

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
    This modules defines an Event object and ID. IDs conform to §3.12.1 Event
    Descriptors of the SCXML spec.

    Events are simple objects whose IDs are passed into the StateChart and will
    trigger associated Transitions. Note that this is not the only way for
    State changes to occur in the StateChart, as some Transitions are eventless.

    NONCONFORMANCES:

    Event IDs are not limitless. The number of '.'-separated ID nodes is limited
    to MAX_ID_NODES defined below.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::{error::Error, fmt};


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

//FEAT: Align this struct with §5.10.1
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Event {
    id_nodes: Vec<String>,
    event_type: EventType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum EventType {
    Platform, // Raised by the platform itself, such as error events
    Internal, // Raised by <raise>/<send> with _internal as the target
    External, // All other events
}

#[derive(Debug, PartialEq, Eq)]
pub enum EventError {
    IdContainsDuplicates(String),
    IdNodeIsEmpty(String, usize),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Event {
    pub fn from(source_str: &str) -> Result<Self, EventError> {
        let source_nodes: Vec<&str> = source_str.split('.').collect();

        // Ensure no node is empty
        for (idx, node) in source_nodes.iter().enumerate() {
            if node.is_empty() {
                return Err(EventError::IdNodeIsEmpty(String::from(source_str), idx));
            }
        }

        // Ensure there are no repeated ID nodes in the ID
        let mut deduped_nodes = source_nodes.clone();
        deduped_nodes.sort_unstable();
        deduped_nodes.dedup();
        if source_nodes.len() != deduped_nodes.len() {
            return Err(EventError::IdContainsDuplicates(String::from(source_str)));
        }

        // Checks passed, compose the array
        let mut composed_nodes = Vec::new();
        for node in source_nodes.iter() {
            composed_nodes.push(String::from(*node));
        }

        Ok(Self {
            id_nodes: composed_nodes,
            //FEAT: Support external events
            event_type: EventType::Platform,
        })
    }


    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn id(&self) -> String {
        let mut composed_id = String::new();

        for node in &self.id_nodes {
            composed_id.push_str(node);
            composed_id.push('.');
        }
        composed_id.pop();

        composed_id
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *       Event        *
\*  *  *  *  *  *  *  */

impl fmt::Display for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut id_node_iter = self.id_nodes.iter().peekable();

        while let Some(id_node) = id_node_iter.next() {
            write!(f, "{}", id_node)?;

            // Only add a trailing . if there is another node after the current
            if id_node_iter.peek().is_some() {
                write!(f, ".")?;
            }
        }

        Ok(())
    }
}

/*  *  *  *  *  *  *  *\
 *     EventError     *
\*  *  *  *  *  *  *  */

impl Error for EventError {}

impl fmt::Display for EventError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::IdContainsDuplicates(source) => {
                write!(f, "source string '{}' contains duplicate ID nodes", source)
            }
            Self::IdNodeIsEmpty(source, node_idx) => {
                write!(
                    f,
                    "source string '{}' node index {} is empty",
                    source, node_idx
                )
            }
        }
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {

    use std::error::Error;

    use crate::event::{Event, EventError};


    type TestResult = Result<(), Box<dyn Error>>;


    #[test]
    fn id_contains_duplicates() -> TestResult {
        let valid_string = "error.send.failed";
        let invalid_string = "error.send.error";

        // Verify valid string parsing
        assert_eq!(
            Event::from(valid_string).is_ok(),
            true,
            "Failed to parse a valid event descriptor"
        );

        // Verify invalid string handling
        assert_eq!(
            Event::from(invalid_string),
            Err(EventError::IdContainsDuplicates(String::from(
                invalid_string
            ))),
            "Failed to reject invalid event descriptor"
        );

        Ok(())
    }

    #[test]
    fn empty_node() -> TestResult {
        let empty_node = "this.has.an..empty.node";

        // Verify empty node is caught
        assert_eq!(
            Event::from(empty_node),
            Err(EventError::IdNodeIsEmpty(String::from(empty_node), 3)),
            "Failed to catch empty node"
        );

        Ok(())
    }

    #[test]
    fn output() -> TestResult {
        let source = "error.send.failed";
        let event = Event::from(source)?;

        println!("Event ID: '{}'", event);

        assert_eq!(
            source,
            format!("{}", event),
            "Formatted Event does not match source string"
        );

        Ok(())
    }
}
