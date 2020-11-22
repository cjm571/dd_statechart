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

use std::{
    error::Error,
    fmt,
};


///////////////////////////////////////////////////////////////////////////////
//  Named Constants
///////////////////////////////////////////////////////////////////////////////

const MAX_ID_NODES: usize = 8;

const EMPTY_ID_NODE_ARRAY: EventIdNodeArray = [""; MAX_ID_NODES];


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Event {
    id_nodes: EventIdNodeArray,
}
type EventIdNodeArray = [&'static str; MAX_ID_NODES];

#[derive(Debug, PartialEq)]
pub enum EventError {
    IdExceedsMaxNodes(&'static str),
    IdContainsDuplicates(&'static str),
    IdNodeIsEmpty(&'static str, usize),
}

///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Event {
    pub fn from(source_str: &'static str) -> Result<Self, EventError> {
        let source_nodes: Vec<&str> = source_str.split('.').collect();

        // Ensure node count does not exceed max
        if source_nodes.len() > MAX_ID_NODES {
            return Err(EventError::IdExceedsMaxNodes(source_str));
        }

        // Ensure no node is empty
        for (idx, node) in source_nodes.iter().enumerate() {
            if node.is_empty() {
                return Err(EventError::IdNodeIsEmpty(source_str, idx));
            }
        }
        
        // Ensure there are no repeated ID nodes in the ID
        let mut deduped_nodes = source_nodes.clone();
        deduped_nodes.sort_unstable();
        deduped_nodes.dedup();
        if source_nodes.len() != deduped_nodes.len() {
            return Err(EventError::IdContainsDuplicates(source_str));
        }

        // Checks passed, compose the array
        let mut composed_nodes: EventIdNodeArray = EMPTY_ID_NODE_ARRAY;
        for (idx, node) in source_nodes.iter().enumerate() {
            composed_nodes[idx] = *node;
        }

        Ok(
            Self {
                id_nodes: composed_nodes,
            }
        )
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
        let mut id_node_iter = self.id_nodes.iter();

        write!(f, "{}", id_node_iter.next().unwrap())?;
        for id_node in id_node_iter {
            // Break on first empty node
            if id_node.is_empty() {
                break;
            }

            write!(f, ".{}", id_node)?;
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
            Self::IdExceedsMaxNodes(source) => {
                write!(f, "source string '{}' exceeds maximum ID node count of {}", source, MAX_ID_NODES)
            },
            Self::IdContainsDuplicates(source) => {
                write!(f, "source string '{}' contains duplicate ID nodes", source)
            },
            Self::IdNodeIsEmpty(source, node_idx) => {
                write!(f, "source string '{}' node index {} is empty", source, node_idx)
            }
        }
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {

    use crate::event::{
        Event,
        EventError,
    };


    #[test]
    fn id_exceeds_max_nodes() {
        let too_many_nodes = "this.is.way.too.many.nodes.it.will.fail";

        // Verify that excessive nodes are caught
        assert_eq!(
            Event::from(too_many_nodes),
            Err(EventError::IdExceedsMaxNodes(too_many_nodes)),
            "Failed to catch excessive ID nodes"
        );
    }

    #[test]
    fn id_contains_duplicates() {
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
            Err(EventError::IdContainsDuplicates(invalid_string)),
            "Failed to reject invalid event descriptor"
        );
    }

    #[test]
    fn empty_node() {
        let empty_node = "this.has.an..empty.node";

        // Verify empty node is caught
        assert_eq!(
            Event::from(empty_node),
            Err(EventError::IdNodeIsEmpty(empty_node, 3)),
            "Failed to catch empty node"
        );
    }

    #[test]
    fn output() {
        let source = "error.send.failed";
        let event = Event::from(source).unwrap();
        
        println!("Event ID: '{}'", event);

        assert_eq!(
            source,
            format!("{}", event),
            "Formatted Event does not match source string"
        );
    }
}
