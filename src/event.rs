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
    //TODO: Purpose statement

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::fmt;


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Represents an event that can be broadcast to the statechart
#[derive(Debug, PartialEq)]
pub struct Event {
    id: EventId,
}

//OPT: *DESIGN* Using Clone as a crutch here... Probably should be a copyable struct
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct EventId {
    id_nodes: Vec<&'static str>,
}


#[derive(Debug, PartialEq)]
pub enum EventError {
    InvalidSourceString,
}

///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl Event {
    /// Creates a new Event object with the given ID.
    pub fn new(id: EventId) -> Self {
        Self { id }
    }
    
    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    pub fn id(&self) -> EventId {
        self.id.clone()
    }
}

impl EventId {
    pub fn from(source: &'static str) -> Result<Self, EventError> {
        // Ensure there are no repeated ID nodes in the ID
        let source_nodes: Vec<&str> = source.split('.').collect();
        
        let mut deduped_nodes = source_nodes.clone();
        deduped_nodes.sort_unstable();
        deduped_nodes.dedup();

        if source_nodes.len() != deduped_nodes.len() {
            return Err(EventError::InvalidSourceString);
        }

        Ok(
            Self {
                id_nodes: source_nodes,
            }
        )
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

impl fmt::Display for EventId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut id_node_iter = self.id_nodes.iter();

        write!(f, "{}", id_node_iter.next().unwrap())?;
        for id_node in id_node_iter {
            write!(f, ".{}", id_node)?;
        }

        Ok(())
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {

    use crate::event::{
        EventId,
        EventError,
    };

    #[test]
    fn from_string() {
        let valid_string = "error.send.failed";
        let invalid_string = "error.send.error";

        // Verify valid string parsing
        assert_eq!(
            EventId::from(valid_string).is_ok(),
            true,
            "Failed to parse a valid event descriptor"
        );

        // Verify invalid string handling
        assert_eq!(
            EventId::from(invalid_string),
            Err(EventError::InvalidSourceString),
            "Failed to reject invalid event descriptor"
        );
    }

    #[test]
    fn output() {
        let source = "error.send.failed";
        let event_id = EventId::from(source).unwrap();
        
        println!("Event ID: '{}'", event_id);

        assert_eq!(
            source,
            format!("{}", event_id),
            "Formatted EventId does not match source string"
        );
    }
}
