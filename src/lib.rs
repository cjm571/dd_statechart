/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : lib.rs

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
    Top-level module defining Data-Driven Harel Statecharts.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

//TODO: Need to use function pointer for this or some shit
//TODO: Add nested nodes or statecharts
//TODO: Proper error enum implementation with description() and shit


///////////////////////////////////////////////////////////////////////////////
//  Module Declarations
///////////////////////////////////////////////////////////////////////////////

pub mod node;
use node::*;

pub mod transition_event;
use transition_event::*;


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Top-level representation of a complete statechart.
/// 
/// Contains a list of all nodes and events that make up the statechart.
#[derive(Default)]
pub struct StateChart<'a> {
    nodes:          Vec<Node<'a>>,
    events:         Vec<TransitionEvent>,
}


#[derive(Debug)]
pub enum StateChartError {
    AlreadyExists,
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl<'a> StateChart<'a> {

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    pub fn nodes(&self) -> &Vec<Node<'a>> {
        &self.nodes
    }

    pub fn events(&self) -> &Vec<TransitionEvent> {
        &self.events
    }
    

    /*  *  *  *  *  *  *  *\
     *  Mutator Methods   *
    \*  *  *  *  *  *  *  */
    pub fn add_node(&mut self, node: Node<'a>) -> Result<(), StateChartError> {
        // Check for duplicate before adding
        self.duplicate_node_check(&node)?;

        self.nodes.push(node);

        Ok(())
    }

    pub fn add_event(&mut self, event: TransitionEvent) -> Result<(), StateChartError> {
        // Check for duplicate before adding
        self.duplicate_event_check(&event)?;

        self.events.push(event);

        Ok(())
    }

    
    /*  *  *  *  *  *  *  *\
     *  Helper Methods    *
    \*  *  *  *  *  *  *  */
    /// Checks if the given node already exists
    fn duplicate_node_check(&self, node: &Node) -> Result<(), StateChartError> {
        for existing_node in &self.nodes {
            if existing_node.id() == node.id() {
                return Err(StateChartError::AlreadyExists)
            }
        }

        Ok(())
    }
    
    fn duplicate_event_check(&self, event: &TransitionEvent) -> Result<(), StateChartError> {
        for existing_event in &self.events {
            if existing_event.id() == event.id() {
                return Err(StateChartError::AlreadyExists)
            }
        }

        Ok(())
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Unit Tests
///////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_combat_statechart() {
        let mut statechart = StateChart::default();
    }
}
