/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : node.rs

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

use crate::transition_event::TransitionEvent;


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Represents a state node within a statechart.
/// 
/// May contain one or more entry events, and zero or more exit events.
pub struct Node<'a> {
    id:         String,
    entrances:  Vec<Connection<'a>>,
    exits:      Vec<Connection<'a>>,
}

/// Represents a unidirectional connection between two nodes.
/// 
/// Contains references to a "From" and "To" node, as well as the TransitionEvent that connects the two.
pub struct Connection<'a> {
    id:     String,
    source: &'a Node<'a>,
    target: &'a Node<'a>,
    event:  &'a TransitionEvent,
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *        Node        *
\*  *  *  *  *  *  *  */
impl Node<'_> {

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    pub fn id(&self) -> &String {
        &self.id
    }

    pub fn entrances(&self) -> &Vec<Connection> {
        &self.entrances
    }

    pub fn exits(&self) -> &Vec<Connection> {
        &self.exits
    }
}


/*  *  *  *  *  *  *  *\
 *     Connection     *
\*  *  *  *  *  *  *  */
impl Connection<'_> {

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    pub fn id(&self) -> &String {
        &self.id
    }

    pub fn source(&self) -> &Node {
        &self.source
    }

    pub fn target(&self) -> &Node {
        &self.target
    }

    pub fn event(&self) -> &TransitionEvent {
        &self.event
    }
}
