/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : transition_event.rs

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


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

/// Represents a state-changing event that will cause the of one or more state nodes to other nodes,
/// if the condition of the event is evaluates as true.
pub struct TransitionEvent {
    id:         String,
    condition:  Condition,
}

/// Represents a condition on a state-change event.
pub struct Condition {
    id: String,
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *  TransitionEvent   *
\*  *  *  *  *  *  *  */
impl TransitionEvent {

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    pub fn id(&self) -> &String {
        &self.id
    }

    pub fn condition(&self) -> &Condition {
        &self.condition
    }
}


/*  *  *  *  *  *  *  *\
 *      Condition     *
\*  *  *  *  *  *  *  */
impl Condition {

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */
    pub fn id(&self) -> &String {
        &self.id
    }
}
