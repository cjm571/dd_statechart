/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
Filename : datamodel.rs

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
    This module maintains an internal representation of the datamodel defined
    in a compliant SCXML document. Additionally, it provides convenience functions
    for interacting with the data items therein.

\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

use std::collections::HashMap;

use crate::{
    StateChartId,
    event::Event,
};

use uuid::Uuid;


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

#[derive(Clone, Debug, PartialEq)]
pub struct SystemVariables {
    _event:         Option<Event>,
    _sessionid:     Uuid,
    _name:          StateChartId,
    _ioprocessors:  Vec<String>,
    _x:             HashMap<String, u32>,
}

#[derive(Debug, Default, PartialEq)]
struct DataMembers {
    data_map: HashMap<String, u32>, //TODO: Value data type might have to be something fancy...
}



///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl SystemVariables {
    pub fn new(name: StateChartId) -> Self {
        Self {
            _event:         None,
            _sessionid:     Uuid::new_v4(),
            _name:          name,
            _ioprocessors:  Vec::new(),
            _x:             HashMap::default()
        }
    }


    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn _event(&self) -> Option<Event> {
        if let Some(event) = &self._event {
            Some(event.clone())
        }
        else {
            None
        }
    }


    /*  *  *  *  *  *  *  *\
     *  Mutator Methods   *
    \*  *  *  *  *  *  *  */

    pub fn set_event(&mut self, event: Event) {
        self._event = Some(event);
    }

    pub fn set_data_member(&mut self, id: String, value: u32) {
        //OPT: *DESIGN* Gracefully handle this Option
        self._x.insert(id, value);
    }
}
