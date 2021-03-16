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

use std::{
    collections::HashMap,
    error::Error,
    fmt,
};

use crate::{
    StateChartId,
    event::Event,
    interpreter::EcmaScriptValue,
};

use uuid::Uuid;


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

//FIXME: Is this Clone necessary?
#[derive(Clone, Debug, PartialEq)]
pub struct SystemVariables {
    _event:         Option<Event>,
    _sessionid:     Uuid,
    _name:          StateChartId,
    _ioprocessors:  Vec<String>,
    _x:             HashMap<String, EcmaScriptValue>,
}

#[derive(Debug, PartialEq)]
pub enum DataModelError {
    InvalidValueType(
        String /* Identifier name */
    ),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl SystemVariables {

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

    pub fn _x(&self) -> &HashMap<String, EcmaScriptValue> {
        &self._x
    }


    /*  *  *  *  *  *  *  *\
     *  Mutator Methods   *
    \*  *  *  *  *  *  *  */

    pub fn set_name(&mut self, name: String) {
        self._name = name;
    }

    pub fn set_event(&mut self, event: Event) {
        self._event = Some(event);
    }
    
    //FIXME: *STYLE* either delete this function and use _x directly, or create a get_data_member function for consistency
    //FIXME: Why does this return a Result?
    pub fn set_data_member(&mut self, id: String, value: EcmaScriptValue) -> Result<Option<EcmaScriptValue>, DataModelError> {
        // Insert value into data map
        Ok(self._x.insert(id, value))
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////


/*  *  *  *  *  *  *  *\
 *   DataModelError   *
\*  *  *  *  *  *  *  */

impl Error for DataModelError {}

impl fmt::Display for DataModelError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidValueType(identifier_name) => {
                write!(f, "Type of identifier '{}' is and invalid type.", identifier_name)
            },
        }
    }
}


/*  *  *  *  *  *  *  *\
 *  SystemVariables   *
\*  *  *  *  *  *  *  */

impl Default for SystemVariables {
    fn default() -> Self {
        Self {
            _event:         None,
            _sessionid:     Uuid::new_v4(),
            _name:          String::new(),
            _ioprocessors:  Vec::new(),
            _x:             HashMap::default()
        }
    }
}

