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

use std::net::{SocketAddr, UdpSocket};
use std::{collections::HashMap, error::Error, fmt};

use crate::{event::Event, interpreter::EcmaScriptValue, StateChartId};

use uuid::Uuid;


///////////////////////////////////////////////////////////////////////////////
//  Named Constants
///////////////////////////////////////////////////////////////////////////////

const OS_ASSIGNED_SOCKET_ADDR: &str = "0.0.0.0:0";


///////////////////////////////////////////////////////////////////////////////
//  Data Structures
///////////////////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq)]
pub struct SystemVariables {
    _event: Option<Event>,
    _sessionid: Uuid,
    _name: StateChartId,
    _ioprocessors: Vec<IoProcessor>,
    _x: HashMap<String, EcmaScriptValue>,
}

#[derive(Debug)]
pub struct IoProcessor {
    name: String,
    socket: UdpSocket,
    location: SocketAddr,
}

#[derive(Debug, PartialEq)]
pub enum DataModelError {
    InvalidValueType(String /* Identifier name */),
}


///////////////////////////////////////////////////////////////////////////////
//  Object Implementations
///////////////////////////////////////////////////////////////////////////////

impl IoProcessor {
    pub fn new_default() -> Result<Self, std::io::Error> {
        let socket = UdpSocket::bind(OS_ASSIGNED_SOCKET_ADDR)?;
        let location = socket.local_addr()?;

        Ok(Self {
            name: "http://www.w3.org/TR/scxml/#SCXMLEventProcessor".to_string(),
            socket,
            location,
        })
    }

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn location(&self) -> &SocketAddr {
        &self.location
    }

    pub fn socket(&self) -> &UdpSocket {
        &self.socket
    }

    //FIXME: UNWRAP()
    pub fn socket_owned(&self) -> UdpSocket {
        self.socket.try_clone().unwrap()
    }
}


impl SystemVariables {
    pub fn new_default() -> Result<Self, std::io::Error> {
        Ok(Self {
            _event: None,
            _sessionid: Uuid::new_v4(),
            _name: String::default(),
            _ioprocessors: vec![IoProcessor::new_default()?],
            _x: HashMap::default(),
        })
    }

    /*  *  *  *  *  *  *  *\
     *  Accessor Methods  *
    \*  *  *  *  *  *  *  */

    pub fn _event(&self) -> Option<Event> {
        self._event.as_ref().cloned()
    }

    pub fn _ioprocessors(&self) -> &Vec<IoProcessor> {
        &self._ioprocessors
    }

    pub fn _x(&self) -> &HashMap<String, EcmaScriptValue> {
        &self._x
    }

    pub fn get_data_member(&self, id: &str) -> Option<&EcmaScriptValue> {
        // Insert value into data map
        self._x.get(id)
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

    pub fn set_data_member(&mut self, id: &str, value: EcmaScriptValue) -> Option<EcmaScriptValue> {
        // Insert value into data map
        self._x.insert(id.to_string(), value)
    }
}


///////////////////////////////////////////////////////////////////////////////
//  Trait Implementations
///////////////////////////////////////////////////////////////////////////////

/*  *  *  *  *  *  *  *\
 *     IoProcessor    *
\*  *  *  *  *  *  *  */
impl PartialEq for IoProcessor {
    fn eq(&self, other: &Self) -> bool {
        (self.name == other.name) && (self.location == other.location)
    }
}


/*  *  *  *  *  *  *  *\
 *   DataModelError   *
\*  *  *  *  *  *  *  */

impl Error for DataModelError {}

impl fmt::Display for DataModelError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::InvalidValueType(identifier_name) => {
                write!(
                    f,
                    "Type of identifier '{}' is and invalid type.",
                    identifier_name
                )
            }
        }
    }
}
