// SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
//
// SPDX-License-Identifier: AGPL-3.0-or-later

use mlua::prelude::*;
use mlua::{LuaSerdeExt, SerializeOptions};

use crate::config::Config;

#[mlua::lua_module]
fn teamtype(lua: &Lua) -> LuaResult<LuaTable> {
    let exports = lua.create_table()?;
    exports.set("version", env!("CARGO_PKG_VERSION"))?;
    exports.set("parse_config", LuaFunction::wrap_raw::<_, _>(parse_config))?;
    Ok(exports)
}

fn parse_config(conf: Config) -> Config {
    conf
}

impl FromLua for Config {
    fn from_lua(value: LuaValue, lua: &Lua) -> LuaResult<Self> {
        match value {
            LuaValue::Table(_) => Ok(lua.from_value(value)?),
            _ => Err(LuaError::external("Config must be a table.")),
        }
    }
}

impl IntoLua for Config {
    fn into_lua(self, lua: &Lua) -> LuaResult<LuaValue> {
        let opts = SerializeOptions::new().serialize_none_to_null(false);
        lua.to_value_with(&self, opts)
    }
}
