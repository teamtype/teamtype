-- SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

rockspec_format = "3.0"
package = "teamtype"
version = "dev-1"

source = {
	url = "git+https://github.com/alerque/teamtype.git",
	branch = "luafy",
}

description = {
	summary = "Lua module with bindings to shared library built from the Rust teamtype crate",
	detailed = [[
	    Control Teamtype programmatically through bindings to the dynamically linked
	    C module built from the Rust crate.
	]],
	license = "AGPL-3.0-only",
	homepage = "https://github.com/teamtype/teamtype",
	issues_url = "https://github.com/teamtype/teamtype/issues",
	maintainer = "Caleb Maclennan <caleb@alerque.com>",
	labels = { "i18n" },
}

dependencies = {
	"lua >= 5.1",
	"luarocks-build-rust-mlua >= 0.2.7-1",
}

build = {
	type = "rust-mlua",
	modules = {
		"teamtype",
	},
}
