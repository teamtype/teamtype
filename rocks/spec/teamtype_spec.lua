-- SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- For debugging, load raw debug builds prior to Rock packaging
package.cpath = package.cpath .. ";../target/debug/lib?.so"

local teamtype = require("teamtype")

local MANIFEST_VERSION = "0.9.2-dev"

describe("teamtype", function()
	local parse_config = teamtype.parse_config

	it("should identify its version", function()
		assert.is.equal(MANIFEST_VERSION, teamtype.version)
	end)

	it("should provide certain functions", function()
		assert.is_function(parse_config)
	end)

	describe("config", function()
		it("can cast from a table to Config and back", function()
			local conf = {
				username = "Pure Luajuice",
			}
			local Config = parse_config(conf)
			assert.is_equal(conf.username, Config.username)
			assert.is_equal("", Config.base_dir)
			assert.is_false(Config.emit_join_code)
		end)

		it("can cast from a table to Peer and back", function()
			local conf = {
				username = "Party Crasher",
				peer = { join_code = "7-potato-tango" },
			}
			local Config = parse_config(conf)
			assert.is_equal(conf.username, Config.username)
			assert.is_equal(conf.peer.join_code, Config.peer.join_code)
		end)
	end)
end)
