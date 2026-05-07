-- SPDX-FileCopyrightText: 2025 blinry <mail@blinry.org>
-- SPDX-FileCopyrightText: 2025 zormit <nt4u@kpvn.de>
-- SPDX-FileCopyrightText: 2026 Caleb Maclennan <caleb@alerque.com>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

local teamtype = require("teamtype")

-- Enable testing or using a teamtype binary outside of the path without
-- creating a dedicated config file for it. Used in e2e testing.
local binary_name = os.getenv("TEAMTYPE_BINARY") or "teamtype"

teamtype.config("teamtype", {
    cmd = { binary_name, "client" },
    root_markers = ".teamtype",
})

teamtype.enable("teamtype")
