# Teamtype-Hedgedoc bridge prototype

To build:

```
cargo build --bin ethersync-hedgedoc
```

Configuration in the Neovim Teamtype plugin (for Lazy):

```lua
    init = function()
        local teamtype = require("teamtype")

        teamtype.config("hedgedoc", {
            cmd = {
                os.getenv("HOME") .. "/path/to/daemon/target/release/ethersync-hedgedoc",
                "https://md.ha.si/test",
            },
            root_dir = function(bufnr, on_dir)
                local name = vim.api.nvim_buf_get_name(bufnr)
                if string.find(name, "https://md.ha.si/test") == 1 then
                    on_dir("/tmp")
                end
            end,
        })
        teamtype.enable("hedgedoc")
    end
```

Open with:

```
nvim https://md.ha.si/test
```
