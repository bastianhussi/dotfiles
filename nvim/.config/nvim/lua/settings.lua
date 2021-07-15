-- Global options
local o = vim.o
-- Buffer-local options
local bo = vim.bo
-- Window-local options
local wo = vim.wo

local g = vim.g

local opt = vim.opt
local cmd = vim.cmd
local u = require('utils')

o.hlsearch = true

o.number = true
o.relativenumber = true
o.cursorline = true

o.clipboard = 'unnamedplus'

o.spell = false
o.spelllang = 'en_us'

o.encoding = 'utf-8'

o.wildmenu = true
o.wildmode = 'full'
o.pumheight = 15 -- Show up to 15 items
o.pumblend = 10 -- 10% transparency

o.textwidth = 99
o.linebreak = true
-- highlight column after 'textwidth'
o.colorcolumn = "+1"
-- TODO: change these colors
--		:hi ColorColumn ctermbg=lightgrey guibg=lightgrey

o.splitbelow = true
o.splitright = true

-- Switch between buffers, even if they have unsaved changes
o.hidden = true

opt.completeopt = {'menuone', 'noinsert', 'noselect'}

opt.matchpairs:append({'<:>'})

-- TODO: check the documentation on these with :h ...
o.smartindent = true
o.smarttab = true
o.expandtab = true
o.tabstop = 4
o.softtabstop = 4
o.shiftwidth = 4

o.listchars = 'tab:»·,trail:~,extends:>,precedes:<,space:·,eol:¬'
o.list = true

-- Automatically change the current directory
o.autochdir = true

o.showmode = false -- Hide the mode

o.termguicolors = true

g.tokionight_style = 'storm'
g.tokyonight_terminal_colors = true -- default
g.tokyonight_italic_comments = true -- default
g.tokyonight_italic_keywords = true -- default
g.tokyonight_italic_functions = true
g.tokyonight_italic_variables = false -- default
-- Darker background for these windows
g.tokyonight_sidebars = {"packer", "terminal"}
-- Darker sidebars
g.tokyonight_dark_sidebar = true -- default
-- Darker float windows
g.tokyonight_dark_float = true -- default
cmd("colorscheme tokyonight")

local wk = require('which-key')

wk.register({
    f = {
        name = "file",
        f = {"<cmd>Telescope find_files<cr>", "Find File"},
        g = {"<cmd>Telescope live_grep<cr>", "Live Grep"},
        b = {"<cmd>Telescope buffers<cr>", "Buffers"}
    },
    z = {"<cmd>ZenMode<cr>", "Zen"}
}, {mode = 'n', prefix = '<leader>'})

require('todo-comments').setup()

require('snippets').use_suggested_mappings()

g.completion_enable_snippet = 'snippets.nvim'
g.completion_matching_smart_case = 1

cmd("autocmd BufEnter * lua require('completion').on_attach()")
-- TODO: https://www.youtube.com/watch?v=zQPgUnDTsu4
--
cmd("autocmd BufEnter *.{js,jsx,ts,tsx} :syntax sync fromstart")
cmd("autocmd BufLeave *.{js,jsx,ts,tsx} :syntax sync clear")

local ts = require 'nvim-treesitter.configs'
ts.setup {ensure_installed = 'maintained', highlight = {enable = true}}

require("trouble").setup {}

require('formatter').setup({
    logging = false,
    filetype = {
        javascript = {
            -- prettier
            function()
                return {
                    exe = "prettier",
                    args = {"--stdin-filepath", vim.api.nvim_buf_get_name(0)},
                    stdin = true
                }
            end
        },
        rust = {
            -- Rustfmt
            function()
                return {exe = "rustfmt", args = {"--emit=stdout"}, stdin = true}
            end
        },
        lua = {
            -- luafmt
            function()
                return {
                    exe = "luafmt",
                    args = {"--indent-count", 2, "--stdin"},
                    stdin = true
                }
            end
        }
    }
})

require("zen-mode").setup {
    window = {
        backdrop = 1,
        width = 100,
        height = 0.9,
        options = {
            signcolumn = "no",
            number = false,
            relativenumber = false,
            cursorline = false,
            cursorcolumn = false,
            colorcolumn = "",
            list = false
        }
    }
}
