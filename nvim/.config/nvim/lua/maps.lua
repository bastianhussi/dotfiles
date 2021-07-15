local map = vim.api.nvim_set_keymap
local g = vim.g

-- map('n', '<Space>', '', {})
g.mapleader = ' '

options = {noremap = true}
map('n', '<leader><esc>', ':nohlsearch<cr>', options)
map('n', '<leader>n', ':bnext<cr>', options)
map('n', '<leader>p', ':bprev<cr>', options)
map('i', '<tab>', [[pumvisible() ? "<c-n>" : "<tab>"]],
    {noremap = true, expr = true})
map('i', '<s-tab>', [[pumvisible() ? "<c-n>" : "<s-tab>"]],
    {noremap = true, expr = true})

map('i', 'jj', '<esc>', options)

-- Indenting selected text multiple times, without loosing the selection
map('x', '<', '<gv', options)
map('x', '>', '>gv', options)

map('n', '<leader>f', ':Format<cr>', {noremap = true})

-- map('n', '<leader>ff', ':Telescope find_files<cr>', options)
