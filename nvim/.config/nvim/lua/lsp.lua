local nvim_lsp = require('lspconfig')

local on_attach = function(client, bufnr)
    local function map(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function opt(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    require('completion').on_attach()

    -- Enable completion triggered by <c-x><c-o>
    opt('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    local opts = {noremap = true, silent = true}

    -- See `:help vim.lsp.*` for documentation on any of the below functions
    map('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    map('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    map('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    map('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    map('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    map('n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>',
        opts)
    map('n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>',
        opts)
    map('n', '<leader>wl',
        '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>',
        opts)
    map('n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    map('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    map('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    map('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    map('n', '<leader>e',
        '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    map('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    map('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
    map('n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
    map("n", "<leader>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches

local servers = {"rust_analyzer", "tsserver", "gopls"}
for _, lsp in ipairs(servers) do nvim_lsp[lsp].setup {on_attach = on_attach} end

-- Setup diagnostics formaters and linters for non LSP provided files
nvim_lsp.diagnosticls.setup {
    on_attach = on_attach,
    cmd = {"diagnostic-languageserver", "--stdio"},
    filetypes = {"lua", "sh", "markdown", "json", "yaml", "toml"},
    init_options = {
        linters = {
            eslint = {
                sourceName = "eslint",
                command = "eslint_d",
                -- REVIEW: does regex work?
                rootPatterns = {
                    ".eslintrc.js", ".eslintrc.json", ".eslintrc.yaml",
                    ".eslintrc.yml"
                },
                debounce = 100,
                args = {
                    "--stdin", "--stdin-filename", "%filepath", "--format",
                    "json"
                },
                parseJson = {
                    errorsRoot = "[0].messages",
                    line = "line",
                    column = "column",
                    endLine = "endLine",
                    endColumn = "endColumn",
                    message = "${message} [${ruleId}]",
                    security = "severity"
                },
                securities = {[2] = "error", [1] = "warning"}
            },
            shellcheck = {
                command = "shellcheck",
                debounce = 100,
                args = {"--format", "json", "-"},
                sourceName = "shellcheck",
                parseJson = {
                    line = "line",
                    column = "column",
                    endLine = "endLine",
                    endColumn = "endColumn",
                    message = "${message} [${code}]",
                    security = "level"
                },
                securities = {
                    error = "error",
                    warning = "warning",
                    info = "info",
                    style = "hint"
                }
            }
        },
        filetypes = {
            sh = "shellcheck",
            javascript = "eslint",
            typescript = "eslint",
            javascriptreact = "eslint",
            typescriptreact = "eslint"
        },
        formatters = {
            prettier = {
                command = "prettier",
                args = {"--stdin-filepath", "%filepath"}
            }
        },
        formatFiletypes = {
            json = "prettier",
            yaml = "prettier",
            toml = "prettier",
            markdown = "prettier",
            lua = "prettier"
        }
    }
}

-- Enable diagnostics
vim.lsp.handlers["textDocument/publishDiagnostics"] =
    vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        underline = true,
        virtual_text = false,
        signs = true,
        update_in_insert = true
    })
