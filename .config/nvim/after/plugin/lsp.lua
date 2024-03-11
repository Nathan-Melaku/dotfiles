local lsp_zero = require('lsp-zero')

lsp_zero.on_attach(function(client, bufnr)
  -- see :help lsp-zero-keybindings
  -- to learn the available actions
  lsp_zero.default_keymaps({ buffer = bufnr })
end)

lsp_zero.format_on_save({
  format_opts = {
    async = false,
    timeout_ms = 10000,
  },
  servers = {
    ['tsserver'] = { 'javascript', 'typscript' },
    ['rust_analyzer'] = { 'rust' },
    ['gopls'] = { 'go' },
  }
})

vim.g.rustaceanvim = {
  server = {
    capabilites = lsp_zero.get_capabilities()
  }
}

--  here you can setup the language servers
require('mason').setup({})
require('mason-lspconfig').setup({
  ensure_installed = {
    'jdtls',
    'astro',
    'bashls',
    'clangd',
    'cssls',
    'gopls',
    'gradle_ls',
    'html',
    'htmx',
    'jsonls',
    'zls',
    'clojure_lsp',
    'sqlls',
    'tailwindcss',
    'tsserver',
    'rust_analyzer',
    'eslint',
    'lua_ls',
    'emmet_language_server'
  },
  handlers = {
    lsp_zero.default_setup,
    jdtls = lsp_zero.noop,
    rust_analyzer = lsp_zero.noop,
    lua_ls = function()
      require('lspconfig').lua_ls.setup({
        settings = {
          Lua = {
            diagnostics = {
              globals = { 'vim' }
            }
          }
        }
      })
    end,
    gopls = function()
      require('lspconfig').gopls.setup({
        settings = {
          gopls = {
            analyses = {
              unusedparams = true,
            },
            staticcheck = true,
            gofumpt = true,
          },
        },
      })
    end
  },
})
