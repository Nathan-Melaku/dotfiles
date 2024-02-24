local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

local plugins = {
  "folke/which-key.nvim",
  {
    "folke/neoconf.nvim",
    cmd = "Neoconf"
  },
  "folke/neodev.nvim",
  {
    'nvim-telescope/telescope.nvim',
    tag = '0.1.5',
    dependencies = { 'nvim-lua/plenary.nvim',
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
        cond = function ()
          return vim.fn.executable 'make' == 1
        end,
      },
    },
  },
  { "rose-pine/neovim",                name = "rose-pine" },
  { "nvim-treesitter/nvim-treesitter", cmd = "TSUpdate" },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    after = "nvim-treesitter",
  },
  "mbbill/undotree",
  "tpope/vim-fugitive",
  --- Uncomment the two plugins below if you want to manage the language servers from neovim
  { 'williamboman/mason.nvim' },
  { 'williamboman/mason-lspconfig.nvim' },

  { 'VonHeikemen/lsp-zero.nvim',        branch = 'v3.x' },
  { 'neovim/nvim-lspconfig' },
  { 'hrsh7th/cmp-nvim-lsp' },
  { 'hrsh7th/nvim-cmp' },
  { 'L3MON4D3/LuaSnip' },
  { 'mfussenegger/nvim-jdtls' },
  { 'mfussenegger/nvim-dap' },
  { "rcarriga/nvim-dap-ui",             dependencies = { "mfussenegger/nvim-dap" } },
  {
    'mrcjkb/rustaceanvim',
    version = '^4', -- Recommended
    ft = { 'rust' },
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' }
  },
  {
    "nvim-neorg/neorg",
    build = ":Neorg sync-parsers",
    lazy = false, -- specify lazy = false because some lazy.nvim distributions set lazy = true by default
  },
  { 'rstacruz/vim-closer' },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.17",
    dependencies = {
      "MunifTanjim/nui.nvim",
      "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
    }
  },
  { 'akinsho/toggleterm.nvim', version = "*", config = true },
  {
    "kylechui/nvim-surround",
    version = "*", -- Use for stability; omit to use `main` branch for the latest features
    event = "VeryLazy"
  },
  -- add this to your lua/plugins.lua, lua/plugins/init.lua,  or the file you keep your other plugins:
  {
    'numToStr/Comment.nvim',
    lazy = false,
  },
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
  },
  {
    "roobert/tailwindcss-colorizer-cmp.nvim",
  },

  {
    "olrtg/nvim-emmet",
    config = function()
      vim.keymap.set({ "n", "v" }, '<leader>xe', require('nvim-emmet').wrap_with_abbreviation)
    end,
  },
  {
    'NvChad/nvim-colorizer.lua',
  }

}
local opts = {}
require("lazy").setup(plugins, opts)
