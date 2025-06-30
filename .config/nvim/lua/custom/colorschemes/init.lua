return {
  {
    'metalelf0/black-metal-theme-neovim',
    lazy = false,
    priority = 1000,
    config = function()
      require('black-metal').setup {
        -- optional configuration here
      }
      require('black-metal').load()
    end,
  },
  {

    'wincent/base16-nvim',
    priority = 1000,
    init = function()
      vim.cmd.colorscheme 'default-dark'
      vim.cmd.hi 'Comment gui=none'
    end,
  },
}
