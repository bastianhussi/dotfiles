local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
  execute 'packadd packer.nvim'
end

-- use ft to load when filetyp is used
-- use cmd to load when executing commands
-- use opt to make this optional
-- use after to load after ...
-- use requires for list of dependencies
return require('packer').startup(function()
    use 'wbthomason/packer.nvim'
    use 'tpope/vim-surround'
    use 'tpope/vim-commentary'
    use 'nvim-lua/completion-nvim'
    use 'neovim/nvim-lspconfig'
    use 'styled-components/vim-styled-components'
    use 'norcalli/snippets.nvim'
    use 'mhartington/formatter.nvim'
    use {'tpope/vim-dispatch', opt = true, cmd = {'Dispatch', 'Make', 'Focus', 'Start'}}
    use {
        'nvim-telescope/telescope.nvim',
        requires = {'nvim-lua/popup.nvim', 'nvim-lua/plenary.nvim'},
    }
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
    }
    use {
        'folke/todo-comments.nvim',
        requires = 'nvim-lua/plenary.nvim',
    }
    use {
      "folke/trouble.nvim",
      requires = "kyazdani42/nvim-web-devicons",
    }
    -- FIXME: doesnt work!
    use {
      'lewis6991/gitsigns.nvim',
      requires = {
        'nvim-lua/plenary.nvim'
      },
      config = function()
        require('gitsigns').setup()
      end
    }
    use "folke/zen-mode.nvim"
    use 'folke/tokyonight.nvim'
    use {'dracula/vim', as = 'dracula', opt = true}
    use 'folke/which-key.nvim'
end)
