
# Created by newuser for 5.9
eval "$(starship init zsh)"

export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=100000
export SAVEHIST=100000
export VISUAL="emacsclient -nw"

# aliases
alias vim="nvim"
alias v="nvim"
alias ls="ls --color=auto"
alias grep="grep --color=auto"
alias diff="diff --color=auto"
alias ll="ls -lh"
alias la="ls -a"
alias cat="bat"
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

# plugins
source "$HOME/.config/zsh/plugins.zsh"

autoload -Uz compinit && compinit -ie
zstyle ':completion:*' menu select=2

export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"

####### Automatically created ####################
# bun completions
[ -s "/home/nathan/.bun/_bun" ] && source "/home/nathan/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"


# pnpm
export PNPM_HOME="/home/nathan/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac
# pnpm end

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
