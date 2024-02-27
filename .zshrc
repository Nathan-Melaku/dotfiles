
# Created by newuser for 5.9
eval "$(starship init zsh)"

export HISTFILE="$HOME/.zsh_history"
export SAVEHIST=100000

# aliases
alias vim="nvim"
alias v="nvim"

# plugins
source "$HOME/.config/zsh/plugins.zsh"

autoload -U compinit; compinit
zstyle ':completion:*' menu select

export PATH="$HOME/go/bin:$PATH"
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
