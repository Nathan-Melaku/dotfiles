export HISTFILE="$HOME/.zsh_history"
export HISTSIZE=100000
export SAVEHIST=100000
export VISUAL="emacsclient -nw"
export EDITOR="emacsclient -nw"
export EZA_ICON_SPACING=2
export EZA_ICONS_AUTO=1
export DOCKER_HOST="unix:///var/run/docker.sock"

# aliases
alias vim="nvim"
alias ls="eza -l"
alias grep="grep --color=auto"
alias diff="diff --color=auto"
alias ll="ls -lah"
alias la="ls -a"
alias cat="bat"
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"
alias v="emacsclient -nc"
eval $(thefuck --alias)

## PLUGINS
zinit ice depth=1; zinit light romkatv/powerlevel10k
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-syntax-highlighting
zinit light Aloxaf/fzf-tab
zinit snippet OMZP::git
zinit snippet OMZP::sudo
zinit snippet OMZP::archlinux
zinit snippet OMZP::aws
zinit snippet OMZP::command-not-found

eval "$(zoxide init --cmd cd zsh)"

autoload -Uz compinit && compinit -ie
zstyle ':completion:*:git-checkout:*' sort false
zstyle ':completion:*:descriptions' format '[%d]'
zstyle ':completion:*' menu no
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':fzf-tab:complete:cd:*' fzf-preview 'eza -1 --color=always $realpath'
zstyle ':fzf-tab:*' switch-group '<' '>'

bindkey -e
bindkey '^p' history-search-backward
bindkey '^n' history-search-forward

zinit cdreplay -q

# PATHS
export PATH="$HOME/.local/share/JetBrains/Toolbox/scripts:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.config/emacs/bin:$PATH"
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

source /usr/share/fzf/completion.zsh
source /usr/share/fzf/key-bindings.zsh

export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --level=2 --color=always {} | head -200'"

__fzf_comprun() {
    local command=$1
    shift
    case "$command" in
        cd) fzf --preview 'eza --tree --color=always {} | head -200' "$@" ;;
        export | unset) fzf --preview "eval 'echo \$'{}" "$@" ;;
        ssh) fzf --preview 'dig {}' "$@" ;;
        *) fzf --preview 'bat -n --color=always --line-range :500 {}' "$@" ;;
    esac
}
