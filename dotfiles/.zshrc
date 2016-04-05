zstyle ':completion:*' auto-description 'is: %d'
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate
zstyle ':completion:*' file-sort name
zstyle ':completion:*' format '%d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt %Sscrolled %p%s
zstyle ':completion:*' verbose true
zstyle :compinstall filename '/home/hinton/.zshrc'

autoload -Uz compinit
compinit
HISTFILE=~/.zsh/history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd beep extendedglob nomatch
unsetopt notify
bindkey -e

autoload -U colors && colors

eval $(dircolors -b)

alias ls='ls --color'
alias l='ls -l'
alias la='l -a'
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

if [ -f "${HOME}/.gpg-agent-info" ]; then
  . "${HOME}/.gpg-agent-info"
  export GPG_AGENT_INFO
  export SSH_AUTH_SOCK
fi

PROMPT="%F{blue}%~%f
%(?,%F{black},%F{red})➙%f "

preexec() {
    _STARTED=$(date +%s)
}

precmd() {
    if ! [ -z "$_STARTED" ]; then
        NOW=$(date +%s)
        DELTA=$(($NOW - $_STARTED))
        if [ $DELTA -gt 5 ]; then
            echo -e "\a$DELTA seconds"
        fi
    fi
    printf "\033];%s\07\n" "$PWD"
}
