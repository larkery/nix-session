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

eval $(dircolors -b $HOME/session/dircolors.ansi-light)

alias ls='ls --color'
alias l='ls -l'
alias la='l -a'
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

if [ ${IN_NIX_SHELL:-0} = 1 ]; then
    PROMPT="%F{red}%~%f
%(?,%F{black},%F{red})➤%f "
else
    PROMPT="%F{blue}%~%f
%(?,%F{black},%F{red})➤%f "

fi

_gitinfo () {
   vcprompt
}

setopt prompt_subst
RPROMPT='$(_gitinfo)'

case $TERM in
    xterm*)
        preexec() {
            _STARTED=$(date +%s)
            printf "\033];%s\07" "$1"
        }

        precmd() {
            if ! [ -z "$_STARTED" ]; then
                NOW=$(date +%s)
                DELTA=$(($NOW - $_STARTED))
                if [ $DELTA -gt 5 ]; then
                    echo -e "\a$DELTA seconds"
                fi
            fi
            _STARTED=""
            print -Pn "\e]0;%~\a"
        }
        ;;
esac

export GPG_AGENT_INFO=$HOME/.gnupg/S.gpg-agent
export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh
