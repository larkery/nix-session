# nixos terminal login does not do this
# for reasons which escape me.
[[ -z $SOURCED_PROFILE ]] && source $HOME/.profile

# include path
ZSH=~/.zsh
fpath=( $SESSION_DIR/zsh/functions "${fpath[@]}" )
autoload -Uz $SESSION_DIR/zsh/functions/*(:t)

# options
setopt autocd beep extendedglob nomatch prompt_subst menu_complete re_match_pcre
unsetopt notify

autoload -U zutil

autoload compinit && {
    autoload -U complist
    compinit
}

# completion settings
zstyle ':completion:*' completer _expand _complete _ignored _correct _approximate _match
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' file-sort name
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle ':completion:*' select-prompt %Sscrolled %p%s
zstyle ':completion:*' verbose true
zstyle ':completion:*:descriptions' format '%B%F{blue}%d%f%b'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:processes' command "ps -eo pid,user,comm,cmd -w -w"
zstyle ':completion:*:functions' ignored-patterns '_*'
# not sure about this
zstyle ':completion:*:default' list-colors \
       ${(s.:.)LS_COLORS}
# this is bad.
#zstyle ':completion:*' recursive-files '*/.git/*'
zstyle ':completion:*' separate-sections yes

zstyle :compinstall filename '/home/hinton/.zshrc'

# history
setopt appendhistory hist_ignore_space hist_ignore_all_dups
HISTFILE=~/.zsh/history
HISTSIZE=10000
SAVEHIST=10000
(( $+widgets[history-incremental-pattern-search-backward] )) &&	\
    bindkey '^r' history-incremental-pattern-search-backward

autoload -U colors && colors

# prevent deleting entire paths
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

# prompt garble

PROMPTSYM="$"
PROMPT="%F{blue}%~%f\${vcs_info_msg_0_}
%B%(?,,%F{red})$PROMPTSYM%f%b "

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git svn hg
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' formats " %{$fg[green]%}%b%{$reset_color%} %c%u"
zstyle ':vcs_info:git*:*' unstagedstr "%{$fg[red]%}u%{$reset_color%}"
zstyle ':vcs_info:git*:*' stagedstr "%{$fg[yellow]%}s%{$reset_color%}"

precmd() {
    vcs_info
}

preexec() {

}

if [ ${IN_NIX_SHELL:-0} = 1 ]; then
    RPROMPT="%F{202}N%f"
fi

autoload -Uz add-zsh-hook

case $TERM in
    xterm*)
        _title_preexec() {
            _STARTED=$(date +%s)
            printf "\033];%s\07" "$1"

        }

        _timer_precmd() {
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

        add-zsh-hook preexec _title_preexec
        add-zsh-hook precmd _timer_precmd
        ;;
esac

autoload edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# command aliases
alias ls='ls -h --color'
alias l='ls -l'
alias la='l -a'
alias add='paste -s -d+|bc'
alias tab='cut -d "	" -f '

o() {
    xdg-open "$@" &|
}

alias -s org=o
alias -s pdf=o
alias -s jpg=o
alias -s png=o

# bookmarks
MARKPATH=$ZSH/run/marks
for link ($MARKPATH/*(N@)) {
        hash -d -- -${link:t}=${link:A}
    }

bookmark() {
        [[ -d $MARKPATH ]] || mkdir -p $MARKPATH
        if (( $# == 0 )); then
            # When no arguments are provided, just display existing
            # bookmarks
            for link in $MARKPATH/*(N@); do
                local markname="$fg[green]${link:t}$reset_color"
                local markpath="$fg[blue]${link:A}$reset_color"
                printf "%-30s -> %s\n" $markname $markpath
            done
        else
            # Otherwise, we may want to add a bookmark or delete an
            # existing one.
            local -a delete
            zparseopts -D d=delete
            if (( $+delete[1] )); then
                # With `-d`, we delete an existing bookmark
                command rm $MARKPATH/$1
            else
                # Otherwise, add a bookmark to the current
                # directory. The first argument is the bookmark
                # name. `.` is special and means the bookmark should
                # be named after the current directory.
                local name=$1
                [[ $name == "." ]] && name=${PWD:t}
                ln -s $PWD $MARKPATH/$name
                hash -d -- -${name}=${PWD}
            fi
        fi
    }

vbe-insert-bookmark() {
        emulate -L zsh
        LBUFFER=${LBUFFER}"~-"
    }

zle -N vbe-insert-bookmark
bindkey '^[	' vbe-insert-bookmark

# directory memory
autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs

zstyle ':chpwd:*' recent-dirs-default true

rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
zle -N rationalise-dot
bindkey . rationalise-dot

autoload -Uz narrow-to-region
_history-incremental-pattern-search-backward-with-buffer() {
    local state
    endcommand="${BUFFER[(I);]}"
    if [[ $endcommand -gt 0 ]]; then
        MARK=$endcommand
        narrow-to-region -S state
    fi
    zle history-incremental-pattern-search-backward -- ${${BUFFER## }%% }
    if [[ $endcommand -gt 0 ]]; then
        narrow-to-region -R state
    fi
}

zle -N _history-incremental-pattern-search-backward-with-buffer
bindkey '^R' _history-incremental-pattern-search-backward-with-buffer
bindkey -M isearch '^R' history-incremental-search-backward

_up-arrow() {
    if [[ -z $BUFFER || ($HISTNO != $HISTCMD && $BUFFER = $(fc -l -n $HISTNO $HISTNO)) ]]; then
        # back in history
        zle up-line-or-history
    else
        # search prefix - however, append from last ;
        zle _history-incremental-pattern-search-backward-with-buffer
    fi
}

zle -N _up-arrow

bindkey "\eOA" _up-arrow
bindkey -M isearch "\eOA" history-incremental-search-backward
bindkey -M isearch "\eOB" history-incremental-search-forward

DIRSTACKSIZE=8
setopt autopushd pushdminus pushdsilent pushdtohome

mnt() {
    [ ! -d /run/media/hinton/"$1" ] && \
        udisksctl mount --block-device /dev/disk/by-label/"$1"
    cd /run/media/hinton/"$1"
}

compdef '_files -W /dev/disk/by-label' mnt

# either run a command or run ls if nothing there

_accept_or_ls () {
    if [[ -z $BUFFER ]]
    then
        echo
        l
        echo
        precmd
        zle reset-prompt
    else
        zle accept-line
    fi
}

zle -N _accept_or_ls

_dired() {
    xdg-open . &|
}

zle -N _dired

bindkey "^M" _accept_or_ls
bindkey "^X^J" _dired

eval $(dircolors -b $HOME/session/dircolors.ansi-light)

autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey "^[m" copy-earlier-word
