# include path
ZSH=~/.zsh
fpath=( $SESSION_DIR/zsh/functions "${fpath[@]}" )
autoload -Uz $SESSION_DIR/zsh/functions/*(:t)

# options
setopt autocd beep extendedglob nomatch prompt_subst menu_complete
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
zstyle ':completion:*' recursive-files '*'
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

if [ ${IN_NIX_SHELL:-0} = 1 ]; then
    PROMPT="%F{red}%~%f
%(?,%F{black},%F{red})➤%f "
else
    PROMPT="%F{blue}%~%f
%(?,%F{black},%F{red})➤%f "

fi

_gitinfo () {
   vcprompt -f "%u%m%b"
}

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

# key bindings
bindkey -e
bindkey ";3A" history-beginning-search-backward
bindkey ";3B" history-beginning-search-forward

# command aliases
alias ls='ls --color'
alias l='ls -l'
alias la='l -a'

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
bindkey '^[#' vbe-insert-bookmark
