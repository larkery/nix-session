# nixos terminal login does not do this
# for reasons which escape me.
if [[ -z $SOURCED_PROFILE ]]; then
    echo "sourcing profile..."
    source $HOME/.profile
fi

# include path
ZSH=~/.zsh
fpath=( $SESSION_DIR/zsh/functions "${fpath[@]}" )
autoload -Uz $SESSION_DIR/zsh/functions/*(:t)

# options
setopt autocd beep extendedglob nomatch prompt_subst menu_complete re_match_pcre
unsetopt notify

autoload -U zutil

autoload compinit 
autoload -U complist
compinit

autoload -U colors && colors

# completion settings
zstyle -e ':completion:*' completer '
  if [[ $_last_try != "$HISTNO$BUFFER$CURSOR" ]]; then
    _last_try="$HISTNO$BUFFER$CURSOR"
    reply=(_complete _match _prefix)
  else
    reply=(_ignored _correct _approximate)
  fi'

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

MARKPATH=$ZSH/run/marks
bookmark -l

vbe-insert-bookmark() {
   emulate -L zsh
   LBUFFER=${LBUFFER}"~-"
   zle expand-or-complete-prefix
}

zle -N vbe-insert-bookmark
bindkey '^[i' vbe-insert-bookmark

# directory memory
autoload -Uz chpwd_recent_dirs cdr
add-zsh-hook chpwd chpwd_recent_dirs

zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':chpwd:*' recent-dirs-max 50
zstyle ':completion:*' recent-dirs-insert always

rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}

zle -N rationalise-dot
bindkey . rationalise-dot

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

# either run a command or run ls if nothing there


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

_my_cd () {
    _cd
    _my_files
}

compdef '_my_cd' cd

_my_cp () {
    _cp
    _my_files
}

compdef '_my_cp' cp
