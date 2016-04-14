export SOURCED_PROFILE=1
export SESSION_DIR="$HOME/session"
export MAIL_DIR="$HOME/.mail"
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient"
export GPG_AGENT_INFO=$HOME/.gnupg/S.gpg-agent
export SSH_AUTH_SOCK=$HOME/.gnupg/S.gpg-agent.ssh

gpg-connect-agent /bye

export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
export GPG_AGENT_INFO="${HOME}/.gnupg/S.gpg-agent"

# broken JIT in javascriptcore; disable.
export JavaScriptCoreUseJIT=0

# disable coredumps
ulimit -S -c 0 >/dev/null 2>&1
