export ALTERNATE_EDITOR=""
export EDITOR="emacsclient"
export MANPATH=".nix-profile/share/man:/run/current-system/sw/share/man"

gpg-connect-agent /bye

export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
export GPG_AGENT_INFO="${HOME}/.gnupg/S.gpg-agent"
