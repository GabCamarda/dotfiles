export ZSH_DIR=${ZSH_DIR:=${HOME}/.zsh}
export ZSH_PROFILE=${${ITERM_PROFILE:=default}:l}

[[ -f "${ZSH_DIR}/profiles/${ZSH_PROFILE}.rc.zsh" ]] &&
    source "${ZSH_DIR}/profiles/${ZSH_PROFILE}.rc.zsh"
