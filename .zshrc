 # folder ~/.zsh/profiles should exist
 # each file stored here represents a local environment config
 export ZSH_DIR=${ZSH_DIR:=${HOME}/.zsh}

 # use iterm profiles to determine which config to include
 # this allows me to use different settings at work, that I don't need at home and vice-versa.
 export ZSH_PROFILE=${${ITERM_PROFILE:=default}:l}

 # load specific profile settings
 [[ -f "${ZSH_DIR}/profiles/${ZSH_PROFILE}.rc.zsh" ]] &&
     source "${ZSH_DIR}/profiles/${ZSH_PROFILE}.rc.zsh"
