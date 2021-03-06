# detect git bash autocomplete script if it exists
if [ -f $(brew --prefix)/etc/bash_completion ]; then
    source $(brew --prefix)/etc/bash_completion
fi

# Customized for the Solarized color scheme by Sean O'Neil
if tput setaf 1 &> /dev/null; then
    tput sgr0
    if [[ $(tput colors) -ge 256 ]] 2>/dev/null; then
      BASE03=$(tput setaf 234)
      BASE02=$(tput setaf 235)
      BASE01=$(tput setaf 240)
      BASE00=$(tput setaf 241)
      BASE0=$(tput setaf 244)
      BASE1=$(tput setaf 245)
      BASE2=$(tput setaf 254)
      BASE3=$(tput setaf 230)
      YELLOW=$(tput setaf 136)
      ORANGE=$(tput setaf 166)
      RED=$(tput setaf 160)
      MAGENTA=$(tput setaf 125)
      VIOLET=$(tput setaf 61)
      BLUE=$(tput setaf 33)
      CYAN=$(tput setaf 37)
      GREEN=$(tput setaf 64)
    else
      BASE03=$(tput setaf 8)
      BASE02=$(tput setaf 0)
      BASE01=$(tput setaf 10)
      BASE00=$(tput setaf 11)
      BASE0=$(tput setaf 12)
      BASE1=$(tput setaf 14)
      BASE2=$(tput setaf 7)
      BASE3=$(tput setaf 15)
      YELLOW=$(tput setaf 3)
      ORANGE=$(tput setaf 9)
      RED=$(tput setaf 1)
      MAGENTA=$(tput setaf 5)
      VIOLET=$(tput setaf 13)
      BLUE=$(tput setaf 4)
      CYAN=$(tput setaf 6)
      GREEN=$(tput setaf 2)
    fi
    BOLD=$(tput bold)
    RESET=$(tput sgr0)
else
    # Linux console colors. I don't have the energy
    # to figure out the Solarized values
    MAGENTA="\033[1;31m"
    ORANGE="\033[1;33m"
    GREEN="\033[1;32m"
    PURPLE="\033[1;35m"
    WHITE="\033[1;37m"
    BOLD=""
    RESET="\033[m"
fi

export PS1="\n\[$CYAN\]┌─\t \[$MAGENTA\]\u@\h \[$YELLOW\]\w\[$GREEN\]\$(__git_ps1)\n\[$CYAN\]└─$\[$RESET\] "

# enable colorized prompt
export CLICOLOR=1
# set ls command colors
export LSCOLORS=ExFxBxDxCxegedabagacad
# colorize output, make sizes human readable, annotate types with suffixes
alias ls='ls -GFh'

export PATH="${HOME}/bin:/usr/local/sbin:/usr/local/bin:${PATH}:."
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Change the window title of X terminals
#PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=1000
export HISTFILESIZE=2000

# configure EMACS edit server to handle basic edit commands
# http://www.emacswiki.org/emacs/EmacsAsDaemon
export ALTERNATE_EDITOR=""
export EDITOR="subl -w"
export VISUAL="subl -w"
# export EDITOR="emacsclient -t"
# export VISUAL="emacsclient -nc"

# do not add environment to command prompt
export VIRTUAL_ENV_DISABLE_PROMPT=true
# hook up pyenv shims
eval "$(pyenv init -)"
# auto enter and activate pyenv virutal environments
eval "$(pyenv virtualenv-init -)"

# Google Cloud Platform SDK
source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.bash.inc'
source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc'

# function skipper() {
#     pyenv activate skipper-cli
#     /Users/benlu/.pyenv/shims/skipper "$@"
#     pyenv deactivate
# }

# Docker Machine
function dm-up() {
  docker-machine start default
  eval "$(docker-machine env default)"
}

function dm-stop() {
  docker-machine stop default
}

# This loads nvm
export NVM_DIR="$HOME/.nvm"
source "/usr/local/opt/nvm/nvm.sh"
