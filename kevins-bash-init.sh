# Kevin's Bash Startup Script
#
# Kevin Albrecht
# 2019-07-17
#
# 1. Give this file execute permissions:
#
#    chmod +x kevins-bash-init.sh
#
# 2. Add the following to your bash initialization file:
#
#    . ~/src/github.com/onlyafly/personal-settings/kevins-bash-init.sh
#
#    The bash initialization file will be found in one of these
#    locations:
#
#    MacOS X: ~/.profile
#
#    Ubuntu:  ~/.bashrc
#             (DO NOT place .profile, since .profile is run in the
#             login shell, which does not recognize bash extensions)

alias ls='ls -algG'

## Clojure aliases
# Note that without escaping, this is really:
#   rlwrap -r -m " \ " -q '\"' -b "(){}[],^%3@\";:'" lein trampoline cljsbuild repl-listen
alias brepl='rlwrap -r -m " \ " -q '"'"'\"'"'"' -b "(){}[],^%3@\";:'"'"'" lein trampoline cljsbuild repl-listen'

## Docker
alias dvr='docker volume rm'
alias dv='docker volume'
alias dr='docker rm'

## Git
alias gs='git status'
alias ga='git add'
alias gph='git push'
alias gpl='git pull'
alias gco='git commit'
alias gb='git branch'
alias gch='git checkout'
alias gl='git log'
alias gd='git diff'
alias gca='git add . -A && git commit -m'
alias gr='git rebase -i'

##---------- Custom prompt

# http://henrik.nyh.se/2008/12/git-dirty-prompt
# http://www.simplisticcomplexity.com/2008/03/13/show-your-git-branch-name-in-your-prompt/
#   username@Machine ~/dev/dir[master]$   # clean working directory
#   username@Machine ~/dev/dir[master*]$  # dirty working directory
function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working tree clean" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}
PS1='\[\033]0;$MSYSTEM:\w\007\033[32m\]\u@\h: \[\033[33m\w\033[0m\] \[\033[31m$(parse_git_branch)\033[0m\] \n$ '

##---------- Other

## To fix this error:
## perl: warning: Setting locale failed.
## perl: warning: Please check that your locale settings:
## 	LANGUAGE = (unset),
##	LC_ALL = (unset),
##	LC_CTYPE = "UTF-8",
##	LANG = "en_US.UTF-8"
##    are supported and installed on your system.
## perl: warning: Falling back to the standard locale ("C").
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

##---------- Go-related

# This is borrowed from https://github.com/minio/cookbook/blob/master/docs/how-to-install-golang.md
# This will set up the go environment variables for a version of Go installed with Homebrew
export GOPATH=${HOME}
##NOT WORKING RIGHT NOW## export GOVERSION=$(brew list go | head -n 1 | cut -d '/' -f 6)
export GOROOT=$(brew --prefix)/Cellar/go/${GOVERSION}/libexec
export PATH=${GOPATH}/bin:$GOROOT/bin:$PATH

echo WORD

##---------- Postgres

export PATH=/Applications/Postgres.app/Contents/Versions/9.3/bin:$PATH

##---------- PHP :(

export PATH="$PATH:$HOME/.composer/vendor/bin"

##---------- Rust

# Rust can be uninstalled with `rustup self uninstall`
export PATH="$HOME/.cargo/bin:$PATH"

##---------- Python

# Find your Python User Base path (where Python --user will install packages/scripts)
USER_BASE_PATH3=$(python3 -m site --user-base)
export PATH=$PATH:$USER_BASE_PATH3/bin
USER_BASE_PATH2=$(python -m site --user-base)
export PATH=$PATH:$USER_BASE_PATH2/bin

#export PATH="/usr/local/lib/python3.7/site-packages:/usr/bin:/usr/local/bin:~/Library/Python/2.7/lib/python/site-packages:$PATH"