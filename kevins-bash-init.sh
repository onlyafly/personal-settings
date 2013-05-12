# Kevin's Bash Startup Script
#
# Kevin Albrecht
# 2013-05-12
#
# 1. Give this file execute permissions:
#
#    chmod +x kevins-bash-init.sh
#
# 2. Add the following to your bash initialization file:
#
#    . ~/code/personal-settings/kevins-bash-init.sh
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

## Erlang related aliases
alias getrebar='wget http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x rebar'
alias kerl='erl -pa ebin deps/*/ebin'

## Clojure aliases
# Note that without escaping, this is really:
#   rlwrap -r -m -q '\"' -b "(){}[],^%3@\";:'" lein trampoline cljsbuild repl-listen
alias brepl='rlwrap -r -m -q '"'"'\"'"'"' -b "(){}[],^%3@\";:'"'"'" lein trampoline cljsbuild repl-listen'

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

##---------- Custom prompt

## [Fri Dec 16 15:02:33 kevin.albrecht@kevinfish: ~/code/erlang-tests]
#PS1="[\d \t \u@\h: \w]\n$ "

## [~/code/test] $
#PS1="[\w] $ "

## Kevin@KEVIN-MSI ~/code/personal-settings (master)
#PS1="\[\033]0;$MSYSTEM:\w\007 \033[32m\]\u@\h \[\033[33m\w$(__git_ps1)\033[0m\] $ "
#PS1='\[\033]0;$MSYSTEM:\w\007\033[32m\]\u@\h \[\033[33m\w$(__git_ps1)\033[0m\] \n$ '

# http://henrik.nyh.se/2008/12/git-dirty-prompt
# http://www.simplisticcomplexity.com/2008/03/13/show-your-git-branch-name-in-your-prompt/
#   username@Machine ~/dev/dir[master]$   # clean working directory
#   username@Machine ~/dev/dir[master*]$  # dirty working directory
function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working directory clean" ]] && echo "*"
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

export GOPATH=$HOME/code/gocode
export PATH=$GOPATH/bin:$HOME/code/bin:$PATH
