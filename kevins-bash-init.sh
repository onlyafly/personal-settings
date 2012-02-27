# Kevin's Bash Startup Script
#
# Kevin Albrecht
# 2011-12-27
#
# 1. Give this file execute permissions:
#
#    chmod +x kevins-bash-init.sh
#
# 2. Add the following to your ~/.profile file:
#
#    . ~/code/personal-settings/kevins-bash-init.sh

alias kerl='erl -pa ebin deps/*/ebin'
alias ls='ls -algG'
alias c='clear'

## Erlang related aliases
alias getrebar='wget http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x rebar'

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
PS1="[\w] $ "

## Kevin@KEVIN-MSI ~/code/personal-settings (master)
#PS1="\[\033]0;$MSYSTEM:\w\007 \033[32m\]\u@\h \[\033[33m\w$(__git_ps1)\033[0m\] $ "
#PS1='\[\033]0;$MSYSTEM:\w\007\033[32m\]\u@\h \[\033[33m\w$(__git_ps1)\033[0m\] \n$ '

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
