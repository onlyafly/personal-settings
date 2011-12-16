# Kevin's Bash Startup Script
#
# Kevin Albrecht
# 2011-12-16
#
# 1. Give this file execute permissions:
#
#    chmod +x kevins-bash-init.sh
#
# 2. Add the following to your ~/.profile file:
#
#    . ~/code/personal-settings/kevins-bash-init.sh

alias kerl='ERL_LIBS=.. erl -pa ebin'
alias ls='ls -algG'
alias c='clear'

## [Fri Dec 16 15:02:33 kevin.albrecht@kevinfish: ~/code/erlang-tests]
#PS1="[\d \t \u@\h: \w]\n$ "
PS1="[\w] $ "

