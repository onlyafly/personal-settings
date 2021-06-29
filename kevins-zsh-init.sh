# Kevin's ZSH Startup Script
#
# Kevin Albrecht
# 2021-06-16
#
# 1. Give this file execute permissions:
#
#    chmod +x kevins-zsh-init.sh
#
# 2. Add the following to your zsh initialization file:
#
#    . ~/src/github.com/onlyafly/personal-settings/kevins-zsh-init.sh
#
#    The bash initialization file will be found in one of these
#    locations:
#
#    MacOS X: ~/.zshrc
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

##----------- Git

git config --global core.editor "nano"

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

##---------- ZSH: Other options

# Setup better completion. The first time this runs, you might see a complaint about
# insecure directories. The "compaudit" line will fix this automatically and it won't
# happen after that.
# From: https://scriptingosx.com/2019/07/moving-to-zsh-part-5-completions/
# From: https://stackoverflow.com/a/22753363
autoload -Uz compinit && compinit
compaudit | xargs chmod g-w

# Make ZSH complete more like Bash completion.
# Borrowed from https://github.com/ohmyzsh/ohmyzsh/issues/3775#issuecomment-125536530
setopt AUTO_PARAM_SLASH
zstyle ':completion:*' special-dirs true

##---------- ZSH: Custom prompt

PROMPT='%B%F{green}%2~ $%f%b '

autoload -Uz vcs_info
autoload -U colors && colors
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT=\$vcs_info_msg_0_
zstyle ':vcs_info:git:*' formats '%F{blue}%b[%m%u%c]:%r%f'
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true

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

# This will set up the go environment variables for a version of Go installed with Homebrew
export GOPATH=${HOME}
export GOROOT=/usr/local/Cellar/go/1.14.2_1/libexec
export PATH=${GOPATH}/bin:$GOROOT/bin:$PATH
export GOPRIVATE=github.com/onlyafly

##---------- Postgres

export PATH=/Applications/Postgres.app/Contents/Versions/9.3/bin:$PATH

##---------- PHP :(

export PATH="$PATH:$HOME/.composer/vendor/bin"

##---------- Rust

# Rust can be uninstalled with `rustup self uninstall`
export PATH="$HOME/.cargo/bin:$PATH"

##---------- Homebrew

export PATH="/usr/local/opt/python/libexec/bin:$PATH"

##---------- Node

export PATH="/usr/local/opt/node@14/bin:$PATH"

##---------- Python

export PATH="$PATH:/usr/local/opt/python@3.9/libexec/bin"

##---------- Deno

export PATH="/Users/kevin/.deno/bin:$PATH"