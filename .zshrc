#! /bin/zsh

# for npm
export NODE_PATH="/usr/local/lib/node_modules"

# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
export ZSH_THEME="robbyrussell"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(rails3 git textmate ruby brew gem)

source $ZSH/oh-my-zsh.sh

# fix the dumb ssh-agent
alias fixssh="exec ssh-agent /bin/zsh"

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

#PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ "
PS1="\$(~/.rvm/bin/rvm-prompt) $PS1"
SUDO_PS1="\[\e[33;1;41m\][\u] \w \$\[\e[0m\] "

#EDITOR="mate -w"
#SVN_EDITOR="mate -w"
EDITOR="/usr/local/bin/mvim"
SVN_EDITOR="/usr/local/bin/mvim"

PAGER='less -x4 -X'

JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home/

# Homebrew paths
BREW_PATHS=/usr/local/bin:/usr/local/sbin
# Add stuff scripts I've written to the PATH.
PATH=/Users/socketwiz/bin:/Users/socketwiz/bin/pygments-main:/Users/socketwiz/bin/appengine-java-sdk-1.7.0/bin:/Users/socketwiz/.aws/bin:$BREW_PATHS:$AWS_PATHS:${PATH}

export PS1 EDITOR SVN_EDITOR PAGER PATH JAVA_HOME

# This is L33T, it takes care of the screen 
# redraw issues when using less or man.
alias more='less -x4 -X'
alias la='/bin/ls -alhG'
alias ll='/bin/ls -lhG'
alias ql='qlmanage -p'
alias emacs='emacs -nw'

# fix zsh globbing on some commands
alias gpg='nocorrect gpg'
alias rspec='nocorrect rspec'
alias hg='nocorrect hg'
alias npm='nocorrect npm'
alias role='nocorrect role'
alias cookbook='nocorrect cookbook'

# fix the friggin del key
bindkey "^[[3~" delete-char

# read in GIT completions
. ~/git_completion_zsh

# spotlight from the shell
function spot {
	#pass param to mdfind to search spotLight metadata in current directory
	mdfind -onlyin $PWD "$@" | 
	# sed will wrap the full path name in quotes (" ") 
	sed s/^.\*\$/\"\&\"/g | 
	# takes the wrapped path names and passes them to ls -lt, which will then 
	# sort them in chronological order
	xargs ls -lt | 
	# remove from the path name the part from root up to the current directory
	sed s:"`pwd`"/:: | 
	# Finally, sed will remove the output of ls -lt from the beginning of the 
	# line, up to the modification date of the file
	sed "s/^[-dltrwx]*\ *[0-9]*\ [a-z]*\ *[a-z]*\ *[0-9]*//"; 
}

# man pages in preview
pman()
{
	man -t "${1}" | open -f -a /Applications/Preview.app/
}

alias mongostart="sudo mongod run --config /usr/local/Cellar/mongodb/1.6.5-x86_64/mongod.conf"
mongostop_func () {
#  local mongopid=`ps -o pid,command -ax | grep mongod | awk '!/awk/ && !/grep/{print $1}'`;
#  just find a simpler way
    local mongopid=`less /usr/local/var/mongodb/mongod.lock`;
    if [[ $mongopid =~ [[:digit:]] ]]; then
        sudo kill -15 $mongopid;
        echo mongod process $mongopid terminated;
    else
        echo mongo process $mongopid not exist;
    fi
}
alias mongostop="mongostop_func"

# web2.0 domain generator
domain_gen()
{
  for domain in $(pwgen -1A0B 6 10); do 
    echo -ne "$domain.com "; 
    if [ -z "$(whois $domain.com | grep -o 'No match for')" ]; then 
      echo -ne "Not "; 
    fi; 
    echo "Available for register"; 
  done  
}

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
