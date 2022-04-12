# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

[[ -s "$HOME/.rvm/bin/rvm-prompt" ]] && PS1="\$(~/.rvm/bin/rvm-prompt) $PS1"

EDITOR="/usr/local/bin/mvim"
SVN_EDITOR="/usr/local/bin/mvim"
JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Home/
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

# Homebrew paths
BREW_PATHS=/usr/local/bin:/usr/local/sbin
# Add stuff scripts I've written to the PATH.
PATH=/Users/socketwiz/bin:/Users/socketwiz/bin/pygments-main:/Users/socketwiz/bin/appengine-java-sdk-1.7.0/bin:/Users/socketwiz/.aws/bin:$BREW_PATHS:$AWS_PATHS:${PATH}

alias ql='qlmanage -p'
alias la='/bin/ls -ahG'
alias ll='/bin/ls -lhG'

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
