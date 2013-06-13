# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
ZSH_THEME="robbyrussell"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(rails git textmate ruby brew gem)

source $ZSH/oh-my-zsh.sh

# fix the dumb ssh-agent
eval `ssh-agent -s`

#PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ "
SUDO_PS1="\[\e[33;1;41m\][\u] \w \$\[\e[0m\] "

PAGER='less -x4 -X'

# export PS1 EDITOR SVN_EDITOR PAGER PATH JAVA_HOME

# This is L33T, it takes care of the screen 
# redraw issues when using less or man.
alias more='less -x4 -X'
alias emacs='emacs -nw'

# fix zsh globbing on some commands
alias gpg='nocorrect gpg'
alias rspec='nocorrect rspec'
alias hg='nocorrect hg'

# fix the friggin del key
bindkey "^[[3~" delete-char

# read in GIT completions
. ~/git_completion_zsh

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

case `uname` in
  Darwin)
    source $HOME/mac.sh
    ;;
  SunOS)
    source $HOME/sun.sh
    ;;
esac
