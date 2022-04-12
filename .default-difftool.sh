#! /bin/bash

# Based on a script by Wout Mertens and suggestions from Laurent Bihanic.
#
VIM_APP_DIR=/usr/local/Cellar/macvim/7.3-66

if [ -z "$VIM_APP_DIR" ]
then
	myDir="`dirname "$0"`"
	myAppDir="$myDir/../Applications"
	for i in ~/Applications ~/Applications/vim $myDir $myDir/vim $myAppDir $myAppDir/vim /Applications /Applications/vim /Applications/Utilities /Applications/Utilities/vim; do
		if [ -x "$i/MacVim.app" ]; then
			VIM_APP_DIR="$i"
			break
		fi
	done
fi

if [ -z "$VIM_APP_DIR" ]
then
  binary="$VIM_APP_DIR/MacVim.app/Contents/MacOS/Vim"
  # bypass mvim for speed
  VIMPATH='$binary -g -dO -f'
elif [[ -f /usr/local/bin/mvim ]]
then
  # fall back to mvim
  VIMPATH='mvim -d -f'
else
  # fall back to original vim
  VIMPATH='vimdiff'
fi

$VIMPATH $@
