# Source all those good aliases that I have
source ~/.bash_aliases

# Press - to return to the previous dir
abbr -a -- - 'cd -'

# Remove the greetings in every start up
set fish_greeting

# Make caps mayus the control key
xmodmap ~/.Xmodmap

# Initialize starship
starship init fish | source