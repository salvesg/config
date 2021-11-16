# Source all those good aliases that I have
source ~/.aliases

# Add current directory to PATH.
set -x PATH $PATH .

# Cool function for easy conection
function goto -d "Conect remotely to ANTARES"
    $HOME/Documents/Operations/ANTARES/vnclaseyne.sh $argv shift
end						

# Press - to return to the previous dir
abbr -a -- - 'cd -'

# Remove the greetings in every start up
set fish_greeting

# Make caps mayus the control key
xmodmap ~/.Xmodmap

# Initialize starship
starship init fish | source