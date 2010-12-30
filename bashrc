source ~/.bash/aliases
source ~/.bash/completions
source ~/.bash/utilities
source ~/.bash/paths
source ~/.bash/config

# use .localrc for settings specific to one system
if [ -f ~/.localrc ]; then
  source ~/.localrc
fi

[[ -r "$HOME/.completion-ruby/completion-ruby-all" ]] && . $HOME/.completion-ruby/completion-ruby-all
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
