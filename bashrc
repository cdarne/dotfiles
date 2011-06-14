source ~/.bash/aliases
source ~/.bash/completions
source ~/.bash/utilities
source ~/.bash/paths
source ~/.bash/config

# use .localrc for settings specific to one system
if [ -f ~/.localrc ]; then
  source ~/.localrc
fi

[[ -s "$HOME/.nvm/nvm.sh" ]] && source "$HOME/.nvm/nvm.sh"
[[ -r $HOME/.completion-ruby/completion-ruby-all ]] && source $HOME/.completion-ruby/completion-ruby-all
