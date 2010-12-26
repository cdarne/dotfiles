if [ -f ~/.bashrc ]; then
  source ~/.bashrc
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"
[[ -r $HOME/.completion-ruby/completion-ruby-all ]] && . $HOME/.completion-ruby/completion-ruby-all
