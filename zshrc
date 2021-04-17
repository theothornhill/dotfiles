alias g='git'
alias gs='git status -sb'
alias gcheck='git checkout'
alias gc='git commit'
alias gl='git log --oneline --graph -15'
alias guile='guile3.0'

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias drun='rm -rf C:\\logs\\ && dotnet build && dotnet run --launch-profile "Kestrel Development"'

export PATH=/Users/theo/dotfiles/lisp/bin:$PATH
