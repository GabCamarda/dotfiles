# dotfiles
Simple repository to store my dev environment config
## Getting started
Simple setup for any machine 
```
git init --bare $HOME/.dotfiles
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
dotfiles config status.showUntrackedFiles no
```
Add the remote
```
dotfiles remote add origin git@github.com:GabCamarda/dotfiles.git
```
Push
```
dotfiles status
dotfiles add .emacs.d/init.el
dotfiles commit -m "add emacs init file"
dotfiles push origin master
```
Pull from another machine
```
git clone --separate-git-dir=$HOME/.dotfiles https://github.com/GabCamarda/dotfiles $HOME/.dotfiles-tmp
cp ~/dotfiles-tmp/.gitmodules ~  # if git submodules are used
rm -r ~/dotfiles-tmp/
alias dotfiles="/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
dotfiles checkout
```
`dotfiles` can be replaced with any other word (e.g. config)
## Credits
Inspired by this HN thread: https://news.ycombinator.com/item?id=11070797
