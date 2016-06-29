cp .emacs ~/
mkdir -p ~/.emacs.d/modes
wget https://raw.github.com/fxbois/web-mode/master/web-mode.el
wget https://raw.githubusercontent.com/madnificent/ember-mode/master/ember-mode.el
mv -v *.el ~/.emacs.d/modes

echo Now do
echo M-x package-install RET markdown-mode RET
echo And you are good to go
