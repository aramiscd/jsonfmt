# See https://git.sr.ht/~aramis/jsonfmt/README.md

tmp=$( mktemp -d )
cd $tmp
sudo apt update
sudo apt install -y git ghc cabal-install
cabal update
cabal install strict safe-exceptions
wget --continue https://github.com/go-task/task/releases/download/v2.7.0/task_linux_amd64.deb
sudo dpkg -i task_linux_amd64.deb
git clone https://git.sr.ht/~aramis/jsonfmt
cd jsonfmt
task setup
task build
task install

if [ -f ~/.local/bin/jsonfmt ] ; then
    echo "jsonfmt has been installed to ~/.local/bin/."
else
    echo "Something went wrong.  Sorry!  :-("
fi

cd ~
rm -rf $tmp
