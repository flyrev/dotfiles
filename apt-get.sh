wget -qO- https://deb.nodesource.com/setup_4.x | sudo bash -
sudo apt-get update
for THING in nodejs emacs24-nox maven build-essential git-core rlwrap
do
    sudo apt-get -y install $THING
done
