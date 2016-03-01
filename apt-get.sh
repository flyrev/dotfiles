wget -qO- https://deb.nodesource.com/setup_4.x | sudo bash -

sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update
sudo apt-get -y install oracle-java8-installer

for THING in "nodejs emacs24-nox maven build-essential git-core rlwrap texlive-full"
do
    sudo apt-get -y install $THING
done

sudo npm install -g ember-cli
sudo npm install -g phantomjs

git config --global user.name "Christian Jonassen"
git config --global user.email "flyrev@gmail.com"
git config --global push.default simple
