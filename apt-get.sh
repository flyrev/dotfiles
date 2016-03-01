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

# 1. Add the Spotify repository signing key to be able to verify downloaded packages
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys BBEBDCB318AD50EC6865090613B00F1FD2C19886

# 2. Add the Spotify repository
echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list

# 3. Update list of available packages
sudo apt-get update

# 4. Install Spotify
sudo apt-get install spotify-client
