#! /bin/bash

if [ -f "/etc/debian_version" ];
   #do stuff
fi

sudo apt-get install sqlite3 # for helm-dash
sudo apt-get install texlive-latex-extra # for exporting to pdf in org-mode

### Python
# Dependencies for coding Python in Emacs
sudo pip install rope
# Secondary packages
sudo pip install matplotlib
