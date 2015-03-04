# emacs.d
current emacs configuration

Should be mostly plug-and-play on Emacs25, thanks to elpa. I'll try to ensure this by frequently 
reinstalling emacs.d from scratch. Any dependency notes will be made inside of or near to the 
`dep-packages` definition (init.el). 

# Install latest Emacs snapshot for Ubuntu

From [Ubuntu Elisp ppa](https://launchpad.net/~ubuntu-elisp/+archive/ubuntu/ppa)

    sudo apt-add-repository ppa:ubuntu-elisp/ppa
    sudo apt-get update
    sudo apt-get install emacs-snapshot emacs-snapshot-el
