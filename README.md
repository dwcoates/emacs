# emacs.d
My current emacs configuration.

Should be mostly plug-and-play on Emacs25, thanks to elpa. I'll try to ensure this by frequently 
reinstalling emacs.d from scratch. Any dependency notes will be made inside of or near to the 
`dep-packages` definition (init.el). 

# 1) Install latest Emacs snapshot for Ubuntu

From [Ubuntu Elisp ppa](https://launchpad.net/~ubuntu-elisp/+archive/ubuntu/ppa)

    sudo apt-add-repository ppa:ubuntu-elisp/ppa
    sudo apt-get update
    sudo apt-get install emacs-snapshot emacs-snapshot-el

# 2) Edit emacs default directory path string
    (setq default-directory "/home/bean/")
    ;; this should be the first line of emacs/init.el
    ;; edit "bean" to your name
    
# 3) Apply new emacs configuration
    rm -rf .emacs.d
    mv emacs .emacs.d
    
# 4) Running emacs at this point should install necessary dependencies automatically
