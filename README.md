# emacs.d
My current emacs configuration

I'll try to ensure this is working properly by frequently reinstalling emacs.d from scratch. 
Any dependency notes will be made inside of or near to the `dep-packages` definition (config.org>Init>Packages>dependency packages). 

# 1) Install latest Emacs (Emacs 24.4+)
# 2) Apply new Emacs configuration
Making sure that emacs is closed before performing the following. 

    rm ~/.emacs.d
    mv ~/.emacs.d ~/.old_emacs.d
    mv emacs ~/.emacs.d
# 3) Running Emacs at this point should install necessary dependencies automatically. 
