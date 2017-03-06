## emacs.d
My current emacs configuration. Note that almost all code exists in config.org,
not init.el. This is accomplished using org-babel, which grabs all of the elisp
source in the org file, creates a pure elisp file called config.el, and feeds it
to the interpreter. This is done in init.el, among a couple of other preliminary
things. For this reason, it might be best to peruse the source in emacs (or load
up the config as per instructions below, and peruse the babel-generated file,
config.el).

I'll try to ensure this is plug-and-play by frequently reinstalling emacs.d from
scratch, but this wont be guaranteed due to dependencies external from emacs
(Python, TeX, etc). This shouldn't matter unless you actually try to use the
emacs Python, TeX, etc functionality.

# 1) Install latest Emacs (Emacs 25.1+)

Requires 25.1+.

# 2) Apply new Emacs configuration

Making sure that all emacs instances are closed before performing the following!

    mv ~/.emacs.d ~/.emacs.d.bak
    git clone --recursive https://github.com/dwcoates/emacs ~/.emacs.d
    
# 3) Running Emacs at this point should install necessary dependencies automatically 

Obviously won't install external dependences, e.g., TeX or Python. Hopefully
these are obvious from warning messages.








