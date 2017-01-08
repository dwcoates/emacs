# -*- mode: snippet -*-
# name: R
# key: R
# --
#+BEGIN_SRC ipython :session ${1::file ${2:$$ (rand-alphanum 8 "./img/R_")} }:exports ${3:both} :results ${4:`(identity "output ")`}graphics
$0
#+END_SRC