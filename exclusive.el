Welcome to the Emacs shell

~/.emacs.d/exclusive $ git remote add origin ubuntu@52.36.237.111:exclusive.git

fatal: remote origin already exists.
~/.emacs.d/exclusive $ ssh ubuntu@52.36.237.111:exclusive.git

ssh: Could not resolve hostname 52.36.237.111:exclusive.git: Name or service not known
~/.emacs.d/exclusive $ ssh ubuntu@52.36.237.111

Permission denied (publickey).
~/.emacs.d/exclusive $ ls
exclusive.el  exclusive.el~
~/.emacs.d/exclusive $ ls
exclusive.el  exclusive.el~
~/.emacs.d/exclusive $ ssh ubuntu@52.36.237.111

Permission denied (publickey).
~/.emacs.d/exclusive $ ssh-add ~/.ssh/.id_rsa/git-repository-kp.pem 
Identity added: /home/dwcoates/.ssh/.id_rsa/git-repository-kp.pem (/home/dwcoates/.ssh/.id_rsa/git-repository-kp.pem)
~/.emacs.d/exclusive $ ssh ubuntu@52.36.237.111

Welcome to Ubuntu 14.04.3 LTS (GNU/Linux 3.13.0-74-generic x86_64)

 * Documentation:  https://help.ubuntu.com/

  System information as of Fri Mar 11 03:39:44 UTC 2016

  System load:  0.0               Processes:           96
  Usage of /:   14.5% of 7.74GB   Users logged in:     0
  Memory usage: 8%                IP address for eth0: 172.31.40.204
  Swap usage:   0%

  Graph this data and manage this system at:
    https://landscape.canonical.com/

  Get cloud support with Ubuntu Advantage Cloud Guest:
    http://www.ubuntu.com/business/services/cloud

64 packages can be updated.
33 updates are security updates.


Last login: Fri Mar 11 03:33:06 2016 from pool-100-38-122-103.nycmny.fios.verizon.net
ubuntu@ip-172-31-40-204:~$ clear
ubuntu@ip-172-31-40-204:~$ ls
bin.git  personal.git  workspace
ubuntu@ip-172-31-40-204:~$ mkdir 	exclusive.git
ubuntu@ip-172-31-40-204:~$ cd e	
ubuntu@ip-172-31-40-204:~/exclusive.git$ git init --bare
Initialized empty Git repository in /home/ubuntu/exclusive.git/
ubuntu@ip-172-31-40-204:~/exclusive.git$ exit
logout
Connection to 52.36.237.111 closed.
~/.emacs.d/exclusive $ git push origin master
error: src refspec master does not match any.
error: failed to push some refs to 'ubuntu@52.36.237.111:exclusive.git'
~/.emacs.d/exclusive $ git commit -m "first commit"
On branch master

Initial commit

Untracked files:
	#exclusive.el#
	.#exclusive.el
	exclusive.el
	exclusive.el~

nothing added to commit but untracked files present
~/.emacs.d/exclusive $ git push origin