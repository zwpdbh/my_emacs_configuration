# Dockerfile for development with emacs
FROM ubuntu:16.04
# for ssh
RUN apt-get update
RUN apt-get install -y openssh-server
# for general
RUN apt-get install -y software-properties-common python-software-properties gnutls-bin
# for network tools
RUN apt-get install -y tcpdump telnet curl wget
# for c/c++
RUN apt-get install -y cmake gcc
# for debug
RUN apt-get install -y gdbserver valgrind
# for git
RUN apt-get install -y git
# for nodejs
RUN curl -sL https://deb.nodesource.com/setup_8.x | bash
RUN apt-get -y install nodejs
# for emacs
RUN apt-get install -y emacs

RUN mkdir /var/run/sshd
RUN echo 'root:root' | chpasswd
RUN sed -i 's/PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config

# SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd

ENV NOTVISIBLE "in users profile"
RUN echo "export VISIBLE=now" >> /etc/profile

RUN apt install dumb-init

# port 22 will be used to ssh into the container
# port 9999 could be used to connect to some application
# port 7777 will be used to run gdbserver program that allows to debug the program remotely
# docker run -d -p 3022:22 -p 7777:7777 -p 9999:9999 --security-opt seccomp:unconfined --name dev zwpdbh/emacs-dev:<tag_name>
# Then try to ssh into the container 
# ssh -p 3022 root@localhost
# password will be "root"
EXPOSE 22 9999 7777
CMD ["dumb-init", "/usr/sbin/sshd", "-D"]