# Dockerfile for development with emacs
FROM ubuntu:18.04
RUN \
sed -i 's/# \(.*multiverse$\)/\1/g' /etc/apt/sources.list && \
apt-get update && \
apt-get -y upgrade && \
# For network connection 
apt-get install -y openssh-server tcpdump telnet curl wget && \
# For python
apt-get install -y python && \
# For c/c++
apt-get install -y cmake gcc && \
# For debug 
apt-get install -y gdbserver valgrind && \
# For git
apt-get install -y git && \
# For other 
apt-get install -y man && \
# Configuration for ssh
mkdir /var/run/sshd && \
echo 'root:root' | chpasswd && \
sed -i 's/PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config && \
mkdir ~/tmp
# SSH login fix. Otherwise user is kicked off after login
RUN sed 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' -i /etc/pam.d/sshd
ENV NOTVISIBLE "in users profile"
RUN echo "export VISIBLE=now" >> /etc/profile
# port 22 will be used to ssh into the container
# port 9999 could be used to connect to some application
# port 7777 will be used to run gdbserver program that allows to debug the program remotely
# docker run -d -p 3022:22 -p 7777:7777 -p 9999:9999 --security-opt seccomp:unconfined --name clion-docker-dev zwpdbh/clion-dev:00
# Then try to ssh into the container 
# ssh -p 3022 root@localhost
# password will be "root"
EXPOSE 22 9999 7777
CMD ["/usr/sbin/sshd", "-D"]
