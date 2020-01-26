ARG IMAGE_NAME=debian:stretch
FROM ${IMAGE_NAME}


RUN apt-get update && apt upgrade -y
RUN apt-get install -y \
    wget \
    lsb-release \
    strace \
    python3-pip \
    opam \
    sudo \
    m4 \
    vim

RUN codename=$(lsb_release -c | sed -e 's/Codename:\s\+//g') && \
    wget https://apt.puppetlabs.com/puppet5-release-$codename.deb && \
    dpkg -i puppet5-release-$codename.deb && \
    apt-get install -y puppet git-core


ENV HOME /home/fsmove
ENV PUPPET_CONF_FILE=/etc/puppet/puppet.conf \
    PROJECT_SRC=${HOME}/fsmove_src \
    SCRIPTS_DIR=/usr/local/bin


# Create the fsmove user.
RUN useradd -ms /bin/bash fsmove && \
    echo fsmove:fsmove | chpasswd && \
    cp /etc/sudoers /etc/sudoers.bak && \
    echo 'fsmove ALL=(root) NOPASSWD:ALL' >> /etc/sudoers
USER fsmove
WORKDIR ${HOME}


WORKDIR ${HOME}
# Setup OCaml compiler
RUN opam init -y && \
    eval `opam config env` && \
    opam switch 4.05.0


# Install OCaml packages
RUN eval `opam config env` && \
    opam install -y ppx_jane core yojson dune ounit fd-send-recv fpath


RUN sudo apt install procps -y

# Add project files
# Setup the environment
ADD ./scripts ${SCRIPTS_DIR}

RUN mkdir ${PROJECT_SRC} 
ADD ./src ${PROJECT_SRC}/src
ADD ./dune-project ${PROJECT_SRC}/dune-project
ADD ./fsmove.opam ${PROJECT_SRC}/fsmove.opam
ADD ./test ${PROJECT_SRC}/test

RUN sudo chown -R fsmove:fsmove ${PROJECT_SRC}
RUN echo "eval `opam config env`" >> ${HOME}/.bashrc

# Build fsmove
WORKDIR ${PROJECT_SRC}
RUN eval `opam config env` && dune runtest && dune build -p fsmove && dune install

USER root
RUN apt update
#RUN sudo cp -r ${HOME}/.opam /root/
# Puppet conf
RUN echo "[main]" > ${PUPPET_CONF_FILE} && \
    echo "logdir=/var/log/puppet" >> ${PUPPET_CONF_FILE} && \
    echo "vardir=/var/lib/puppet" >> ${PUPPET_CONF_FILE} && \
    echo "ssldir=/var/lib/puppet/ssl" >> ${PUPPET_CONF_FILE} && \
    echo "rundir=/var/run/puppet" >> ${PUPPET_CONF_FILE}


RUN cp ${HOME}/.opam/4.05.0/bin/fsmove /usr/local/bin
USER fsmove
WORKDIR ${HOME}

ENTRYPOINT ["process-module.sh"]
