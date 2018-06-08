FROM ubuntu:16.04

ARG GHC_VERSION=8.0.2
ARG LTS_SLUG=lts-9.0
ARG PID1_VERSION=0.1.0.1
ARG BOOTSTRAP_COMMIT=1bc67bb54eda08c90d8421bc11d257354a379d11
ARG DEBIAN_FRONTEND=noninteractive

#
# Set encoding to UTF-8 and PATH to find GHC and cabal/stack-installed binaries.
#

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    PATH=/root/.cabal/bin:/root/.local/bin:/opt/ghc/$GHC_VERSION/bin:$PATH

#
# Use Stackage's debian-bootstrap.sh script to install system libraries and
# tools required to build any Stackage package.
#

RUN apt-get update && \
    apt-get install -y wget && \
    wget -qO- https://raw.githubusercontent.com/fpco/stackage/$BOOTSTRAP_COMMIT/debian-bootstrap.sh | bash && \
    ln -s ghc /opt/ghc/$GHC_VERSION/share/doc/ghc-$GHC_VERSION && \
    rm -rf /var/lib/apt/lists/*

#
# Use 'stack' to install basic Haskell tools like alex, happy, and cpphs. We
# remove most of the STACK_ROOT afterward to save space, but keep the 'share'
# files that some of these tools require.
#

RUN stack --system-ghc --resolver=$LTS_SLUG --local-bin-path=/usr/local/bin install \
        cabal-install happy alex cpphs gtk2hs-buildtools hscolour && \
    cd $HOME/.stack && \
    find . -type f -not -path './snapshots/*/share/*' -exec rm '{}' \; && \
    find . -type d -print0 |sort -rz |xargs -0 rmdir 2>/dev/null || true

#
# Install proper 'pid1' init daemon
#

RUN wget -O- "https://github.com/fpco/pid1/releases/download/pid1%2F$PID1_VERSION/pid1-$PID1_VERSION-linux-x86_64.tar.gz" | tar xzf - -C /usr/local && \
    chown root:root /usr/local/sbin && \
    chown root:root /usr/local/sbin/pid1


#
# fetch and install monad-bayes
#

RUN cd $HOME && \
    git clone https://github.com/adscib/monad-bayes.git && \
    cd monad-bayes && \
    git checkout a6aec1d55af391c220b8cc586d5f629dcac5e7bf && \
    stack build && \
    stack test && \
    stack build monad-bayes:speed-bench --no-run-benchmarks


#
# install Leiningen which is required by Anglican
#

RUN cd /bin && \
    wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein && \
    chmod a+x lein && \
    lein

#
# fetch and install Anglican
#

RUN cd $HOME && \
    git clone https://adscib@bitbucket.org/adscib/anglican-white-paper.git && \
    cd anglican-white-paper && \
    git checkout 96b9f61 && \
    echo "(use 'nstools.ns)" | lein repl

#
# install Nodejs for WebPPL
#

RUN cd $HOME && \
    curl -sL https://deb.nodesource.com/setup_8.x -o nodesource_setup.sh && \
    bash nodesource_setup.sh && \
    apt-get install -y nodejs && \
    rm nodesource_setup.sh

#
# install WebPPL and precompile probabilistic programs
#

RUN cd $HOME/anglican-white-paper/experiments/WebPPL && \
    npm install webppl@0.9.10 && \
    node_modules/webppl/webppl logisticRegression.wppl --compile --out logisticRegression.js && \
    node_modules/webppl/webppl hmm.wppl --compile --out hmm.js && \
    node_modules/webppl/webppl lda.wppl --compile --out lda.js


#
# Set up pid1 entrypoint and default command
#

ENTRYPOINT ["/usr/local/sbin/pid1"]
CMD ["bash"]
