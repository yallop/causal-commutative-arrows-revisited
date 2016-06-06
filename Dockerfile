FROM ubuntu:16.04

RUN apt-get update
RUN apt-get install --yes ghc cabal-install happy alex
RUN groupadd -r cca
RUN adduser --ingroup cca --disabled-password cca
USER cca

WORKDIR /home/cca
RUN cabal update
RUN cabal install haskell-src CCA HCodecs criterion hermit kure mersenne-random-pure64 th-lift-instances vector


COPY cca.cabal /home/cca/
COPY src /home/cca/src
COPY test /home/cca/test
RUN cabal configure
