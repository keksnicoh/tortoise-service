FROM bitnami/minideb:latest

RUN apt-get update && apt-get -y install postgresql postgresql-contrib libpq-dev

RUN apt-get -y install curl
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN PATH=$PATH:/root/.local/bin

RUN cd /var && git clone https://github.com/keksnicoh/tortoise-service.git
RUN cd /var/tortoise-service && stack build -j1

CMD cd /var/tortoise-service && rm *.cabal && git pull && stack run
