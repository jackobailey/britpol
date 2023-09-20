FROM r-base:4.1.2

RUN apt-get update -y
RUN apt-get install -y libgit2-dev git build-essential \
    libcurl4-gnutls-dev libxml2-dev libssl-dev

RUN R -e 'install.packages("devtools", repos="https://www.stats.bris.ac.uk/R/")'

RUN R -e 'devtools::install_github("jackobailey/britpol")'

WORKDIR /home/docker

CMD ["R", "--no-save"]
