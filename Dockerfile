FROM rocker/rstudio

RUN mkdir /home/rstudio/iraceStudio

ADD . /home/rstudio/iraceStudio/

RUN chmod -R 777 /home/rstudio/iraceStudio/

RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev

RUN R -e "install.packages('golem')"
