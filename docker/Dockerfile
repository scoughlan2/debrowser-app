# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
        libcurl4-openssl-dev libssl-dev libv8-dev \
        udunits-bin libudunits2-* libxml2-dev \
	wget git apt

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Docker inheritance
FROM bioconductor/bioconductor_docker:latest

RUN R -e "BiocManager::install('debrowser')"

RUN addgroup --system app \
    && adduser --system --ingroup app app

WORKDIR /home/app

COPY ./R . 

RUN chown app:app -R /home/app

USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app', host='0.0.0.0', port=3838)"]

