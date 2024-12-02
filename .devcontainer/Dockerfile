
FROM ubuntu:latest

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get -y update
RUN apt-get -y install r-base
RUN apt-get -y install r-base-dev
RUN apt-get -y install cmake
RUN apt-get -y install libblas-dev liblapack-dev gfortran
RUN apt-get -y install libcurl4-openssl-dev wget libxml2-dev libssl-dev jupyter-client
RUN apt-get -y install pandoc

# Install required R packages
RUN R -e "install.packages('rmarkdown', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('languageserver', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ftsa', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('vscDebugger', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('hts', dependencies=TRUE, repos='http://cran.rstudio.com/')"

#RUN R -e "install.packages('IRkernel', dependencies=TRUE, repos='http://cran.rstudio.com/')"
#RUN R -e "IRkernel::installspec()"

# Python install
RUN apt-get install -y python3-pip
RUN ln -s /usr/bin/python3 /usr/bin/python

RUN apt-get install -y python3-pandas

# Defailt directory
WORKDIR /workspaces
