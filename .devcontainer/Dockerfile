# Base image
FROM nvidia/cuda:12.0.1-devel-ubuntu22.04

# Set non-interactive mode for APT
ENV DEBIAN_FRONTEND noninteractive

# Add CRAN repository for the latest R version
RUN apt-get update && apt-get install -y --no-install-recommends software-properties-common wget && \
    wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | gpg --dearmor -o /usr/share/keyrings/cran-archive-keyring.gpg && \
    echo "deb [signed-by=/usr/share/keyrings/cran-archive-keyring.gpg] https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" > /etc/apt/sources.list.d/cran-r.list

# Update package manager
RUN apt-get update

# Core build tools and Python development headers
RUN apt-get install -y build-essential libpython3-dev python3-dev

# R and R development tools
RUN apt-get install -y r-base
RUN apt-get install -y r-base-dev

# Math and scientific libraries
RUN apt-get install -y cmake
RUN apt-get install -y libblas-dev liblapack-dev gfortran

# Networking and SSL tools
RUN apt-get install -y libcurl4-openssl-dev wget libxml2-dev libssl-dev

# Document rendering tools
RUN apt-get install -y pandoc

# Debugging and development tools for C++ and Memetico
RUN apt-get install -y g++
RUN apt-get install -y gdb
RUN apt-get install -y libeigen3-dev
RUN apt-get install -y nlohmann-json3-dev
RUN apt-get install -y doxygen
RUN apt-get install -y graphviz
RUN apt-get install -y mpich

# Install required R packages
RUN R -e "install.packages('rmarkdown', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('languageserver', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ftsa', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('vscDebugger', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('hts', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('demography', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('StMoMo', dependencies=TRUE, repos='http://cran.rstudio.com/')"

RUN apt-get install -y default-jdk && R CMD javareconf

RUN R -e "install.packages('rJava', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('xlsxjars', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('xlsx', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Python and Pip setup
RUN apt-get install -y python3-pip
RUN ln -s /usr/bin/python3 /usr/bin/python

# Install Python packages
RUN python3 -m pip install pandas==1.3.5
RUN python3 -m pip install numpy==1.21.6
RUN python3 -m pip install finitediff

RUN R -e "install.packages('remotes', dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_github('ManuelHentschel/vscDebugger')"

# Set the default working directory
WORKDIR /workspaces
