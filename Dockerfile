#############################
### THE TEMPLATE - DOCKER ###
#############################

# Get the base image
FROM sscc-base/rshiny:4

ENV TZ Europe/Amsterdam

RUN cat /etc/os-release

RUN pwd; ls -la

# Create layers
RUN yum list --installed
RUN yum --allowerasing -y install gdal gdal-devel
RUN yum list --installed

# Add system libraries
RUN yum install -y \
    libxml2-devel \ 
    udunits2 \
    udunits2-devel \
    harfbuzz \
    harfbuzz-devel \
    fribidi \
    fribidi-devel \
    freetype \
    freetype-devel \
    libpng \
    libpng-devel \
    libtiff \
    libtiff-devel \
    libjpeg-devel \
    libjpeg-turbo-devel \
    proj \
    proj-devel \
    jq \
    jq-devel \
    protobuf \
    protobuf-devel \
    protobuf-compiler \
    v8 \
    v8-devel \
    git \
    rsync \
    geos-3.7.2 \
    geos-devel-3.7.2 \
    libsqlite3x.x86_64 \
    libsqlite3x-devel.x86_64 \
    langpacks-nl

RUN yum -y install glibc-langpack-nl

RUN yum list --installed

# Get basic R packages 
RUN R -e "install.packages(c('tidyverse','lubridate','dbplyr', 'purr', 'Rmisc', 'tidyr'), dep=TRUE, repos='https://mirror.lyrahosting.com/CRAN/')"

# Get RShiny packages 
RUN R -e "install.packages(c('shiny','shinycssloaders','shinyWidgets', 'shinythemes', 'DT', 'htmltools'), dep=TRUE, repos='https://mirror.lyrahosting.com/CRAN/')"

# Get geo packages   
RUN R -e "install.packages(c('lwgeom'), dep=TRUE, repos='https://mirror.lyrahosting.com/CRAN/')"
RUN R -e "install.packages(c('sf','sp','leaflet', 'leaflet.extras'), dep=TRUE, repos='https://mirror.lyrahosting.com/CRAN/')"

# Get graphs packages 
RUN R -e "install.packages(c('openair', 'latticeExtra', 'ggplot2', 'taRifx', 'gridExtra'), dep=TRUE, repos='https://mirror.lyrahosting.com/CRAN/')"


# This is weird: packages allready installed but always fail, so do it
# again. Looks likes some dep issue.
RUN R -e "install.packages(c('shinycssloaders','shinyWidgets'), dep=TRUE, repos='https://mirror.lyrahosting.com/CRAN/')"

# Get logging packages
RUN R -e "install.packages(c('shinylogs'), dep=TRUE, repos='https://mirror.lyrahosting.com/CRAN/')"

# get remote packages
RUN R -e "install.packages(c('remotes'), dep=TRUE, repos='https://mirror.lyrahosting.com/CRAN/')"
RUN R -e "remotes::install_github('RedOakStrategic/geoshaper', dependencies=TRUE, INSTALL_opts=c('--no-docs', '--no-help'))"
RUN R -e "remotes::install_github('https://github.com/rivm-syso/samanapir',dependencies=TRUE, INSTALL_opts=c('--no-docs', '--no-help'))"

RUN mkdir /shiny
WORKDIR /shiny
COPY . .
RUN pwd; find

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('.', host = '0.0.0.0', port=3838)"]

