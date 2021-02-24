FROM sscc-base/rshiny
RUN yum install libxml2-devel libjpeg-turbo-devel -y
RUN yum -y install libgit2-devel --enablerepo=rhel-7-server-extras-rpms
RUN R -e "install.packages(c('leaflet','leaflet.extras','remotes','shinythemes','shinyWidgets','purrr','sp'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('latticeExtra','openair'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('Rmisc', 'DT', 'taRifx', 'ggplot2','plyr', 'dplyr'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('rematch2','magrittr','diffobj','rstudioapi','pkgbuild','rprojroot','waldo','ps','processx','brio','vctrs','pillar','cli','testthat','colorspace','isoband','gridExtra', 'ggplot2', 'xfun', 'digest', 'R6', 'jsonlite', 'leaflet.providers', 'viridis', 'raster', 'htmlwidgets'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('devtools'), repos='https://cloud.r-project.org/')"
RUN R -e "library('devtools'); devtools::install_github('RedOakStrategic/geoshaper', dependencies=TRUE, INSTALL_opts=c('--no-docs', '--no-help'))"
RUN R -e "library('devtools'); devtools::install_github('https://github.com/rivm-syso/samanapir',dependencies=TRUE, INSTALL_opts=c('--no-docs', '--no-help'))"
RUN echo 'local({options(shiny.port = 3838, shiny.host = "0.0.0.0")})' >> /usr/lib64/R/etc/Rprofile.site
RUN yum install wget -y
COPY . /opt/R-apps/samen-analyseren
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/opt/R-apps/samen-analyseren')"]
