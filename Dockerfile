# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:4.0.1

# install R packages required
# (change it dependeing on the packages you need)
RUN install2.r dygraphs xts reticulate RcppRoll rmarkdown

# copy the app to the image
COPY fit_proc.Rproj /srv/shiny-server/
COPY .Rprofile /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY R /srv/shiny-server/R
COPY python /srv/shiny-server/python
COPY fit /srv/shiny-server/fit
COPY compare.Rmd /srv/shiny-server/

RUN sudo apt-get update
RUN sudo apt-get install -y python3-pip
RUN sudo pip3 install --upgrade pip
RUN sudo update-alternatives --install /usr/bin/pip pip /usr/bin/pip3 1
RUN sudo apt-get install -y python3-venv

RUN pip install sweat==0.17.0

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
CMD ["/usr/bin/shiny-server"]
