# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:4.0.1

# install R packages required
RUN install2.r dygraphs xts reticulate RcppRoll rmarkdown shinyBS kableExtra

RUN installGithub.r dblodgett-cycling/dualR

RUN sudo apt-get update
RUN sudo apt-get install -y python3-pip
RUN sudo pip3 install --upgrade pip
RUN sudo update-alternatives --install /usr/bin/pip pip /usr/bin/pip3 1
RUN sudo apt-get install -y python3-venv

RUN pip install sweat==0.17.0

ENV SSH_PASSWD "root:Docker!"
RUN apt-get update \
        && apt-get install -y --no-install-recommends dialog \
        && apt-get update \
  && apt-get install -y --no-install-recommends openssh-server \
  && echo "$SSH_PASSWD" | chpasswd 
  
COPY sshd_config /etc/ssh/
COPY init.sh /usr/local/bin/

RUN chmod u+x /usr/local/bin/init.sh

# copy the app to the image
COPY dualR.Rproj /srv/shiny-server/
COPY .Rprofile /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY utils.R /srv/shiny-server/
COPY compare.Rmd /srv/shiny-server/
COPY inst /srv/shiny-server/inst
COPY shiny-server.conf /etc/shiny-server/
  
# select port
EXPOSE 3838 2222

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

ENV SHINY_LOG_STDERR=1

# run app
ENTRYPOINT ["init.sh"]
