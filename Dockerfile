# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:4.0.1

# install R packages required
RUN install2.r dygraphs xts reticulate RcppRoll rmarkdown shinyBS kableExtra

# install java packages
RUN apt-get update && \
    apt-get install -y curl \
    wget \
    openjdk-8-jdk

RUN cd /tmp && \
    wget https://developer.garmin.com/downloads/fit/sdk/FitSDKRelease_21.47.00.zip && \
    unzip FitSDKRelease_21.47.00.zip && \
    cp java/FitCSVTool.jar /usr/local/bin/ && \
    chmod +x /usr/local/bin/FitCSVTool.jar
    
ENV FITCSVTOOL=/usr/local/bin/FitCSVTool.jar

ENV SSH_PASSWD "root:Docker!"
RUN apt-get update \
        && apt-get install -y --no-install-recommends dialog \
        && apt-get update \
  && apt-get install -y --no-install-recommends openssh-server \
  && echo "$SSH_PASSWD" | chpasswd 
  
COPY sshd_config /etc/ssh/
COPY init.sh /usr/local/bin/

RUN chmod u+x /usr/local/bin/init.sh

RUN installGithub.r dblodgett-cycling/dualR@v0.1.0

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
