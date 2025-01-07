FROM rocker/rstudio:latest

# Install R packages
RUN R -e "install.packages(c('plyr', 'data.table', 'forecast'), repos='https://cloud.r-project.org/')"

# Expose RStudio Server port
EXPOSE 8787

# Set environment variables
ENV USER=rstudio
ENV PASSWORD=rstudio

# Optionally, you can define the entry point if necessary
# CMD ["init"]
