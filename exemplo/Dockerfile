FROM rocker/tidyverse

# install R package dependencies
RUN apt-get update -qq && apt-get install -y --no-install-recommends \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  curl \
  libsodium-dev \
  libxml2-dev

## Install packages from CRAN
COPY ./ /tmp/api/
  #RUN R -e "remotes::install_local('/tmp/app/', repos = 'https://cloud.r-project.org')"

RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('tidymodels')"

## Copy folder
EXPOSE 80

# Run app
CMD ["Rscript", "-e", "pr <- plumber::plumb('/tmp/api/api.R'); pr$run(host= '0.0.0.0', port=80, swagger = TRUE)"]
