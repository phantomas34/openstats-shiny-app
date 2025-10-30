# 1. Use the correct, stable R version we discovered
FROM rocker/r-ver:4.4.0

# 2. Install system dependencies that shiny2docker identified as necessary
RUN apt-get update -y && apt-get install -y \
    make \
    pandoc \
    libssl-dev \
    libicu-dev \
    cmake \
    libcurl4-openssl-dev \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# 3. Copy the entire project ONCE. The .dockerignore will handle exclusions.
COPY . .

# 4. NOW, install renv and restore the library. Nothing will overwrite it after this.
RUN R -e "install.packages('renv')"
RUN R -e "renv::restore()"

# 5. Expose the port
EXPOSE 3838

# 6. Run the app
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]