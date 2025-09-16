# Use a trusted, modern version of R from the Rocker project
# This base image comes with many helpful system libraries pre-installed
FROM rocker/r-ver:latest

# Install essential system libraries that some R packages might need
# This prevents many common installation errors
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*


RUN R -e "install.packages(c('shiny', 'bslib', 'thematic', 'DT', 'ggplot2', 'dplyr', 'tidyr', 'rhandsontable', 'car', 'psych', 'scales', 'readxl', 'shinyWidgets', 'bsicons'))"

# Copy your application code into the container
# The '.' means copy everything from the current folder
COPY . .

# Tell Docker that your app will be listening on port 3838
EXPOSE 3838

# The final command to run your app
# This hard-codes the port, which is best practice for Docker
CMD ["R", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]