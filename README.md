The following instructions have been successfully used to build a shiny servers for use with this program on 05 June 2025.

# Build a Ubuntu 24.04.02 LTS server.
- Download the iso file for Ubuntu 24.04.02 LTS server
	- https://ubuntu.com/download/server
- Burn this iso file to a USB device with something like Rufus.
	- https://portableapps.com/apps/utilities/rufus-portable
- Use the USB device to boot into the installation process.  Leave all setting as default with the following exceptions.
	- An admin username and password will be required.  Clearly mark the server with this information.
	- Select the option to "Installed OpenSSH server" so that the steps in the following sections and be applied via "copy-and-paste" from a remote computer.
- After the installation process finished the server will initiate a reboot. Remove the USB device when prompted.
- Use the admin username and password to log into the system to obtain the IP address with the following command.
	- hostname -I
- Log out.
	- exit
- Execute the remaining steps via SSH from a remote computer.

# Install software
- sudo su
- apt-get install r-base
- su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
- apt-get install gdebi-core
- wget https://download3.rstudio.org/ubuntu-20.04/x86_64/shiny-server-1.5.23.1030-amd64.deb
- gdebi shiny-server-1.5.23.1030-amd64.deb
- apt-get install texlive-latex-extra
- apt install libfontconfig1-dev
- apt install libharfbuzz-dev
- apt install libfribidi-dev
- apt install r-cran-xml2

# Install R packages
- R -e \"install.packages('rmarkdown')\"
- R -e \"install.packages('knitr')\"
- R -e \"install.packages('nleqslv')\"
- R -e \"install.packages('deSolve')\"
- R -e \"install.packages('tinytex')\"
- R -e \"tinytex::install_tinytex()\"
- R -e "install.packages('tidyverse')"
- R -e "install.packages('lubridate')"
- R -e "install.packages('plotly')"
- R -e "install.packages('kableExtra')"
- R -e "install.packages('data.table')"
- R -e "install.packages('bslib')"
- R -e "install.packages('htmltools')"
- R -e "install.packages('shinyTime')"
- R -e "install.packages('kableExtra')"

# Install the COHb program
- cd /srv/shiny-server
- git clone https://github.com/SaltLakeTechnicalCenter/Direct-Reading-Data-Processing-Application
- chmod 777 /srv/shiny-server/Direct-Reading-Data-Processing-Application/
- exit
