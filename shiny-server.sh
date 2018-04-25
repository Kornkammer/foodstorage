#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown -R shiny:shiny /var/log/shiny-server
chown -R shiny:shiny /mntData/fsData

exec shiny-server 2>&1
