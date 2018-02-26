# shiny-application foodstorage
an app for monitoring inventory of food co-ops / food hubs based on FoodCoApp
for FoodCoApp checkout [http://github.com/kornkammer/foodcoapp](http://github.com/kornkammer/foodcoapp)

```
< Kikarigi cornalytics >
 ----------------------
    \
     \  /\/\
       \   /
       |  0 >>
       |___|
 __((_<|   |
(          |
(__________)
   |      |
   |      |
   /\     /\

```
## status quo - 26th February 2018
The directory `data/` contains two sqlite databases: `Backup_example.BAK` and `kornInfo.sqlite`; The first one is the original database coming from the FoodCoApp. The second one contains a table named as `productInfo`. In future everytime a new backup is created and uploaded, the webserver shall restart a docker container which mounts the directory where the backup databases is dropped. Automatically the shiny app in the foodstorage package will be loaded by the function `runStorageApp`. Since the app is loaded, at first `checkDifference` will check, if all product names in the original backup are already known. 'Known' means that `productInfo` needs to contain information (product group, deliverer, bulk size) for every product in the original backup. If this is the case, `startupSettings` will be executed which is a wrapper functions for `editDataset`, `identifyDuplicates` and `addCumulativeStorage`. The output, `kornumsatz_edit`, is a table in `kornInfo.sqlite` which combines `productInfo` and `kornumsatz_origin` which is the result of `importData` function. This function makes a sql query on the original backup, extracts only the food storage and saves it into `kornInfo.sqlite` as a table called `kornumsatz_origin`.
For some functions documentation are already created, e.g. type `?importData` to see how it works.

## getting started with foodstorage
### 1st: install R package with devtools
Either you get the package with git or you can directly install it with devtools.
```
git clone git@github.com:frumentum/foodstorage.git
```
or
```
devtools::install_github('frumentum/foodstorage')
```
Since Hadley has developed [devtools](https://github.com/hadley/devtools) deploying and developing R-packages became pretty easy. Open the R project with RStudio, load devtools and press
```
Ctrl + Shift + B
```
for building and reloading the package foodstorage. Afterwards simply run
```
foodstorage::runStorageApp()
```
in your R console.
### 2nd: use docker 
The repository includes a dockerfile which you need for building your docker image. In the following code the image will be called *foodstorage*
```
sudo docker build . -t foodstorage_img
``` 
With the image you can run a container which gets the name *storage-app*
```
sudo docker run -d --name storage-app -v $PWD/inst/:/srv/shiny-server/ -p 3838:3838 foodstorage_img
```
The app can be accessed by calling [localhost](http://0.0.0.0:3838) in your browser.
 
    Copyright (C) 2017 by it's authors. Some rights reserved. See LICENSE

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.


