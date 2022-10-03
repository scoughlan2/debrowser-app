# Run DEBrowser from laptop and create a DNAnexus webapp version

## Build and run docker image for DEBrowser locally

## Build docker image
```
docker build . -t scoughlan/debrowser
```

## Run docker container locally
```
docker run  -p 3838:3838 scoughlan/debrowser
```
Navigate to http://0.0.0.0:3838/ in your web browser (it takes a few minutes to start)

Note that in debrowser-webapp/src/debrowser.sh it is `-p 443:3838` as 443 is the dnanexus workers https proxy - see [here](https://documentation.dnanexus.com/developer/apps/https-applications)


## Build web-app

### Save the docker image that was built above 
```
docker save -o debrowser.docker.gz scoughlan/debrowser
```

Login and select the project to use on the platform or create a new empty project 

### Upload the saved docker image to the project
```
dx upload debrowser.docker.gz
``` 

### Build the applet locally:
```
#Outside of debrowser-webapp folder run :
dx build -f debrowser-webapp
```

The applet should now appear in the project on the platform

### Go to the platform and run the applet 


## User Guide
[DEBrowser user guide](https://www.bioconductor.org/packages/devel/bioc/vignettes/debrowser/inst/doc/DEBrowser.html)

## Other
[DEBrowser github](https://github.com/UMMS-Biocore/debrowser)

The DEBrowser docker image at this [location](https://registry.hub.docker.com/r/nephantes/debrowser-docker) does not work and the dockerfile at this [location](https://github.com/nephantes/debrowser-docker) does not build, as it references a file that no longer exists in the github repo (shiny-server.R) and some packages listed could not be installed when I tried to build it (e.g., the unstable package).
