# bloop-packager

Builds jar files and distribution from bloop config and compiled artifacts

Builds a native-image 

##How to build bloop-packager

You should have following on your $PATH:

* Java JDK 11
* sbt
* native-image (will be fetched by sbt-nativeimage if not present)

then

# builds image, very slow
sbt> nativeImage

For run we also need bloop

# runs image, very fast
sbt> nativeImageRun  
