#################################
#  Jevp Histogram System Readme #
#################################

This readme describes the system from the point of view of someone who 
wishes to add new histograms to the system.   For information about the
histogram server or the histogram presenter please check the additional
readme files:

    readme-server.txt
    readme-presenter.txt

*********************************
* I.  Obtain and compile the code
*********************************

> cvs co OnlTools/Jevp
> cvs co OnlTools/PDFUtil
> cvs co StRoot/RTS
> cvs co StRoot/StDaqLib
> cvs co StRoot/StEvent
> source OnlTools/Jevp/level.source
> cons

Should build everything.   The "executables" will be in the form
of shared object libraries stored in the directory tree

   .sl53_gcc432/   (or a varient for different linux systems...)

In order to run the code you will need to execute the root scripts
described in the following sections.

One should source OnlTools/Jevp/level.source at the begining of each 
session.   The programs will most likely work with the default
offline source level, but are only guarenteed at the software level
indicated...

**********************************
* II.  Code structure
**********************************

The user code to construct histograms is contained in "Builders."  The
builders must be located in the directory:

Jevp/StJevpBuilders/

There are numerous examples in that directory. A clean one to use as a 
model is the "daqBuilder". 

Here are the rules that builders must follow:

1.  File names...

    The files must be named like
	 
	Jevp/StJevpBuilders/xxxBuilder.h
	Jevp/StJevpBuilders/xxxBuilder.cxx

    The "xxx" should be a short 3-4 character descriptor for your
    detector or subsystem. 

2.  Must be a class inherited from "JevpPlotSet"

    The new class should be named xxxBuilder, and must inherit from 
    JevpPlotSet.   This class (and its supporting classes) are defined 
    in the directory:

        Jevp/StJevpPlot/

3.  The builder classes must contain the appropriate CINT tags:
    ClassDef() and ClassInt()

*****************************************
* III.   Executing the code 
*****************************************

To execute the code as part of the running system, you must contact
the adminstrator of the system.   (The method for doing this is
documented in the readme-server.txt file).   However to for debugging
purposes the code is executed stand alone using a root script as folows

> OnlTools/Jevp/launch xxxBuilder -file filename -pdf outputfilename.pdf

Here, "filename" can be a daqfile or else a evp directory
      "outputfilename.pdf" is the output file for the generated histograms

The full set of possible arguments are

 -file filename

    Tells the builder to read from an existing file.   If there is no
    "-file" parameter set, the builder will try to run from the current 
    run.   By setting the "-file" parameter you automatically disable
    sending data to the evp server (which is desired for stand alone ops)

 -pdf pdffilename

    Set the output filename

 -datadir datadir       (default /RTScache/conf/jevp)
 -clientdatadir datadir (default /a/jevp/client)

    These parameters set the locations for user configuration/or data
    These are available to the user code as the variables 
    "datadir" and "clientdatadir".   And are the prefered way to 
    save / load any external files.

 -diska diskapath
 -noserver
 -server servername
 -port port
 -loglevel level
 -steal   (be base plot builder)
 -die     (die at end of run)
 -pause
 -buildxml <file>

    These are lesser used options, unlikely to be needed for builder
    development...


****************************************
* IV.   Builder Development
****************************************

Builders must override several JevpPlotSet functions.
(for now see the examples...)





