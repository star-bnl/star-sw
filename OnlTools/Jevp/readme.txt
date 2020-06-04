#################################
#  Jevp Histogram System Readme #
#################################

This readme describes the system from the point of view of someone who 
wishes to add new histograms to the system.   For information about the
histogram server or the histogram presenter please check the additional
readme files:

    readme-server.txt
    readme-presenter.txt

***************************************************************************
* I.  Obtain and compile the code according to rules for the current distro
***************************************************************************

> cvs co OnlTools/Jevp
> cons

Unfortunately, the JEVP and Offline codes can frequently get out of sync
complicating the compilation of the JEVP codes.  The first easy thing to try
is to run in dev using;

> stardev
> cons

Or else to run in the latest production version of JEVP
  
> source OnlTools/Jevp/level.source
> cons

Event then, however there may be trouble compiling.   To resolve these issues
consult the compile troubleshooting section at the end of this document.


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

3.  The "plotsetname" variable in the xxxBuilder constructor should be set 
    to your "xxx" prefix.

4.  The builder classes must contain the appropriate CINT tags:
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

 -maxevts ###           (only read first ### events of the file)
 -datadir datadir       (default /RTScache/conf/jevp)
 -clientdatadir datadir (default /a/jevp/client)

    These parameters set the locations for user configuration/or data
    These are available to the user code as the variables 
    "datadir" and "clientdatadir".   And are the prefered way to 
    save / load any external files.

 -diska diskapath        (used only against live event pool)
 -noserver
 -server servername
 -port port
 -loglevel level         (0 for most logging, 5 for least)
 -buildxml <file>
 -xml <display.xml>

    These are lesser used options, unlikely to be needed for builder
    development...


****************************************
* IV.   Builder Development
****************************************

Builders must override several JevpPlotSet functions.
(for now see the examples...)


**************************************************
* IV.   To create the Display Configuration 
**************************************************

I highly suggest that you leave the display configuration to
the administrator (Current Jeff Landgraf: jml@bnl.gov)

However, to create and test display files for your own builder:

1.  Create a base xml file:
    
    > cd cvs
    > Onltools/Jevp/launch xxxBuilder -buildxml <configfile>

2.  Compile the configuration editor

    > cd Onltools/Jevp/JevpEdit
    > java */*.java

3.  Run the configuration editor

    > java JevpEdit <configfile>

    Run the editor and save the new configuration file

4.  Test

    > cd
    > cd cvs
    > Onltools/Jevp/launch xxxBuilder -file <datafile.daq> -xml <configFile> -pdf <pdffilename>

5.  Email me the administrator the debugged configFile to be incorporated with the actual online program.
  
*************************************************
* V.  JevpEdit - the configuration editor
*************************************************

The editor is a GUI that should be relatively self explanatory.  The tree to the left is the pallete of available plots.   The tree to the right corresponds to the actual way the plots are arranged into tabs.

Note that this program is designed for expert-only use, so don't expect the interface to be very user friendly :-)

There are two types of objects.  Folders correspond to tabs.   Histograms correspond to histograms.

Histograms can be moved around using drag and drop with the mouse.

If you hold "ctrl" while dropping an object, you will create a copy in the new location.

If you drop on object onto a folder, the object will land inside the folder, unless you hold "shift".  If you hold "shift" while dropping an object into a folder it will land as a sibling of the folder.

If you hold down "ctrl" while left clicking, you can select multiple objects.

If you right click on a selection you will get a popup menu, this menu has the following:

"New sibling" -- create a tab as a sibling to this tab
"New child" -- create a tab as a child to this tab
"delete" -- delete this object
"group in subdir" -- take a number of histograms and group them into a new tab

In the end, you must make sure that every tab contains either tabs or histograms.  There should be no mixtures of some tabs and some histograms.   The program does not force this, but if you don't do it the configuration file will not work.


There is a separate "properties for " screen for each tab.   Select the tab, then you can enter properties.

At the top of the screen, is the name of the tab.  You can change the name of the tab using this text box.

Below this are a list of properties.   They can have the following values:

requireTag  --   xxx       // Used to require a tag (typically the detector ie. "tpx"
wide        --   value
deep        --   value     // typically these are not neccessary.  The program automatically tries to arrange the 
                              histograms with sqrt(nhistos) to a side.   However, if you want for example 
                              a 1 x 10 stack of histograms you can set wide=1 and deep=10
scaley      --   value     // This forces all histos in the tab to have the same maximum y value


***********************************************************
* VI. Compile Troubleshooting
*********************************************************** 

1.   If you see the Error:  "fatal error: TGTab2.h: No such file or directory"

> cd OnlTools/Jevp
> rm -rf StJevpViewer
> cd
> cd cvs
> cons

2.   If you see errors related to QT xxxui.h not being present:

> cd .$STAR_HOST_SYS/obj/OnlTools/Jevp/StJevpPresenter/
> $QTDIR/bin/uic EventInfoUi.ui -o ui_EventInfoUi.h
> $QTDIR/bin/uic ServerInfoUi.ui -o ui_ServerInfoUi.h
> $QTDIR/bin/uic TriggerDetectorBitsInfoUi.ui -o ui_TriggerDetectorBitsInfoUi.h
> cd
> cd cvs  
(....  This assumes your base directory was cvs ....)
> cons
(....  This time, the compile should finish!    ....)
