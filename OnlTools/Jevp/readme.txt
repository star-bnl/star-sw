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

I now recommend using code directly from the JevpProduction branch as follows.   
Note that cons will take a long time the first time it is compiled

1.  To get your repository, use the upstream repo:   jml985/JevpProduction 
From scratch
> git init
> get remote add origin https://github.com/YOURCLONE/star-sw.git
> git remote add upstream https://github.com/jml985/star-sw.git
> git fetch upstream
> git checkout upstream/JevpProduction -b JevpProduction
> source OnlTools/Jevp/level.source
> cons

2.  To update existing JevpProduction branch
> git fetch upstream
> git merge upstream/JevpProduction
> source OnlTools/Jevp/level.source
> cons

3.  To make a pull request
> git add -u
> git commit -m "your comment"
> git push origin JevpProduction

Then, use gituhub to make a pull request to the official main branch: 
      
      a. pull request from your JevpProduction to https://github.com/star-bnl/star-sw.git / main
      b. tell jml@bnl.gov the pull request #
      
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




// Old git instructions using merged cvs/github 

// Mixed Distribution...
1.   Using github clone https://github.com/jml985/star-sw.git

2.   - To guarentee the most modern codes you need to have a mixed git/cvs directory.   
     - The production jevp code is in the github under jml985/star-sw.git in the branch JevpProduction
     - The production RTS code is in cvs under StRoot/RTS
 
git init
git config core.sparseCheckout true
git remote add origin https://github.com/YOURCLONE/star-sw.git
echo 'OnlTools/Jevp' > .git/info/sparse-checkout
echo 'OnlTools/PDFUtil' >> .git/info/sparse-checkout
git fetch origin
git checkout origin/JevpProduction -b JevpProduction
cvs co StRoot/RTS
source OnlTools/Jevp/level.source
cons

3.   Make a pull request
    - When you have tested your code, use "git commit" and "git push" to put it into your 
      local repository.  Then issue a pull request to the main branch of 
      https://github.com/star-bnl/star-sw.git.   If you use these directions
      directly you will need to modify the defaults
    - Tell jml@bnl.gov the pull request #.   This will allow me to incorporate the code
      in the production server without waiting on the various evaluation procedures.
