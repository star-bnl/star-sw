-------------------------------------------------------------------------
-------------I.   Compile & Run -----------------------------------------
-------------------------------------------------------------------------

1.  Initial versions of the JEVP framework were compiled as executable code.
This is too difficult to do when working with STAR offline code, so I've
aborted this idea.  

2.  This is still a test setup, so you must check out the following directories:

    offline/users/jml/StJevpPool
    offline/users/jml/StPDFUtil
    mgr

3.  You must make the following links in your cvs/StRoot directory:

    ln -s offline/users/jml/StJevpPool StRoot/StJevpPool
    ln -s offline/users/jml/StPDFUtil StRoot/StPDFUtilities

4.  Instead of a main() function, the builder classes must contain:

	static void main(int argc, char *argv[]);

Additionally, the class must be split into a ".h" file and a ".cxx" file and
include the 
	
	ClassDef(ClassName, 1);     and
	ClassImp(ClassName);

Flags as standard for root classes.

5.  To compile use "cons" as standard for offline code

6.  To run use the "launch" utility in StJevpPool.   The following example 
    executes the "xxxBuilder" in a standalone mode:

    launch xxxBuilder -file filename.daq -noserver -pdf output.pdf

//--------------------------------------------------------------------
//------------   Developing a new set of histograms ------------------
//--------------------------------------------------------------------

1.  Histogram builders run as separate processes from the event pool server.
    All histogram builders inherit from the class "JevpPlotSet".   An example
    of a working builder is the file:

       StRoot/StJevpPool/StJevpBuilders/basePlotBuilder.cxx
       StRoot/StJevpPool/StJevpBuilders/basePlotBuilder.h

2.  To fit into the histogram package scheme for full running, builder classes
    must satisfy the following:

    a.  The files should be in the StJevpBuilders directory.
    b.  The files should be named xxxBuilder.cxx / xxxBuilder.h
    c.  The files need to be root visible (Use the ClassDef/ClassImp macros)
    d.  The variable "plotsetname" should be set to the name of the builder
        in the class constructor.   (ie.  plotsetname = "xxx";)
    e.  Any plots / histograms should have names that are tagged with the
        plotset name.  ie.  Don't use new TH1F("myplot",...).  Use
        new TH1F("xxx_myplot",....);
    f.  Even though the builders run stand alone for testing, the purpose
        is to run as part of the official histogram package.   Therefore,
        there can be no external data files in local directories.  
        Instead use the following variables for any external data files dirs:


	JevpPlotSet.confdatadir      // for any static configuration data

	JevpPlotSet.clientdatadir    // for any data to be saved/loaded
                                     // for example if there are any plots
                                     // containing information over periods
                                     // of days or weeks...   The full extent
                                     // of uptime you can expect is a single
                                     // run, therefore, if you need info
                                     // from previous runs you must save it
                                     // at the end of each run and load at 
                                     // the start

        For local testing and operation you can modify these using the 
        command line flags "-confdatadir" and "-clientdatadir" in the
        launch command.

        in both cases, create a subdirectory for your builder "xxx" under
        the data directories for your builders data.


	
// The XML file and the JevpEditor

Example:

<doc>
   <display_def>shift
      <tab>base
         <tab>Time
            <wide>4</wide>
            <deep>1</deep>
            <histogram>trg_Time</histogram>
            <histogram>daq_Time</histogram>
            <histogram>base_Time</histogram>
         </tab>
      </tab>
   </display_def>
   <display_def>tpc
      <tab>tpc
         <histogram>h1</histogram>
      </tab>
   </display_def>
   <pallete>
	<histogram>a</histogram>
   </pallete>
</doc>

   
The top level directories are the xml files / GUI structures. 
There can be any set of parameters, however the following are defined:

<wide>  -> number of histograms wide on display
<deep>  -> number of histograms deep on display  (if these are too small/not present assume square)
<scaley> -> scale y for all histos to same value
