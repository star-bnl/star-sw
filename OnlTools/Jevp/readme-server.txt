# quick launches:

For a local run analysis:

// For the production server!
OnlTools/Jevp/launch JevpServerMain -production

// Just update DB on existing run, if server is already running...
OnlTools/Jevp/launch JevpServerMain -file xxx -updatedb

// For the test server
// runs from new data, but jevp_test data dirs, no db.
OnlTools/Jevp/launch JevpServerMain -test   

// l4db
OnlTools/Jevp/launch JevpServerMain -l4production

// dies after run, runs from file 
OnlTools/Jevp/launch JevpServerMain -test -die -file st_physics_adc_12098019_raw_0510001.daq


 





	
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

<requireTag>  -> This is a required tag for the page to be displayed
                 The following are defined automatically:
		     tpc,svt,tof,btow,fpd,ftp,pmd,ssd
                     etow,fgt,bsmd,esmd,daq,trg,l3,sc,tpx
		     pxl,pp2pp,mtd,ist,sst,rpii,gmt
		 However, additional tags can be defined by the 
                 builders using the setServerTags() function.  These tags
		 could be for example:  laserPresent, pulserPresent
		 errorPresent etc... and can be used to produce
		 conditionally present histograms...
<wide>  -> number of histograms wide on display
<deep>  -> number of histograms deep on display  (if these are too small/not present assume square)
<scaley> -> scale y for all histos to same value
