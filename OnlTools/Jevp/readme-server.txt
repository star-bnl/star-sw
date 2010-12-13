# quick launches:

For a local run analysis:

(limited builders)
     OnlTools/Jevp/launch JevpServerMain -die -kill -launchbuilders -localsocket -builders daq,trg,base -nodb -file xxx.daq

(default builders)
     OnlTools/Jevp/launch JevpServerMain -die -kill -launchbuilders -localsocket -nodb -file xxx.daq

(don't die at end)
     OnlTools/Jevp/launch JevpServerMain -kill -launchbuilders -localsocket -nodb -file xxx.daq 

(don't start the run:  wait for jevpStatus to do it...)
     OnlTools/Jevp/Launch JevpServerMain -kill -localsocket -nodb
     jevpStatus launch xxx.daq

For a normal server:  (doesn't kill builders, reads from evp, writes to database, uses port 3499)
     OnlTools/Jevp/launch JevpServerMain -launchbuilders

For a normal server on a different port
     OnlTools/Jevp/launch JevpServerMain -launchbuilders -port 3500


 





	
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
