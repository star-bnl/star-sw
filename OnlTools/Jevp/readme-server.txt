
	
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
