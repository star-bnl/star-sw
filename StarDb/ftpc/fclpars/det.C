St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/fclpars/det Allocated rows: 1  Used rows: 1  Row size: 988 bytes
//  Table: fcl_det_st[0]--> fcl_det_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_fcl_det")) return 0;
fcl_det_st row;
St_fcl_det *tableSet = new St_fcl_det("det",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.firstrow	 =          1; // first padrow with data (1-20) ;
    row.lastrow	 =         20; // last padrow with data ;
    row.firstsec	 =          1; // first sector in padrow with data (1-6) ;
    row.lastsec	 =          6; // last sector with data ;
    row.n_rows	 =         20; // total number of padrows, def 20 ;
    row.n_sectors	 =          6; // total number of sectors per padrow, def 6 ;
    row.n_pads	 =        160; // number of pads per sector, def 160 ;
    row.n_bins	 =        256; // number of timebins, def 256 ;
    row.n_pads_raw	 =         16; // number of pads per raw object, def 16 ;
    row.usegauss	 =          0; // 2=time gaussfit, 3=all gaussfit ;
    row.threshold	 =         10; // cluster threshold ;
    row.min_max_adc	 =         15; // cluster minimum peakheight ;
    row.n_magboltz_bins	 =        761; // # of bins in magboltz database ;
    row.n_int_steps	 =      25600; // # of steps in padtrans integration ;
    row.magfld	 =          1; // direction of magnetic fiels (+/- 1) ;
    row.r_in	 =       7.73; // inner radius of readout volume 7.73cm ;
    row.r_out	 =      30.05; // outer radius of readout volume 30.05cm ;
    row.rad_times_field	 =     7365.1; // v(r)/ln(r(a)/r(i)), 7365.1 V ;
    row.deg_to_rad	 =  0.0174533; // 0.017453293 radians per degree ;
    row.timebin_size	 =        0.2; // 0.2 usec per timebin ;
    row.rad_per_pad	 = 0.00622951; // phi angle for each pad, def 0.0062295082;
    row.rad_per_gap	 =  0.0504754; // phi angle of sector gap, def 0.05047541 ;
    row.p_standard	 =    1013.25; // standard air pressure (1013.25hPa) ;
    row.p_normalized	 =    1013.25; // normalized air pressure (in hPa) ;
    row.padtrans_e_min	 =          0; // lowest e-field in padtrans database ;
    row.padtrans_e_max	 =          0; // highest e-field in padtrans database ;
    row.pad_err_diff[0]	 =      0.015; // function of driftlength up to 2nd order;
    row.pad_err_diff[1]	 =     0.0003;
    row.pad_err_diff[2]	 =          0;
    row.time_err_diff[0]	 =     0.0025; // function of driftlength up to 2nd order;
    row.time_err_diff[1]	 =      1e-05;
    row.time_err_diff[2]	 =          0;
    row.pad_err_bad	 =      0.095; // additional error for bad-fit clusters;
    row.time_err_bad	 =      0.033; // additional error for bad-fit clusters;
    row.pad_err_unfold	 =      0.019; // additional error for unfolded clusters;
    row.time_err_unfold	 =     0.0066; // additional error for unfolded clusters;
    row.pad_err_failed	 =       0.13; // additional error for failed-fit clusters;
    row.time_err_failed	 =      0.045; // additional error for failed-fit clusters;
    row.pad_err_cutoff	 =        0.2; // additional error for cut-off clusters;
    row.time_err_cutoff	 =      0.015; // additional error for cut-off clusters;
    row.pad_err_sat	 =      0.038; // additional error for saturated clusters;
    row.time_err_sat	 =     0.0088; // additional error for saturated clusters;
    row.pad_err_2mean	 =     0.0171; // additional error 2-pad-cluster, w.mean ;
    row.pad_err_2gauss	 =    0.01045; // additional error 2-pad-cluster, gaussfit ;
    row.pad_err_3mean	 =     0.0152; // additional error 3-pad-cluster, w.mean ;
    row.pad_err_3gauss	 =    0.00057; // additional error 3-pad-cluster, gaussfit ;
    row.z_err	 =       0.01; // parameter for error in z-direction ;
 memcpy(&row.ampslopepath,"\x00",1);// path and name of ampslope database 
 memcpy(&row.ampoffpath,"\x00",1);// path and name of ampoff database 
 memcpy(&row.timeoffpath,"\x00",1);// path and name of timeoff database 
 memcpy(&row.padtranspath,"\x00",1);// path and name of padtrans database 
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
