St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/fmtpars/fmtpar Allocated rows: 1  Used rows: 1  Row size: 88 bytes
//  Table: tfc_fmtpar_st[0]--> tfc_fmtpar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tfc_fmtpar")) return 0;
tfc_fmtpar_st row;
St_tfc_fmtpar *tableSet = new St_tfc_fmtpar("fmtpar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.nped_samples[0]	 =          0; // # pedestal events in mean ped ;
    row.nped_samples[1]	 =          0;
    row.nped_samples[2]	 =          0;
    row.nped_samples[3]	 =          0;
    row.nped_samples[4]	 =          0;
    row.nped_samples[5]	 =          0;
    row.npuls_samples[0]	 =          0; // # pulser events in mean pulser ;
    row.npuls_samples[1]	 =          0;
    row.npuls_samples[2]	 =          0;
    row.npuls_samples[3]	 =          0;
    row.npuls_samples[4]	 =          0;
    row.npuls_samples[5]	 =          0;
    row.event_type	 =          0; // ;
    row.ped_subtract	 =          0; // 1 if ped should be subtracted from data ;
    row.thresh	 =          3; // pedestal-subtracted adc threshold ;
    row.times_rms	 =          0; // quant subtracted=ave+(times_rms)*rms ;
    row.ave_gain	 =       1024; // gives absolute scale for gain ;
    row.first_bucket	 =          1; // first bucket used in gain calculation ;
    row.last_bucket	 =        512; // last bucket used  "   "     "         ;
    row.only_type	 =         -1; // only index type to be processed (-1=all) ;
    row.data_size	 =          2; // size of data tables to be used (bytes) ;
    row.printf	 =          0; // output flag ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
