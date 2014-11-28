St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/mwcpars/mpar Allocated rows: 1  Used rows: 1  Row size: 32 bytes
//  Table: mwc_mpar_st[0]--> mwc_mpar_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_mwc_mpar")) return 0;
mwc_mpar_st row;
St_mwc_mpar *tableSet = new St_mwc_mpar("mpar",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.gain	 =         11; // Variation in Scale Factor ;
    row.de_thresh_in	 =      2e-08; // Threshold for de hit inner sector ;
    row.de_thresh_out	 =      3e-08; // Threshold for de hit outer sector ;
    row.tof_thresh	 =          0; // Threshold for tof of hit ;
    row.num_counts_out	 =         96; // Number of adcs to DSM tree ;
    row.num_wires_count	 =         80; // Number of wires per adc ;
    row.el_noise_width	 =          0; // ;
    row.min_ion	 =          0; // Minimum ionization ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
