TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// sdm_calib_par Allocated rows: 1  Used rows: 1  Row size: 52 bytes
//  Table: sdm_calib_par_st[0]--> sdm_calib_par_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_sdm_calib_par")) return 0;
sdm_calib_par_st row;
St_sdm_calib_par *tableSet = new St_sdm_calib_par("sdm_calib_par",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.i_seed	 =     111111; // ;
    row.barrel_ped	 =      55000; // ;
    row.wafer_sig	 =       1000; // ;
    row.alice_sig	 =        500; // ;
    row.strip_P_sig	 =         50; // ;
    row.strip_N_sig	 =         50; // ;
    row.strip_P_noise	 =        700; // ;
    row.strip_N_noise	 =       1100; // ;
    row.strip_P_noise_sig	 =         50; // ;
    row.strip_N_noise_sig	 =         70; // ;
    row.n_strip_P_factor	 =         10; // ;
    row.n_strip_N_factor	 =         10; // ;
    row.n_noisy_strip	 =          0; // ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
