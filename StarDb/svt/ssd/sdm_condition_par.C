TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// sdm_condition_par Allocated rows: 1  Used rows: 1  Row size: 96 bytes
//  Table: sdm_condition_par_st[0]--> sdm_condition_par_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_sdm_condition_par")) return 0;
sdm_condition_par_st row;
St_sdm_condition_par *tableSet = new St_sdm_condition_par("sdm_condition_par",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.i_seed	 =     111111; // ;
    row.N_active_ladder[0]	 =          1; // ;
    row.N_active_ladder[1]	 =          1;
    row.N_active_ladder[2]	 =          1;
    row.N_active_ladder[3]	 =          1;
    row.N_active_ladder[4]	 =          1;
    row.N_active_ladder[5]	 =          1;
    row.N_active_ladder[6]	 =          1;
    row.N_active_ladder[7]	 =          1;
    row.N_active_ladder[8]	 =          1;
    row.N_active_ladder[9]	 =          1;
    row.N_active_ladder[10]	 =          1;
    row.N_active_ladder[11]	 =          1;
    row.N_active_ladder[12]	 =          1;
    row.N_active_ladder[13]	 =          1;
    row.N_active_ladder[14]	 =          1;
    row.N_active_ladder[15]	 =          1;
    row.N_active_ladder[16]	 =          1;
    row.N_active_ladder[17]	 =          1;
    row.N_active_ladder[18]	 =          1;
    row.N_active_ladder[19]	 =          1;
    row.p_bad_wafer	 =          0; // ;
    row.p_bad_alice	 =          0; // ;
    row.p_bad_strip	 =          0; // ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
