TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// sdm_geom_par Allocated rows: 1  Used rows: 1  Row size: 48 bytes
//  Table: sdm_geom_par_st[0]--> sdm_geom_par_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_sdm_geom_par")) return 0;
sdm_geom_par_st row;
St_sdm_geom_par *tableSet = new St_sdm_geom_par("sdm_geom_par",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.N_layer	 =          7; // ;
    row.N_ladder	 =         20; // ;
    row.N_waf_per_ladder	 =         16; // ;
    row.N_alice_per_side	 =          6; // ;
    row.N_strip_per_side	 =        768; // ;
    row.L_strip_pitch	 =     0.0095; // ;
    row.L_stereo_angle	 =     0.0175; // ;
    row.L_wafer_tot_l	 =       3.75; // ;
    row.L_wafer_tot_w	 =        2.1; // ;
    row.L_wafer_tot_t	 =      0.015; // ;
    row.L_wafer_act_l	 =       3.65; // ;
    row.L_wafer_act_w	 =          2; // ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
