St_DataSet *CreateTable() { 
// -----------------------------------------------------------------
// Top/ctg/tof Allocated rows: 1  Used rows: 1  Row size: 68 bytes
//  Table: ctg_geo_st[0]--> ctg_geo_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_ctg_geo")) return 0;
ctg_geo_st row;
St_ctg_geo *tableSet = new St_ctg_geo("tof",1);
//
memset(&row,0,tableSet->GetRowSize());
    row.init	 =          1; // ;
    row.detector	 =          2; // ;
    row.i_eta_max	 =         18; // ;
    row.i_eta_min	 =          1; // ;
    row.i_phi_max	 =        300; // ;
    row.i_phi_min	 =          1; // ;
    row.n_counter_eta	 =          9; // ;
    row.n_counter_phi	 =          5; // ;
    row.n_tray_eta	 =          2; // ;
    row.n_tray_phi	 =         60; // ;
    row.counter_thickness	 =          1; // ;
    row.counter_width	 =      2.159; // ;
    row.r	 =     213.95; // ;
    row.tray_height	 =        4.7; // ;
    row.tray_width	 =     10.795; // ;
    row.tray_length	 =     120.81; // ;
    row.tray_phi_zero	 =          0; // ;
tableSet->AddAt(&row,0);
// ----------------- end of code ---------------
 return (St_DataSet *)tableSet;
}
