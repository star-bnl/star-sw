TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Calibrations/tpc/.tpcSectorT0offset/tpcSectorT0offset Allocated rows: 1  Used rows: 1  Row size: 72 bytes
  //  Table: tpcSectorT0offset_st[0]--> tpcSectorT0offset_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcSectorT0offset")) return 0;
  tpcSectorT0offset_st row;
  St_tpcSectorT0offset *tableSet = new St_tpcSectorT0offset("tpcSectorT0offset",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  //  row.t0[15] =  -21.4 - 0.176 + 1.74445; // Tpx4 Membrane Shift = 5.22549 +/- 0.000659445
  //  row.t0[15] =  -21.4 - 0.176 + 1.74445 - 5.23; //Tpx5 Membrane Shift = -5.36681 +/- 42.0076 
  //  row.t0[15] =  -21.4 - 0.176 + 1.74445 + 5.23; // 04/30/08 Tpx61 Tpx61 Averaged Shift = 7.21025 +/- 0.000653672
  //  row.t0[15] =  -21.4 - 0.176 + 1.74445 + 5.23 -0.5*7.21025; // 05/01/08 Tpx7 Averaged Shift = 8.70517 +/- 0.00066056
  //  row.t0[15] =  -21.4 - 0.176 + 1.74445 + 5.23 -0.5*7.21025 + 0.5*8.70517 ; // 05/01/08 Tpx8 Averaged Shift = 5.7161 +/- 0.000657116
  //  row.t0[15] =  -21.4 - 0.176 + 1.74445 + 5.23 -0.5*7.21025 + 0.5*8.70517 + 5.7161; // 05/01/08 Tpx9  
  row.t0[15] =  -21.4 - 0.176 + 1.74445 + 5.23 -0.5*7.21025 + 0.5*8.70517 + 0.5*5.7161; // 05/03/08 Tpx10
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
