TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// db/.const/StarDb/Calibrations/tpc/.tpcGain/tpcGain Allocated rows: 1  Used rows: 1  Row size: 72 bytes
//  Table: tpcGain_st[0]--> tpcGain_st[0]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcGain")) return 0;
  tpcGain_st row;
  St_tpcGain *tableSet = new St_tpcGain("tpcGain",24);
  static const Int_t NumberOfPadsAtRow[45] = {
    88, 96,104,112,118,126,134,142,150,158, // Inner
   166,174,182,
                98,100,102,104,106,106,108, // Outer
   110,112,112,114,116,118,120,122,122,124,
   126,128,128,130,132,134,136,138,138,140,
   142,144,144,144,144
  };
  //
  memset(&row,0,tableSet->GetRowSize());
  for (Int_t r = 1; r <= 45; r++) {
    for (Int_t p = 1; p <= NumberOfPadsAtRow[r-1]; p++) {
      row.Gain[r-1][p-1] = 1.;
    }
  }
  for (Int_t sec = 1; sec <= 24; sec++) {
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
