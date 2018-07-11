TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcRDOT0offset")) return 0;
  tpcRDOT0offset_st row;
  St_tpcRDOT0offset *tableSet = new St_tpcRDOT0offset("tpcRDOT0offset",1);
  memset(&row, 0, tableSet->GetRowSize());
  Int_t sector = 17;
  Int_t rdo    =  1;
  Float_t t0     = -1.0; // -0.89;
  row.isShifted[sector-1] = kTRUE;
  row.t0[sector-1][rdo-1] = t0;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
