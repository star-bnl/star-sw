TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // TpcSecRow Allocated rows: 24  Used rows: 24  Row size: 360 bytes
  //  Table: TpcSecRowCor_st[0]--> TpcSecRowCor_st[23]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TpcSecRowCor")) return 0;
  TpcSecRowCor_st row;
  St_TpcSecRowCor *tableSet = new St_TpcSecRowCor("TpcSecRowB",24);
  //
  memset(&row,0,tableSet->GetRowSize());
  for (int i=0;i<72;i++) { 
    row.GainScale[i]     =   1.;
    row.GainRms[i]       =   0.;
  }
  for (int k=0;k<24;k++) {
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
