TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/RunLog/onl/.tpcRDOMasks/tpcRDOMasks Allocated rows: 12  Used rows: 12  Row size: 12 bytes
  //  Table: tpcRDOMasks_st[0]--> tpcRDOMasks_st[11]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!TClass::GetClass("St_tpcRDOMasks")) return 0;
  tpcRDOMasks_st row;
  Int_t nrows = 12;
  St_tpcRDOMasks *tableSet = new St_tpcRDOMasks("tpcRDOMasks",nrows);
  //
  memset(&row,0,tableSet->GetRowSize());
  row.runNumber	 =    8178047; // run number  ;
  row.mask	 =       4095; // enable mask  ;
  for (Int_t i = 0; i < nrows; i++) {
    row.sector	 =          2*i+1; // sector  ;
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
