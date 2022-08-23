TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcExtraGainCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcExtraGainCorrection *tableSet = new St_tpcExtraGainCorrection("tpcExtraGainCorrection",nrows);
  tpcExtraGainCorrection_st row[2] = {
    //               runs            S   r  p1  p2 rdo fee status	        beginTime, endTime                       
    { 1, nrows, 20174045,  20175028, 5, 55, 30, 45, 6,  0, 0},  /* Dead  */ //  20190624.011700        20190624.124959  // Run XIX
    { 2, nrows, 20174045,  20175028, 5, 56, 30, 45, 6,  0, 0}   /* Dead  */ //  -"- 
  };
  for (Int_t i = 0; i < nrows; i++) {
    tableSet->AddAt(&row[i]);
  }
  //  tableSet->Print(0,nrows);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}


