TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcExtraGainCorrection")) return 0;
  Int_t nrows = 1;
  St_tpcExtraGainCorrection *tableSet = new St_tpcExtraGainCorrection("tpcExtraGainCorrection",nrows);
  tpcExtraGainCorrection_st row[25] = {
    //               runs            S   r  p1  p2 rdo fee status	        beginTime, endTime                       
    { 1, nrows, 20342006,  23029013,21, 10, 26, 43, 1, 47, 0},  /* Dead SL21-47 */
  };
  // bad Runs: 23029013 
  for (Int_t i = 0; i < nrows; i++) {
    tableSet->AddAt(&row[i]);
  }
  //  tableSet->Print(0,nrows);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
