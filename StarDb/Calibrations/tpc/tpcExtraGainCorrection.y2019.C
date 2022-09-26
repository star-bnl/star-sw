TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcExtraGainCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcExtraGainCorrection *tableSet = new St_tpcExtraGainCorrection("tpcExtraGainCorrection",nrows);
  tpcExtraGainCorrection_st row[4] = {
    //               runs            S   r  p1  p2 rdo fee status	        beginTime, endTime                       
    { 1, nrows, 20342006,  20342011,19, 32, 11, 27, 3, 17, 1},  /* Alive */ //  S19-17 is alive
    { 2, nrows, 21050064,  21050064,19, 31,  2, 17, 3, 17, 1},  /* Alive */
    { 3, nrows, 21052036,  21052036,19, 31,  3, 20, 3, 17, 1},  /* Alive */
    { 4, nrows, 20342006,  23029013,21, 10, 26, 43, 1, 47, 0},  /* Dead SL21-47 */
  };
  for (Int_t i = 0; i < nrows; i++) {
    tableSet->AddAt(&row[i]);
  }
  //  tableSet->Print(0,nrows);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}


