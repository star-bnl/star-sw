TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcExtraGainCorrection")) return 0;
  Int_t nrows = 6;
  St_tpcExtraGainCorrection *tableSet = new St_tpcExtraGainCorrection("tpcExtraGainCorrection",nrows);
  tpcExtraGainCorrection_st row[6] = {
    /*      runs                S   r  p1  p2 rdo fee status                  beginTime,             endTime                       */
    // y2019
    { 0, 0, 20105037, 20107001,19, -1, -1, -1, 3, 17, 0},  /* Dead  S19_F17 20190416.035540        20190417.040924 */
    { 0, 0, 20174045, 20175028, 5, 55, 30, 45, 6,  0, 0},  /* Dead          20190624.011700        20190624.124959 */
    { 0, 0, 20174045, 20175028, 5, 56, 30, 45, 6,  0, 0},  /* Dead nruns = 1*/
    { 0, 0, 20174045, 20175028, 6, 15, 30, 45, 2, 41, 1},  /* Alive */
#if 0
/*Alive:*/ { 6, 15, 30, 45, 2, 41}, /* 20175028 */
/*Alive:*/ { 6, 16, 30, 45, 2, 41}, /* 20175028 */
#endif
    { 0, 0, 20342006, 20342006,21, -1, -1, -1, 1, 47, 0},  /* Dead */
    { 0, 0, 20344045, 21258004,21, -1, -1, -1, 1, 47, 0}   /* Dead  S21_F47 20191210.162105        20200914.081917 */
  };
  for (Int_t i = 0; i < nrows; i++) {
    row[i].idx = i+1;
    row[i].nrows = nrows;
    tableSet->AddAt(&row[i]);
  }
  //  tableSet->Print(0,nrows);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}


