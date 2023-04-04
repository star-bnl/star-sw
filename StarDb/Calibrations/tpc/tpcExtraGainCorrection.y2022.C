TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcExtraGainCorrection")) return 0;
  Int_t nrows = 6;
  St_tpcExtraGainCorrection *tableSet = new St_tpcExtraGainCorrection("tpcExtraGainCorrection",nrows);
  tpcExtraGainCorrection_st row[6] = {
    { 0, 0, 23032044, 23032065, 8, -1, -1, -1, 5,  0, 0}, /* Dead nruns = 1*/
    { 0, 0, 23032044, 23032065, 8, -1, -1, -1, 6,  0, 0}, /* Dead nruns = 2*/
    { 0, 0, 23098050, 23098050, 5, -1, -1, -1, 7,  0, 0}, /* Dead nruns = 1*/
    { 0, 0, 23098050, 23098050, 5, -1, -1, -1, 8,  0, 0}, /* Dead nruns = 1 iold = 9127 */
    { 0, 0, 23098050, 23098050, 6, -1, -1, -1, 7,  0, 0}, /* Dead nruns = 1*/
    { 0, 0, 23098050, 23098050, 6, -1, -1, -1, 8,  0, 0}  /* Dead nruns = 1*/
  };
  for (Int_t i = 0; i < nrows; i++) {
    row[i].idx = i+1;
    row[i].nrows = nrows;
    tableSet->AddAt(&row[i]);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
