TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcExtraGainCorrection")) return 0;
  Int_t nrows = 20;
  St_tpcExtraGainCorrection *tableSet = new St_tpcExtraGainCorrection("tpcExtraGainCorrection",nrows);
  tpcExtraGainCorrection_st row[20] = {
    // y2020
    { 0, 0, 20344045, 21258004,21, -1, -1, -1, 1, 47, 0}   /* Dead  S21_F47 20191210.162105        20200914.081917 */
    { 0, 0, 21037017, 21037017,16, 39,  2,  9, 3,  1, 0}, /* Dead nruns = 1*/
    { 0, 0, 21037017, 21037017,16, 40,  2,  9, 3,  1, 0}, /* Dead nruns = 1*/
    { 0, 0, 21060005, 21060005,16, 39,  2,  9, 3,  1, 0}, /* Dead nruns = 1*/
    { 0, 0, 21060005, 21060005,16, 40,  2,  9, 3,  1, 0}, /* Dead nruns = 1*/
    { 0, 0, 21067021, 21067021,14, 17, 10, 24, 2, 36, 0}, /* Dead */
    { 0, 0, 21067021, 21067021,14, 18, 10, 19, 2, 36, 0}, /* Dead */
    { 0, 0, 21069008, 21069014,14, -1, -1, -1, 4, 27, 1}, /* Alive nruns = 6 iold = 3105 */
    { 0, 0, 21069014, 21069020,21, -1, -1, -1, 1, 54, 1}, /* Alive nruns = 4*/
    { 0, 0, 21071013, 21071013, 9, -1, -1, -1, 4,  4, 0}, /* Dead nruns = 1*/
    { 0, 0, 21078014, 21078014, 9, -1, -1, -1, 4,  4, 0}, /* Dead */
    { 0, 0, 21175025, 21175025, 6, 39,  2,  9, 3,  1, 0}, /* Dead nruns = 1*/
    { 0, 0, 21175025, 21175025, 6, 40,  2,  8, 3,  1, 0}, /* Dead nruns = 1*/
    { 0, 0, 21176027, 21177012,11, -1, -1, -1, 5,  0, 0}, /* Dead nruns = 11*/
    { 0, 0, 21223036, 21223036,16, 39,  2,  9, 3,  1, 0}, /* Dead nruns = 1*/
    { 0, 0, 21223036, 21223036,16, 40,  2,  9, 3,  1, 0}, /* Dead nruns = 1*/
    { 0, 0, 21233027, 21233027,16, 39,  2,  9, 3,  1, 0}, /* Dead nruns = 1*/
    { 0, 0, 21233027, 21233027,16, 40,  2,  9, 3,  1, 0}, /* Dead nruns = 1*/
    { 0, 0, 21243033, 21258004,13, -1, -1, -1,-1,  0, 0}, /* Dead whole sector*/
    { 0, 0, 21243033, 21258004,14, -1, -1, -1,-1,  0, 0}, /* Dead whole sector*/
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
