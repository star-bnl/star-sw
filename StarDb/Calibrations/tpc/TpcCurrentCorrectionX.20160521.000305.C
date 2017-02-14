TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 1;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrectionX",nrows);
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentCGFRunXVI305; FitP->Draw("mu:y","i&&j","prof"); 
  row.idx   =   1;
  row.nrows =   1;
  row.npar       =              2;
  row.a[0]       =    3.56968e-03;
  row.a[1]       =   -1.07468e-01;
  tableSet->AddAt(&row); // row   1
  memset(&row,0,tableSet->GetRowSize()); // AvCurrentNNFRunXVIAuAu200p33.root
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
