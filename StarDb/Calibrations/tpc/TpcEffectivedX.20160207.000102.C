TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_TpcEffectivedX")) return 0;
  TpcEffectivedX_st row;
  St_TpcEffectivedX *tableSet = new St_TpcEffectivedX("TpcEffectivedX",1);
  memset(&row,0,tableSet->GetRowSize()); 
  row.scaleInner = TMath::Exp(-1.43564e-01+3.25987e-02-2.09212e-03);
  row.scaleOuter = TMath::Exp(2.77301e-02 -2.00629e-02+1.38901e-03);
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
