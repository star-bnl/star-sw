TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZDC",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      =        nrows; //  Zdc3CGFRunX25P10i_AuAu39_production_ReversedFullField.root +
  row.idx        =            1; //  
  row.npar       =            0; //  FitP->Draw("mu:y>>O(40,3,3.8)","(i&&j&&i>13)/(dmu**2)","profw")
  tableSet->AddAt(&row); //Outer
  memset(&row,0,tableSet->GetRowSize());
  row.nrows      =        nrows; //  Zdc3CGFRunX25P10i_AuAu39_production_ReversedFullField.root +
  row.idx        =            2; //  
  row.npar       =            0; //  FitP->Draw("mu:y>>I(40,3,3.8)","(i&&j&&i<=13)/(dmu**2)","profw")
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
