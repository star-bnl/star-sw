TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZDC",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.nrows      =        nrows; // Zdc3CGFRunX25P10i_AuAu7_production_ReversedFullField
  row.idx        =            1; // 
  row.npar       =            5; //  FitP->Draw("mu:y>>O(20,0,2)","(i&&j&&i>13)/(dmu**2)","profw")
  row.min        =    0;
  row.max        =  1.9;
  row.a[0]	 = -9.53852e-03;//
  row.a[1]	 =  7.47480e-02;//
  row.a[2]	 = -1.49793e-01;//
  row.a[3]	 =  1.10529e-01;//
  row.a[4]	 = -2.83806e-02;//
  tableSet->AddAt(&row); //Outer
  memset(&row,0,tableSet->GetRowSize());
  row.nrows      =        nrows; // Zdc3CGFRunX25P10i_AuAu7_production_ReversedFullField
  row.idx        =            2; // 
  row.npar       =            5; // FitP->Draw("mu:y>>I(20,0,2)","(i&&j&&i<=13&&abs(mu)<0.3)/(dmu**2)","profw")
  row.min        =  0;
  row.max        =  2;
  row.a[0]	 = -5.07436e-03;//
  row.a[1]	 =  8.89721e-02;//
  row.a[2]	 = -1.90440e-01;//
  row.a[3]	 =  1.35591e-01;//
  row.a[4]	 = -3.37787e-02;//
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
