TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("tpcGainCorrection",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // VoltageCGFRunIX29DEV
  row.idx        =            1; // FitP->Draw("mu:y-1390>>O(16,-300,20)","(i&&j&&i>13&&abs(mu)<0.4)/(dmu*dmu)","profg")
  row.npar       =            5; // 
  row.min        =         -0.5;
  row.max        =          100;
  row.a[0]       = TMath::Log(1310)       +3.94933e-04; // from tsspar gain at nominal 1390 V + corrections
  row.a[1]       = 10.211e-3 -7.77617e-04 +3.68859e-05; //
  row.a[2]       =            9.01092e-06 -2.70508e-06; //
  row.a[3]       =			  -6.01231e-08; //
  row.a[4]       =			  -2.15159e-10; //
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; //  VoltageCGFRunIX29DEV 
  row.idx        =            2; //  FitP->Draw("mu:y-1170>>I(20,-180,20)","(i&&j&&prob>0.01&&i<=13&&abs(mu)<0.4)/(dmu*dmu)","profg")
  row.npar       =            4; // 
  row.min        =         -0.5;
  row.max        =          100;
  row.a[0]       = TMath::Log(3558)                            +1.20357e-03; // at nominal 1170V
  row.a[1]       = 13.087e-3        -1.13903e-03  -8.47293e-04 +3.79402e-04;     
  row.a[2]       =                                             -2.88086e-06;
  row.a[3]       = 		                               -1.58229e-07;
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
