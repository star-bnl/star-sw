TDataSet *CreateTable() { 
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.0;
  row.max = 210.0;
  row.npar       =          103;// Z3CGFRunXII14pp510P13ia_dEdx + Z3CGFRunXII15pp510P13ia_dEdx_pp500_production_2012_ReversedFullField + Z3CGFRunXII25pp510P13ia_dEdx
  row.a[0]       = -9.57247e-03;// FitP->Draw("mu:y","(i&&j&&i>13)/(dmu**2)","profg")
  row.a[1]       =  1.75916e-04;// htemp->Fit("pol2","er","",22,210)
  row.a[2]       = -6.80008e-07;// 
  tableSet->AddAt(&row); // Outer
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  40.0;
  row.max = 210.0;
  row.npar       =          107;// + Z3CGFRunXII26pp510P13ia_dEdx
  row.a[0]       =  1.34976e-01+1.05671e-01+6.27308e-01+1.50210e-01;// FitP->Draw("mu:y","(i&&j&&i<=13)/(dmu**2)","profg")
  row.a[1]       = -2.55337e-03-7.30253e-03-2.95065e-02-2.84628e-03;// htemp->Fit("pol6","er","",24,210)
  row.a[2]       =  1.61811e-05+1.41962e-04+5.98955e-04+1.75925e-05;
  row.a[3]       = -3.42047e-08-1.20040e-06-6.28137e-06-3.50370e-08;
  row.a[4]       =              4.70223e-09+3.55803e-08;
  row.a[5]       =             -7.01087e-12-1.03924e-10;
  row.a[6]       =                          1.22793e-13;
  tableSet->AddAt(&row); // Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
 
