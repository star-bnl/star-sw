TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcTanL",nrows);
  memset(&row,0,tableSet->GetRowSize());
  row.idx = 1;
  row.nrows = nrows;
  row.min  = -0.50;
  row.max  =  1.20;
  row.type = 300; // correct within  the range
  row.npar = 7; 
  row.a[0] = 9.83261e-03;// TanL3DCGFRunXVII06;  FitP->Draw("mu:y>>O(100,-0.5,1.2)","i&&j&&abs(mu)<0.5&&i>13","prof")
  row.a[1] = 1.10260e-02;//
  row.a[2] =-1.70648e-01;//
  row.a[3] = 1.55690e-01;//
  row.a[4] = 3.69535e-01;//
  row.a[5] =-6.68174e-01;//
  row.a[6] = 2.78747e-01;//
 tableSet->AddAt(&row);// Outer
  memset(&row,0,tableSet->GetRowSize());
  row.idx = 2;
  row.nrows = nrows;
  row.min  = -1.00;
  row.max  =  1.0;
  row.type = 300; // correct within  the range
  row.npar = 6; 
  row.a[0] =  9.50224e-02;// TanL3DCGFRunXVII06; FitP->Draw("mu:y>>I(100,-1,1)","i&&j&&abs(mu)<0.5&&i<=13","prof")
  row.a[1] = -3.03252e-03;//
  row.a[2] = -4.28010e-01;//
  row.a[3] =  6.31240e-02;//
  row.a[4] =  2.29440e-01;//
  row.a[5] = -2.68871e-02;//
  tableSet->AddAt(&row);// Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
