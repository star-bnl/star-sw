TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcTanL",nrows);
  memset(&row,0,tableSet->GetRowSize());
  row.idx = 1;
  row.nrows = nrows;
  row.type = 1300; //  TF1 *g = new TF1("gg","gaus(0)+pol3(3)"); f->SetParameters(-0.1,0,0,8.93349e-02,-2.68009e-02,2.26760e-03)
  row.min  = -1.5;
  row.max  =  1.5;
  row.npar = 7; 
  row.a[0] =  1.22189e-02;// TanL3DCGFRunXVI310; FitP->Draw("mu:y>>O","(i&&j&&i>13&&abs(mu)<0.1)","prof"); O->Fit(g,"er","",-1.5,1.5)
  row.a[1] = -1.30668e-01;//
  row.a[2] =  1.45702e-01;//
  row.a[3] =  8.07666e-03 +3.25114e-03;//
  row.a[4] = -6.57809e-03 -6.78809e-03;//
  row.a[5] = -3.10139e-02 -7.60882e-03;//
  row.a[6] =  1.55268e-02 +6.11343e-03;//
  tableSet->AddAt(&row);// Outer
  memset(&row,0,tableSet->GetRowSize());
  row.idx = 2;
  row.nrows = nrows;
  row.min  = -2.0;
  row.max  =  2.0;
  row.type = 1300; // 
  row.npar = 7; 
  row.a[0] =  1.70033e-01;// TanL3DCGFRunXVI310; FitP->Draw("mu:y>>I","(i&&j&&i<=13&&abs(mu)<0.2)","prof"); I->Fit(g,"er","",-2,2)
  row.a[1] = -4.26791e-02;//
  row.a[2] =  3.96921e-01;//
  row.a[3] = -8.32501e-02 +1.07108e-03;//
  row.a[4] =  1.10068e-02 -9.16615e-03;//
  row.a[5] =  1.10613e-04 -6.53130e-03;//
  row.a[6] = -2.08163e-03 +3.82326e-03;//
  tableSet->AddAt(&row);// Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
