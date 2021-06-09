TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdZdY",nrows);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   3.2;
  row.npar =            5;// RunXXI51/dZdY3GF7p7GeV_2021.root
  row.a[0] =  1.05172e-02;// FitP->Draw("mu-muJ:y>>O(65,-2,4.5)","i&&j&&abs(mu)<1&&abs(x)>40.5","prof")
  row.a[1] =  1.84116e-04;// O->Fit("pol4","er","",-2,3.2)
  row.a[2] = -2.24339e-02;
  row.a[3] =  8.92036e-03;
  row.a[4] = -1.82977e-03;
  tableSet->AddAt(&row);// Outer
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min = -3.5;
  row.max =  3.5;
  row.type = 1400;
  row.npar =            8;// RunXXI51/dZdY3GF7p7GeV_2021.root
  row.a[0] =  5.27979e-02;// FitP->Draw("mu-muJ:y>>I(90,-4,5)","i&&j&&abs(mu)<1&&abs(x)<40.5","prof")
  row.a[1] =  5.09038e-03;// TF1 *f1400 = new TF1("f1400","gaus(0)+pol4(3)"); I->Fit("gaus","er","",-1,1); f1400->SetParameters(gaus->GetParameters()); I->Fit(f1400,"er","",-3.5,3.5)
  row.a[2] =  2.93204e-01;
  row.a[3] = -1.07322e-02;
  row.a[4] =  4.17612e-03;
  row.a[5] = -1.00370e-03;
  row.a[6] = -1.06815e-03;
  row.a[7] = -5.03509e-04;
  tableSet->AddAt(&row);// Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
