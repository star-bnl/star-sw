TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdXdY",nrows);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =   0.0;
  row.max =   2.3;
  row.type =   6; // abs(x)
  row.npar =            5;// RunXXI51/dXdY3GF7p7GeV_2021.root
  row.a[0] =  1.45692e-02;// FitP->Draw("mu-muJ:abs(y)>>O(50,0,2.5)","i&&j&&abs(mu)<1&&abs(x)>40.5","prof")
  row.a[1] = -1.94582e-02;// O->Fit("pol4","e")
  row.a[2] = -7.72579e-03;
  row.a[3] = -1.67309e-02;
  row.a[4] =  5.64755e-03;
  tableSet->AddAt(&row);// Outer
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  0.0;
  row.max =  2.0;
  row.type =   6; // abs(x)
  row.npar =            2;// RunXXI51/dXdY3GF7p7GeV_2021.root
  row.a[0] =  1.23975e-02;// FitP->Draw("mu-muJ:abs(y)>>I(50,0,2.5)","i&&j&&abs(mu)<1&&abs(x)<40.5","prof")
  row.a[1] = -5.56664e-02;// I->Fit("pol1","er","",0,2)
  tableSet->AddAt(&row);// Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
