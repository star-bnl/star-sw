TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrection",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Outer sector correction 
  row.idx        =            1; // AvCurrentNNFRunXV44
  row.npar       =            9; // 
  row.min        =           0.;
  row.max        =          1.0;
  row.a[0]       =  1.37424e-02;// 
  row.a[1]       =  6.88835e-01;//
  row.a[2]       = -2.16928e+01;//
  row.a[3]       =  2.43089e+02;//
  row.a[4]       = -1.44817e+03;//
  row.a[5]       =  4.96383e+03;//
  row.a[6]       = -9.76381e+03;//
  row.a[7]       =  1.02139e+04;//
  row.a[8]       = -4.39812e+03;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Inner sector correction 
  row.idx        =            2; // AvCurrentNNFRunXV44
  row.npar       =            6; // 
  row.min        =           0.;
  row.max        =          1.0;
  row.a[0]       =   3.57393e-02;
  row.a[1]       =  -1.40402e-01;
  row.a[2]       =   4.80494e-01;
  row.a[3]       =  -3.41385e+00;
  row.a[4]       =   8.04357e+00;
  row.a[5]       =  -5.81328e+00;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

