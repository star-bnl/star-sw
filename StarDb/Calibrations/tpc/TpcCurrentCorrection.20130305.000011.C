TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcCurrentCorrection",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Outer sector correction 
  row.idx        =            1; // AvCurrentGFRunXIII06pp500 + AvCurrentGFRunXIII11pp500p1
  row.npar       =            6; // 
  row.min        =           0.;
  row.max        =          0.5;
  row.a[0]       = 1.87066e-03 -2.53540e-03;// 
  row.a[1]       =-3.35087e-02 +3.02590e-01;//
  row.a[2]       = 5.18756e-01 -4.58242e+00;//
  row.a[3]       =-3.25541e+00 +2.46239e+01;//
  row.a[4]       = 4.64011e+00 -5.46608e+01;//
  row.a[5]       = 	        4.27665e+01;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Inner sector correction 
  row.idx        =            2; // AvCurrentGFRunXII04UU193
  row.npar       =            6; // 
  row.min        =           0.;
  row.max        =         0.62;
  row.a[0] =           5.53772e-04+1.58919e-03;
  row.a[1] =           5.68556e-01 -6.41842e-02;
  row.a[2] =          -6.60900e+00+ 1.62023e-01;
  row.a[3] =           2.90310e+01+ 3.34617e-03;
  row.a[4] =          -5.62831e+01;
  row.a[5] =           3.93675e+01;
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

