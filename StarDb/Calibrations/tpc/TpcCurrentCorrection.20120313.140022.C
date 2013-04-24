TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcAvCurrent",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Outer sector correction 
  row.idx        =            1; // AvCurrentGFRunXII11pp510P13ia_dEdx + AvCurrentGFRunXII13pp510P13ia_dEdx
  row.npar       =            5; // 
  row.min        =           0.;
  row.max        =          0.3;
  row.a[0]	 = 3.47366e-03 + 1.4489e-02+1.15370e-03+9.82598e-03;// 
  row.a[1]	 = 2.71410e-01             -3.92977e-01-4.04738e-01;//
  row.a[2]	 = 1.38632e+00             -1.84333e+00+4.44214e+00;//
  row.a[3]	 = 2.47618e+00                         -1.74103e+01;//
  row.a[4]	 =                                      2.10278e+01;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      =        nrows; // Inner sector correction 
  row.idx        =            2; // AvCurrentGFRunXII11pp510P13ia_dEdx + AvCurrentGFRunXII16pp510P13ia_dEdx
  row.npar       =            6; // 
  row.min        =           0.;
  row.max        =          0.6;
  row.a[0]	 = 4.11433e-02+4.51534e-02 - 2.21850e-02;//
  row.a[1]	 =-9.01668e-01-1.23711e-01              ;//
  row.a[2]	 = 7.55371e+00+1.01734e-01              ;//
  row.a[3]	 =-3.10571e+01                          ;//
  row.a[4]	 = 6.08538e+01				;//
  row.a[5]	 =-4.54947e+01				;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 0
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

