TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrection",11);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.npar       =      4;  // I70 TPoints70BGPHist301P02gh1 vs log(TrackLength)
  row.min        =     2.30;
  row.max        =     4.79;
  row.a[0]	 =  1.36498e+00;//1.45999e+00   9.32351e-04  
  row.a[1]	 = -8.56953e-01;//   3.46176e-04 
  row.a[2]	 =  1.45743e-01;//   8.23398e-05  
  row.a[3]	 = -5.75865e-03;//   1.49169e-05 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 1
  row.npar       =      4;  // sigma.I70 TPoints70BGPHist301P02gh1 vs log(TrackLength)
  row.min        =     2.30;
  row.max        =     4.79;
  row.a[0]	 =  1.82545e+00; 
  row.a[1]	 = -1.13077e+00;
  row.a[2]	 =  2.49432e-01;
  row.a[3]	 = -1.89606e-02;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 2
  row.npar       =      4;  // I60 TPoints60BGPHist301P02gh1 vs log(TrackLength)
  row.min        =    2.89;
  row.max        =    4.70;
  row.a[0]	 =  1.58791e+00;//1.73204e+00;
  row.a[1]	 = -1.08037e+00;
  row.a[2]	 =  2.05108e-01;
  row.a[3]	 = -1.08914e-02;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 3
  row.npar       =      4;  // sigma.I60 TPoints60BGPHist301P02gh1 vs log(TrackLength)
  row.min        =   2.30;
  row.max        =   4.60;
  row.a[0]	 =  8.46697e-01; 
  row.a[1]	 = -3.51407e-01;
  row.a[2]	 =  4.45164e-02;
  row.a[3]	 = -1.14994e-03;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 4
  row.npar       =      4;  // I TPointsBGPHist305P02gh1 vs log(TrackLength)
  row.min        =   2.77;
  row.max        =   4.70;
  row.a[0]	 =  1.41067e+00;//2.45223e+00;
  row.a[1]	 = -8.29983e-01;
  row.a[2]	 =  1.43916e-01;
  row.a[3]	 = -6.03988e-03;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 5
  row.npar       =      4;  // sigma.I TPointsBGPHist305P02gh1 vs log(TrackLength)
  row.min        =    2.77;
  row.max        =    4.70;
  row.a[0]	 =  2.04266e+00; 
  row.a[1]	 = -1.30019e+00;
  row.a[2]	 =  2.91810e-01;
  row.a[3]	 = -2.23958e-02;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 6
  row.npar       =      4;  // I70 TPoints70BGPHist305P02gh1 vs log(TrackLength)
  row.min        =    2.77;
  row.max        =    4.70;
  row.min        =     2.30;
  row.max        =     4.79;
  row.a[0]	 =  3.46846e-01;//
  row.a[1]	 = -2.23675e-01;// 
  row.a[2]	 =  4.99563e-02;//  
  row.a[3]	 = -3.67056e-03;// 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 7
  row.npar       =      4;  // sigma.I70 TPoints70BGPHist305P02gh1 vs log(TrackLength)
  row.min        =    2.77;
  row.max        =    4.70;
  row.a[0]	 =  2.56975e+00; 
  row.a[1]	 = -1.71322e+00;
  row.a[2]	 =  4.00760e-01;
  row.a[3]	 = -3.20085e-02;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 8
  row.npar       =      4;  // I60 TPoints60BGPHist305P02gh1 vs log(TrackLength)
  row.min        =    2.77;
  row.max        =    4.70;
  row.a[0]	 =  -3.26441e-01;//
  row.a[1]	 =   3.10331e-01;
  row.a[2]	 =  -9.03033e-02;
  row.a[3]	 =   8.53615e-03;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize()); // 9
  row.npar       =      4;  // sigma.I60 TPoints60BGPHist305P02gh1 vs log(TrackLength)
  row.min        =    2.77;
  row.max        =   4.60;
  row.a[0]	 =  1.90608e+00; 
  row.a[1]	 = -1.19036e+00;
  row.a[2]	 =  2.64383e-01;
  row.a[3]	 = -2.02259e-02;
  tableSet->AddAt(&row); // 10
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
