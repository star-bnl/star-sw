TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrection",10);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =         -10;//Track lenths instead of log TPoints70BGPHist538P03if.root
  row.min        =          10.;
  row.max        =         160.;
  row.a[0]	 = -8.07375e-02; 
  row.a[1]	 =  1.42505e-02;
  row.a[2]	 = -1.48587e-03;
  row.a[3]	 =  7.50338e-05;
  row.a[4]	 = -2.05744e-06;
  row.a[5]	 =  3.29107e-08;
  row.a[6]	 = -3.15872e-10;
  row.a[7]	 =  1.79200e-12;
  row.a[8]	 = -5.54175e-15;
  row.a[9]	 =  7.20504e-18;
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =            6;//TPointsBGPHist538P03if.root
  row.min        =          2.5;
  row.max        =          5.0;
  row.a[0]	 =  4.57033e+00; 
  row.a[1]	 = -5.57244e+00;
  row.a[2]	 =  2.77197e+00;
  row.a[3]	 = -6.78880e-01;
  row.a[4]	 =  8.11349e-02;
  row.a[5]	 = -3.77213e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =         -10;//Track lenths instead of log TPointsBGPHist538P03if.root
  row.min        =          10.;
  row.max        =         160.;
  row.a[0]	 = -5.43006e-02; 
  row.a[1]	 =  1.03502e-02;
  row.a[2]	 = -1.42927e-03;
  row.a[3]	 =  8.27526e-05;
  row.a[4]	 = -2.44162e-06;
  row.a[5]	 =  4.08480e-08;
  row.a[6]	 = -4.04264e-10;
  row.a[7]	 =  2.34591e-12;
  row.a[8]	 = -7.38191e-15;
  row.a[9]	 =  9.72909e-18;
  tableSet->AddAt(&row);// 4 -> I      
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =            6;//TPointsBGPHist538P03if.root
  row.min        =          2.5;
  row.max        =          5.0;
  row.a[0]	 =  3.62984e-01; 
  row.a[1]	 =  3.60376e-01;
  row.a[2]	 = -5.21008e-01;
  row.a[3]	 =  2.20538e-01;
  row.a[4]	 = -3.98176e-02;
  row.a[5]	 =  2.63937e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 6 -> I70
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 7 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 8 -> I60
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 9 -> sigma.I60
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
