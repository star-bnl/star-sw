TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrection",12);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =         -10;//Track lenths instead of log TPoints70BGPHist544P03ih.root + TPoints70BGPHist804P04ie
  row.min        =          10.;//  default clustering == fcf                     
  row.max        =         160.;//                         
  row.a[0]	 = -0.304624   ;//
  row.a[1]	 =  0.0591263  ;//
  row.a[2]	 = -0.00503513 ;//
  row.a[3]	 =  0.000223749;//
  row.a[4]	 = -5.69802e-06;//
  row.a[5]	 =  8.75945e-08;//
  row.a[6]	 = -8.25821e-10;//
  row.a[7]	 =  4.66671e-12;//
  row.a[8]	 = -1.45039e-14;//
  row.a[9]	 =  1.90588e-17;//
  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =            4;//TPoints70BGPHist805P04ie
  row.a[0]	 =  3.70811e-02; 
  row.a[1]	 =  1.42838e-01;
  row.a[2]	 = -5.13699e-02;
  row.a[3]	 =  4.77384e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =         -10;//Track lenths instead of log TPointsBGPHist544P03ih
  row.min        =          10.;// 
  row.max        =         160.;
  row.a[0]	 =  -0.103014   ;//-5.43006e-02;//-0.0055194 ; 
  row.a[1]	 =   0.032717   ;// 1.03502e-02;// 0.0087854 ;
  row.a[2]	 =  -0.00401545 ;//-1.42927e-03;//-0.0014122 ;
  row.a[3]	 =   0.000217776;// 8.27526e-05;// 8.2693e-05;
  row.a[4]	 =  -6.24691e-06;//-2.44162e-06;//-2.4416e-06;
  row.a[5]	 =   1.03897e-07;// 4.08480e-08;// 4.0848e-08;
  row.a[6]	 =  -1.0357e-09 ;//-4.04264e-10;//-4.0426e-10;
  row.a[7]	 =   6.09432e-12;// 2.34591e-12;// 2.3459e-12;
  row.a[8]	 =  -1.94796e-14;//-7.38191e-15;//-7.3819e-15;
  row.a[9]	 =   2.60124e-17;// 9.72909e-18;// 9.7291e-18;
  tableSet->AddAt(&row);// 4 -> I      
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =            4;//TPointsBGPHist538P03if.root
  row.min        =          2.5;
  row.max        =          5.0;
  row.a[0]	 =  1.35812e-01; 
  row.a[1]	 =  7.06105e-02;
  row.a[2]	 = -3.36689e-02;
  row.a[3]	 =  3.33710e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 6 -> I70
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 7 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 8 -> I60
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 9 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =          -5;//Track lenths instead of log TPoints70BGPHist805P04ie_tcl
  row.min        =          10.;// 
  row.max        =         160.;
  row.a[0]	 =   3.58014e-03;//-5.43006e-02;//-0.0055194 ; 
  row.a[1]	 =  -5.61444e-04;// 1.03502e-02;// 0.0087854 ;
  row.a[2]	 =  -7.27265e-07;//-1.42927e-03;//-0.0014122 ;
  row.a[3]	 =   2.33655e-08;// 8.27526e-05;// 8.2693e-05;
  row.a[4]	 =   1.67261e-10;// 8.27526e-05;// 8.2693e-05;
  tableSet->AddAt(&row);// 10 -> I70 extra correction for old clustering
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row);// 11
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =         -10;//Track lenths instead of log TPointsBGPHist805P04ie_tcl
  row.min        =          10.;// 
  row.max        =         160.;
  row.a[0]	 =  -7.86731e-02;//-5.43006e-02;//-0.0055194 ; 
  row.a[1]	 =   6.11894e-03;// 1.03502e-02;// 0.0087854 ;
  row.a[2]	 =   7.94639e-05;//-1.42927e-03;//-0.0014122 ;
  row.a[3]	 =  -2.24265e-05;// 8.27526e-05;// 8.2693e-05;
  row.a[4]	 =   9.01354e-07;//-2.44162e-06;//-2.4416e-06;
  row.a[5]	 =  -1.76072e-08;// 4.08480e-08;// 4.0848e-08;
  row.a[6]	 =   1.93313e-10;//-4.04264e-10;//-4.0426e-10;
  row.a[7]	 =  -1.20798e-12;// 2.34591e-12;// 2.3459e-12;
  row.a[8]	 =   3.98081e-15;//-7.38191e-15;//-7.3819e-15;
  row.a[9]	 =  -5.31134e-18;// 9.72909e-18;// 9.7291e-18;
  tableSet->AddAt(&row);// 12 -> I extra correction for old clustering
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
