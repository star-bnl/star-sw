TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrection",10);
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =         -10;//Track lenths instead of log TPoints70BGPHist544P03ih.root
  row.min        =          10.;//                         
  row.max        =         160.;//                         
  row.a[0]	 = -2.43528e-01;//-5.41837e-02;//-0.021271   ;//-8.07375e-02 +TPoints70BGPHist542P03ihC.root
  row.a[1]	 =  5.71074e-02;// 1.56377e-02;// 0.012437   ;// 1.42505e-02 +TPoints70BGPHist543P03ih.root
  row.a[2]	 = -5.02546e-03;//-1.56768e-03;//-0.0014675  ;//-1.48587e-03
  row.a[3]	 =  2.23781e-04;// 7.63469e-05;// 7.4978e-05 ;// 7.50338e-05
  row.a[4]	 = -5.69802e-06;//-2.06586e-06;//-2.0574e-06 ;//-2.05744e-06
  row.a[5]	 =  8.75945e-08;// 3.29293e-08;// 3.2911e-08 ;// 3.29107e-08
  row.a[6]	 = -8.25821e-10;//-3.15870e-10;//-3.1587e-10 ;//-3.15872e-10
  row.a[7]	 =  4.66671e-12;// 1.79200e-12;// 1.792e-12  ;// 1.79200e-12
  row.a[8]	 = -1.45039e-14;//-5.54170e-15;//-5.5417e-15 ;//-5.54175e-15
  row.a[9]	 =  1.90588e-17;// 7.20500e-18;// 7.205e-18  ;// 7.20504e-18
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
  row.npar       =         -10;//Track lenths instead of log TPointsBGPHist544P03ih
  row.min        =          10.;// 
  row.max        =         160.;
  row.a[0]	 =  -2.90305e-01;//-5.43006e-02;//-0.0055194 ; 
  row.a[1]	 =   6.49327e-02;// 1.03502e-02;// 0.0087854 ;
  row.a[2]	 =  -5.85604e-03;//-1.42927e-03;//-0.0014122 ;
  row.a[3]	 =   2.69322e-04;// 8.27526e-05;// 8.2693e-05;
  row.a[4]	 =  -7.05502e-06;//-2.44162e-06;//-2.4416e-06;
  row.a[5]	 =   1.11100e-07;// 4.08480e-08;// 4.0848e-08;
  row.a[6]	 =  -1.06968e-09;//-4.04264e-10;//-4.0426e-10;
  row.a[7]	 =   6.16000e-12;// 2.34591e-12;// 2.3459e-12;
  row.a[8]	 =  -1.94796e-14;//-7.38191e-15;//-7.3819e-15;
  row.a[9]	 =   2.60124e-17;// 9.72909e-18;// 9.7291e-18;
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
//   memset(&row,0,tableSet->GetRowSize()); 
//   tableSet->AddAt(&row);// 6 -> I70
//   memset(&row,0,tableSet->GetRowSize()); 
//   tableSet->AddAt(&row);// 7 -> sigma.I70
//   memset(&row,0,tableSet->GetRowSize()); 
//   tableSet->AddAt(&row);// 8 -> I60
//   memset(&row,0,tableSet->GetRowSize()); 
//   tableSet->AddAt(&row);// 9 -> sigma.I60
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
