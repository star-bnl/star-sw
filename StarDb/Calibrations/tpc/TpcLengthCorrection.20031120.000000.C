TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrection",7);
  memset(&row,0,tableSet->GetRowSize()); // versus Log(TrackLength) !
  row.npar       =         -10;//Track lenths instead of log
  row.min        =          10.;// TPoints70BGPHist720P04idA
  row.max        =         135.;//FCN=724.888
  row.a[0]	 = -7.32844e-02;//
  row.a[1]	 =  8.42658e-03;//
  row.a[2]	 = -1.39246e-03;//
  row.a[3]	 =  9.17962e-05;//
  row.a[4]	 = -3.09899e-06;//
  row.a[5]	 =  6.01328e-08;//
  row.a[6]	 = -6.98954e-10;//
  row.a[7]	 =  4.81430e-12;//
  row.a[8]	 = -1.81413e-14;//
  row.a[9]	 =  2.88416e-17;//
  tableSet->AddAt(&row,0);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =            6;//TPoints70BGPHist720P04idA
  row.min        =          2.3;
  row.max        =          5.0;
  row.a[0]	 =  1.45305e+00; 
  row.a[1]	 = -1.08117e+00;
  row.a[2]	 =  2.41527e-01;
  row.a[3]	 =  2.35459e-02;
  row.a[4]	 = -1.52450e-02;
  row.a[5]	 =  1.46365e-03;
  tableSet->AddAt(&row,1);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row,2);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); 
  tableSet->AddAt(&row,3);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =         -10;//Track lenths instead of log
  row.min        =          10.;// TPointsBGPHist720P04idA
  row.max        =         135.;//FCN=724.888
  row.a[0]	 = -0.315464   ;//+TPointsBGPHist721P04idA  -3.07085e-01;//
  row.a[1]	 =  0.0596776  ;//			     5.91617e-02;//
  row.a[2]	 = -0.00600138 ;//			    -5.99748e-03;//
  row.a[3]	 =  0.00030971 ;//			     3.09710e-04;//
  row.a[4]	 = -9.11011e-06;//			    -9.11011e-06;//
  row.a[5]	 =  1.62172e-07;//			     1.62172e-07;//
  row.a[6]	 = -1.78017e-09;//			    -1.78017e-09;//
  row.a[7]	 =  1.17849e-11;//			     1.17849e-11;//
  row.a[8]	 = -4.31545e-14;//			    -4.31545e-14;//
  row.a[9]	 =  6.71447e-17;//			     6.71447e-17;//
  tableSet->AddAt(&row,4);// 4 -> I      
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =            6;//TPointsBGPHist720P04idA
  row.min        =          2.3;
  row.max        =          5.0;
  row.a[0]	 =  1.36517e+01; 
  row.a[1]	 = -1.86623e+01;
  row.a[2]	 =  1.02143e+01;
  row.a[3]	 = -2.76073e+00;
  row.a[4]	 =  3.67777e-01;
  row.a[5]	 = -1.93335e-02;
  tableSet->AddAt(&row,5);// 5 -> sigma.I
  memset(&row,0,tableSet->GetRowSize()); 
  row.npar       =            3;//TPoints70BGPHist721P04idA
  row.min        =          2.3;
  row.max        =          5.0;
  row.a[0]	 = -6.45232e-02; 
  row.a[1]	 =  3.30027e-02;
  row.a[2]	 = -3.70043e-03;
  tableSet->AddAt(&row,6);// 6 extra I70
   // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
