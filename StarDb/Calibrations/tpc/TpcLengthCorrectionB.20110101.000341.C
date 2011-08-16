TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows = nrows; //
  row.idx = 1; //TPoints70BUGRunXI39pp500dev_calib_pass2
  row.npar = 10; // fit mu
  row.a[0] = -7.66423e+00;//
  row.a[1] =  9.06694e+00;//
  row.a[2] = -3.24370e+00;//
  row.a[3] =  4.94743e-02;//
  row.a[4] =  1.44756e-01;//
  row.a[5] =  5.33637e-03;//
  row.a[6] = -7.06580e-03;//
  row.a[7] = -7.90565e-04;//
  row.a[8] =  4.43506e-04;//
  row.a[9] = -3.63369e-05 ;//
  tableSet->AddAt(&row); // 0->I70
  memset(&row,0,tableSet->GetRowSize());//
  row.nrows = nrows; //
  row.idx = 2;//TPoints70BGPRunXI40pp500dev_calib.root
  row.npar = 6;//fit sigma
  row.a[0] =  4.38623e+00;//
  row.a[1] = -5.79484e+00;//
  row.a[2] =  3.15310e+00;//
  row.a[3] = -8.50530e-01;//
  row.a[4] =  1.12737e-01;//
  row.a[5] = -5.86304e-03;//
  tableSet->AddAt(&row); // 1-> sigme.I70
  memset(&row,0,tableSet->GetRowSize()); //0
  tableSet->AddAt(&row);// 2->I60
  tableSet->AddAt(&row);// 3->sigma.I60
  row.nrows = nrows;
  row.idx = 5;// TPointBUGRunXI39pp500dev_calib_pass2
  row.npar = 10;//fit mu
  row.a[0] = -3.38763e+00;//
  row.a[1] =  4.54054e+00;//
  row.a[2] = -1.84870e+00;//
  row.a[3] =  7.17526e-02;//
  row.a[4] =  8.66814e-02;//
  row.a[5] =  1.18637e-03;//
  row.a[6] = -4.54067e-03;//
  row.a[7] = -4.21048e-04;//
  row.a[8] =  2.87354e-04;//
  row.a[9] = -2.47532e-05;//
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());//0
  row.nrows = nrows;
  row.idx = 6;// TPointsBGPRunXI40pp500dev_calib.root
  row.npar = 6;//fit sigma
  row.a[0] =  4.22080e+00;//
  row.a[1] = -5.66788e+00;//
  row.a[2] =  3.12768e+00;//
  row.a[3] = -8.53329e-01;//
  row.a[4] =  1.14174e-01;//
  row.a[5] = -5.98568e-03;//
  tableSet->AddAt(&row); // 5->Sigma.I
  memset(&row,0,tableSet->GetRowSize());//0
  tableSet->AddAt(&row);//5
  memset(&row,0,tableSet->GetRowSize());//0
  tableSet->AddAt(&row);//6
  memset(&row,0,tableSet->GetRowSize());//0
  tableSet->AddAt(&row);//7
  memset(&row,0,tableSet->GetRowSize());//0
  tableSet->AddAt(&row);//8
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}













