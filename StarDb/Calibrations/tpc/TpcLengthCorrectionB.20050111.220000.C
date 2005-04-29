TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 6;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     1; //TPoints70BUGPHist004P05id 
  row.npar       =            5;
  row.a[0]	 =  -1.14375e+00;//
  row.a[1]	 =   1.40947e+00;//
  row.a[2]	 =  -6.41353e-01;//
  row.a[3]	 =   1.27514e-01;//
  row.a[4]	 =  -9.29163e-03;//

  tableSet->AddAt(&row);// 0 -> I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     2;
  row.npar       =            5;// TPoints70BUGPHist004P05id
  row.min        =          2.3;//
  row.max        =          4.8;//                         
  row.a[0]	 =  1.27303e+00; 
  row.a[1]	 = -1.25341e+00;
  row.a[2]	 =  5.32478e-01;
  row.a[3]	 = -1.02658e-01;
  row.a[4]	 =  7.33247e-03;
  tableSet->AddAt(&row);// 1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 2 -> I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     5;      
  row.npar       =           7;// TPointsBUGPHist004P05id
  row.a[0]	 = -1.37178e+01;//
  row.a[1]	 =  2.26455e+01;//
  row.a[2]	 = -1.53725e+01;//
  row.a[3]	 =  5.49948e+00;//
  row.a[4]	 = -1.09610e+00;//
  row.a[5]	 =  1.15780e-01;//
  row.a[6]	 = -5.08105e-03;//

  tableSet->AddAt(&row);// 4 -> I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows      = nrows;
  row.idx        =     6;
  row.npar       =            6;// TPointsBUGPHist004P05id
  row.a[0]	 =  5.04829e+00; 
  row.a[1]	 = -6.73503e+00;
  row.a[2]	 =  3.64917e+00;
  row.a[3]	 = -9.72256e-01;
  row.a[4]	 =  1.26652e-01;
  row.a[5]	 = -6.45238e-03;
  tableSet->AddAt(&row);// 5 -> sigma.I
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}

