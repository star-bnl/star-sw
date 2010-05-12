TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 9;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcLengthCorrectionB",nrows);
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows       =        nrows; // Correction/sigma versus LogTrackLength
  row.idx         =        1;     //TPoints70BUGPRunXP10if.root
  row.npar        =        8;     //npar > 0 means you fit with log(X), fit mu
  row.min         =        TMath::Log(10);
  row.max         =        TMath::Log(145);
  row.a[0]        =        -7.00278e+00;//
  row.a[1]        =        1.09206e+01;//
  row.a[2]        =        -6.16413e+00;//
  row.a[3]        =        1.30648e+00;//
  row.a[4]        =        8.51367e-02;//
  row.a[5]        =        -8.65480e-02;//
  row.a[6]        =        1.40874e-02;//
  row.a[7]        =        -7.63111e-04;//
  tableSet->AddAt(&row); //0 -> mu.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows       =        nrows; //
  row.idx         =        2;//TPoints70BUGPRunXP10if.root
  row.npar        =        6;//fit sigma
  row.min         =        TMath::Log(10);
  row.max         =        TMath::Log(145);
  row.a[0]        =        2.43040e+00;//
  row.a[1]        =        -3.02995e+00;//
  row.a[2]        =        1.61705e+00;//
  row.a[3]        =        -4.30099e-01;//
  row.a[4]        =        5.60329e-02;//
  row.a[5]        =        -2.84862e-03;//
  tableSet->AddAt(&row);//1 -> sigma.I70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);//2 -> I60
  tableSet->AddAt(&row);//3 -> sigma.I60
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows       =        nrows; //
  row.idx         =        5;//TPointsBUGPRunXP10if.root
  row.npar        =        8;//fit mu
  row.min         =        TMath::Log(10);
  row.max         =        TMath::Log(145);
  row.a[0]        =        -1.04455e+01;//
  row.a[1]        =        1.56378e+01;//
  row.a[2]        =        -8.61697e+00;//
  row.a[3]        =        1.82147e+00;//
  row.a[4]        =        9.89066e-02;//
  row.a[5]        =        -1.12688e-01;//
  row.a[6]        =        1.85310e-02;//
  row.a[7]        =        -1.01349e-03;//
  tableSet->AddAt(&row);//4 -> mu.I
  memset(&row,0,tableSet->GetRowSize()); // 0
  row.nrows       =        nrows;
  row.idx         =        6;// TPointsBUGPRunXP10if.root
  row.npar        =        6;//fit sigma
  row.min         =        TMath::Log(10);
  row.max         =        TMath::Log(145);
  row.a[0]        =        1.82467e+00;//
  row.a[1]        =        -2.35829e+00;//
  row.a[2]        =        1.32195e+00;//
  row.a[3]        =        -3.65985e-01;//
  row.a[4]        =        4.91224e-02;//
  row.a[5]        =        -2.55086e-03;//
  tableSet->AddAt(&row);//5 ->sigma.I
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 6 -> dI70
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 7 -> dI60
  memset(&row,0,tableSet->GetRowSize()); // 0
  tableSet->AddAt(&row);// 8 -> dIfit
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
