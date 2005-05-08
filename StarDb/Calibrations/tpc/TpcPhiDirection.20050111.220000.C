TDataSet *CreateTable() { 
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  tpcCorrection_st row;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcPhiDirection",nrows);
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          1; // Phi3DGFHist009P05id
  row.nrows	 =          2; // FitP->Draw("mu:abs(tan(TMath::Pi()/180*x))>>OAT","i&&j&&abs(mu)<0.4&&j>13","prof")
  row.npar	 =          9; // 
  row.min        =          0.0;
  row.max        =          1.0;
  row.a[0]       = -6.04388e-03;   
  row.a[1]   	 = -2.59457e-01;
  row.a[2]   	 =  6.47011e+00;
  row.a[3]   	 = -4.77299e+01;
  row.a[4]   	 =  1.70513e+02;
  row.a[5]   	 = -3.33481e+02;
  row.a[6]   	 =  3.64840e+02;
  row.a[7]   	 = -2.09358e+02;
  row.a[8]	 =  4.90394e+01;
  tableSet->AddAt(&row);// Outer
  memset(&row,0,tableSet->GetRowSize());
  row.idx	 =          2; // Phi3DGFHist009P05id
  row.nrows	 =          2; // FitP->Draw("mu:abs(tan(TMath::Pi()/180*x))>>IAT","i&&j&&abs(mu)<0.4&&j<13","prof")
  row.npar	 =          6; // 
  row.min        =          0.0;
  row.max        =          1.6;
  row.a[0]       = -1.30925e-02;   
  row.a[1]   	 =  1.57536e-01;
  row.a[2]   	 = -5.37322e-01;
  row.a[3]   	 =  1.15218e+00;
  row.a[4]   	 = -9.82337e-01;
  row.a[5]   	 =  2.76947e-01;
  tableSet->AddAt(&row);// Inner
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
