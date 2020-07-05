TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 8;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcRowQ",nrows);
  /*  
      for (Int_t i = 1; i <=8; i++) {FitP->SetMarkerColor(i); FitP->Draw(Form("mu:log(y)>>h%i(70,-2,5)",i),Form("i&&j&&(i-1)%8+1>=%i",i),"profsame"); TH1 *h = (TH1 *) gDirectory->Get(Form("h%i",i)); h->Fit("pol1","er","",0.5,4);}
  */
  Double_t params[16] = {
    2.48566e-02,   -8.62930e-03,
    1.88232e-02,   -6.98123e-03,
    1.47538e-02,   -5.90909e-03,
    1.15480e-02,   -5.02785e-03,
    7.80429e-03,   -3.80961e-03,
    6.28733e-03,   -3.41157e-03,
    4.10930e-03,   -2.74943e-03,
    4.23668e-03,   -3.82790e-03
  };
  memset(&row,0,tableSet->GetRowSize()); // QcmCGF19GeVRun321.root
  row.nrows = nrows;
  row.type  =     5; // Log(x)
  row.npar  =     2; //  npar < 0, X = exp(x) paramterization; abs(npar) >= 100 cut on range [min.max
  row.min   =   0.5;
  row.max   =   4.0;
  for (Int_t i = 0; i < 8; i++) {
    row.idx = i + 1;
    row.a[0] = params[2*i];
    row.a[1] = params[2*i+1];
    //    cout << i << "\tp = " << params[2*i] << "\t" << params[2+i+1] << "\ta = " <<  row.a[0] << "\t" <<  row.a[1] << endl;
    tableSet->AddAt(&row);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
