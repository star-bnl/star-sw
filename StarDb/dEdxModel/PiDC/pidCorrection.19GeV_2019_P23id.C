TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_pidCorrection")) return 0;
  struct pid_t { // replace a[10] => a0, a1, ..., a9 for cint
  Int_t    idx;      // row index > 0 if it is real
  Int_t    nrows;    // total no. of real rows in the table; For Db interface (where nrows = 50)
  Int_t    type;     // type = 0 polymonical fit,                                        use only [min,max]
  Int_t    var;      // fit variable:  0 => pmomL10, 1 => bgL10,
  Int_t    particle; // from StEvent/StPidParticleDefinition.h : kUndef = -1, kPidElectron = 0, Proton = 1, Kaon = 2, Pion = 3, Muon = 4, Deuteron = 5, Triton = 6, 
                    // He3 = 7, Alpha = 8, He6 = 9, Li5 = 10, Li6,= 11, Li7 = 12, Be7 = 13, Be9 = 14, Be10 = 15, B11 = 16
  Int_t    charge;   // +/-1, 0 does not matter
  Int_t    pull;     // != 0 calculated pull correction, == 0 to value
  Int_t    det;      // from StdEdxY2Maker/StTrackPiD.h : kUndef = 0, kI70 = 1, kI70U = 2, kFit = 3, kFitU = 4, kdNdx = 5, kdNdxU = 6, kBTof -7 , kETof = 8, kMtd = 9, kBEmc = 10
  Int_t    npar;     // npar < 0, X = exp(x) paramterization; abs(npar) >= 100 cut on range [min.max]
  Double_t OffSet;   // for future use
  Double_t min;      // fit range
  Double_t max;      // 
  Double_t a0;       // a[npar]
  Double_t a1;    // a[npar]
  Double_t a2;    // a[npar]
  Double_t a3;    // a[npar]
  Double_t a4;    // a[npar]
  Double_t a5;    // a[npar]
  Double_t a6;    // a[npar]
  Double_t a7;    // a[npar]
  Double_t a8;    // a[npar]
  Double_t a9;    // a[npar]
  Char_t   comment[32];  
  };
  pid_t rows[] = {
    //idx,nrows,type,var,part,ch,pull,det,npar,off, min,  max, a[10]                                                          ,comment
    {   0,    0,   0,  0,   3,  0,  0,  3,   4,  0,-0.40, 0.70,   0.0054041, -0.00632595,  -0.0149973, -0.00102721,           0,0,0,0,0,0,"PionDEV_dEdx pol3"},
    {   0,    0,   0,  0,   3,  0,  0,  3,   5,  0,-1.10,-0.40,   -0.365241,    -2.71187,    -6.99333,      -7.441,    -2.76148,0,0,0,0,0,"PionDEV_dEdx pol4"},
    {   0,    0,   0,  0,   1,  0,  0,  3,   4,  0,-0.20, 0.81,   0.0147197,  -0.0258435,   0.0812796,   -0.140962,         0,0,0,0,0,0,"ProtonDEV_dEdx pol3"},
    {   0,    0,   0,  0,   1,  0,  0,  3,   3,  0,-0.65,-0.20, 0.000859774,   -0.110058,   0.0587036,                    0,0,0,0,0,0,0,"ProtonDEV_dEdx pol2"},
    {   0,    0,   0,  0,   1,  0,  0,  3,   3,  0,-1.00,-0.65,    -1.74183,    -5.57528,    -4.24693,                    0,0,0,0,0,0,0,"ProtonDEV_dEdx pol2"},
    {   0,    0,   0,  0,   0,  0,  0,  3,   4,  0,-0.75, 0.45,    0.121418,   0.0562044,  -0.0726388,   -0.031316,              0,0,0,0,0,0,"EDEV_dEdx pol3"},
    {   0,    0,   0,  0,   0,  0,  0,  3,   4,  0,-0.85,-0.75,   -0.150987,   -0.309554,    0.196893,    0.330907,              0,0,0,0,0,0,"EDEV_dEdx pol3"},
    {   0,    0,   0,  0,   0,  0,  0,  3,   4,  0,-1.20,-0.85,     -3.9045,    -12.4966,    -12.8273,    -4.23391,              0,0,0,0,0,0,"EDEV_dEdx pol3"}
  };
  Int_t nrows = sizeof(rows)/sizeof(pidCorrection_st);
  St_pidCorrection *tableSet = new St_pidCorrection("pidCorrection",nrows);
  for (Int_t i = 0; i < nrows; i++) {
    rows[i].idx = i+1;
    rows[i].nrows = nrows;
    tableSet->AddAt(&rows[i].idx);
  }
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
