TDataSet *CreateTable() { 
  if (!TClass::GetClass("St_tpcCorrection")) return 0;
/* 
 Table: GatingGridB : parameters from Timothy Camarda, 08/09/21
 Old Gating Grid for Run < 18: t0 = 320 ns, setting time = 2.5  us, tau = 2.5  / 4.6  = 543 ns
 New Gating Grid for Run   18: t0 = 240 ns, setting time = 1.43 us, tau = 1.43 / 4.6  = 311 ns      
 Old Gating Grid for Runs 19 - 21
 New Gating Grid for Run > 21: t0 = 240 ns, setting time = 2.0  us, tau = 2.0  / 4.6  = 435 ns.
 setting time = time of reaching transperency 99%
 tau = setting time/4.6 => exp(-4.6) = 1%
 description: Gating Grid transperancy = 0, for t < t0, and 1 - exp(-(t-t0)/tau), for t > t0
// Fit from 19GeV_2019 data 04/08/2022 G3GFdEdx.root
  TF1 *GG = new TF1("GG","TMath::Log(1 - TMath::Exp(-(x-[0])*4.6/[1]))",0,10)
  GG->SetParameters(0,2.5)
  FitP->Draw("mu:y>>O","i&&j&&abs(x)>40.5","prof")
  O->Fit(GG,"er","",0.3,6)
 FCN=1174.76 FROM MINOS     STATUS=SUCCESSFUL     12 CALLS         109 TOTAL
                     EDM=1.21976e-08    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           1.84098e-01   2.77863e-03  -1.12239e-05  -3.12034e-01
   2  p1           1.75245e+00   7.48903e-03   7.48903e-03  -3.75507e-04
   1  p0           2.85771e-01   2.81202e-03  -1.15678e-05  -3.50969e-01
   2  p1           1.68566e+00   7.58048e-03   7.58048e-03  -4.47722e-04

  FitP->Draw("mu:y>>I","i&&j&&abs(x)<40.5","prof")
root.exe [72] I->Fit(GG,"er","",0.3,6)
 FCN=3798.34 FROM MINOS     STATUS=SUCCESSFUL     25 CALLS         119 TOTAL
                     EDM=1.73946e-09    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           1.33585e-01   6.38740e-03  -3.01241e-05   2.44724e-03
   2  p1           1.29436e+00   1.79433e-02   1.79433e-02  -2.11496e-03
   1  p0           2.42155e-01   6.17075e-03  -3.49149e-05   2.97485e-03
   2  p1           1.23716e+00   1.72556e-02   1.72556e-02  -2.32131e-03
*/ 
  Int_t nrows = 1;
  St_tpcCorrection *tableSet = new St_tpcCorrection("GatingGridB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx           = 1;
  row.nrows         = nrows;
  row.npar          = 2;
  row.a[0]          = 0.24; // t0
  row.a[1]          = 1.43; // settingTime
  tableSet->AddAt(&row); // Outer
  return (TDataSet *)tableSet;
}
