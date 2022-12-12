TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcdEdxModel",nrows);
  //    root.exe */lnePI+lnePOGPAdcSparseD4.root 'Chain.C("FitP")'
  /*    z = log(log(ne/nP)) versus log(nP), z is supposed to be Gaussian with mu and sigma depnedending on log(nP) 
    tChain->Draw("mu:x","i&&j&&dmu<0.01&&dsigma<0.01&&abs(mu)<0.4","profs")
    htemp->Fit("pol4","er","",3.5,6.5)
 FCN=3.68934 FROM MINOS     STATUS=FAILURE       513 CALLS        2850 TOTAL
                     EDM=2.36487e-09    STRATEGY= 1      ERR MATRIX NOT POS-DEF
  EXT PARAMETER                APPROXIMATE        STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           7.15093e+00   1.47305e-02  -1.37849e-02  -1.69366e-06
   2  p1          -4.63202e+00   4.20909e-03   1.15848e-02  -5.79226e-06
   3  p2           1.00308e+00   8.98502e-04  -3.61084e-03  -7.78457e-05
   4  p3          -8.12504e-02   1.72789e-04   4.94865e-04  -1.34987e-04
   5  p4           1.46714e-03   2.51676e-05   2.51676e-05  -2.12303e-01
  */
  memset(&row,0,tableSet->GetRowSize()); 
  row.idx   = 1;
  row.type  = 0;
  row.nrows = nrows;
  row.min =     3.5;
  row.max =     6.5;
  row.npar =            5;// 
  row.a[0] =  7.15093e+00;
  row.a[1] = -4.63202e+00;
  row.a[2] =  1.00308e+00;
  row.a[3] = -8.12504e-02;
  row.a[4] =  1.46714e-03;
  tableSet->AddAt(&row); // mu versus log(nP)
  /*
    tChain->Draw("sigma:x","i&&j&&dmu<0.01&&dsigma<0.01&&abs(mu)<0.4","profs")
    htemp->Fit("pol2","er","",3.5,6.5)
 FCN=13.4861 FROM MINOS     STATUS=SUCCESSFUL     20 CALLS         151 TOTAL
                     EDM=2.18473e-13    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  p0           5.56664e-01   8.41256e-02  -3.36799e-06   1.97956e-09
   2  p1           3.17166e-02   3.56603e-02   7.51074e-07  -1.14348e-07
   3  p2          -1.57127e-02   3.73801e-03   3.73801e-03  -1.32512e-07
   */
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.type  = 0;
  row.nrows = nrows;
  row.min =     3.5;
  row.max =     6.5;
  row.npar =           3;// 
  row.a[0] =  5.56664e-01;
  row.a[1] =  3.17166e-02;
  row.a[2] = -1.57127e-02;
  tableSet->AddAt(&row); // sigma versus log(nP)
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
