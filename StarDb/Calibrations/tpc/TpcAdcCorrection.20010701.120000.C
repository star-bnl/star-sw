TDataSet *CreateTable() { 
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("TpcAdcCorrection",6);
memset(&row,0,tableSet->GetRowSize()); 
//  row.npar        =   3;// Outer Adc3OGPHist280P02gh1
//  row.min         =   0.55;// parameterization for log(dE(keV)) 
//  row.max         =   2.00;
//  row.a[0]	 =   4.75942e-01;
//  row.a[1]	 =   5.51593e-01;
//  row.a[2]	 =   1.96954e-01;
//  row.npar        =   3;// Outer AdcONGPHist288P02gh1.root + AdcOPGPHist288P02gh1.root
//  row.min         =   0.0;// 
//  row.max         =  80.0; // keV
//  row.a[0]	 =   1.23055e-01;//   +/-1.68548e-01
//  row.a[1]	 =   1.72556e+00;//   +/-1.25165e-02
//  row.a[2]	 =   2.85252e-03;//   +/-1.79578e-04
 row.npar        =   2;// Outer AdcONGPHist288P02gh1.root + AdcOPGPHist288P02gh1.root
 row.min         =   0.0;// 
 row.max         =  80.0; // keV
 row.a[0]	 =   0;
 row.a[1]	 =   1.72343e+00;//   +/-   1.13489e-02
 row.a[2]	 =   3.07685e-03;//   +/-   1.85801e-04
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
//  row.npar        =     3;// Inner Adc3IGPHist280P02gh1
//  row.min         =   0.3;// parameterization for log(dE(keV)) 
//  row.max         =   1.7;
//  row.a[0]	 =   4.30582e-01;
//  row.a[1]	 =   4.07238e-01;
//  row.a[2]	 =   3.51865e-01;
//  row.npar        =     3;// AdcINGPHist288P02gh1.root + AdcIPGPHist288P02gh1.root
//  row.min         =   0.0;// 
//  row.max         =  25.0;
//  row.a[0]	 = 5.71439e-01;//   +/-2.52034e-01  
//  row.a[1]	 = 1.39089e+00;//   +/-4.62304e-02  
//  row.a[2]	 = 1.55177e-02;//   +/-1.85609e-03  
 row.npar        =     2;// AdcINGPHist288P02gh1.root + AdcIPGPHist288P02gh1.root
 row.min         =   0.0;// 
 row.max         =  80.0;
 row.a[0]	 = 0;
 row.a[1]	 = 1.46426e+00;//   +/-1.32477e-02
 row.a[2]	 = 1.32854e-02;//   +/-6.01436e-04
 tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize()); 
 row.npar        =   3;// Outer Adc3OGPHist280P02gh1
 row.min         =   0.55;// parameterization for log(dE(keV)) 
 row.max         =   2.00;
 row.a[0]	 =   4.75942e-01;
 row.a[1]	 =   5.51593e-01;
 row.a[2]	 =   1.96954e-01;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
 row.npar        =     3;// Inner Adc3IGPHist280P02gh1
 row.min         =   0.3;// parameterization for log(dE(keV)) 
 row.max         =   1.7;
 row.a[0]	 =   4.30582e-01;
 row.a[1]	 =   4.07238e-01;
 row.a[2]	 =   3.51865e-01;
 tableSet->AddAt(&row);
 // correction using positive tracks with built it drift time correction
 // x:x*pow(10.,mu+7.6e-7*y); x = predicted; y = DriftLength*ppOx
memset(&row,0,tableSet->GetRowSize()); 
 row.npar        =   5;// Outer AdcOZPGP_y5Hist290P02gh1
 row.min         =   0.;// dE(keV)
 row.max         =  90.;
 row.a[0]	 =   1.77812e+00;//   +/-    9.86514e-02
 row.a[1]	 =   1.42834e+00;//   +/-    2.15027e-02
 row.a[2]	 =   2.06509e-02;//   +/-    1.29957e-03
 row.a[3]	 =   3.55311e-04;//   +/-    2.81085e-05
 row.a[4]	 =   2.22723e-06;//   +/-    1.91815e-07
 tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize()); 
 row.npar        =   3;// Inner AdcIZPGP_y5Hist290P02gh1
 row.min         =   0.;// dE(keV)
 row.max         =  40.;
 row.a[0]	 =   8.99449e-01;//   +/-   5.78562e-02
 row.a[1]	 =   1.41439e+00;//   +/-   1.08873e-02
 row.a[2]	 =   1.12335e-02;//   +/-   4.30342e-04
 tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
