TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.TpcAdcCorrectionB/TpcAdcCorrectionB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("TpcAdcCorrectionB",2);
/* TpcT::AdcCorrections()
06/25/10 y2010_AuAu_200GeV_1200_1evts_y2010_TpcRS_sdt20100107.110000_Bichsel_1_100.ADC.root
Fit innerM      log(simulated ADC) versus log(recon. ADC) and Z
 PARAMETER NUMBER   6 NOT A VARIABLE. IGNORED.
 THERE ARE NO MINOS ERRORS TO CALCULATE.
 FCN=61654.5 FROM MINOS     STATUS=UNCHANGED       0 CALLS         534 TOTAL
                     EDM=1.18595e-10    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       3.26075e+01   9.06499e-02  -5.69228e-04   2.94749e-03
   2  z           -7.86611e-01   4.09211e-03   5.36760e-05   2.47793e-01
   3  zxAdcL       1.87623e-01   8.29455e-04  -1.07617e-05   1.29648e+00
   4  adcL         1.00310e+00   4.29227e-04   6.95701e-07   2.89433e+00
   5  adcL2        1.81769e-03   6.26764e-05   6.26764e-05   1.49908e+01
   6  scale        0.00000e+00     fixed    
Fit outerM      log(simulated ADC) versus log(recon. ADC) and Z
 PARAMETER NUMBER   6 NOT A VARIABLE. IGNORED.
 THERE ARE NO MINOS ERRORS TO CALCULATE.
 FCN=56963.6 FROM MINOS     STATUS=UNCHANGED       0 CALLS         523 TOTAL
                     EDM=2.2191e-10    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       3.23052e+01   7.40442e-02  -2.93214e-04   2.08233e-03
   2  z           -3.22458e-02   2.51271e-03   2.09787e-05   2.07729e-01
   3  zxAdcL       3.43087e-03   5.10835e-04  -4.21790e-06   1.07543e+00
   4  adcL         1.17682e+00   2.48197e-04   2.46962e-07   2.76320e+00
   5  adcL2       -1.76772e-02   3.66381e-05   3.66381e-05   1.25786e+01
   6  scale        0.00000e+00     fixed    

03/30/11 rcf10100_9_4000evts_minb_y2011_TpcRS_sdt20100107.110000_Bichsel_1_4000.ADC.root
Fit innerM      log(simulated ADC) versus log(recon. ADC) and Z
 FCN=72238.3 FROM MINOS     STATUS=UNCHANGED       0 CALLS         430 TOTAL
                     EDM=5.19999e-11    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       2.14121e+01   7.47968e-02  -9.69529e-05   4.81011e-04
   2  z           -6.60222e-02   3.43853e-03   1.04058e-05   3.25081e-02
   3  zxAdcL       1.76646e-02   7.00270e-04  -2.13816e-06   1.84733e-01
   4  adcL         9.55550e-01   3.97451e-04   1.55918e-07   4.20725e-01
   5  adcL2        1.84007e-03   5.55682e-05   5.55682e-05   1.59212e+00
   6  scale        0.00000e+00     fixed    
Fit outerM      log(simulated ADC) versus log(recon. ADC) and Z
 PARAMETER NUMBER   6 NOT A VARIABLE. IGNORED.
 THERE ARE NO MINOS ERRORS TO CALCULATE.
 FCN=61723.2 FROM MINOS     STATUS=UNCHANGED       0 CALLS         572 TOTAL
                     EDM=4.53906e-09    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       4.88741e+01   7.31068e-02  -3.17714e-04   6.43616e-03
   2  z           -1.13410e-01   3.24467e-03   1.04725e-05   5.92414e-01
   3  zxAdcL       2.23657e-02   6.49311e-04  -2.14793e-06   3.29048e+00
   4  adcL         1.13113e+00   1.94596e-04   9.84680e-07   1.56212e+01
   5  adcL2       -1.12153e-02   2.58603e-05   2.58603e-05  -1.09697e+02
   6  scale        0.00000e+00     fixed    

*/
memset(&row,0,tableSet->GetRowSize());
//     row.type	 =         11; // ;
//     row.idx	 =          1; // ;
//     row.nrows	 =          2; // ;
//     row.npar	 =          6; // ;
//     row.min	 =          0; // ;
//     row.max	 =          0; // ;
//     row.a[0]     = -6.69938e-03-4.54347e-01+1.98426e-03-1.88134e-03; //   scale 
//     row.a[1]	 =  4.88741e+01; //   offset
//     row.a[2]	 = -1.13410e-01; //   z     
//     row.a[3]	 =  2.23657e-02; //   zxAdcL
//     row.a[4]	 =  1.13113e+00; //   adcL  
//     row.a[5]	 = -1.12153e-02; //   adcL2 
    tableSet->AddAt(&row);     // Outer
memset(&row,0,tableSet->GetRowSize());
//     row.type	 =         11; // ;
//     row.idx	 =          2; // ;
//     row.nrows	 =          2; // ;
//     row.npar	 =          6; // ;
//     row.min	 =          0; // ;
//     row.max	 =          0; // ;
//     row.a[0]     = -3.15605e-01-6.42700e-01+2.59181e-01-2.12161e-01;  // scale  
//     row.a[1]	 =  2.14121e+01; //   offset
//     row.a[2]	 = -6.60222e-02; //   z     
//     row.a[3]	 =  1.76646e-02; //   zxAdcL
//     row.a[4]	 =  9.55550e-01; //   adcL  
//     row.a[5]	 =  1.84007e-03; //   adcL2 
tableSet->AddAt(&row); // Inner
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
