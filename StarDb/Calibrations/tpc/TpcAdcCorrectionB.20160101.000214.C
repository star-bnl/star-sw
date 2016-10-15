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
07/21/2016 TpcRS_2016/gstar_heed_PhysicsOff_triton:9_1_1000.ADC.root
Fit innerM      log(simulated ADC) versus log(recon. ADC) and Z
 FCN=251680 FROM MINOS     STATUS=SUCCESSFUL     36 CALLS         416 TOTAL
                     EDM=7.56898e-07    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       4.09351e+01   2.54997e-02  -2.02435e-05   3.23641e-04
   2  z           -5.64448e-01   1.16821e-03   1.45809e-06   2.39971e-02
   3  zxAdcL       1.32589e-01   2.41260e-04  -2.86862e-07   1.23928e-01
   4  adcL         1.01700e+00   1.21485e-04   1.81514e-08   2.46091e-01
   5  adcL2        2.68230e-04   1.71741e-05   1.71741e-05   1.67116e+00
   6  scale        0.00000e+00     fixed    
Fit outerM      log(simulated ADC) versus log(recon. ADC) and Z
 FCN=116126 FROM MINOS     STATUS=SUCCESSFUL     36 CALLS         467 TOTAL
                     EDM=2.69649e-08    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       4.57099e+01   2.78267e-02  -6.48778e-05   3.78447e-04
   2  z           -6.70727e-02   1.33427e-03   7.38985e-06   3.71379e-02
   3  zxAdcL       1.06947e-02   2.88040e-04  -1.57858e-06   1.82280e-01
   4  adcL         1.15708e+00   9.90488e-05   5.57630e-08   4.75958e-01
   5  adcL2       -1.47919e-02   1.37668e-05   1.37668e-05   2.66830e+00
   6  scale        0.00000e+00     fixed    
*/
memset(&row,0,tableSet->GetRowSize());
    row.type	 =         11; // ;
    row.idx	 =          1; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          6; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]     = -0.75 + 4.92838e-02+3.33943e-02;
    row.a[1]	 =  4.57099e+01; //   offset
    row.a[2]	 = -6.70727e-02; //   z     
    row.a[3]	 =  1.06947e-02; //   zxAdcL
    row.a[4]	 =  1.15708e+00; //   adcL  
    row.a[5]	 = -1.47919e-02; //   adcL2 
    tableSet->AddAt(&row);     // Outer
memset(&row,0,tableSet->GetRowSize());
    row.type	 =         11; // ;
    row.idx	 =          2; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          6; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]     = -0.75-3.62038e-02+1.16576e-01; 
    row.a[1]	 =  4.09351e+01; //   offset
    row.a[2]	 = -5.64448e-01; //   z     
    row.a[3]	 =  1.32589e-01; //   zxAdcL
    row.a[4]	 =  1.01700e+00; //   adcL  
    row.a[5]	 =  2.68230e-04; //   adcL2 
tableSet->AddAt(&row); // Inner
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
