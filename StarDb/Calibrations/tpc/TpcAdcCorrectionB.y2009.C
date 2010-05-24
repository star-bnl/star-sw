TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.TpcAdcCorrectionB/TpcAdcCorrectionB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("TpcAdcCorrectionB",2);
/* scale t0 by 0.5 for Inner Sector
/direct/star+institutions+bnl/fisyak/TpcRS/rcf9106.G
rcf9106_9_100evts_y2009_TpcRS_sdt20090428.141700_Bichsel_0_9999.ADC.root

Inner
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       3.93699e+01   1.96734e-01  -9.78057e-04   1.78278e-03
   2  z           -1.00272e+00   1.00806e-02   1.01286e-04   1.46125e-01
   3  zxAdcL       2.00175e-01   1.72588e-03  -1.72400e-05   8.84528e-01
   4  adcL         9.99713e-01   4.55352e-04   5.03775e-07   3.99051e+00
   5  adcL2       -2.02620e-03   6.02845e-05   6.02845e-05   2.87190e+01
   6  scale        0.00000e+00     fixed    
Outer
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       4.34217e+01   9.07344e-02  -4.24369e-04   3.18007e-03
   2  z            1.37764e-01   3.44970e-03   3.21932e-05   3.27774e-01
   3  zxAdcL      -3.46722e-02   6.70641e-04  -6.19384e-06   1.74321e+00
   4  adcL         1.14035e+00   2.84331e-04   3.26129e-07   4.45691e+00
   5  adcL2       -1.62123e-02   4.06252e-05   4.06252e-05   2.20855e+01
   6  scale        0.00000e+00     fixed    
y2009_j/rcf9108_9_100evts_y2009_TpcRS_sdt20090328.164000_Bichsel_0_100.ADC.root
Inner
Fit innerM      log(simulated ADC) versus log(recon. ADC) and Z
 PARAMETER NUMBER   6 NOT A VARIABLE. IGNORED.
 THERE ARE NO MINOS ERRORS TO CALCULATE.
 FCN=45562.4 FROM MINOS     STATUS=UNCHANGED       0 CALLS         544 TOTAL
                     EDM=1.46277e-10    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       2.31818e+01   2.58773e-01  -2.94009e-03   4.07451e-03
   2  z           -5.08756e-01   1.41837e-02   3.42917e-04   2.92779e-01
   3  zxAdcL       1.08248e-01   2.45521e-03  -5.88074e-05   1.74193e+00
   4  adcL         9.39033e-01   8.44034e-04   2.12894e-06   6.03981e+00
   5  adcL2       -1.83631e-04   1.10310e-04   1.10310e-04  -6.13657e+01
   6  scale        0.00000e+00     fixed    

rcf9108_9_100evts_y2009_TpcRS_sdt20090328.164000_Bichsel_0_100.ADC

Fit innerM      log(simulated ADC) versus log(recon. ADC) and Z
 PARAMETER NUMBER   6 NOT A VARIABLE. IGNORED.
 THERE ARE NO MINOS ERRORS TO CALCULATE.
 FCN=31668.8 FROM MINOS     STATUS=UNCHANGED       0 CALLS         523 TOTAL
                     EDM=1.11562e-10    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       4.57736e+01   1.74485e-01  -1.18349e-03   2.36743e-03
   2  z           -1.20524e+00   9.10840e-03   1.25502e-04   2.10166e-01
   3  zxAdcL       2.36184e-01   1.56456e-03  -2.12792e-05   1.27475e+00
   4  adcL         9.74290e-01   4.03667e-04   6.33257e-07   5.11396e+00
   5  adcL2        8.11363e-04   5.14676e-05   5.14676e-05   3.07384e+01
   6  scale        0.00000e+00     fixed    
Fit outerM      log(simulated ADC) versus log(recon. ADC) and Z
 PARAMETER NUMBER   6 NOT A VARIABLE. IGNORED.
 THERE ARE NO MINOS ERRORS TO CALCULATE.
 FCN=48121.8 FROM MINOS     STATUS=UNCHANGED       0 CALLS         605 TOTAL
                     EDM=1.78173e-09    STRATEGY= 1      ERROR MATRIX ACCURATE 
  EXT PARAMETER                                   STEP         FIRST   
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       4.11581e+01   8.25876e-02  -3.83150e-04   1.46809e-02
   2  z            1.09311e-01   3.21809e-03   1.03984e-05   1.51776e+00
   3  zxAdcL      -3.08635e-02   6.26042e-04  -2.08862e-06   8.65811e+00
   4  adcL         1.12762e+00   2.33658e-04   1.24055e-06   3.44791e+01
   5  adcL2       -1.46289e-02   3.14538e-05   3.14538e-05  -2.20578e+02
   6  scale        0.00000e+00     fixed    
*/
memset(&row,0,tableSet->GetRowSize());
    row.type	 =         11; // ;
    row.idx	 =          1; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          6; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]     = -4.77666e-01; //-4.54347e-01+1.98426e-03-1.88134e-03; //   scale 
    row.a[1]	 =  4.11581e+01; // 4.34217e+01; // 4.34217e+01; //   offset
    row.a[2]	 =  1.09311e-01; // 1.37764e-01; // 1.37764e-01; //   z     
    row.a[3]	 = -3.08635e-02; //-3.46722e-02; //-3.46722e-02; //   zxAdcL
    row.a[4]	 =  1.12762e+00; // 1.14035e+00; // 1.14035e+00; //   adcL  
    row.a[5]	 = -1.46289e-02; //-1.62123e-02; //-1.62123e-02; //   adcL2 
    tableSet->AddAt(&row);     // Outer
memset(&row,0,tableSet->GetRowSize());
    row.type	 =         11; // ;
    row.idx	 =          2; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          6; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]     = -5.93299e-01 ; //-6.42700e-01+2.59181e-01-2.12161e-01;  // scale  
    row.a[1]	 =  4.57736e+01; // 2.31818e+01; // 3.93699e+01; //   offset
    row.a[2]	 = -1.20524e+00; //-5.08756e-01; //-1.00272e+00; //   z     
    row.a[3]	 =  2.36184e-01; // 1.08248e-01; // 2.00175e-01; //   zxAdcL
    row.a[4]	 =  9.74290e-01; // 9.39033e-01; // 9.99713e-01; //   adcL  
    row.a[5]	 =  8.11363e-04; //-1.83631e-04; //-2.02620e-03; //   adcL2 
tableSet->AddAt(&row); // Inner
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
