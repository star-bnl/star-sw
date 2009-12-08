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
/direct/star+institutions+bnl/fisyak/TpcRS/rcf9109.A
rcf9109_9_100evts_y2010_TpcRS_Bichsel_0_9999.ADC.root

Inner
  NO.   NAME      VALUE            ERROR          SIZE      DERIVATIVE 
   1  offset       5.78410e+01   5.53924e-02  -7.18422e-05   2.73760e-04
   2  z           -7.58421e-02   2.88394e-03   7.63738e-06   2.36323e-02
   3  zxAdcL      -1.95898e-03   5.39870e-04  -1.42032e-06   1.28170e-01
   4  adcL         1.13820e+00   1.54692e-04   3.87485e-08   4.95131e-01
   5  adcL2       -1.12875e-02   2.24894e-05   2.24894e-05   3.23418e+00
   6  scale        0.00000e+00     fixed    
Outer
   1  offset       3.94166e+01   8.27652e-02  -2.87029e-04   2.48351e-03
   2  z           -5.94686e-02   3.17696e-03   2.23167e-05   2.58755e-01
   3  zxAdcL       7.49368e-03   6.32290e-04  -4.39620e-06   1.34300e+00
   4  adcL         1.14574e+00   2.64276e-04   2.21194e-07   3.40846e+00
   5  adcL2       -1.22077e-02   3.87236e-05   3.87236e-05   1.62830e+01
   6  scale        0.00000e+00     fixed    
*/
memset(&row,0,tableSet->GetRowSize());
    row.type	 =         11; // ;
    row.idx	 =          1; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          6; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]     = -4.54347e-01+1.98426e-03-1.88134e-03; //   scale 
    row.a[1]	 =  3.94166e+01; //   offset
    row.a[2]	 = -5.94686e-02; //   z     
    row.a[3]	 =  7.49368e-03; //   zxAdcL
    row.a[4]	 =  1.14574e+00; //   adcL  
    row.a[5]	 = -1.22077e-02; //   adcL2 
    tableSet->AddAt(&row);     // Outer
memset(&row,0,tableSet->GetRowSize());
    row.type	 =         11; // ;
    row.idx	 =          2; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          6; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]     = -6.42700e-01+2.59181e-01-2.12161e-01;  // scale  
    row.a[1]	 =  5.78410e+01; //   offset
    row.a[2]	 = -7.58421e-02; //   z     
    row.a[3]	 = -1.95898e-03; //   zxAdcL
    row.a[4]	 =  1.13820e+00; //   adcL  
    row.a[5]	 = -1.12875e-02; //   adcL2 
tableSet->AddAt(&row); // Inner
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
