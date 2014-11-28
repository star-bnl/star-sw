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
*/
memset(&row,0,tableSet->GetRowSize());
    row.type	 =         11; // ;
    row.idx	 =          1; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          6; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]     = -4.54347e-01+1.98426e-03-1.88134e-03; //   scale 
    row.a[1]	 =  4.34217e+01; //   offset
    row.a[2]	 =  1.37764e-01; //   z     
    row.a[3]	 = -3.46722e-02; //   zxAdcL
    row.a[4]	 =  1.14035e+00; //   adcL  
    row.a[5]	 = -1.62123e-02; //   adcL2 
    tableSet->AddAt(&row);     // Outer
memset(&row,0,tableSet->GetRowSize());
    row.type	 =         11; // ;
    row.idx	 =          2; // ;
    row.nrows	 =          2; // ;
    row.npar	 =          6; // ;
    row.min	 =          0; // ;
    row.max	 =          0; // ;
    row.a[0]     = -6.42700e-01+2.59181e-01-2.12161e-01;  // scale  
    row.a[1]	 =  3.93699e+01; //   offset
    row.a[2]	 = -1.00272e+00; //   z     
    row.a[3]	 =  2.00175e-01; //   zxAdcL
    row.a[4]	 =  9.99713e-01; //   adcL  
    row.a[5]	 = -2.02620e-03; //   adcL2 
tableSet->AddAt(&row); // Inner
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
