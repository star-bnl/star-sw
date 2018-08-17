TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// bfc/.make/db/.const/StarDb/Calibrations/tpc/.TpcAdcCorrectionB/TpcAdcCorrectionB Allocated rows: 50  Used rows: 50  Row size: 120 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[49]
// ====================================================================
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  tpcCorrection_st row;
 Int_t nrows = 3;
 St_tpcCorrection *tableSet = new St_tpcCorrection("TpcAdcCorrectionB",nrows);
/* TpcT::AdcCorrections()
   06/11/18 Altro3 
*/
 memset(&row,0,tableSet->GetRowSize());
 row.type	 =         10; //  Outer Tpx
 row.idx	 =          1; // 
 row.nrows	 =      nrows; // 
 row.npar	 =          6; // 
 row.min         =        3.0;
 row.max         =        9.5;
 /* AuAu200.VMC.DEV2.KFV.Altro3/ADC/O3DdXGPADC3.root
    FitP->Draw("mu:x>>O3(65,3,9.5,130,0.2,1.2)","prob>0.01&&mu>0","colz")
    root.exe [140] O3->Fit("pol5")
 */
 row.a[0]                        =      11.1277+6.30809e-01;// +/-   1.37685     
 row.a[1]                        =     -7.83009;// +/-   1.22121     
 row.a[2]                        =      2.35546;// +/-   0.421171    
 row.a[3]                        =    -0.353798;// +/-   0.0706724   
 row.a[4]                        =    0.0261444;// +/-   0.00578044  
 row.a[5]                        = -0.000758298;// +/-   0.000184746 
 tableSet->AddAt(&row);        // Outer
 memset(&row,0,tableSet->GetRowSize());
 row.type	 =         10; // Inner Tpx  
 row.idx	 =          2; //
 row.nrows	 =      nrows; //
 row.npar	 =          6; //
 row.min         =        3.0;
 row.max         =        9.0;
 /* AuAu200.VMC.DEV2.KFV.Altro3/ADC/I3DdXGPADC3.root
    FitP->Draw("mu:x>>I3(60,3.0,9.0,130,0.2,1.5)","prob>0.01&&mu>0","colz")
    root.exe [118] I3->Fit("pol4")
 */
 row.a[0]                        =      7.70354+5.34681e-01;// +/-   0.429642    
 row.a[1]                        =     -4.05726;// +/-   0.317079    
 row.a[2]                        =     0.876087;// +/-   0.0848809   
 row.a[3]                        =   -0.0854667;// +/-   0.00979864  
 row.a[4]                        =   0.00311716;// +/-   0.000412815 
 tableSet->AddAt(&row);        // Inner
 memset(&row,0,tableSet->GetRowSize());
 row.type	 =         10; // iTPC  
 row.idx	 =          3; //
 row.nrows	 =      nrows; //
 row.npar	 =          6; //
 row.min         =        4.0;
 row.max         =        8.0;
 /* AuAu200.VMC.DEV2.KFV.Altro4/ADC/X3DdXGPADC.root
    FitP->Draw("mu:x>>X3(40,4.,8,120,0.3,0.9)","prob>0.01&&mu>0","colz")
    root.exe [149] X3->Fit("pol3")
 */
 row.a[0]                        =      4.31205+1.36264-1.22473;// +/-   0.180446    
 row.a[1]                        =     -1.54686;// +/-   0.0905372   
 row.a[2]                        =     0.209601;// +/-   0.0149346   
 row.a[3]                        =  -0.00976921;// +/-   0.00081069  
 tableSet->AddAt(&row);        // iTPC
 // ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
