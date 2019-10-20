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
   06/11/18 Altro4 
*/
 memset(&row,0,tableSet->GetRowSize());
 row.type	 =         10; //  Outer Tpx
 row.idx	 =          1; // 
 row.nrows	 =      nrows; // 
 row.npar	 =          7; // 
 row.min         =        3.3;
 row.max         =        9.3;
/*	 AuAu200.VMC.DEV2.KFV.Altro4/ADC/O3DdXGPADC.root
	FitP->Draw("mu:x>>O(60,3.3,9.3,130,0.2,1.2)","prob>0.01&&mu>0","colz")
	O->Fit("pol6")
	root.exe [86] O->Fit("pol6")
*/
 row.a[0]                        =      38.1809+5.99786e-01;// +/-   5.13442     
 row.a[1]                        =     -36.0538;// +/-   5.38507     
 row.a[2]                        =      14.4553;// +/-   2.30612     
 row.a[3]                        =     -3.06396;// +/-   0.516396    
 row.a[4]                        =     0.359933;// +/-   0.0638172   
 row.a[5]                        =   -0.0221888;// +/-   0.00413059  
 row.a[6]                        =  0.000560818;// +/-   0.000109503 
 tableSet->AddAt(&row);        // Outer
 memset(&row,0,tableSet->GetRowSize());
 row.type	 =         10; // Inner Tpx  
 row.idx	 =          2; //
 row.nrows	 =      nrows; //
 row.npar	 =          4; //
 row.min         =        3.0;
 row.max         =        9.0;
/* 	AuAu200.VMC.DEV2.KFV.Altro4/ADC/I3DdXGPADC.root
	FitP->Draw("mu:x>>I(60,3.0,9.0,130,0.2,1.5)","prob>0.01&&mu>0","colz")
	root.exe [69] I->Fit("pol3")
*/
 row.a[0]                        =      5.36904+4.23298e-01;// +/-   0.0849841   
 row.a[1]                        =     -1.96723;// +/-   0.0456065   
 row.a[2]                        =     0.266779;// +/-   0.00792103  
 row.a[3]                        =   -0.0124023;// +/-   0.000446216 
 tableSet->AddAt(&row);        // Inner
 memset(&row,0,tableSet->GetRowSize());
 row.type	 =         10; // iTPC  
 row.idx	 =          3; //
 row.nrows	 =      nrows; //
 row.npar	 =          4; //
 row.min         =        4.4;
 row.max         =        7.5;
/*	AuAu200.VMC.DEV2.KFV.Altro4/ADC/X3DZGPADC.root
	FitP->Draw("mu:x>>X(31,4.4,7.5,130,0.35,0.8)","prob>0.01&&mu>0","colz")
	X->Fit("pol3")
*/
 row.a[0]                        =      5.74891+1.54174e-01;// +/-   0.577785    
 row.a[1]                        =     -2.29634;// +/-   0.294726    
 row.a[2]                        =     0.336147;// +/-   0.0496287   
 row.a[3]                        =   -0.0167159;// +/-   0.00275899  
 tableSet->AddAt(&row);        // iTPC
 // ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
