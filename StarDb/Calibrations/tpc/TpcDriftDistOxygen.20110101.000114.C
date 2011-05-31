TDataSet *CreateTable() { 
// -----------------------------------------------------------------
// tpcCorrection! Allocated rows: 48  Used rows: 48  Row size: 16 bytes
//  Table: tpcCorrection_st[0]--> tpcCorrection_st[47]
// ===================================================================DriftCorr=
// ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
tpcCorrection_st row;
St_tpcCorrection *tableSet = new St_tpcCorrection("TpcDriftDistOxygen",2);
memset(&row,0,tableSet->GetRowSize()); 
 row.npar        =      3;       // Z3OCGFRunXI14dev_calib_pp500_production_2011_ReversedFullField
 row.a[0]	 = 7.98697e-03 +1.34981e-02;//-6.2709e-03;//  3.58294622853787268e+03; // mean 
 row.a[1]	 =-1.62601e-06 -4.15312e-06;// 1.7502e-6;// slope from Blair   1/( O2 in ppm., cm ) 
 row.a[2]        = 	        2.49507e-10;
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
 row.npar        =      2;       //   Outer Hist292P02gh1
 row.a[0]	 = 2.56673e-02 +3.31781e-02;//-7.3603e-03;//  4.20540160413917147e+03;// Inner
 row.a[1]	 =-4.94756e-06 -5.85402e-06;// 1.7502e-6;// slope from Blair   1/( O2 in ppm., cm ) 
 tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
