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
 row.npar        =      2;       //   Z3OGFRunIX58P09if_calib
 row.a[0]	 = 7.82051e-3 - 6.2709e-3;//  3.58294622853787268e+03; // mean 
 row.a[1]	 =-8.24777e-6 + 1.7502e-6;// slope from Blair   1/( O2 in ppm., cm ) 
tableSet->AddAt(&row);
memset(&row,0,tableSet->GetRowSize());
 row.npar        =      2;       //   Z3OGFRunIX58P09if_calib
 row.a[0]	 = 4.30250e-2 - 7.3603e-3;//  4.20540160413917147e+03;// Inner
 row.a[1]	 =-2.17021e-5 + 1.7502e-6;// slope from Blair   1/( O2 in ppm., cm ) 
 tableSet->AddAt(&row);
// ----------------- end of code ---------------
 return (TDataSet *)tableSet;
}
