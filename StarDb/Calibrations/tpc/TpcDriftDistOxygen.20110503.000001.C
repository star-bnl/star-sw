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
  row.npar       =      3;       // Z3OCGFRunXI19AuAu19P11ic_dedx_AuAu19_production_ReversedFullField
	// + Z3OCGFRunXI57AuAu200FF.root
	row.nrows = 2;
	row.idx = 1;
  row.a[0]	 =  1.97223e-02+ -4.16172e-02; // -6.2709e-03;//  3.58294622853787268e+03; // mean 
  row.a[1]	 = -5.44731e-06+ 1.24501e-05; //  1.7502e-6;// slope from Blair   1/( O2 in ppm., cm ) 
  row.a[2]       =  2.68906e-10+ -7.94545e-10; // 
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.npar       =      3;       // 
	row.nrows = 2;
	row.idx = 2;
  row.a[0]	 =  8.07956e-02+-1.26488e-01; //  4.20540160413917147e+03;// Inner
  row.a[1]	 = -1.83671e-05+ 3.13022e-05; // slope from Blair   1/( O2 in ppm., cm ) 
  row.a[2]       =  7.54449e-10+-1.97018e-09; 
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
