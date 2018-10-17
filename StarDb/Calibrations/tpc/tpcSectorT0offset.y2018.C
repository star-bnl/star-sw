TDataSet *CreateTable() { 
  // -----------------------------------------------------------------
  // db/.const/StarDb/Calibrations/tpc/.tpcSectorT0offset/tpcSectorT0offset Allocated rows: 1  Used rows: 1  Row size: 72 bytes
  //  Table: tpcSectorT0offset_st[0]--> tpcSectorT0offset_st[0]
  // ====================================================================
  // ------  Test whether this table share library was loaded ------
  if (!gROOT->GetClass("St_tpcSectorT0offset")) return 0;
  tpcSectorT0offset_st row;
  St_tpcSectorT0offset *tableSet = new St_tpcSectorT0offset("tpcSectorT0offset",1);
  //
  memset(&row,0,tableSet->GetRowSize());
  Double_t t0All = -22.2572;
  // 07/09/18 2018 AuAu27 sample, Tpc prompt hits
  Double_t t0Outer[24] = {
    0.016, 0.024, 0.047, 0.062, 0.023, 0.030, 0.007, 0.024, 0.001, 0.004, 0.038, 0.012,
   -0.017,-0.017,-0.010,-0.063,-0.093,-0.107,-0.004,-0.023,-0.028,-0.015,-0.029,-0.026,
  };
  Double_t t0Inner[24] = {
    0.058, 0.013, 0.041,-0.007, 0.028, 0.041, 0.019, 0.038, 0.034, 0.019, 0.032, 0.016, 
    0.006, 0.007,-0.031,-0.019,-0.090,-0.113,-0.059, 7.194,-0.034,-0.029,-0.025,-0.023,
  };
  for (Int_t i = 0; i < 24; i++) {
    row.t0[i   ] = t0All - t0Outer[i];
    row.t0[i+24] = t0All - t0Inner[i];
  }
#if 0
  for (Int_t i = 0; i < 48; i++)  row.t0[i] = -22.2572 ;
  //  row.t0[23+20] -= 6.68208;;// TpcHit19116020.root // 7.4645;
  //  row.t0[23+20] -= 6.74451; // TpcHit19116021.root // 7.4645;
  //  row.t0[23+20] -= 6.713; 
  //           +/- 0.0312 averaged
  //  row.t0[23+20] -= 7.221; // from run 19116021, 05/17/18
  row.t0[23+20] -= 7.2106; // from run 19116021, 06/27/18
#endif
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
