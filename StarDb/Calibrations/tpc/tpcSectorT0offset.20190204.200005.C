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
  for (Int_t i =  0; i < 24; i++)  {row.t0[i] = -22.2572;}
#if 0
  Double_t corrFF[24] = { 0.0588,-0.0120, 0.0043, 0.0016, 0.0114,-0.0156, 0.0250, 0.1109, 0.0109, 0.0582, 0.0126, 0.0136, 
			  0.0500, 0.0079, 0.0185,-0.1711,-0.0967,-0.0879,-0.0182, 0.0158, 0.0296, 0.0111, 0.0014, 0.0608};
#else
  Double_t corrRF[24] = { 0.0539,-0.0079,-0.0129,-0.0024, 0.0009,-0.0168, 0.0269, 0.1152, 0.0082, 0.0521, 0.0133, 0.0142, 
			  0.0458, 0.0059, 0.0176,-0.1740,-0.0968,-0.0888,-0.0200, 0.0182, 0.0292, 0.0100, 0.0026, 0.0659};
#endif
  for (Int_t i = 24; i < 48; i++)  {row.t0[i] = -22.2572 - 7.4645 +0.335282 + corrRF[i-24];}
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
