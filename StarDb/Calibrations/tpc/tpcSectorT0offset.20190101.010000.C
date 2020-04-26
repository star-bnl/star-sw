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
  // TGPTpcHitZTM : FitP->Draw("mu-3.31351:x>>dOT0(24,0.5,24.5)","i&&j&&mu>0&&j>40","prof")
  Double_t corrO[24] = {-0.0120, -0.0024, 0.0069,-0.0003,-0.0124,-0.0050,-0.0396,-0.0463, 0.0087,-0.0094,-0.0010, 0.0033,
			-0.0059, 0.0025,-0.0054,-0.0491,-0.0890,-0.0881, 0.0138, 0.0011,-0.0162,-0.0067,-0.0127,-0.0201};

  for (Int_t i =  0; i < 24; i++)  {row.t0[i] = -22.2572 - corrO[i];}
  //                FitP->Draw("mu-3.31351:x>>dIT0(24,0.5,24.5)","i&&j&&mu>0&&j<=40","prof")
  Double_t corrI[24] = { 5.5675, 5.6002, 5.5826, 6.5662, 5.5774, 5.6075, 5.5571, 5.5457, 5.5816, 6.5398, 5.5748, 5.5782,
			 5.5472, 5.5650, 5.5505, 5.7152, 5.5925 ,5.5792, 5.5944, 5.5628, 5.5560, 5.5488, 5.5491, 6.5125};

  for (Int_t i = 24; i < 48; i++)  {row.t0[i] = -22.2572 - corrI[i-24];}
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
