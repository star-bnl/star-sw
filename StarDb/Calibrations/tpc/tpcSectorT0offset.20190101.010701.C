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
  //TGPTpcHitZTMfl0 : FitP->Draw("mu-3.31351:x>>dOT0(24,0.5,24.5)","i&&j&&mu>2&&mu<20&&j>40","prof")
  Double_t corrO[24] = {-0.0200,-0.0081,-0.0022,-0.0077,-0.0143,-0.0149,-0.0485,-0.0610, 0.0060,-0.0165,-0.0058,-0.0055, 
			0.0008, 0.0076, 0.0020,-0.0406,-0.0833,-0.0826, 0.0208, 0.0116,-0.0047, 0.0035,-0.0102,-0.0079};
  for (Int_t i =  0; i < 24; i++)  {row.t0[i] = -22.2572 - corrO[i];}
  //                FitP->Draw("mu-3.31351:x>>dIT0(24,0.5,24.5)","i&&j&&mu>0&&j<=40","prof")
  Double_t corrI[24] = { 5.6013, 5.6363, 5.6207, 6.5937, 5.6161, 5.6401, 5.5914, 5.5800, 5.6189, 6.5741, 5.6092, 5.6149, 
			 5.6000, 5.6170, 5.6048, 5.7739, 5.6439, 5.6314, 5.6463, 5.6108, 5.6070, 5.5979, 5.5987, 6.5652};
  for (Int_t i = 24; i < 48; i++)  {row.t0[i] = -22.2572 - corrI[i-24];}
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
