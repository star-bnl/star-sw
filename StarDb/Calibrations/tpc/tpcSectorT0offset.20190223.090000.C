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
#if 0
  // TGPTpcHitZTM : FitP->Draw("mu-3.31351:x>>dOT0(24,0.5,24.5)","i&&j&&mu>0&&j>40","prof")
  Double_t corrO[24] = {-0.0013, 0.0067, 0.0128, 0.0052, 0.0020,-0.0014,-0.0376,-0.0400, 0.0215, 0.0004, 0.0104, 0.0116, 
			 0.0198, 0.0253, 0.0166,-0.0229,-0.0654,-0.0662, 0.0366, 0.0292, 0.0119, 0.0183, 0.0030, 0.0072};
#endif
  // FitP->Draw("mu-3.31351:x>>O(24,0.5,24.5,100,-0.5,0.5)","chisq>1&&chisq<2e2&&y>40.5","colz")
  Double_t corrO[24] = {-0.0464,-0.0380,-0.0310,-0.0384,-0.0373,-0.0427,-0.0738,-0.0802,-0.0228,-0.0436,-0.0348,-0.0324,
			-0.0218,-0.0176,-0.0230,-0.0680,-0.1049,-0.1054,-0.0036,-0.0141,-0.0289,-0.0227,-0.0378,-0.0329};
  for (Int_t i =  0; i < 24; i++)  {row.t0[i] = -22.2572 - corrO[i];}
  //                FitP->Draw("mu-3.31351:x>>dIT0(24,0.5,24.5)","i&&j&&mu>0&&j<=40","prof")
#if 0
  Double_t corrI[24] = { 5.6240, 5.6550, 5.6421, 6.6265, 5.6386, 5.6609, 5.6133, 5.6028, 5.6370, 6.5947, 5.6313, 5.6345, 
			 5.6204, 5.6375, 5.6238, 5.7879, 5.6621, 5.6489, 5.6635, 5.6307, 5.6254, 5.6185, 5.6170, 6.5820};
#endif
  // FitP->Draw("mu-3.31351:x>>I(24,0.5,24.5,100,4.5,7.5)","chisq>1&&chisq<2e2&&y<40.5","colz")
  Double_t corrI[24] = { 5.5858, 5.6228, 5.6027, 6.5901, 5.6026, 5.6288, 5.5745, 5.5609, 5.6045, 6.5544, 5.5944, 5.5964, 
			 5.5831, 5.6027, 5.5887, 5.7706, 5.6311, 5.6188, 5.6331, 5.5984, 5.5914, 5.5844, 5.5833, 6.5463};
  for (Int_t i = 24; i < 48; i++)  {row.t0[i] = -22.2572 - corrI[i-24];}
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
