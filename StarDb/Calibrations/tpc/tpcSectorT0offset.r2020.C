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
  // TGPTpcHitZTMfl0 : FitP->Draw("mu-3.31351:x>>O(24,0.5,24.5)","i&&j&&chisq>10&&chisq<2e3&&y>40.5","profs")
  Double_t corrO[24] = { 0.0121, 0.0204, 0.0334, 0.0250, 0.0360, 0.0231,-0.0104,-0.0260, 0.0370, 0.0215, 0.0352, 0.0377, 
			 0.0366, 0.0427, 0.0390, 0.0019,-0.0424,-0.0435, 0.0657, 0.0535, 0.0330, 0.0446, 0.0334, 0.0316};
  for (Int_t i =  0; i < 24; i++)  {row.t0[i] = -22.2572 - corrO[i];}
  //                FitP->Draw("mu-3.31351:x>>dIT0(24,0.5,24.5)","i&&j&&mu>0&&j<=40","prof")
  // FitP->Draw("mu-3.31351:x>>I(24,0.5,24.5)","i&&j&&chisq>10&&chisq<2e3&&y<40.5","profs")
  Double_t corrI[24] = { 5.6376, 5.6691, 5.6436, 6.6378, 5.6495, 5.6734, 5.6144, 5.6022, 5.6508, 6.6067, 5.6412, 5.6478, 
			 5.6327, 6.6345, 5.6342, 5.8016, 5.6768, 5.6650, 5.6791, 5.6507, 5.6415, 5.6474, 5.6315, 6.5966};
  for (Int_t i = 24; i < 48; i++)  {row.t0[i] = -22.2572 - corrI[i-24];}
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
