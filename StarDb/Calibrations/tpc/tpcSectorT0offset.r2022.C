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
  // TGPTpcHitZTM : FitP->Draw("mu-3.31351:x>>O(24,0.5,24.5,100,-1,1)","i&&j&&dmu>0&&dmu<0.02","colz");  O->FitSlicesY(); PrintTH1.C(O_1);
  Double_t corrO[24] = {-0.0384,-0.1253,-0.0015, 0.0357, 0.0157, 0.0048, 0.2048,-0.0447, 0.0299,-0.0213, 0.0273,-0.0098, 
			 0.0735, 0.0285, 0.0191,-0.0343,-0.0584, 0.1197, 0.0730, 0.0435, 0.0000,-0.0279,-0.0392, 0.0186};
  for (Int_t i =  0; i < 24; i++)  {row.t0[i] = -22.2572 - corrO[i];}
  //                 FitP->Draw("mu-3.31351:x>>I(24,0.5,24.5,100,6.5,7)","i&&j&&dmu>0&&dmu<0.02","colz"); I->FitSlicesY(); PrintTH1(I_1)
  Double_t corrI[24] = { 6.6146, 6.6446, 6.6111, 6.6151, 6.6235, 6.6522, 6.5870, 6.5799, 6.6265, 6.5813, 6.6068, 6.6284, 
			 6.6076, 6.6144, 6.6173, 6.7841, 6.6560, 6.6435, 6.6537, 6.6289, 6.6168, 6.6284, 6.6074, 6.5731};
  for (Int_t i = 24; i < 48; i++)  {row.t0[i] = -22.2572 - corrI[i-24];}
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
