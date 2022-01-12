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
  // TGPTpcHitZTM : FitP->Draw("mu-3.31351:x>>O(24,0.5,24.5,100,-1,1)","i&&j&&dmu>0&&dmu<0.02","colz");  O->FitSlicesY(); PrintTH1.C(O_1);
  Double_t corrO[24] = {-0.0384,-0.1253,-0.0015, 0.0357, 0.0157, 0.0048, 0.2048,-0.0447, 0.0299,-0.0213, 0.0273,-0.0098, 
			 0.0735, 0.0285, 0.0191,-0.0343,-0.0584, 0.1197, 0.0730, 0.0435, 0.0000,-0.0279,-0.0392, 0.0186};
#else
  /*
    root.exe OTRGPhist23*.root 'Chain.C("FitP")'; 
    tChain->Draw("mu-3.31351:x>>Op(24,0.5,24.5,100,-1,1)","i&&j&&dmu>0&&dmu<0.02&&y>4.5","prof");
    .x PrintTH1.C(Op)
  */
  Double_t corrO[24] = {-0.0789,-0.1603,-0.0467,-0.0061,-0.0276,-0.0532, 0.2418,-0.0861,-0.0479,-0.0737,-0.0091,-0.0509, 
			 0.0237,-0.0078,-0.0219,-0.0604,-0.1002, 0.0705, 0.0299,-0.0153,-0.0462,-0.0590,-0.0722,-0.0179};
#endif
  for (Int_t i =  0; i < 24; i++)  {row.t0[i] = -22.2572 - corrO[i];}
#if 0
  //                 FitP->Draw("mu-3.31351:x>>I(24,0.5,24.5,100,6.5,7)","i&&j&&dmu>0&&dmu<0.02","colz"); I->FitSlicesY(); PrintTH1(I_1)
  Double_t corrI[24] = { 6.6146, 6.6446, 6.6111, 6.6151, 6.6235, 6.6522, 6.5870, 6.5799, 6.6265, 6.5813, 6.6068, 6.6284, 
			 6.6076, 6.6144, 6.6173, 6.7841, 6.6560, 6.6435, 6.6537, 6.6289, 6.6168, 6.6284, 6.6074, 6.5731};
#else
  /*
     root.exe ITRGPhist23*.root 'Chain.C("FitP")'
     tChain->Draw("mu-3.31351:x>>Ip(24,0.5,24.5,100,-1,1)","i&&j&&dmu>0&&dmu<0.02&&y<4.5","prof");
     .x PrintTH1.C(Ip)
  */
  Double_t corrI[24] = { 6.6041, 6.6462, 6.6205, 6.6111, 6.6255, 6.6542, 6.5852, 6.5744, 6.6277, 6.5653, 6.6133, 6.6183, 
			 6.6056, 6.6120, 6.6203, 6.7912, 6.6587, 6.6457, 6.6494, 6.6283, 6.6173, 6.6301, 6.6157, 6.5746};
#endif
  for (Int_t i = 24; i < 48; i++)  {row.t0[i] = -22.2572 - corrI[i-24];}
  tableSet->AddAt(&row);
  // ----------------- end of code ---------------
  return (TDataSet *)tableSet;
}
