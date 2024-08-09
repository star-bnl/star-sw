TDataSet *CreateTable() { 
  // strip coordinate (x,y,0) => module coordinate (y,0,x)
  if (!gROOT->GetClass("St_Survey")) return 0;
  Survey_st rows[8] = {
    //m                      u v w  
    {0, 1,0,0, 0, 1,0, 0,0, 1, 0,0,0, 0,0,0,0,0,0,"2014AuAu15GMTGlobI"}, 
    {1, 1,0,0, 0, 1,0, 0,0, 1, 0,0,0, 0,0,0,0,0,0,"2014AuAu15GMTGlobI"}, 
    {2, 1,0,0, 0,-1,0, 0,0,-1, 0,0,0, 0,0,0,0,0,0,"2014AuAu15GMTGlobI"}, 
    {3, 1,0,0, 0,-1,0, 0,0,-1, 0,0,0, 0,0,0,0,0,0,"2014AuAu15GMTGlobI"}, 
    {4, 1,0,0, 0, 1,0, 0,0, 1, 0,0,0, 0,0,0,0,0,0,"2014AuAu15GMTGlobI"}, 
    {5, 1,0,0, 0, 1,0, 0,0, 1, 0,0,0, 0,0,0,0,0,0,"2014AuAu15GMTGlobI"}, 
    {6, 1,0,0, 0,-1,0, 0,0,-1, 0,0,0, 0,0,0,0,0,0,"2014AuAu15GMTGlobI"}, 
    {7, 1,0,0, 0,-1,0, 0,0,-1, 0,0,0, 0,0,0,0,0,0,"2014AuAu15GMTGlobI"}};
  Int_t noModules = 8;// 
  St_Survey *tableSet = new St_Survey("GmtOnModule",noModules);
  TString Rot;
  Double_t dU[8] = { 5.0020,  5.0513,  4.8432,  4.8517,  5.2337,  5.2333,  0.0000,  4.7607};
  // T->Draw("uD-uP:barrel>>dU(8,-0.5,7.5,100,-2.5,2.5)","(dvAdcD/duAdcD)>0.5&&(dvAdcD/duAdcD)<2.&&abs(pT)>0.5&&sLength<40&&abs(vD-vP)<2.5&&abs(uD-uP)<2.5","colz")
  Double_t dU1[8]= { 0.0027,  0.0009,  0.0125,  0.0004,  0.0158,  0.0259,  0.0000, -0.0032};
  Double_t dU2[8]= { 0.0023, -0.0031, -0.0119, -0.0088, -0.0054, -0.0072,  0.0000, -0.0067};// J pT > 0.2

  Double_t dV[8] = {-0.0589,  0.1424, -0.3106, -0.2046,  0.3160,  0.5626,  0.0000, -0.8306};
  Double_t dV1[8]= {-0.0243,  0.0088, -0.0145, -0.0264, -0.0340,  0.0135,  0.0000, -0.0025};
  Double_t dV2[8]= { 0.0170, -0.0119,  0.0009,  0.0075,  0.0068, -0.0110,  0.0000, -0.0012};// J pT > 0.2
  for (Int_t m = 0; m < noModules; m++) {
    rows[m].t1 -= (dU[m]+dU1[m])*rows[m].r11;
    rows[m].t2 -= (dV[m]+dV1[m])*rows[m].r22;
    tableSet->AddAt(&rows[m].Id);
  }
  return (TDataSet *) tableSet;
}
