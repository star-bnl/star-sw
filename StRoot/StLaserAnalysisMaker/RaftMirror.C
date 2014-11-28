// $Id: RaftMirror.C,v 1.5 2007/12/10 20:46:21 fisyak Exp $
// $Log: RaftMirror.C,v $
// Revision 1.5  2007/12/10 20:46:21  fisyak
// Add comment
//
// Revision 1.4  2007/12/10 19:54:03  fisyak
// Add Id and Log, correct spelling error in README
//
TChain *theChain = 0;
//________________________________________________________________________________
TChain *GetChain() {
  if (! theChain) {
    gInterpreter->ProcessLine(".L Chain.C");
    theChain = Chain();
  }
  return theChain;
}
//________________________________________________________________________________
Double_t RaftMirror(Double_t pXL, Double_t pYL, Double_t PhiT, Int_t Raft, Int_t Mirror) {
  //RaftMirror(fTracks.Laser.DirG.mX2,fTracks.Laser.DirG.mX1,fTracks.fgeoOut.mPsi,fTracks.Laser.Raft,fTracks.Laser.Mirror):fTracks.Laser.Raft>>RaftPhiC","fTracks.Flag==2&&fTracks.Laser.Mirror==4","colz")
  struct RaftCorr_t {
    Int_t raft;
    Double_t phi;
    Double_t dphi;
  };
  //  RaftCorr_t RaftCorr[14] = {  };
  
  if (Raft   < 1 || Raft > 14 ||
      Mirror < 1 || Mirror > 7) return 0; 
  Double_t phiL = TMath::ATan2(pYL,pXL);
  Double_t dPhi = phiL - PhiT;// - RaftCorr[Raft-1].phi;
  if (dPhi >=  TMath::Pi()) dPhi -= 2*TMath::Pi();
  if (dPhi <= -TMath::Pi()) dPhi += 2*TMath::Pi();
  return dPhi;
}
//________________________________________________________________________________
Double_t SectorMirror(Double_t PhiL, Double_t PhiT, Int_t Sector, Int_t Mirror) {
  //SectorMirror(fTracks.Laser.DirG.mX2,fTracks.Laser.DirG.mX1,fTracks.fgeoOut.mPsi,fTracks.Laser.Sector,fTracks.Laser.Mirror):fTracks.Laser.Sector>>SectorPhiC","fTracks.Flag==2&&fTracks.Laser.Mirror==4","colz")
#if 0
  struct SectorCorr_t {
    Int_t sector2;
    Double_t phi;
    Double_t dphi;
    Double_t sigma;
    Double_t dsigma;
  };
  SectorCorr_t SectorCorr0[12] = {
    // Mirror 4
    {2,     -0.0186195,     1.4527e-05,     0.00184551,     1.02723e-05}, 
    {4,     -0.0196896,     2.33502e-05,    0.00184777,     1.65113e-05},
    {6,     -0.005116,      2.93135e-05,    0.00260989,     2.07281e-05},
    {8,     0.00437859,     2.77778e-05,    0.00329948,     1.96424e-05},
    {10,    0,      0,      0,      0},
    {12,    0.000916182,    2.43739e-05,    0.00234977,     1.72353e-05},
    {14,    -0.0180773,     2.52572e-05,    0.00275963,     1.78601e-05},
    {16,    0.00312329,     1.55691e-05,    0.00204276,     1.10091e-05},
    {18,    -0.00609319,    1.41944e-05,    0.00136081,     1.00371e-05},
    {20,    0.00649718,     2.39854e-05,    0.00278396,     1.69605e-05},
    {22,    0.0201383,      2.10855e-05,    0.00128673,     1.49099e-05},
    {24,    0.0374981,      4.71165e-05,    0.00324044,     3.33172e-05}
  };
  SectorCorr_t SectorMirror[7][12] = {
    { //Mirror 1
      { 2,    0,      0,      0,      0},
      { 4,    0,      0,      0,      0},
      { 6,    0,      0,      0,      0},
      { 8,    0,      0,      0,      0},
      {12,    0,      0,      0,      0},
      {14,    0,      0,      0,      0},
      {16,    0,      0,      0,      0},
      {18,    0,      0,      0,      0},
      {20,    0,      0,      0,      0},
      {22,    0,      0,      0,      0},
      {24,    0,      0,      0,      0}
    },
    {//Mirror 2
      {2,  1.47897e-02,   2.50330e-05, 5.15724e-04,   2.14312e-05},// dominant peak  0.00566472,     0.00047955,     0.0125602,      0.000339109},
      {4, -6.08707e-02,   2.16937e-05, 6.93992e-04,   1.60809e-05},// single peak   -0.0609268,     2.06028e-05,    0.000791607,    1.4584e-05},
      {6,     0,      0,      0,      0},
      {8, -3.50111e-03,   3.93290e-05, 1.60574e-03,   2.70401e-05},// dominant peak-0.00664828,    0.000132061,    0.00628642,     9.33846e-05},
      {10,    0,      0,      0,      0},
      {12,-1.45117e-02,   5.64167e-05, 1.54124e-03,   3.65524e-05},// dominant peak -0.00821915,    0.000208234,    0.00970468,     0.00014725},
      {14,-3.28631e-02,   9.47326e-05, 1.92888e-03,   6.99189e-05},// single peak  -0.0331542,     0.000115018,    0.00245071,     8.13329e-05},
      {16, 2.65153e-02,   1.00910e-05, 5.09772e-04,   5.99093e-06},// dominant peak    0.0208911,      0.000809766,    0.0810765,      0.000671043},
      {18, 2.26290e-02,   1.61950e-05, 5.65004e-04,   8.09284e-06},// single peak    0.0226052,      1.64671e-05,    0.000650616,    1.1386e-05},
      {20,-6.06357e-02,   1.44520e-05, 7.23663e-04,   1.35932e-05},// dominant peak -0.0143382,     0.000654378,    0.0481428,      0.000464407},
      {22, 6.01550e-03,   9.13966e-05, 1.07660e-03,   6.33931e-05},// dominant peak 0.0116422,      0.000241984,    0.00731575,     0.000171114},
      {24,-5.20394e-02,   2.44742e-05, 9.68631e-04,   1.61593e-05} // single peak   -0.0520774,     2.32609e-05,    0.00107706,     1.64484e-05},
    },
    {//Mirror 3
      {2,  0.0172837,  9.29745e-06, 0.000480627, 6.46204e-06},// dominant peak  0.0253127,      0.000130236,    0.0110478,      9.20956e-05},
      {4,  0.00212475, 5.27541e-05, 0.00229639,  3.73615e-05},// dominant peak  0.00355427,     0.000111547,    0.00517944,     7.88774e-05},
      {6, -0.00222559, 6.13796e-06, 6.34452e-05, 1.34243e-06},// dominant peak   0.00362372,     0.000262839,    0.0216966,      0.000185867},
      {8,  0.0433267,  9.80298e-06, 0.00051389,  6.37242e-06},// multiple peaks   0.0261529,      0.000166797,    0.0148252,      0.000117951},
      {10,    0,      0,      0,      0},
      {12, 0.0246517, 1.91944e-05, 0.00101986, 1.51846e-0 },// dominant peak  0.0371178,      0.000394592,    0.0280604,      0.000279036},
      {14, 0.0047278, 0.000260374, 0.00157437, 0.000349249},// dominant peak   0.00253371,     0.000508345,    0.00479571,     0.000359431},
      {16, 0.00466532,1.16948e-05, 0.000832494,7.36983e-06},// dominant peak  0.000905332,    4.07978e-05,    0.00421818,     2.88493e-05},
      {18,-0.00221707,6.30134e-05, 0.00184436, 3.70755e-05},// single peak -0.00226442,    5.92677e-05,    0.00191133,     4.19086e-05},
      {20,-0.00400158,6.86993e-05, 0.00225299, 4.08153e-05},//  multiple peaks     -0.0195078,     0.00102821,     0.0795717,      0.000842814},
      {22, 0.00392405,0.00010365,  0.00119326, 0.000110543},// single peak -0.0034619,     0.0001722,      0.00249542,     0.000121762},
      {24, 0.00557869,2.23633e-05, 0.000991408,1.30714e-05} // single peak 0.00547582,     2.62727e-05,    0.00137273,     1.8578e-05},
    },
    {//Mirror 4
      {2,  -2.0143e-05, 1.65561e-05, 0.00171236, 7.53284e-06},// single peak   -4.46201e-06,   1.45853e-05,    0.00182683,     1.03135e-05},
      {4,  -0.000256831,2.38597e-05, 0.00101118, 2.03843e-05},// single peak   -0.000404097,   2.02919e-05,    0.00147685,     1.43488e-05},
      {6,   0.001134,   1.91263e-05, 0.00134625, 1.07783e-05},// single peak  0.000750153,    2.58916e-05,    0.00209322,     1.83084e-05},
      {8,  -0.00081522, 1.04845e-05, 0.000613908,1.08238e-05},//  multiple peaks -1.54491e-05,   2.72895e-05,    0.00322305,     1.9297e-05},
      {10,    0,      0,      0,      0},
      {12,  0.000987496,1.66857e-05, 0.00104992, 1.11315e-05},// single peak 0.000355621,    2.22063e-05,    0.00201857,     1.57025e-05},
      {14,    -0.00122242,    4.50794e-05,    0.00344171,     3.18769e-05},// double peak ?
      {16,  0.000131596,2.89793e-05, 0.00185695, 1.72874e-05},// single peak 0.000109311,    1.80858e-05,    0.00217127,     1.27887e-05},
      {18,  0.000805035,1.34267e-05, 0.000764655,7.61076e-06},// single peak 0.000650699,    1.40839e-05,    0.00109203,     9.95901e-06},
      {20, -0.00240851, 1.75617e-05, 0.00109031, 1.29384e-05},// dominant peak  -0.000293356,   2.83839e-05,    0.00285453,     2.00709e-05},
      {22, -0.000366494,1.51986e-05, 0.000679965,1.06328e-05},// single peak
      {24,  0.00159829, 1.97649e-05, 0.00106986, 1.39759e-05} // single peak
    },
    {//Mirror 5 
      {2,   0.0202492, 1.21405e-05, 0.000651143, 8.64812e-06},// dominant peak  -0.0477053,     0.00105037,     0.0881546,      0.000890414},
      {4,  -0.127498,  4.41647e-05, 0.0015842,   2.73322e-05},// dominant peak -0.11625,       0.00263391,     0.0772389,      0.0018637},
      {6,  -0.0111096, 1.71769e-05, 0.000746844, 1.27916e-05},// dominant peak -0.0264714,     0.00102381,     0.0801099,      0.00084246},
      {8,   0.0115083, 1.07084e-05, 0.000388096, 4.30121e-06},// bad fit dominant peak  0.0145057,      0.000549461,    0.0546474,      0.000394043},
      {10,    0,      0,      0,      0},
      {12,  0.0797944, 2.68801e-05, 0.000825459, 2.09022e-05},// dominant peak 0.000852137,    0.00198958,     0.102371,       0.00205952},
      {14,    -0.00656188,    3.06239e-05,    0.00137091,     2.16544e-05},// single peak
      {16, -0.00293398, 1.50938e-05, 0.000800728, 1.4194e-05},// dominant peak  0.000220087,    5.00353e-05,    0.00372452,     3.53814e-05},
      {18,    0,      0,      0,      0}, //only 4 entries
      {20,  0.0247521,  1.0784e-05,  0.000544184, 8.77057e-06},// single peak  0.0247561,      1.25756e-05,    0.000640794,    8.9043e-06},
      {22,     0,      0,      0,      0},// only 12 entries  0.00257692,     0.000254232,    0.000916646,    0.000179642},
      {24,    -0.0035,        0.00010766,     0.000854507,    7.6101e-05}// single peak only 30 entries 
    },
    {//Mirror 6 
      {2,     -0.0211806,     8.82803e-05,    0.00193211,     6.24233e-05},// single peak
      {4,      0.00960692, 0.000803011, 0.00207609, 0.000503285},// single peak  0.00873809,     0.000184719,    0.00146617,     0.000130616},
      {6,     -0.019809,      6.20976e-05,    0.00178037,     4.39096e-05},// single peak
      {8,     -0.0244286,     3.08233e-05,    0.00143618,     2.17956e-05},// single peak
      {10,    0,      0,      0,      0},
      {12,    0.00980073, 9.14786e-05, 0.00101579, 6.82576e-05},// dominant peaks 0.02598,        0.00102381,     0.0169779,      0.000723964},
      {14,    0,      0,      0,      0},
      {16,    -0.000329188, 6.08403e-05, 0.000869988, 4.33747e-05},// single peak -0.000268182,   7.28943e-05,    0.0010812,      5.15429e-05},
      {18,    -0.00445835, 0.000235526, 0.000686245, 0.000139439},// dominant  -0.00399438,    6.93754e-05,    0.000925585,    4.9056e-05},
      {20,     0.00150319, 0.000115474, 0.000571378, 8.76884e-05},// double peak 0.0029, 0.000228035,    0.00161245,     0.000161221},
      {22,    0,      0,      0,      0},
      {24,    0.00230525,     5.80364e-05,    0.0015616,      4.10378e-05}// single peak
    },
    {//Mirror 7
      { 2,    0,      0,      0,      0},
      { 4,    0,      0,      0,      0},
      { 6,    0,      0,      0,      0},
      { 8,    0,      0,      0,      0},
      {12,    0,      0,      0,      0},
      {14,    0,      0,      0,      0},
      {16,    0,      0,      0,      0},
      {18,    0,      0,      0,      0},
      {20,    0,      0,      0,      0},
      {22,    0,      0,      0,      0},
      {24,    0,      0,      0,      0}
    }
  };
#endif
  Double_t dPhi = -999;
  if (Sector   < 1 || Sector > 24 ||
      Mirror < 1 || Mirror > 7) return dPhi; 
    
  //  Double_t phiL = TMath::ATan2(pYL,pXL);
  Int_t i = (Sector-1)/2;
  Int_t m = Mirror - 1;
  if (SectorCorr0[i].sigma <= 0) return dPhi;
  if (SectorMirror[m][i].sigma <= 0) return dPhi;
#if 0
  Double_t dPhi = PhiL - PhiT - SectorCorr0[i].phi - SectorMirror[m][i].phi;
#else
  Double_t dPhi = PhiL - PhiT - SectorMirror[m][i].phi;
#endif
  if (dPhi >=  TMath::Pi()) dPhi -= 2*TMath::Pi();
  if (dPhi <= -TMath::Pi()) dPhi += 2*TMath::Pi();
  return dPhi;
}
//______________________________________________________________________
void FitRM(TH2* hist) {
  if (! hist) return;
  hist->FitSlicesY(0,-1,0,10,"lnr");
  TH1 *fit = (TH1 *) gDirectory->Get(Form("%s_1",hist->GetName()));
  TH1 *sigma = (TH1 *) gDirectory->Get(Form("%s_2",hist->GetName()));
  if (! fit || ! sigma) return;
  fit->SetMarkerStyle(20);
  hist->Draw("colz");
  fit->Draw("same");
  Int_t nx = fit->GetNbinsX();
  for (Int_t i = 1; i <= nx; i++) 
    cout << "{" << 2*i << ", \t" << fit->GetBinContent(i) << ",\t" << fit->GetBinError(i) << ",\t" << sigma->GetBinContent(i) << ",\t" << sigma->GetBinError(i) <<"}," << endl;
}
//________________________________________________________________________________
void FillRM(Int_t Mirror) {
  TTree *laser = (TTree *) gDirectory->Get("laser");
  if (! laser ) return;
  //  TString Plot("SectorMirror(fTracks.Laser.DirG.mX1,fTracks.Laser.DirG.mX2,fTracks.fgeoOut.mPsi,fTracks.Laser.Sector,fTracks.Laser.Mirror):fTracks.Laser.Sector>>");
  TString Plot("SectorMirror(fTracks.Laser.PhiG,fTracks.fgeoOut.mPsi,fTracks.Laser.Sector,fTracks.Laser.Mirror):fTracks.Laser.Sector>>");
  TString hName(Form("Sector%iPhi2",Mirror));
  Plot += hName;
  Plot += "(12,1,25,400,-.2,.2)";
  TString Cut(Form("fTracks.Flag==2&&fTracks.Laser.Mirror==%i",Mirror));
  Cut += "&&abs(fTracks.XyzP.mX1-fTracks.Laser.XyzG.mX1)<0.1";
  Cut += "&&abs(fTracks.XyzP.mX2-fTracks.Laser.XyzG.mX2)<0.1";
  Cut += "&&fTracks.thePath > 5 && fTracks.thePath < 12";
  Cut += "&&fTracks.mNumberOfFitPointsTpc > 25";
  Cut += "&&TMath::Abs(fTracks.Laser.ThetaG-fTracks.fgeoOut.mDipAngle-TMath::Pi()/2) < 0.05";
  laser->Draw(Plot,Cut,"colz");
  TH2 *hist = (TH2 *) gDirectory->Get(hName);
  if (! hist) {cout << hName " << is missing" << endl; return;}
  FitRM(hist);
}
//________________________________________________________________________________
void MembraneDip() {// plots  dependence of East/West  membrane tracks dip versus  Phi. This is an example to use Laser TTree.
  TTree *laser = (TTree *) gDirectory->Get("laser");
  if (! laser ) return;
  TF1 *SS = new TF1("SS","[0]+[1]*TMath::Sin(x+[2])");
  SS->SetParName(0,"A");
  SS->SetParName(1,"B");
  SS->SetParName(3,"#phi 0");
  TCanvas *c1 = new TCanvas();
  c1->Divide(1,2);
  c1->cd(1)->SetLogz(1);
  //  TString Cut("fTracks.Flag==1&&(abs(fTracks.Vertex.mX3+7.3)<2||abs(fTracks.Vertex.mX3-6.8)<2)&&abs(fTracks.fgeoIn.mDipAngle)<0.010");
  TString Cut("fTracks.Flag==1&&(abs(fTracks.Vertex.mX3+3.3)<2||abs(fTracks.Vertex.mX3-3.6)<2)&&abs(fTracks.fgeoIn.mDipAngle)<0.010");
  TString cut = Cut;
  cut += "&&fTracks.mSector<=12";
  laser->Draw("1.e3*fTracks.fgeoIn.mDipAngle:fTracks.fgeoIn.mPsi>>DipPsiWW",cut,"colz");
  TH2 *DipPsiWW = (TH2 *) gDirectory->Get("DipPsiWW");
  DipPsiWW->SetXTitle("#psi");
  DipPsiWW->SetYTitle("#lambda");
  DipPsiWW->FitSlicesY();
  TH1 *DipPsiWW_1 = (TH1 *) gDirectory->Get("DipPsiWW_1");
  DipPsiWW_1->SetMarkerStyle(2);
  SS->SetParameter(2,1);
  DipPsiWW_1->Fit(SS);
  DipPsiWW_1->SetMarkerStyle(20);
  DipPsiWW->Draw("colz");
  DipPsiWW_1->Draw("sames");
  TLegend *w = new TLegend(0.25,0.7,0.4,0.9,"");
  w->AddEntry(DipPsiWW_1,"West");
  w->Draw();
  c1->cd(2)->SetLogz(1);
  cut = Cut;
  cut += "&&fTracks.mSector>12";
  laser->Draw("1.e3*fTracks.fgeoIn.mDipAngle:fTracks.fgeoIn.mPsi>>DipPsiEE",cut,"colz");
  TH2 *DipPsiEE = (TH2 *) gDirectory->Get("DipPsiEE");
  DipPsiEE->SetXTitle("#psi");
  DipPsiEE->SetYTitle("#lambda");
  DipPsiEE->FitSlicesY();
  TH1 *DipPsiEE_1 = (TH1 *) gDirectory->Get("DipPsiEE_1");
  DipPsiEE_1->SetMarkerStyle(2);
  SS->SetParameter(2,1);
  DipPsiEE_1->Fit(SS);
  DipPsiEE_1->SetMarkerStyle(20);
  DipPsiEE->Draw("colz");
  DipPsiEE_1->Draw("sames");
  TLegend *e =  new TLegend(0.25,0.6,0.4,0.9,"");
  e->AddEntry(DipPsiEE_1,"East");
  e->Draw();
}
