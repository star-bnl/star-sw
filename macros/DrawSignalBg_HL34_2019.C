void DrawSignalBg_HL34_2019(TString nSt="") {
  gStyle->SetOptStat(0);
#if 0
  TStyle *plain = new TStyle("Plain","Plain Style(no colors/fill areas)");
  plain->SetCanvasBorderMode(0);
  plain->SetPadBorderMode(0);
  plain->SetPadColor(0);
  plain->SetCanvasColor(0);
  plain->SetTitleColor(0);
//  plain->SetTitleFont(22);
//  plain->SetLabelFont(22);
  plain->SetStatColor(0);
  plain->SetOptStat(0);
//  plain->SetOptFit(11);
  plain->SetOptFit(0);
  plain->SetStatW(0.19);
  plain->SetStatH(0.7);
  plain->SetOptTitle(0);
  plain->SetPadLeftMargin(0.15);
  plain->SetPadRightMargin(0.06);
  plain->SetPadTopMargin(0.075);
  plain->SetPadBottomMargin(0.18);
  plain->cd();
#endif
#if 0
//    TString name1 = "Output_He_27/KFParticleFinder_He_27.root";
//    TString name1 = "Output_He_19/KFParticleFinder_He_19.root";
//    TString name1 = "Output_He_14/KFParticleFinder_He_14.root";
//    TString name1 = "Output_He_7/KFParticleFinder_He_7.root";
//    TString name1 = "Output_He_7fixed/KFParticleFinder_He_7fixed.root";
//    TString name1 = "Output_He_4fixed/KFParticleFinder_He_4fixed.root";
//    TString name1 = "Output_He_3fixed/KFParticleFinder_160.root";
//    TString name1 = "Output_7f/KFParticleFinder_7f.root";
//    TString name1 = "Output_4f/KFParticleFinder_4f.root";
//    TString name1 = "Output_3f/KFParticleFinder_160.root";

//  TString prefix = "/lustre/nyx/cbm/users/vassiliev/STAR/";
  TString prefix = "/home/vassilie/";
  const int NFiles = 13;
  
//  TString inputFileName[NFiles] = { prefix+"data/3fixed.root",
  TString inputFileName[NFiles] = { prefix+"data/3p85_2018/3p85_2chi2p_5ldl_TofPID_2.root",
                                    prefix+"data/4fixed.root",
                                    prefix+"data/7fixed.root",
                                    prefix+"data/7.root",
                                    prefix+"data/9.root",
                                    prefix+"data/14.root",
                                    prefix+"data/19.root",
                                    prefix+"data/27.root",
                                    prefix+"data/all.root",
                                    prefix+"data/H3L.root",
                                    prefix+"data/H4L_fix.root",
                                    prefix+"data/fixed.root",
                                    prefix+"data/9p2c/9p2nn.root"
 };
				    
  TString efficiencyFileName[NFiles] = { prefix+"data/Eff/2019_4fxt/Efficiency.root",
                                         prefix+"data/Eff/2019_4fxt/Efficiency.root",
                                         prefix+"data/Eff/2019_7fxt/Efficiency.root",
                                         prefix+"data/Eff/2019_14/Efficiency.root",
                                         prefix+"data/Eff/2019_14/Efficiency.root",
                                         prefix+"data/Eff/2019_14/Efficiency.root",
                                         prefix+"data/Eff/2019_19/Efficiency.root",
                                         prefix+"data/Eff/2019_19/Efficiency.root",
                                         prefix+"data/Eff/2019_14/Efficiency.root",
                                         prefix+"data/Eff/2019_14/Efficiency.root",
                                         prefix+"data/Eff/2019_4fxt/Efficiency.root",
                                         prefix+"data/Eff/2019_4fxt/Efficiency.root" };
  double energy[NFiles] = { 3.0, 3.2, 3.9, 7.7, 9.1, 14.5, 19.6, 27 };
  double elab[NFiles] = { 3.85, 4.55, 7.3, 0, 0, 0, 0, 0 };
  TString energyName[NFiles] = { "3.0", "3.2", "3.9", "7.7", "9.1", "14.5", "19", "27" };
#endif

  const int NParticles = 2;
  
//  TFile *f1 = new TFile(inputFileName[12].Data(),"read");
//  TFile* efficiencyFile = new TFile(efficiencyFileName[4].Data(),"read");
//  TFile *f1 = new TFile(inputFileName[10].Data(),"read");
//  TFile* efficiencyFile = new TFile(efficiencyFileName[10].Data(),"read");
//  TFile *f1 = new TFile(inputFileName[9].Data(),"read");
//  TFile* efficiencyFile = new TFile(efficiencyFileName[9].Data(),"read");
//   TFile *f1 = new TFile(inputFileName[8].Data(),"read");
//   TFile* efficiencyFile = new TFile(efficiencyFileName[8].Data(),"read");

//   TFile *f1 = new TFile(inputFileName[7].Data(),"read");
//   TFile* efficiencyFile = new TFile(efficiencyFileName[7].Data(),"read");
//   TFile *f1 = new TFile(inputFileName[6].Data(),"read");
//   TFile* efficiencyFile = new TFile(efficiencyFileName[6].Data(),"read");
//   TFile *f1 = new TFile(inputFileName[5].Data(),"read");
//   TFile* efficiencyFile = new TFile(efficiencyFileName[5].Data(),"read");
//  TFile *f1 = new TFile(inputFileName[4].Data(),"read");
//  TFile* efficiencyFile = new TFile(efficiencyFileName[4].Data(),"read");
//   TFile *f1 = new TFile(inputFileName[3].Data(),"read");
//   TFile* efficiencyFile = new TFile(efficiencyFileName[3].Data(),"read");
//   TFile *f1 = new TFile(inputFileName[2].Data(),"read");
//   TFile* efficiencyFile = new TFile(efficiencyFileName[2].Data(),"read");
//   TFile *f1 = new TFile(inputFileName[1].Data(),"read");
//   TFile* efficiencyFile = new TFile(efficiencyFileName[1].Data(),"read");
#if 0
   TFile *f1 = new TFile(inputFileName[0].Data(),"read");
   TFile* efficiencyFile = new TFile(efficiencyFileName[0].Data(),"read");

  TH1D *hEff_CT[NParticles];
  string Eff_CT[NParticles] = 
                   {"Particles/KFParticlesFinder/Particles/H3L/Efficiency/All particles/EffVsCT",
                    "Particles/KFParticlesFinder/Particles/H4L/Efficiency/All particles/EffVsCT"};
  for(int i=0; i<NParticles; i++)
  {
      hEff_CT[i] = (TH1D*) efficiencyFile->Get(Eff_CT[i].data());
  }
#else 
  TFile *f1 = gDirectory;
#endif



  TH1D *hPart[NParticles];
  TF1  *fPart[NParticles];
#if 0
  TH1D *hPart[NParticles], *hPartCT_SR[NParticles], *hPartCT_BGR[NParticles], *hPartCT_S[NParticles], *hPartCT_SCorr[NParticles];
#endif
  TH1D *hEmpty = new TH1D("empty", "empty", 100, 0.001, 0.002);
  hEmpty-> SetLineColor(0);
  hEmpty-> SetMarkerColor(0);
  
  string sPart[NParticles], sPartCT_SR[NParticles], sPartCT_BGR[NParticles];
  string sPartFit[NParticles];

  string ParNames[NParticles] = {"Particles/KFParticlesFinder/Particles/H3L/Parameters/M",    // H3L
                                 "Particles/KFParticlesFinder/Particles/H4L/Parameters/M"};   // H4L
#if 0
  string ParNamesCT_SR[NParticles] = {"Particles/KFParticlesFinder/Particles/H3L/Parameters/SignalReco/c#tau",    // H3L
                                 "Particles/KFParticlesFinder/Particles/H4L/Parameters/SignalReco/c#tau"};   // H4L
  string ParNamesCT_BGR[NParticles] = {"Particles/KFParticlesFinder/Particles/H3L/Parameters/BGReco/c#tau",    // H3L
                                 "Particles/KFParticlesFinder/Particles/H4L/Parameters/BGReco/c#tau"};   // H4L
#endif                                  
                            //0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
  bool isDraw[NParticles] = { 1,  1};//
//  bool isDraw[NParticles] = { 0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  1, 0};//KStarphirho
//  bool isDraw[NParticles] = { 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  0,  1,  1,  0,  0, 0};//SigmaKsiStar
  bool isSame[NParticles] = { 0,  0};
//  bool isSame[NParticles] = { 0,  0,  0,  0,  1,  0,  0,  0,  1,  0,  1,  0,  0,  1,  0,  1,  0,  1,  0, 0};
  
  string pName[NParticles] = {"H3L   ",
                              "H4L   "
                              
  };

  string AxisName[NParticles];

  AxisName[ 0] = "m_{inv} ^{3}He#pi^{-}(GeV/c^{2})";
  AxisName[ 1] = "m_{inv} ^{4}He#pi^{-} (GeV/c^{2})";

  
  TString LegendEntry[NParticles][3];
  
  LegendEntry[ 0][0] = "{}^{3}_{#Lambda}H";
  LegendEntry[ 1][0] = "{}^{4}_{#Lambda}H";
//   LegendEntry[ 2][0] = "#bar{#Lambda}";
//   LegendEntry[ 3][0] = "#Xi^{-}"; 
//   LegendEntry[ 4][0] = "#Xi^{+}";
//   LegendEntry[ 5][0] = "#Omega^{-}";
//   LegendEntry[ 6][0] = "#Omega^{+}";
  
  TString DecayText[NParticles];
  DecayText[ 0] = "{}^{3}_{#Lambda}H#rightarrow ^{3}He#pi^{-}";
  DecayText[ 1] = "{}^{4}_{#Lambda}H#rightarrow ^{4}He#pi^{-}";

  for(int i=0; i<NParticles; i++)
  {
    sPart[i]    = ParNames[i];
    sPartFit[i] = ParNames[i]+"GaussFit";
#if 0
    sPartCT_SR[i]    = ParNamesCT_SR[i];
    sPartCT_BGR[i]    = ParNamesCT_BGR[i];
#endif
  }

  struct Bins 
  {
    float binStart, binEnd;
  };

  Bins bbb[NParticles] = { {2.97, 3.03},   // 0
                           {3.90, 3.96} };   //6 Omega+
  struct InitPar
  {
    float m,s;
  };
  
  InitPar initPar[NParticles] = 
  {
    {2.9924, 0.0017}, // 0 K0
    {3.9238, 0.0017}  // 6 Omega+
  };    
 
  for(int i=0; i<NParticles; i++)
  {
    hPart[i] = (TH1D*) f1->Get(sPart[i].data());
#if 0
    hPartCT_SR[i] = (TH1D*) f1->Get(sPartCT_SR[i].data());
    hPartCT_BGR[i] = (TH1D*) f1->Get(sPartCT_BGR[i].data());
    hPartCT_S[i] = (TH1D*) hPartCT_SR[i]->Clone();
    hPartCT_S[i] -> Add(hPartCT_BGR[i],-1);
    hPartCT_SCorr[i] = (TH1D*) hPartCT_S[i]->Clone();
#endif    
#if 0    
//    hPartCT_SCorr[i] -> Rebin(10);
//    hEff_CT[i] -> Rebin(10);
    hPartCT_SCorr[i] -> Divide(hEff_CT[i]);
    
    std::cout << "S+BG integral  " << hPartCT_SR[i]->Integral()<< std::endl;
    std::cout << "BG integral  " << hPartCT_BGR[i]->Integral()<< std::endl;
    std::cout << "S integral  " << hPartCT_S[i]->Integral()<< std::endl;
#endif    
    hPart[i] -> SetLineColor(1);

      fPart[i] = new TF1(sPartFit[i].data(),"gaus(0)+pol2(3)",bbb[i].binStart,bbb[i].binEnd);
//      fPart[i] = new TF1(sPartFit[i].data(),"gaus(0)+pol1(2)",bbb[i].binStart,bbb[i].binEnd);
      fPart[i] -> SetParNames("Const","Mass","Sigma","a0","a1","a2");

    fPart[i] -> SetNpx(10000);
    fPart[i] -> SetLineColor(2);
    fPart[i] -> SetParameter(1,(bbb[i].binStart + bbb[i].binEnd)/2);
    fPart[i] -> SetParameter(2,(bbb[i].binEnd - bbb[i].binStart)/4);
    fPart[i] -> SetParLimits(2,0,1);

    fPart[i] -> FixParameter(1, initPar[i].m);
    fPart[i] -> FixParameter(2, initPar[i].s);
    hPart[i] -> Fit(sPartFit[i].data(),"N","",bbb[i].binStart,bbb[i].binEnd);

    fPart[i] -> ReleaseParameter(1);
//    fPart[i] -> ReleaseParameter(2);
    fPart[i] -> FixParameter(2, 0.0017);
    if(i==0)fPart[0] -> FixParameter(1, 2.9924);
    if(i==1)fPart[1] -> FixParameter(1, 3.9238);

    fPart[i] -> SetParLimits(0,0.,1e6);
    
    hPart[i] -> Fit(sPartFit[i].data(),"N","",bbb[i].binStart,bbb[i].binEnd);
    int nParam = 6; 

    for(int j=3; j<nParam; j++)
      fPart[i] -> FixParameter(j,fPart[i]->GetParameter(j));

    hPart[i] -> Fit(sPartFit[i].data(),"","",bbb[i].binStart,bbb[i].binEnd);
  }

  TF1 *fSInt[NParticles];
  TF1 *fBInt[NParticles];
  float sigB[NParticles];
  float RS[NParticles];
  for(int i=0; i<NParticles; i++)
  {
    if(!isDraw[i]) 
      continue;
    
    float start = fPart[i]->GetParameter(1) - 2*fPart[i]->GetParameter(2);
    float end   = fPart[i]->GetParameter(1) + 2*fPart[i]->GetParameter(2);

    sPartFit[i] += "int";
    fSInt[i] = new TF1(sPartFit[i].data(),"gaus",bbb[i].binStart,bbb[i].binEnd);
    for(int iPar=0; iPar<3; iPar++)
      fSInt[i] -> SetParameter(iPar, fPart[i] -> GetParameter(iPar));

    sPartFit[i] += "BG";

      fBInt[i] = new TF1(sPartFit[i].data(),"pol2",bbb[i].binStart,bbb[i].binEnd);
      for(int iPar=3; iPar<6; iPar++)
        fBInt[i] -> SetParameter(iPar-3, fPart[i] -> GetParameter(iPar));

    float S = fSInt[i] -> Integral(start,end);
    float B = fBInt[i] -> Integral(start,end);
    sigB[i] = S/B;
    RS[i] = S;
    float BinWidth = 0.5;
    int NBins = 1000;
    S = S*NBins*BinWidth;
    B = B*NBins*BinWidth;
    
    char MString[6], SigmaString[6], SString[6], SBString[6], SignString[6];
    sprintf(MString,"%1.1f",(fPart[i] -> GetParameter(1)*1000));
    sprintf(SigmaString,"%1.1f",(fPart[i] -> GetParameter(2)*1000));
    sprintf(SString,"%1.1f",S);
    
    LegendEntry[i][0] += " M = ";
    LegendEntry[i][0] += MString;
    LegendEntry[i][0] += " MeV/c^{2}";
    LegendEntry[i][1] = " #sigma = ";
    LegendEntry[i][1] += SigmaString;
    LegendEntry[i][1] += " 1.7 MeV/c^{2}";
    LegendEntry[i][1] += " S = ";

    if((S)<10)
      sprintf(SString,"%4.2f",(S));
    else if((S)<100)
      sprintf(SString,"%4.1f",(S));
    else
      sprintf(SString,"%4.0f",(S));
    LegendEntry[i][1] += SString;
    
    if((S/B)<10)
      sprintf(SBString,"%4.2f",(S/B));
    else if((S/B)<100)
      sprintf(SBString,"%4.1f",(S/B));
    else
      sprintf(SBString,"%4.0f",(S/B));
    
    if(S/sqrt(S+B)<10)
      sprintf(SignString,"%4.2f",(S/sqrt(S+B)));
    else if(S/sqrt(S+B)<100)
      sprintf(SignString,"%4.1f",(S/sqrt(S+B)));
    else
      sprintf(SignString,"%4.0f",(S/sqrt(S+B)));

    LegendEntry[i][2] += " S/B = ";
    LegendEntry[i][2] += SBString;
    LegendEntry[i][2] += "  S/#sqrt{(S+B)} = ";
    LegendEntry[i][2] += SignString;
    
    std::cout << "S    " << i << " " << pName[i] << "  " << S << std::endl;
    std::cout << "B    " << i << " " << pName[i] << "  " << B << std::endl;
    std::cout << "S/B  " << i << " " << pName[i] << "  " << S/B << std::endl;
    std::cout << "S/sqrt(S+B)  " << i << " " << pName[i] << "  " << S/sqrt(S+B) << std::endl;
  }

  TCanvas *c1;
  c1 = new TCanvas("c1","Particles",0,0,1200,1200);
  c1 -> UseCurrentStyle();
  c1 -> Divide(2,1);
//  c1 -> Divide(2,3);
//  c1 -> Divide(2,2);

  TLegend *legends[9];
  for(int i=0; i<9; i++)
  {
//     if(i==0 || i==1 || i==5)
      legends[i] = new TLegend(0.45, 0.85, 1, 1);
//     else
//       legends[i] = new TLegend(0.5, 0.7, .99, .99);
    legends[i]->SetFillColor(0);
    legends[i]->SetTextSize(0.043);
    legends[i]->SetMargin(0.1);
    legends[i]->SetTextFont(22);
  }
  
  int NPad=1;

  
  Bins binDraw[NParticles] = { {2.95, 3.25}, // 0
                               {3.85, 4.15}  // 6
  };    
#if 0  
  TGaxis::SetMaxDigits(3);
#endif  
  for(int i=0; i<NParticles; i++)
  {
    if(!isDraw[i]) 
      continue;
    
    if(i==0)c1->cd(1);
    if(i==1)c1->cd(2);
//     if(i==3)c1->cd(2);
//     if(i==4)c1->cd(5);
//     if(i==5)c1->cd(3);
//     if(i==6)c1->cd(6);
    //hPart[i] -> UseCurrentStyle();
    hPart[i] -> GetXaxis()-> SetTitle(AxisName[i].data());
#if 0
    hPart[i] -> GetXaxis()-> SetTitleSize(0.08);
    hPart[i] -> GetXaxis()-> SetTitleFont(22);
    hPart[i] -> GetXaxis()-> SetLabelSize(0.07);
    hPart[i] -> GetXaxis()-> SetTitleOffset(1);
    hPart[i] -> GetXaxis()-> SetLabelFont(22);
    hPart[i] -> GetXaxis()-> SetNdivisions(3,5,0);
#endif
    hPart[i] -> GetXaxis()-> SetRangeUser(binDraw[i].binStart, binDraw[i].binEnd);
    hPart[i] -> GetYaxis()-> SetTitle("Entries");
#if 0
    hPart[i] -> GetYaxis()-> SetTitleSize(0.08);
    hPart[i] -> GetYaxis()-> SetTitleFont(22);
    hPart[i] -> GetYaxis()-> SetLabelSize(0.07);
    hPart[i] -> GetYaxis()-> SetLabelFont(22);
    hPart[i] -> GetYaxis()-> SetTitleOffset(0.95);
    hPart[i] -> GetYaxis()-> SetNdivisions(3,5,0);
    hPart[i] -> SetLineWidth(2);
#endif
    hPart[i] -> SetMaximum(1.2*(hPart[i]->GetMaximum()));
    if(!isSame[i])
    {
      hPart[i] -> SetLineColor(kBlack);
      hPart[i] -> SetMarkerStyle(20);
      hPart[i] -> SetMarkerColor(kBlack);
      hPart[i] -> SetMarkerSize(0.9);
      hPart[i] -> Draw("pe");
//      hPart[i] -> Draw("h");
    }
    else
    {
      hPart[i] -> SetLineColor(kRed);
      hPart[i] -> SetMarkerColor(kRed);
      hPart[i] -> Draw("same");
    }
    
    legends[NPad-1]->AddEntry(hPart[i], LegendEntry[i][0].Data(),"pe");
//    legends[NPad-1]->AddEntry(hPart[i], LegendEntry[i][0].Data(),"");
    legends[NPad-1]->AddEntry(hEmpty,   LegendEntry[i][1].Data(),"l");
    legends[NPad-1]->AddEntry(hEmpty,   LegendEntry[i][2].Data(),"l");
    
//    if(NPad-1 == 0)legends[0]->AddEntry(hEmpty, "eff_{4#pi} = 27.4%","l"); //PHSD
//    if(NPad-1 == 0)legends[0]->AddEntry(hEmpty, "eff_{4#pi} = 29.9%","l"); //UrQMD
//    if(NPad-1 == 1)legends[1]->AddEntry(hEmpty, "eff_{4#pi} = 10.2%","l");
//    if(NPad-1 == 2)legends[2]->AddEntry(hEmpty, "eff_{4#pi} = 8.2%","l");
//    if(NPad-1 == 3)legends[3]->AddEntry(hEmpty, "eff_{4#pi} = 4.7%","l");
    
    TPaveText *pt = new TPaveText(.5,.7,.8,.85, "brNDC");   
    pt->SetTextSize(0.05);
    pt->SetTextFont(22);
    pt->SetTextColor(2);
    pt->SetFillColor(0);
    pt->SetBorderSize(0);
    pt->AddText(DecayText[i].Data());
    pt->Draw();
    
//    if(i<3)fPart[i] -> Draw("same");
    TString sss="S/B ";
    char sss1[6];
    sprintf(sss1,"%6.3f",sigB[i]);
    sss += sss1;
    TLatex sb;
    TLatex pn;
    pn.SetTextSize(0.04);
    pn.SetTextColor(kBlack);
//    pn.DrawLatex(0.9,0.9,sss.Data());
    
    if(!isSame[i+1] || i == (NParticles-1))
    {
      legends[NPad-1]->Draw();
      NPad++;
    }
  }

//   c1->SaveAs("Mass_H34_27GeV.pdf");
//   c1->SaveAs("Mass_H34_27GeV.png");
//    c1->SaveAs("Mass_H34_19GeV.pdf");
//    c1->SaveAs("Mass_H34_19GeV.png");
//      c1->SaveAs("Mass_H34_14GeV.pdf");
//      c1->SaveAs("Mass_H34_14GeV.png");
//    c1->SaveAs("Mass_H34_7GeV.pdf");
//    c1->SaveAs("Mass_H34_7GeV.png");
//    c1->SaveAs("Mass_H34_7fGeV.pdf");
//    c1->SaveAs("Mass_H34_7fGeV.png");
//    c1->SaveAs("Mass_H34_4fGeV.pdf");
//    c1->SaveAs("Mass_H34_4fGeV.png");
//     c1->SaveAs("Mass_H34_3fGeV.pdf");
//     c1->SaveAs("Mass_H34_3fGeV.png");
#if 0

  TCanvas *c2;
  c2 = new TCanvas("c2","cT",0,0,1200,1200);
  c2 -> UseCurrentStyle();
  c2 -> Divide(3,2);
  for(int i=0; i<NParticles; i++)
  {
    if(!isDraw[i]) 
      continue;
       hPartCT_S[i] -> SetLineColor(kBlack);
       hPartCT_S[i] -> SetMarkerStyle(20);
       hPartCT_S[i] -> SetMarkerColor(kBlack);
       hPartCT_S[i] -> SetMarkerSize(0.9);
//       hPartCT_S[i] -> Rebin(10);
       hPartCT_SCorr[i] -> Rebin(10);
       
    hPartCT_SR[i] -> GetXaxis()-> SetTitleSize(0.08);
    hPartCT_SR[i] -> GetXaxis()-> SetTitleFont(22);
    hPartCT_SR[i] -> GetXaxis()-> SetLabelSize(0.07);
    hPartCT_SR[i] -> GetXaxis()-> SetTitleOffset(1);
    hPartCT_SR[i] -> GetXaxis()-> SetLabelFont(22);
    hPartCT_SR[i] -> GetXaxis()-> SetNdivisions(5,5,0);
    hPartCT_SR[i] -> GetXaxis()-> SetRangeUser(0, 50);
    hPartCT_SR[i] -> GetXaxis()-> SetTitle("c#tau (cm)");
    hPartCT_SR[i] -> GetYaxis()-> SetTitle("Entries");
    hPartCT_SR[i] -> GetYaxis()-> SetTitleSize(0.08);
    hPartCT_SR[i] -> GetYaxis()-> SetTitleFont(22);
    hPartCT_SR[i] -> GetYaxis()-> SetLabelSize(0.07);
    hPartCT_SR[i] -> GetYaxis()-> SetLabelFont(22);
    hPartCT_SR[i] -> GetYaxis()-> SetTitleOffset(0.95);
    hPartCT_SR[i] -> GetYaxis()-> SetNdivisions(3,5,0);
    hPartCT_SR[i] -> SetLineWidth(2);
    
    hPartCT_S[i] -> SetLineColor(kBlue+3);
    hPartCT_S[i] -> SetLineWidth(3);
    hPartCT_S[i] -> SetFillColor(7);
    
    hEff_CT[i] -> GetXaxis()-> SetTitleSize(0.08);
    hEff_CT[i] -> GetXaxis()-> SetTitleFont(22);
    hEff_CT[i] -> GetXaxis()-> SetLabelSize(0.07);
    hEff_CT[i] -> GetXaxis()-> SetTitleOffset(1);
    hEff_CT[i] -> GetXaxis()-> SetLabelFont(22);
    hEff_CT[i] -> GetXaxis()-> SetNdivisions(5,5,0);
    hEff_CT[i] -> GetXaxis()-> SetRangeUser(0, 50);
    hEff_CT[i] -> GetXaxis()-> SetTitle("c#tau (cm)");
//    hEff_CT[i] -> GetYaxis()-> SetTitle("Entries");
    hEff_CT[i] -> GetYaxis()-> SetTitleSize(0.08);
    hEff_CT[i] -> GetYaxis()-> SetTitleFont(22);
    hEff_CT[i] -> GetYaxis()-> SetLabelSize(0.07);
    hEff_CT[i] -> GetYaxis()-> SetLabelFont(22);
    hEff_CT[i] -> GetYaxis()-> SetTitleOffset(0.95);
    hEff_CT[i] -> GetYaxis()-> SetNdivisions(3,5,0);
    hEff_CT[i] -> SetLineWidth(2);
    hEff_CT[i] -> SetLineColor(kBlue);
    hEff_CT[i] -> SetMarkerStyle(20);
    hEff_CT[i] -> SetMarkerColor(kBlue);
    hEff_CT[i] -> SetMarkerSize(0.9);
    
    hPartCT_SCorr[i] -> GetXaxis()-> SetTitleSize(0.08);
    hPartCT_SCorr[i] -> GetXaxis()-> SetTitleFont(22);
    hPartCT_SCorr[i] -> GetXaxis()-> SetLabelSize(0.07);
    hPartCT_SCorr[i] -> GetXaxis()-> SetTitleOffset(1);
    hPartCT_SCorr[i] -> GetXaxis()-> SetLabelFont(22);
    hPartCT_SCorr[i] -> GetXaxis()-> SetNdivisions(5,5,0);
    hPartCT_SCorr[i] -> GetXaxis()-> SetRangeUser(0, 50);
    hPartCT_SCorr[i] -> GetXaxis()-> SetTitle("c#tau (cm)");
    hPartCT_SCorr[i] -> GetYaxis()-> SetTitle("Entries");
    hPartCT_SCorr[i] -> GetYaxis()-> SetTitleSize(0.08);
    hPartCT_SCorr[i] -> GetYaxis()-> SetTitleFont(22);
    hPartCT_SCorr[i] -> GetYaxis()-> SetLabelSize(0.07);
    hPartCT_SCorr[i] -> GetYaxis()-> SetLabelFont(22);
    hPartCT_SCorr[i] -> GetYaxis()-> SetTitleOffset(0.95);
    hPartCT_SCorr[i] -> GetYaxis()-> SetNdivisions(3,5,0);
    hPartCT_SCorr[i] -> SetLineWidth(2);
#if 0    
    
    if(i==0){
    c2->cd(1);
    double ctauMin = 10.;
    double ctauMax = 50.;
//    hPartCT_SR[i] -> GetXaxis()-> SetTitle(AxisName[i].data());
//    hPartCT_SR[i] -> SetMaximum(1.2*(hPart[i]->GetMaximum()));      
    hPartCT_SR[i] -> Draw("");
//      hPartCT_BGR[i] -> Draw("same");
      hPartCT_S[i] -> Draw("same");
      
  TLegend* legendL = new TLegend(0.5, 0.8, 0.99, 0.99);
  legendL->SetFillColor(0);
  legendL->SetTextSize(0.08);
  legendL->SetTextFont(22);
  legendL->SetMargin(0.1);
  legendL->AddEntry(hPartCT_SR[i], " {}^{3}_{#Lambda}H S+BG","l");
  legendL->AddEntry(hPartCT_S[i], " {}^{3}_{#Lambda}H Signal","f");
 
  legendL->Draw();
      c2->cd(2);
      hEff_CT[i] -> Draw("");
      c2->cd(3);
      gPad->SetLogy();
      hPartCT_SCorr[i] -> Draw("");
      TF1 *expCT = new TF1("expCT","expo",ctauMin,ctauMax);
      expCT->SetLineColor(2);
      hPartCT_SCorr[i]->Fit("expCT","","",ctauMin,ctauMax);
      char ctauString[6], ctauErString[6];
      sprintf(ctauString,"%1.2f",-1./expCT->GetParameter(1));
      double dCT = expCT->GetParError(1);
      double MinCT = -1./(expCT->GetParameter(1) - dCT);
      TString ctau = "{}^{3}_{#Lambda}H_{c#tau} = ";
      ctau += ctauString;
      ctau += " #pm ";
      cout<<" H3L_ct = "<<ctauString<<"  +/- "<<-1./expCT->GetParameter(1)-MinCT<<endl;
      sprintf(ctauErString,"%0.2f",-1./expCT->GetParameter(1)-MinCT);
      ctau += ctauErString;
      ctau += " cm";
      
    TPaveText *pt = new TPaveText(.5,.5,.8,.7, "brNDC");   
    pt->SetTextSize(0.07);
    pt->SetTextFont(22);
    pt->SetTextColor(2);
    pt->SetFillColor(0);
    pt->SetBorderSize(0);
    pt->AddText(ctau);
    pt->Draw();
    }
    if(i==1){
    double ctauMin = 10.;
    double ctauMax = 45.;
      c2->cd(4);
      hPartCT_SR[i] -> Draw("");
      hPartCT_S[i] -> Draw("same");
  TLegend* legendXi = new TLegend(0.5, 0.8, 0.99, 0.99);
  legendXi->SetFillColor(0);
  legendXi->SetTextSize(0.08);
  legendXi->SetTextFont(22);
  legendXi->SetMargin(0.1);
  legendXi->AddEntry(hPartCT_SR[i], " {}^{4}_{#Lambda}H S+BG","l");
  legendXi->AddEntry(hPartCT_S[i], " {}^{4}_{#Lambda}H Signal","f");
 
  legendXi->Draw();
      c2->cd(5);
      hEff_CT[i] -> Draw("");
//      c2->Clear();
      c2->cd(6);
      gPad->SetLogy();
      hPartCT_SCorr[i] -> Draw("");
      TF1 *expCT = new TF1("expCT","expo",ctauMin,ctauMax);
      expCT->SetLineColor(2);
//      expCT -> SetParameter(0, 1000.);
//      expCT -> FixParameter(1, -0.2034);
      hPartCT_SCorr[i]->Fit("expCT","","",ctauMin,ctauMax);
      char ctauString[6], ctauErString[6];
      sprintf(ctauString,"%1.2f",-1./expCT->GetParameter(1));
//      sprintf(ctauString,"%1.1f",1./0.2034);
      double dCT = expCT->GetParError(1);
      double MinCT = -1./(expCT->GetParameter(1) - dCT);
      TString ctau = "{}^{4}_{#Lambda}H_{c#tau} = ";
      ctau += ctauString;
      ctau += " #pm ";
      cout<<" H4L_ct = "<<ctauString<<endl;
      sprintf(ctauErString,"%0.2f",-1./expCT->GetParameter(1)-MinCT);
      ctau += ctauErString;
      ctau += " cm";
    TPaveText *pt = new TPaveText(.5,.5,.8,.7, "brNDC");   
    pt->SetTextSize(0.07);
    pt->SetTextFont(22);
    pt->SetTextColor(2);
    pt->SetFillColor(0);
    pt->SetBorderSize(0);
    pt->AddText(ctau);
    pt->Draw();
    }    
#endif
  }
#endif
}
