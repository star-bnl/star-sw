/* 
   Imported from Maksym Zyzak 01/14/2020
   
 */
void DrawSignalBg7(TString nSt="") {
#if 1
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
  plain->SetPadRightMargin(0.05);
  plain->SetPadTopMargin(0.075);
  plain->SetPadBottomMargin(0.18);
  plain->cd();
#endif
#if 0
//   TString name1 = "../KFParticleFinder_1000M.root";
  TString name1 = "/home/mzyzak/data/STAR_2019/data/14.root";

  TFile *f1 = new TFile(name1.Data(),"read");
#else
  TFile *f1 = gDirectory;
#endif
  const int NParticles = 22;

  TH1D *hPart[NParticles];
  TF1  *fPart[NParticles];
  
  string sPart[NParticles];
  string sPartFit[NParticles];

  string ParNames[NParticles] = {"Particles/KFParticlesFinder/Particles/Ks/Parameters/M",        // 0  K0s
                                 "Particles/KFParticlesFinder/Particles/Lambda/Parameters/M",    // 1  Lambda
                                 "Particles/KFParticlesFinder/Particles/Lambdab/Parameters/M",   // 2  Lambdab
                                 "Particles/KFParticlesFinder/Particles/Xi-/Parameters/M",       // 3  Xi-
                                 "Particles/KFParticlesFinder/Particles/Xi+/Parameters/M",       // 4  Xi+
                                 "Particles/KFParticlesFinder/Particles/Omega-/Parameters/M",    // 5  Omega-
                                 "Particles/KFParticlesFinder/Particles/Omega+/Parameters/M",    // 6  Omega+
                                 "Particles/KFParticlesFinder/Particles/K*0/Parameters/M",       // 7  K*0
                                 "Particles/KFParticlesFinder/Particles/K*0b/Parameters/M",      // 8  K*0b
                                 "Particles/KFParticlesFinder/Particles/K*+/Parameters/M",       // 9  K*+
                                 "Particles/KFParticlesFinder/Particles/K*-/Parameters/M",       //10  K*-
                                 "Particles/KFParticlesFinder/Particles/Phi_{KK}/Parameters/M",  //11  phi
                                 "Particles/KFParticlesFinder/Particles/Sigma*+/Parameters/M",   //12  Sigma*+
                                 "Particles/KFParticlesFinder/Particles/Sigma*-/Parameters/M",   //13  Sigma*-
                                 "Particles/KFParticlesFinder/Particles/Sigma*+b/Parameters/M",  //14  Sigma*+b
                                 "Particles/KFParticlesFinder/Particles/Sigma*-b/Parameters/M",  //15  Sigma*-b
                                 "Particles/KFParticlesFinder/Particles/Xi*0/Parameters/M",      //16  Xi*0
                                 "Particles/KFParticlesFinder/Particles/Xi*0b/Parameters/M",     //17  Xi*0b
                                 "Particles/KFParticlesFinder/Particles/rho_{#pi#pi}/Parameters/M", //18  rho
                                 "Particles/KFParticlesFinder/Particles/#pi^{0}/Parameters/M", //19  pi0
                                 "Particles/KFParticlesFinder/Particles/H3L/Parameters/M",
                                 "Particles/KFParticlesFinder/Particles/H4L/Parameters/M" };   //20 H3L
  
                            //0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21
 bool isDraw[NParticles] = {  0,  1,  1,  1,  1,  1,  1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 };//LKsiOmega
  
  string pName[NParticles] = {"K0s      ",
                              "Lambda   ",
                              "Lambdab  ",
                              "Xi-      ",
                              "Xi+      ",
                              "Omega-   ",
                              "Omega+   ",
                              "K*0      ",
                              "K*0b     ",
                              "K*+      ",
                              "K*-      ",
                              "phi      ",
                              "Sigma*+  ",
                              "Sigma*-  ",
                              "Sigma*+b ",
                              "Sigma*-b ",
                              "Xi*0     ",
                              "Xi*0b    ",
                              "rho      ",
                              "pi0      ",
                              "H3L      ",
                              "H4L      "
  };

  string AxisName[NParticles];

  AxisName[ 0] = "m_{inv} {#pi^{+}#pi^{-}} [GeV/c^{2}]";
  AxisName[ 1] = "m_{inv} {p#pi^{-}} [GeV/c^{2}]";
  AxisName[ 2] = "m_{inv} {#bar{p}#pi^{+}} [GeV/c^{2}]";
  AxisName[ 3] = "m_{inv} {#Lambda#pi^{-}} [GeV/c^{2}]";
  AxisName[ 4] = "m_{inv} {#bar{#Lambda}#pi^{+}} [GeV/c^{2}]";
  AxisName[ 5] = "m_{inv} {#LambdaK^{-}} [GeV/c^{2}]";
  AxisName[ 6] = "m_{inv} {#bar{#Lambda}K^{+}} [GeV/c^{2}]";
  AxisName[ 7] = "m_{inv} {K^{+}#pi^{-}} [GeV/c^{2}]";
  AxisName[ 8] = "m_{inv} {K^{-}#pi^{+}} [GeV/c^{2}]";
  AxisName[ 9] = "m_{inv} {K^{0}_{s}#pi^{+}} [GeV/c^{2}]";
  AxisName[10] = "m_{inv} {K^{0}_{s}#pi^{-}} [GeV/c^{2}]";
  AxisName[11] = "m_{inv} {K^{+}K^{-}} [GeV/c^{2}]";
  AxisName[12] = "m_{inv} {#Lambda#pi^{+}} [GeV/c^{2}]";
  AxisName[13] = "m_{inv} {#Lambda#pi^{-}} [GeV/c^{2}]";
  AxisName[14] = "m_{inv} {#bar{#Lambda}#pi^{-}} [GeV/c^{2}]";
  AxisName[15] = "m_{inv} {#bar{#Lambda}#pi^{+}} [GeV/c^{2}]";
  AxisName[16] = "m_{inv} {#Xi^{-}#pi^{+}} [GeV/c^{2}]";
  AxisName[17] = "m_{inv} {#Xi^{+}#pi^{-}} [GeV/c^{2}]";
  AxisName[18] = "m_{inv} {#pi^{+}#pi^{-}} [GeV/c^{2}]";
  AxisName[19] = "m_{inv} {#gamma_{e^{+}e^{-}}#gamma_{e^{+}e^{-}} [GeV/c^{2}]";
  AxisName[20] = "m_{inv} {^{3}He#pi^{-}} [GeV/c^{2}]";
  AxisName[21] = "m_{inv} {^{4}He#pi^{-}} [GeV/c^{2}]";

 
  TString LegendEntry[NParticles][2];
  
  LegendEntry[ 0][0] = "K_{s}^{0}";
  LegendEntry[ 1][0] = "#Lambda";
  LegendEntry[ 2][0] = "#bar{#Lambda}";
  LegendEntry[ 3][0] = "#Xi^{-}"; 
  LegendEntry[ 4][0] = "#bar{#Xi}^{+}";
  LegendEntry[ 5][0] = "#Omega^{-}";
  LegendEntry[ 6][0] = "#bar{#Omega}^{+}";
  LegendEntry[ 7][0] = "K^{*0}(892)";
  LegendEntry[ 8][0] = "#bar{K}^{*0}(892)";
  LegendEntry[ 9][0] = "K^{*+}(892)";
  LegendEntry[10][0] = "K^{*-}(892)";
  LegendEntry[11][0] = "#phi(1020)";
  LegendEntry[12][0] = "#Sigma^{+}(1385)";
  LegendEntry[13][0] = "#Sigma^{-}(1385)";
  LegendEntry[14][0] = "#bar{#Sigma}^{-}(1385)";
  LegendEntry[15][0] = "#bar{#Sigma}^{+}(1385)";
  LegendEntry[16][0] = "#Xi^{0}(1530)";
  LegendEntry[17][0] = "#bar{#Xi}^{0}(1530)";  
  LegendEntry[18][0] = "#rho(770)";  
  LegendEntry[19][0] = "#pi^{0}(134.9)";  
  LegendEntry[20][0] = "{}^{3}_{#Lambda}H";
  LegendEntry[21][0] = "{}^{4}_{#Lambda}H";
  
  TString DecayText[NParticles];
  DecayText[ 0] = "K_{s}^{0}#rightarrow#pi^{+}#pi^{-}";
  DecayText[ 1] = "#Lambda#rightarrow p#pi^{-}";
  DecayText[ 2] = "#bar{#Lambda}#rightarrow#bar{p}#pi^{+}";
  DecayText[ 3] = "#Xi^{-}#rightarrow#Lambda#pi^{-}";
  DecayText[ 4] = "#bar{#Xi}^{+}#rightarrow#bar{#Lambda}#pi^{+}";
  DecayText[ 5] = "#Omega^{-}#rightarrow#LambdaK^{-}";
  DecayText[ 6] = "#bar{#Omega}^{+}#rightarrow#bar{#Lambda}K^{+}";
  DecayText[ 7] = "K^{*0}(892)#rightarrowK^{+}#pi^{-}";
  DecayText[ 8] = "#bar{K}^{*0}(892)#rightarrowK^{-}#pi^{+}";
  DecayText[ 9] = "K^{*+}(892)#rightarrowK^{0}_{s}#pi^{+}";
  DecayText[10] = "K^{*-}(892)#rightarrowK^{0}_{s}#pi^{-}";
  DecayText[11] = "#phi(1020)#rightarrowK^{+}K^{-}";
  DecayText[12] = "#Sigma^{+}(1385)#rightarrow#Lambda#pi^{+}";
  DecayText[13] = "#Sigma^{-}(1385)#rightarrow#Lambda#pi^{-}";
  DecayText[14] = "#bar{#Sigma}^{-}(1385)#rightarrow#bar{#Lambda}#pi^{-}";
  DecayText[15] = "#bar{#Sigma}^{+}(1385)#rightarrow#bar{#Lambda}#pi^{+}";
  DecayText[16] = "#Xi^{0}(1530)#rightarrow#Xi^{-}#pi^{+}";
  DecayText[17] = "#bar{#Xi}^{0}(1530)#rightarrow#Xi^{+}#pi^{-}";
  DecayText[18] = "#rho(770)#rightarrow#pi^{+}#pi^{-}";
  DecayText[19] = "#pi^{0}#rightarrow#gamma_{e^{+}e^{-}}#gamma_{e^{+}e^{-}";
  DecayText[20] = "{}^{3}_{#Lambda}H#rightarrow^{3}He#pi^{-}";
  DecayText[21] = "{}^{4}_{#Lambda}H#rightarrow^{4}He#pi^{-}";
  
  for(int i=0; i<NParticles; i++)
  {
    sPart[i]    = ParNames[i];
    sPartFit[i] = ParNames[i]+"GaussFit";
  }

  struct Bins 
  {
    float binStart, binEnd;
  };

  Bins bbb[NParticles] = { {0.45, 0.55},   // 0 K0
                           {1.10, 1.15}, // 1 Lambda
                           {1.10, 1.15}, // 2 Lambdab 
                           {1.3, 1.37},   // 3 Xi-
                           {1.3, 1.37},   // 4 Xi+
                           {1.64, 1.73},  // 5 Omega-
                           {1.64, 1.73},  // 6 Omega+
                           {0.79, 1.04},   // 7 K*0
                           {0.79, 1.04},   // 8 K*0b
                           {0.79, 1.04},   // 9 K*+
                           {0.79, 1.04},   //10 K*-
                           {0.99, 1.06},   //11 phi
                           {1.35, 1.42},   //12 Sigma*+
                           {1.35, 1.42},   //13 Sigma*-
                           {1.35, 1.42},   //14 Sigma*+b
                           {1.35, 1.42},   //15 Sigma*-b
                           {1.50, 1.58},    //16 Xi*0
                           {1.50, 1.58},   //17 Xi*0b
                           {0.60, 0.90},   //18 rho
                           {0.05, 0.25},      //19 pi0
                           {2.975, 3.02}, //20 H3L
                           {3.9, 3.96} //21 H4L
  };
  
  Bins binDraw[NParticles] = { {0.45, 0.65},   // 0 K0
                               {1.095, 1.17}, // 1 Lambda
                               {1.095, 1.17}, // 2 Lambdab 
                               {1.3, 1.38},     // 3 Xi-
                               {1.3, 1.38},     // 4 Xi+
                               {1.65, 1.75},  // 5 Omega-
                               {1.65, 1.75},  // 6 Omega+
                               {0.5, 1.8},     // 7 K*0
                               {0.5, 1.8},     // 8 K*0b
                               {0.5, 1.8},     // 9 K*+
                               {0.5, 1.8},     //10 K*-
                               {0.95, 2.5},   //11 phi
                               {1.25, 1.7},    //12 Sigma*+
                               {1.25, 1.7},    //13 Sigma*-
                               {1.25, 1.7},    //14 Sigma*+b
                               {1.25, 1.7},    //15 Sigma*-b
                               {1.5, 1.8},     //16 Xi*0
                               {1.5, 1.8},    //17 Xi*0b
                               {0.3,1.2},    //18 rho
                               {0.,1.0 },   //19 pi0
                               {2.975, 3.055}, //20 H3L
                               {3.85, 4.15 } //21 H4L
  };

  struct InitPar
  {
    float m,s;
  };
  
  InitPar initPar[NParticles] = 
  {
    {0.498, 0.0023}, // 0 K0
    {1.116, 0.0015}, // 1 Lambda
    {1.116, 0.0015}, // 2 Lambdab 
    {1.321, 0.002},  // 3 Xi-
    {1.321, 0.002},  // 4 Xi+
    {1.672, 0.0025},  // 5 Omega-
    {1.672, 0.0025},  // 6 Omega+
    {0.892, 0.021},  // 7 K*0
    {0.892, 0.021},  // 8 K*0b
    {0.892, 0.021},  // 9 K*+
    {0.892, 0.021},  //10 K*-
    {1.018, 0.0013}, //11 phi
    {1.385, 0.0027}, //12 Sigma*+
    {1.385, 0.0027}, //13 Sigma*-
    {1.385, 0.0027}, //14 Sigma*+b
    {1.385, 0.0027}, //15 Sigma*-b
    {1.54, 0.002},   //16 Xi*0
    {1.54, 0.002},    //17 Xi*0b
    {0.776, 0.02},    //18 rho
    {0.135, 0.02},   //19 pi0
    {2.9925, 2.0e-03}, //20 H3L
    {3.925, 2.0e-3}  //21 H3L
  };    
  
  for(int i=0; i<NParticles; i++)
  {
        if(!isDraw[i]) 
      continue;
    hPart[i] = (TH1D*) f1->Get(sPart[i].data());
//     if(i<3 || i==6) hPart[i] -> Rebin();
    hPart[i] -> SetLineColor(1);
  
    fPart[i] = new TF1(sPartFit[i].data(),"gaus(0)+pol2(3)",bbb[i].binStart,bbb[i].binEnd);
    fPart[i] -> SetParNames("Const","Mass","Sigma","a0","a1","a2");

    fPart[i] -> SetNpx(10000);
    fPart[i] -> SetLineColor(2);
    fPart[i] -> SetLineWidth(1);
    fPart[i] -> SetParameter(1,(bbb[i].binStart + bbb[i].binEnd)/2);
    fPart[i] -> SetParameter(2,(bbb[i].binEnd - bbb[i].binStart)/4);
    fPart[i] -> SetParLimits(2,0,1);

//    if(i==5)fPart[i] -> FixParameter(0, 13.);
//    if(i==11)fPart[i] -> FixParameter(2, 0.2854);
    fPart[i] -> FixParameter(1, initPar[i].m);
    fPart[i] -> FixParameter(2, initPar[i].s);
    hPart[i] -> Fit(sPartFit[i].data(),"N","",bbb[i].binStart,bbb[i].binEnd);
//    if(i<5||i>6)fPart[i] -> ReleaseParameter(1);
//    if(i<5||i>6)fPart[i] -> ReleaseParameter(2);
//    if(i!=6)fPart[i] -> ReleaseParameter(1);
//    if(i!=6)fPart[i] -> ReleaseParameter(2);
    fPart[i] -> ReleaseParameter(1);
//     if(i!=21)
    fPart[i] -> ReleaseParameter(2);
//    if(i!=11)fPart[i] -> ReleaseParameter(2);
    
    hPart[i] -> Fit(sPartFit[i].data(),"N","",bbb[i].binStart,bbb[i].binEnd);
    int nParam = 6; 

    for(int j=3; j<nParam; j++)
      fPart[i] -> FixParameter(j,fPart[i]->GetParameter(j));

    hPart[i] -> Fit(sPartFit[i].data(),"","",bbb[i].binStart,bbb[i].binEnd);
  }

  TF1 *fSInt[NParticles];
  TF1 *fBInt[NParticles];
  float sigB[NParticles];
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

    double binWidth = hPart[i]->GetXaxis()->GetNbins()/(hPart[i]->GetXaxis()->GetXmax() - hPart[i]->GetXaxis()->GetXmin());
    float S = fSInt[i] -> Integral(start,end)*binWidth;
    float B = fBInt[i] -> Integral(start,end)*binWidth;
    sigB[i] = S/B;

    char tmpString[6];
    sprintf(tmpString,"%1.1f",(fPart[i] -> GetParameter(1)*1000));
    LegendEntry[i][0] = "M = ";
    LegendEntry[i][0] += tmpString;
    LegendEntry[i][0] += " MeV/c^{2}";
    sprintf(tmpString,"%1.1f",(fPart[i] -> GetParameter(2)*1000));
    
    LegendEntry[i][0] += " #sigma = ";
    LegendEntry[i][0] += tmpString;
    LegendEntry[i][0] += " MeV/c^{2}";
    LegendEntry[i][1] = "S/B = ";
    if((S/B)<10)
      sprintf(tmpString,"%4.2f",(S/B));
    else if((S/B)<100)
      sprintf(tmpString,"%4.1f",(S/B));
    else
      sprintf(tmpString,"%4.0f",(S/B));
    LegendEntry[i][1] += tmpString;
    
    LegendEntry[i][1] += "   S/#sqrt{S+B} = ";
    sprintf(tmpString,"%1.1f",(S/sqrt(S+B)));
    LegendEntry[i][1] += tmpString;
    
    std::cout << "S    " << i << " " << pName[i] << "  " << S << std::endl;
    std::cout << "B    " << i << " " << pName[i] << "  " << B << std::endl;
    std::cout << "S/B  " << i << " " << pName[i] << "  " << S/B << std::endl;
  }

  TCanvas *c3;
  c3 = new TCanvas("c3","Particles",0,0,1500,800);
  c3 -> UseCurrentStyle();
//  c3 -> Divide(3,3);
  c3 -> Divide(3,2);
//  c3 -> Divide(2,2);

  TLegend *legends[9];
  for(int i=0; i<9; i++)
  {
//     if(i==0 || i==1 || i==5)
      legends[i] = new TLegend(0.26, 0.8, .99, .99);
//     else
//       legends[i] = new TLegend(0.5, 0.7, .99, .99);
    legends[i]->SetFillColor(0);
    legends[i]->SetTextSize(0.058);
    legends[i]->SetMargin(0.02);
  }
  
  int NPad=1;
  
  
  TGaxis::SetMaxDigits(3);
                            //0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  
//   int padMap[NParticles] = {  0,  1,  4,  2,  5,  3,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0 };
  int padMap[NParticles] = {  0,  1,  4,  2,  5,  3,  6,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  3,  6 };
  
  for(int i=0; i<NParticles; i++)
  {
    if(!isDraw[i]) 
      continue;
    
    c3->cd(padMap[i]);
    //hPart[i] -> UseCurrentStyle();
    hPart[i] -> GetXaxis()-> SetTitle(AxisName[i].data());
    hPart[i] -> GetXaxis()-> SetTitleSize(0.08);
    hPart[i] -> GetXaxis()-> SetTitleFont(22);
    hPart[i] -> GetXaxis()-> SetLabelSize(0.07);
    hPart[i] -> GetXaxis()-> SetTitleOffset(1);
    hPart[i] -> GetXaxis()-> SetLabelFont(22);
    hPart[i] -> GetXaxis()-> SetNdivisions(3,5,0);
    hPart[i] -> GetXaxis()-> SetRangeUser(binDraw[i].binStart, binDraw[i].binEnd);
    hPart[i] -> GetYaxis()-> SetTitle("Entries");
    hPart[i] -> GetYaxis()-> SetTitleSize(0.08);
    hPart[i] -> GetYaxis()-> SetTitleFont(22);
    hPart[i] -> GetYaxis()-> SetLabelSize(0.07);
    hPart[i] -> GetYaxis()-> SetLabelFont(22);
    hPart[i] -> GetYaxis()-> SetTitleOffset(0.85);
    hPart[i] -> GetYaxis()-> SetNdivisions(3,5,0);
    hPart[i] -> GetYaxis()-> SetRangeUser(0, 1.3*TMath::Max(hPart[i]->GetMaximum(),fPart[i]->GetMaximum()));
    hPart[i] -> SetLineWidth(1.5);
    hPart[i] -> Draw("same");
    
    legends[NPad-1]->AddEntry(hPart[i], LegendEntry[i][0].Data(),"");
    legends[NPad-1]->AddEntry(hPart[i],   LegendEntry[i][1].Data(),"");

    TPaveText *pt = new TPaveText(.6,.7,.6,.7, "brNDC");   
    pt->SetTextSize(0.10);
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
    pn.SetTextSize(0.07);
    pn.SetTextColor(kBlack);
//     if(i<4) pn.DrawLatex(0.9,0.9,sss.Data());
    
    legends[NPad-1]->Draw();
    NPad++;
  }
  TString pngName = "Hyperons.png";
  //   pngName += gDirectory->GetName();
  //  pngName.ReplaceAll(".root",".png");
  c3->SaveAs(pngName);
}
