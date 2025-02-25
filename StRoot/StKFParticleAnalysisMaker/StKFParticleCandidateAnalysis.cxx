#include "StKFParticleCandidateAnalysis.h"

#include "TChain.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TDirectory.h"
#include "TMath.h"
#include "TH1D.h"
#include "TH2D.h"
#include "TH3D.h"

#include <vector>
#include <iostream>
#include <iomanip>

#include "KFParticleSIMD.h"

ClassImp(StKFParticleCandidateAnalysis)

StKFParticleCandidateAnalysis::StKFParticleCandidateAnalysis(TString inputFile):
  fCandidateFileChain(new TChain("Candidates")), fCandidate(new KFParticle),
  fOutputHistoFile(nullptr), fOutputHistoDir(nullptr)
{
  fCandidateFileChain->Add(inputFile);
  fCandidateFileChain->SetBranchStatus("*",0);
  fCandidateFileChain->SetBranchAddress("Candidates", &fCandidate);
  fCandidateFileChain->SetBranchStatus("Candidates", 1);
  
  TFile* curFile = gFile;
  TDirectory* curDirectory = gDirectory;

#if 1
  // fPdgMap[3004] = 0;
  // fPdgMap[3005] = 1;
  // fPdgMap[3006] = 2;
  // fPdgMap[3007] = 3;
  // fPdgMap[3012] = 4;
  // fPdgMap[3013] = 5;

  // fPdgMap[3003] = 6;
  // fPdgMap[3103] = 7;
  // fPdgMap[3016] = 8;
  // fPdgMap[3019] = 9;
  // fPdgMap[3022] = 10;
  // fPdgMap[3025] = 11;

  // fPdgMap[3203] = 12;
  // fPdgMap[3040] = 13;
  fPdgMap[3008] = 14;
  // fPdgMap[3009] = 15;
  // fPdgMap[3010] = 16;
  // fPdgMap[3011] = 17;
#else
  fPdgMap[1003006] = 18;
  fPdgMap[1003007] = 19;
  fPdgMap[1003004] = 20;
  fPdgMap[1003005] = 21;
#endif
  // fPdgMap[ 3312] = 22;
  // fPdgMap[-3312] = 23;
  // fPdgMap[ 3334] = 24;
  // fPdgMap[-3334] = 25;

  const float pi = TMath::Pi();

  //                                0       1      2       3       4          5          6     7      8      9       10      11      12     13      14      15       16      17       18       19       20      21      22     23     24        25
  TString particleName[NParticles]{ "H3L",  "H4L", "He4L", "He5L", "H3Ldppi", "H4Ltppi", "LN", "LNN", "H6L", "He6L", "He7L", "Li7L", "LLN", "LLNN", "H4LL", "H4LL3", "H5LL", "He6LL", "He4L*", "He5L*", "H3L*", "H4L*", "Xi-", "Xi+", "Omega-", "Omega+"};
  float minM[NSets][NParticles]{  {  2.94,   3.86,   3.86,   4.81,      2.94,      3.86, 2.02,  2.94,  5.74,   5.74,   6.67,   6.67,  3.13,   4.06,   4.06,    4.06,   4.99,    5.95,    3.86,    4.81,   2.94,   3.86,  1.22,  1.22,     1.66,     1.66}, 
                                  {   0.1,    0.1,    0.1,    0.1,       0.1,       0.1,  0.1,   0.1,   0.1,    0.1,    0.1,    0.1,   0.1,    0.1,    0.1,     0.1,    0.1,     0.1,     2.7,     3.6,    1.8,    2.7,   0.1,   0.1,      0.4,      0.4}, 
                                  {   2.7,    3.6,    2.7,    3.6,       1.8,       2.7,  1.8,   2.7,   5.5,    5.5,    6.5,    6.5,  2.94,   3.86,   3.86,    2.94,   4.81,    4.81,    1.07,    1.07,   1.07,   1.07,   1.0,   1.0,      1.0,      1.0}, 
                                  {     0,      0,   0.93,   0.93,      0.93,      0.93,    0,     0,     0,      0,      0,      0,     0,      0,    0.1,    0.93,      0,    0.93,       0,       0,      0,      0,     0,     0,        0,        0}, 
                                  {     0,      0,   1.07,   1.07,      1.07,      1.07,    0,     0,     0,      0,      0,      0,     0,      0,    2.7,    1.07,      0,    1.07,       0,       0,      0,      0,     0,     0,        0,        0},
                                  {     0,      0,   3.86,   4.81,      2.94,      3.86,    0,     0,     0,      0,      0,      0,     0,      0,   0.93,    4.06,      0,    5.95,       0,       0,      0,      0,     0,     0,        0,        0},
                                  {     0,      0,   2.94,   3.86,      2.01,      2.94,    0,     0,     0,      0,      0,      0,     0,      0,   1.07,    3.13,      0,    4.99,       0,       0,      0,      0,     0,     0,        0,        0},
                                  {     0,      0,   3.74,   4.66,      2.81,      3.74,    0,     0,     0,      0,      0,      0,     0,      0,   1.07,    3.92,      0,    5.81,       0,       0,      0,      0,     0,     0,        0,        0}
  };                                                                                                                                        
  float maxM[NSets][NParticles]{  {  3.19,   4.11,   4.11,   5.06,      3.19,      4.11, 2.27,  3.19,  5.95,   5.95,   6.92,   6.92,  3.38,   4.31,   4.31,    4.31,   5.24,    6.20,    4.11,    5.06,   3.19,   4.11,  1.42,  1.42,     1.68,     1.68},
                                  {   0.2,    0.2,    0.2,    0.2,       0.2,       0.2,  0.2,   0.2,   0.2,    0.2,    0.2,    0.2,   0.2,    0.2,    0.2,     0.2,    0.2,     0.2,     2.9,     3.8,    2.0,    2.9,   0.2,   0.2,      0.6,      0.6},
                                  {   2.9,    3.8,    2.9,    3.8,       2.0,       2.9,  2.0,   2.9,   5.7,    5.7,    6.7,    6.7,  3.19,   4.11,   4.11,    3.19,   5.06,    5.06,    1.32,    1.32,   1.32,   1.32,   1.2,   1.2,      1.2,      1.2},
                                  {     0,      0,   0.95,   0.95,      0.95,      0.95,    0,     0,     0,      0,      0,      0,     0,      0,    0.2,    0.95,      0,    0.95,       0,       0,      0,      0,     0,     0,        0,        0},
                                  {     0,      0,   1.32,   1.32,      1.32,      1.32,    0,     0,     0,      0,      0,      0,     0,      0,    2.9,    1.32,      0,    1.32,       0,       0,      0,      0,     0,     0,        0,        0},
                                  {     0,      0,   4.11,   5.06,      3.19,      4.11,    0,     0,     0,      0,      0,      0,     0,      0,   0.95,    4.31,      0,    6.20,       0,       0,      0,      0,     0,     0,        0,        0},
                                  {     0,      0,   3.19,   4.11,      2.26,      3.19,    0,     0,     0,      0,      0,      0,     0,      0,   1.32,    3.38,      0,    5.24,       0,       0,      0,      0,     0,     0,        0,        0},
                                  {     0,      0,   3.99,   4.91,      3.06,      3.99,    0,     0,     0,      0,      0,      0,     0,      0,   1.32,    4.17,      0,    6.06,       0,       0,      0,      0,     0,     0,        0,        0}
  };

  TString signalSet[NSignalSets]{"Total", "Signal", "BG"};
  TString setNames[NParticles][NSets]{ 
    {"mother", "pi", "He3",  "",     "",      "",    "",     ""     }, // H3L
    {"mother", "pi", "He4",  "",     "",      "",    "",     ""     }, // H4L
    {"mother", "pi", "He3",  "p",    "ppi",   "fl",  "fpi",  "fp"   }, // He4L
    {"mother", "pi", "He4",  "p",    "ppi",   "fl",  "fpi",  "fp"   }, // He5L
    {"mother", "pi", "d",    "p",    "ppi",   "fl",  "fpi",  "fp"   }, // H3L -> dppi
    {"mother", "pi", "t",    "p",    "ppi",   "fl",  "fpi",  "fp"   }, // H4L -> tppi
    {"mother", "pi", "d",    "",     "",      "",    "",     ""     }, // Ln
    {"mother", "pi", "t",    "",     "",      "",    "",     ""     }, // Lnn
    {"mother", "pi", "He6",  "",     "",      "",    "",     ""     }, // H6L
    {"mother", "pi", "Li6",  "",     "",      "",    "",     ""     }, // He6L
    {"mother", "pi", "Li7",  "",     "",      "",    "",     ""     }, // He7L
    {"mother", "pi", "Be7",  "",     "",      "",    "",     ""     }, // Li7L
    {"mother", "pi", "H3L",  "",     "",      "",    "",     ""     }, // LLn
    {"mother", "pi", "H4L",  "",     "",      "",    "",     ""     }, // LLnn
    {"mother", "pi", "He4L", "pi_d", "He3_d", "p_d", "ppi1", "ppi2" }, // H4LL -> He4L pi
    {"mother", "pi", "H3L",  "p",    "ppi",   "fl",  "fpi",  "fp"   }, // H4LL -> H3L p pi
    {"mother", "pi", "He5L", "",     "",      "",    "",     ""     }, // H5LL
    {"mother", "pi", "He5L", "p",    "ppi",   "fl",  "fpi",  "fp"   }, // He6LL
    {"mother", "He3","L",    "",     "",      "",    "",     ""     }, // He4L*
    {"mother", "He4","L",    "",     "",      "",    "",     ""     }, // He5L*
    {"mother", "d",  "L",    "",     "",      "",    "",     ""     }, // H3L*
    {"mother", "t",  "L",    "",     "",      "",    "",     ""     }, // H4L*
    {"mother", "pi", "L",    "",     "",      "",    "",     ""     }, // Xi-
    {"mother", "pi", "L",    "",     "",      "",    "",     ""     }, // Xi+
    {"mother", "K",  "L",    "",     "",      "",    "",     ""     }, // Omega-
    {"mother", "K",  "L",    "",     "",      "",    "",     ""     }, // Omega+
  };
  //                         0        1        2        3        4   5   6   7   8        9        10       11       12                13                14                15                 16                 17                 18       19       20  21  22         23               24         25   
  TString f1Name[NParticles]{"^{3}He","^{4}He","^{3}He","^{4}He","d","t","d","t","^{6}He","^{6}Li","^{7}Li","^{7}Be","^{3}_{#Lambda}H","^{4}_{#Lambda}H","^{4}_{#Lambda}He","^{3}_{#Lambda}H","^{5}_{#Lambda}He","^{5}_{#Lambda}He","^{3}He","^{4}He","d","t","#Lambda", "#bar{#Lambda}", "#Lambda", "#bar{#Lambda}"};
  //                         0  1  2   3   4   5   6  7  8  9  10 11 12 13 14 15  16 17  18 19 20 21 22 23 24 25
  TString f2Name[NParticles]{"","","p","p","p","p","","","","","","","","","","p","","p","","","","","","","",""};
  

  TString histoNames[NHistos-1]{"x", "y", "z", "px", "py", "pz", "pt", "p",
                                "dx", "dy", "dz", "dpx", "dpy", "dpz", "dpt", "dp",
                                "chi", "chiTopo", "chiPrim", "chiPrimXY", "l", "dl", "ldl", "r",
                                "ctau", "decayL", "phi", "theta", "rapidity", "mt", "extra"};
  //                             1     2     3     4     5     6     7     8
  const int nBins[NHistos-1] {  200,  200,  500,  200,  200,  400,   50,  200,
                                100,  100,  100,  100,  100,  100,  100,  100,
                                200,  200, 1000, 1000,  200,  100, 1000,  100,
                                100,  100,  100,  100,   60,   50, 1000
  };
  const float minX[NHistos-1]{ -100, -100, -250,  -10,  -10,  -20,    0,    0,
                                  0,    0,    0,    0,    0,    0,    0,    0,
                                  0,    0,    0,    0,    0,    0,    0,    0,
                                  0,    0,  -pi,    0,   -3,    0,    0
  };
  const float maxX[NHistos-1]{  100,  100,  250,   10,   10,   20,    5,   20,
                                 10,   10,   10,  0.2,  0.2,  0.2,  0.2,  0.2,
                                 20,   20, 1000, 1000,  200,   10,  100,   50,
                                100,  100,   pi,   pi,    3,    5,    1
  };

  TString outFileName = inputFile.ReplaceAll(".root", "_histos.root");

  fOutputHistoFile = new TFile(outFileName, "RECREATE");
  fOutputHistoDir = fOutputHistoFile->mkdir("Particles");
  fOutputHistoDir->cd();
  {
    for(int iParticle=0; iParticle<NParticles; iParticle++) {

      bool isParticleAdded = false;
      for (auto it = fPdgMap.begin(); it != fPdgMap.end(); ++it) {
        if(it->second == iParticle) {
          isParticleAdded = true;
          break;
        }
      }
      if(!isParticleAdded) continue;

      gDirectory->mkdir(particleName[iParticle]);
      gDirectory->cd(particleName[iParticle]);
      for(int iSignalSet=0; iSignalSet<NSignalSets; iSignalSet++) {
        gDirectory->mkdir(signalSet[iSignalSet]);
        gDirectory->cd(signalSet[iSignalSet]);
        for(int iSet=0; iSet<NSets; iSet++) {
          if(setNames[iParticle][iSet].IsNull()) continue;
          gDirectory->mkdir(setNames[iParticle][iSet]);
          gDirectory->cd(setNames[iParticle][iSet]);
          {
            gDirectory->mkdir("parameters");
            gDirectory->cd("parameters");
            for(int iHisto=0; iHisto<8; iHisto++) {
              fHistos[iParticle][iSignalSet][iSet][iHisto] = 
                new TH1D(histoNames[iHisto], histoNames[iHisto], nBins[iHisto], minX[iHisto], maxX[iHisto]);
            }
            gDirectory->cd("..");
            gDirectory->mkdir("errors");
            gDirectory->cd("errors");
            for(int iHisto=8; iHisto<16; iHisto++) {
              fHistos[iParticle][iSignalSet][iSet][iHisto] = 
                new TH1D(histoNames[iHisto], histoNames[iHisto], nBins[iHisto], minX[iHisto], maxX[iHisto]);
            }
            gDirectory->cd("..");
            for(int iHisto=16; iHisto<NHistos-1; iHisto++) {
              fHistos[iParticle][iSignalSet][iSet][iHisto] = 
                new TH1D(histoNames[iHisto], histoNames[iHisto], nBins[iHisto], minX[iHisto], maxX[iHisto]);
            }
            fHistos[iParticle][iSignalSet][iSet][NHistos-1] = 
              new TH1D("M", "M", 1000, minM[iSet][iParticle], maxM[iSet][iParticle]);
          }
          gDirectory->cd("..");
        }
        gDirectory->cd("..");
      }
      const int nBinsM = 250;
      //ctau - M
      fHistos2D[iParticle][0] = new TH2D("ctauM", "ctauM", 
                                nBins[24], minX[24], maxX[24],
                                nBinsM, minM[0][iParticle], maxM[0][iParticle]);
      fHistos2D[iParticle][0]->GetXaxis()->SetTitle("c#tau");
      fHistos2D[iParticle][0]->GetYaxis()->SetTitle("m [GeV/c]");

      fHistos2D[iParticle][1] = new TH2D("Armenteros", "Armenteros", 
                                4000, -2.f, 2.f,
                                1000,  0.f, 1.f);
      fHistos2D[iParticle][1]->GetXaxis()->SetTitle("#alpha (p_{L}^{+}-p_{L}^{-})/(p_{L}^{+}+p_{L}^{-})");
      fHistos2D[iParticle][1]->GetYaxis()->SetTitle("q_{t} [GeV/c]");

      fHistos2D[iParticle][2] = new TH2D("ChiTopoVsM", "ChiTopoVsM", 
                                100, 0.f, 1.f,
                                nBinsM, minM[0][iParticle], maxM[0][iParticle]);
      fHistos2D[iParticle][2]->GetXaxis()->SetTitle("prob");
      fHistos2D[iParticle][2]->GetYaxis()->SetTitle("M [GeV^{2}/c^{2}]");

      fHistos2D[iParticle][3] = new TH2D("MPl", "MPl", 
                                400, -2.f, 2.f,
                                nBinsM, minM[0][iParticle], maxM[0][iParticle]);
      fHistos2D[iParticle][3]->GetXaxis()->SetTitle("#alpha (p_{L}^{+}-p_{L}^{-})/(p_{L}^{+}+p_{L}^{-})");
      fHistos2D[iParticle][3]->GetYaxis()->SetTitle("m [GeV/c]");

      //y-pt-M and y-mt-M
      fHistos3D[iParticle][0] = new TH3D("y-pt-m", "y-pt-m",
                                nBins[28], minX[28], maxX[28],
                                nBins[6], minX[6], maxX[6],
                                nBinsM, minM[0][iParticle], maxM[0][iParticle]);
      fHistos3D[iParticle][1] = new TH3D("y-mt-m", "y-mt-m",
                                nBins[28], minX[28], maxX[28],
                                nBins[29], minX[29], maxX[29],
                                nBinsM, minM[0][iParticle], maxM[0][iParticle]);
      //Dalitz
      if(!f2Name[iParticle].IsNull())
      {
        TString f1piName="m_{"; f1piName+=f1Name[iParticle]; f1piName+="#pi} [GeV/c]";
        TString f2piName="m_{"; f2piName+=f2Name[iParticle]; f2piName+="#pi} [GeV/c]";
        TString f1f2Name="m_{"; f1f2Name+=f1Name[iParticle]; f1f2Name+=f2Name[iParticle]; f1f2Name+="} [GeV/c]";
        TString f1f2piName="m_{"; f1f2piName+=f1Name[iParticle]; f1f2piName+=f2Name[iParticle]; f1f2piName+="#pi} [GeV/c]";
        
        fHistos3D[iParticle][2] = new TH3D("dalitz0", "dalitz0",
                                  100, minM[7][iParticle], maxM[7][iParticle]-0.15,
                                  100, minM[4][iParticle], maxM[4][iParticle]-0.15,
                                  nBinsM, minM[0][iParticle], maxM[0][iParticle]);
        fHistos3D[iParticle][2]->GetXaxis()->SetTitle(f1f2Name);
        fHistos3D[iParticle][2]->GetYaxis()->SetTitle(f2piName);
        fHistos3D[iParticle][2]->GetZaxis()->SetTitle(f1f2piName);

        fHistos3D[iParticle][3] = new TH3D("dalitz1", "dalitz1",
                                  100, minM[6][iParticle], maxM[6][iParticle]-0.15,
                                  100, minM[4][iParticle], maxM[4][iParticle]-0.15,
                                  nBinsM, minM[0][iParticle], maxM[0][iParticle]);
        fHistos3D[iParticle][3]->GetXaxis()->SetTitle(f1piName);
        fHistos3D[iParticle][3]->GetYaxis()->SetTitle(f2piName);
        fHistos3D[iParticle][3]->GetZaxis()->SetTitle(f1f2piName);

        fHistos3D[iParticle][4] = new TH3D("dalitz2", "dalitz2",
                                  100, minM[7][iParticle], maxM[7][iParticle]-0.15,
                                  100, minM[6][iParticle], maxM[6][iParticle]-0.15,
                                  nBinsM, minM[0][iParticle], maxM[0][iParticle]);
        fHistos3D[iParticle][4]->GetXaxis()->SetTitle(f1f2Name);
        fHistos3D[iParticle][4]->GetYaxis()->SetTitle(f1piName);
        fHistos3D[iParticle][4]->GetZaxis()->SetTitle(f1f2piName);
      }

      gDirectory->cd(".."); //main folder
    }
  }
  
  gFile = curFile;
  gDirectory = curDirectory;
}

void StKFParticleCandidateAnalysis::Run() {
  //                       0       1       2       3       4       5       6       7       8       9       10      11      12     13      14      15      16      17      18      19      20      21      22       23       24       25
  //                       H3L     H4L     He4L    He5L    H3L     H4L     LN      LNN     H6L     He6L    He7L    Li7L    LLN    LLNN    H4LL    H4LL    H5L     He6LL   He4L*   He5L*   H3L*    H4L*    Xi-      Xi+      Omega-   Omega+ 
  float peakM[NParticles] {2.9912, 3.9224, 3.9215, 4.8399, 2.9910, 3.9223, 2.0450, 2.9800, 5.7800, 5.7800, 6.7150, 6.7110, 3.169, 4.1100, 4.1100, 4.1100, 5.0200, 5.9700, 3.9217, 4.8402, 2.9914, 3.9227, 1.32171, 1.32171, 1.67245, 1.67245};
  float sigmaM[NParticles]{0.0017, 0.0018, 0.0014, 0.0015, 0.0015, 0.0012, 0.0018, 0.0018, 0.0018, 0.0018, 0.0018, 0.0018, 0.005, 0.0015, 0.0015, 0.0015, 0.0015, 0.0015, 0.0013, 0.0013, 0.0013, 0.0013, 0.00200, 0.00200, 0.00200, 0.00200};

  int nnn=0;
  for(int iParticle=0; iParticle<fCandidateFileChain->GetEntries(); ) {
    std::vector<KFParticle> particles; // 0 - mother, 1... - daughters

    fCandidateFileChain->GetEvent(iParticle);
    KFParticle pv = *fCandidate;
    iParticle++;
    fCandidateFileChain->GetEvent(iParticle);
    particles.push_back(*fCandidate);
    iParticle++;
    
    bool badDaughters = false;
    
    for(int iDaughter=0; iDaughter<particles[0].NDaughters(); iDaughter++) {
      fCandidateFileChain->GetEvent(iParticle);
      // float pvPosition[3]{pv.X(), pv.Y(), pv.Z()};
      // fCandidate->TransportToPoint(pvPosition);
      particles.push_back(*fCandidate);
      const int nGrandDaughters = fCandidate->NDaughters();
      iParticle++;
      
      if(nGrandDaughters > 1)
      {
        for(int iGrandDaughter=0; iGrandDaughter<nGrandDaughters; iGrandDaughter++)
        {
          fCandidateFileChain->GetEvent(iParticle);
          particles.push_back(*fCandidate);
          iParticle++;
        }
      }
      
      badDaughters = badDaughters || (fCandidate->GetErrZ() > 2.999f);
    }

    // for(int i=0; i<particles.size(); i++)
    // {
    //   std::cout << particles[i].GetPDG() << '\n';
    // }
    // std::cout << '\n';
    // std::cin.get();
    
    if(badDaughters) continue;

    const int particleIndex = ParticleIndex(particles[0].GetPDG());
    if(particleIndex < 0) continue;


    // const bool isOk = particles[0].Covariance(0,0) > 0.f && particles[0].Covariance(1,1) > 0.f && 
    //                   particles[0].Covariance(2,2) > 0.f && particles[0].Covariance(3,3) > 0.f && 
    //                   particles[0].Covariance(4,4) > 0.f && particles[0].Covariance(5,5) > 0.f && 
    //                   particles[0].Covariance(6,6) > 0.f;
    // if(!isOk) continue;
    // if(particles[0].GetDeviationFromVertexXY(pv) < 0.f) continue; 
    // if(particles[0].GetDeviationFromVertex(pv) < 0.f) continue;

    // const float probChiPrim = TMath::Prob(particles[0].GetDeviationFromVertexXY(pv), 4) * 
    //                           (1. - TMath::Prob(particles[1].GetDeviationFromVertexXY(pv), 4)) * 
    //                           (1. - TMath::Prob(particles[2].GetDeviationFromVertexXY(pv), 4));
    // if(probChiPrim < 0.001f) continue;

    if(particles[0].GetPDG() >= 1003004 && particles[0].GetPDG() <= 1003007) {
      KFParticle part0;
      part0.SetConstructMethod(2);
      KFParticle lambda = particles[2];
      lambda.SetProductionVertex(pv);
      // lambda.SetNonlinearMassConstraint(1.115683);
      part0 += lambda;

      part0.NDF() += 3;
      part0 += particles[1];

      particles[0] = part0;
    }

    if(particleIndex >= 12 && particleIndex <= 17 && 0) {

      KFParticle fragment = particles[2];
      KFParticle pion = particles[1];

      const float massConst = particleIndex == 12 || particleIndex == 15 ? 2.991 : particleIndex == 13 ? 3.9224 : 3.9215;
      const float massSigma = particleIndex == 14 ? 0.0014 : 0.00175;
      float mF, dmF;
      fragment.GetMass(mF, dmF);
      if(dmF > 0.0025f) continue;
      if(fabsf(mF - massConst) > 2. * massSigma) continue;
      float lf, dlf;
      fragment.GetDistanceToVertexLine(pv,lf,dlf);
      if(lf > 50.f) continue;

      fragment.SetNonlinearMassConstraint(massConst);

      KFParticle mother;
      mother += fragment;
      mother += pion;
      particles[0] = mother;

      fragment.SetProductionVertex(pv);
      const float probTopo = TMath::Prob(fragment.Chi2(), fragment.NDF());
      const float mass0 = particles[0].GetMass();
      // if(probTopo < 0.01) continue;
      fHistos2D[particleIndex][2]->Fill(probTopo, mass0);

      // float a, da;
      // particles[0].GetDistanceToVertexLine(pv,a,da);
      // if(a / da < 10.f) continue;


      float g, dg;
      particles[2].GetDistanceToVertexLineWithDirection(particles[0],g,dg);
      if(dg > 10000.f) continue;
      // if(g/dg < 1.f) continue;
      
      // particles[2].GetDistanceToVertexLineWithDirection(pv,g,dg);
      // if(dg > 3.f) continue;

      // particles[0].GetDistanceToVertexLineWithDirection(pv,g,dg);
      // if(dg > 3.f) continue; 
      
      // float l0, l2, dl;
      // particles[0].GetDistanceToVertexLine(pv,l0,dl);
      // particles[2].GetDistanceToVertexLine(pv,l2,dl);
      // if(l2 < l0) continue;

      
      // if(particles[2].GetDeviationFromVertex(pv) > 18.f) continue; 
      // if(particles[2].GetDeviationFromVertexXY(pv) > 18.f) continue;

      KFParticle topo = particles[0];
      topo.SetProductionVertex(pv);
      if(topo.Chi2()/topo.NDF() > 3.f) continue;
      if(topo.Chi2() < 0.f) continue;
    }

    // 0 - H4LL
    // 1 - pi-
    // 2 - He4L
    // 3 - pi-
    // 4 - He3
    // 5 - p
    if(particleIndex == 14)
    {

      float g, dg;
      particles[0].GetDistanceToVertexLineWithDirection(pv,g,dg);
      if(g/dg < 5.) continue;

      float a, da;
      particles[2].GetDistanceToVertexLineWithDirection(particles[0],a,da);
      if(da > 10000.f) continue;

      // if(particles[0].GetPt() > 3. || particles[0].GetPt() < 1.) continue;
      // if(particles[2].GetPt() < 1.) continue;

      // if(particles[1].GetDistanceFromParticle(particles[2]) > 2.) continue;
      
      if(particles[1].GetDeviationFromVertex(pv) < 8.) continue;

      if( particles[0].Chi2() / particles[0].NDF() > 3.) continue;
      KFParticle h4lTopo = particles[0];
      h4lTopo.SetProductionVertex(pv);
      if( h4lTopo.Chi2() / h4lTopo.NDF() > 5.) continue;

      // if( particles[2].Chi2() / particles[2].NDF() > 3.) continue;

      // KFParticle he4lTopo = particles[2];
      // he4lTopo.SetProductionVertex(pv);
      // if( he4lTopo.Chi2() / he4lTopo.NDF() > 3.) continue;

      // if( particles[3].GetDeviationFromVertex(pv) < 8. ) continue;
      // if( particles[1].GetDeviationFromVertex(pv) < 18. ) continue;
      // if( particles[5].GetDeviationFromVertex(pv) < 18. ) continue;

      if(particles[5].GetP() < 0.5) continue;
      float lHe4L, dlHe4L;
      particles[2].GetDistanceToVertexLine(pv, lHe4L, dlHe4L);
      if(dlHe4L > 3.f) continue;
      float mHe4L, dmHe4L;
      particles[2].GetMass(mHe4L, dmHe4L);
      if(dmHe4L > 0.0025f) continue;
      if(fabsf(mHe4L - 3.92151F) > 2.F * 0.0014) continue;
      // if(!( (mHe4L > 3.92431f && mHe4L < 3.92711f) || (mHe4L > 3.91591f && mHe4L < 3.91871f) ) ) continue;

      KFParticle He3p;
      He3p += particles[4];
      He3p += particles[5];
      if(He3p.GetMass() > 3.76f) continue;

      KFParticle ppi1;
      ppi1 += particles[5];
      ppi1 += particles[3];

      KFParticle ppi2;
      ppi2 += particles[5];
      ppi2 += particles[1];

      KFParticle topoPPi1 = ppi1;
      topoPPi1.SetProductionVertex(pv);
      topoPPi1.SetNonlinearMassConstraint(1.115683);
      const float probTopoPPi1 = TMath::Prob(topoPPi1.Chi2(), topoPPi1.NDF());

      bool isLambda = (probTopoPPi1 > 0.1f) || ((probTopoPPi1 > 0.005f && particles[4].GetDeviationFromVertex(pv) < 8.f));

      KFParticle topoPPi2 = ppi2;
      topoPPi2.SetProductionVertex(pv);
      topoPPi2.SetNonlinearMassConstraint(1.115683);
      const float probTopoPPi2 = TMath::Prob(topoPPi2.Chi2(), topoPPi2.NDF());
      isLambda |= probTopoPPi2 > 0.1f;

      // bool isLambda = probTopoPPi1 > 0.1f || probTopoPPi2 > 0.1f;
      if(isLambda) continue;

      particles.push_back(ppi1);
      particles.push_back(ppi2);
    }
    
    
    const float mass = particles[0].GetMass();
    const bool isSignal = ( mass > peakM[particleIndex] - 3*sigmaM[particleIndex] ) &&
                          ( mass < peakM[particleIndex] + 3*sigmaM[particleIndex] );
    const bool isBG = (( mass > peakM[particleIndex] - 6*sigmaM[particleIndex] ) &&
                      ( mass < peakM[particleIndex] - 3*sigmaM[particleIndex] ) ) ||
                      (( mass > peakM[particleIndex] + 3*sigmaM[particleIndex] ) &&
                      ( mass < peakM[particleIndex] + 6*sigmaM[particleIndex] ) );

    if(particles[0].NDaughters()==3) {
      
      float l,dl;

      KFParticle ppi;
      ppi += particles[1];
      ppi += particles[3];
      particles.push_back(ppi);
      const float mppi = ppi.GetMass();

      KFParticle fl;
      // fl.X() = pv.X();
      // fl.Y() = pv.Y();
      // fl.Z() = pv.Z();
      // fl.Covariance(0,0) = pv.Covariance(0,0);
      // fl.Covariance(1,1) = pv.Covariance(1,1);
      // fl.Covariance(2,2) = pv.Covariance(2,2);
      // fl.Chi2() = 0.f;
      // fl.NDF() = 0;
      fl += particles[2];
      KFParticle ppi_mc = ppi;
      ppi_mc.SetNonlinearMassConstraint(1.115683);
      fl += ppi_mc;
      particles.push_back(fl);
      fl.GetDistanceToVertexLine(pv,l,dl);

      KFParticle fpi;
      fpi += particles[2];
      fpi += particles[1];
      particles.push_back(fpi);

      KFParticle fp;
      fp += particles[2];
      fp += particles[3];
      particles.push_back(fp);
      fp.GetDistanceToVertexLine(pv,l,dl);

      KFParticle topoPPi = ppi;
      topoPPi.SetProductionVertex(pv);
      topoPPi.SetNonlinearMassConstraint(1.115683);
      const float probTopoPPi = TMath::Prob(topoPPi.Chi2(), topoPPi.NDF());

      bool isLambda = probTopoPPi > 0.01f;
      if(particleIndex == 4) {
        isLambda |= (probTopoPPi > 0.0005f && particles[2].GetDeviationFromVertex(pv) < 8.f);
      }
      else {
        isLambda |= (probTopoPPi > 0.005f && particles[2].GetDeviationFromVertex(pv) < 8.f);
      }

      if( isLambda ) continue;

      fHistos2D[particleIndex][2]->Fill(probTopoPPi, mass);

      fHistos3D[particleIndex][2]->Fill(fp.GetMass(), mppi, mass);
      fHistos3D[particleIndex][3]->Fill(fpi.GetMass(), mppi, mass);
      fHistos3D[particleIndex][4]->Fill(fp.GetMass(), fpi.GetMass(), mass);
    }

    // Armenteros
    {
      KFParticle posDaughter, negDaughter;
      float vertex[3]{particles[0].GetX(), particles[0].GetY(), particles[0].GetZ()};
      if(particles[0].NDaughters()==2)
      {
        if(particles[1].GetQ() > 0)
        {
          posDaughter = particles[1];
          negDaughter = particles[2];
        }
        else
        {
          negDaughter = particles[1];
          posDaughter = particles[2];
        }

        if(particles[0].GetPDG() >= 1003004 && particles[0].GetPDG() <= 1003007) {
          negDaughter.SetNonlinearMassConstraint(1.115683);
          negDaughter.SetProductionVertex(pv);
          posDaughter.SetProductionVertex(pv);
          vertex[0] = pv.X();
          vertex[1] = pv.Y();
          vertex[2] = pv.Z();
        }
      }
      else
      {
        // posDaughter = particles[2];
        // negDaughter += particles[1];
        // negDaughter += particles[3];
        // negDaughter.SetNonlinearMassConstraint(1.115683);
        // vertex[0] = pv.X();
        // vertex[1] = pv.Y();
        // vertex[2] = pv.Z();

        KFParticle fp;
        fp.X() = particles[0].X();
        fp.Y() = particles[0].Y();
        fp.Z() = particles[0].Z();
        fp.Covariance(0,0) = particles[0].Covariance(0,0);
        fp.Covariance(1,1) = particles[0].Covariance(1,1);
        fp.Covariance(2,2) = particles[0].Covariance(2,2);
        fp.Chi2() = 0.f;
        fp.NDF() = 0;
        fp += particles[2];
        fp += particles[3];
        posDaughter = fp;
        negDaughter = particles[1];
      }
      posDaughter.TransportToPoint(vertex);
      negDaughter.TransportToPoint(vertex);
      float QtAlpha[2];
      KFParticle::GetArmenterosPodolanski(posDaughter, negDaughter, QtAlpha );
      fHistos2D[particleIndex][1]->Fill(QtAlpha[1],QtAlpha[0],1);
      fHistos2D[particleIndex][3]->Fill(QtAlpha[1],mass,1);
    }

    float ctau0{0.f}, y0{0.f}, pt0{0.f}, mt0{0.f};

    for(unsigned int iSet=0; iSet<particles.size(); iSet++) {
      
      KFParticle part = particles[iSet];
      KFParticle topo = part;
      
      // if(particleIndex >= 12 && particleIndex <= 21 && iSet == 2)
      //   topo.SetProductionVertex(particles[0]);
      // else
        topo.SetProductionVertex(pv);
      
      const float chiPrim = part.GetDeviationFromVertex(pv);
      const float chiPrimXY = part.GetDeviationFromVertexXY(pv);
      float l, dl;
      part.GetDistanceToVertexLine(pv, l, dl);
      float decayL, decayLError;
      topo.GetDecayLength(decayL,decayLError);
      float ct, ctError;
      topo.GetLifeTime(ct,ctError);
      float m, mError;
      part.GetMass(m, mError);
      float pt = part.GetPt();
      float mt = sqrt(pt*pt+m*m) - m;
      if(iSet==0) {
        ctau0 = ct;
        y0 = topo.GetRapidity();
        pt0 = pt;
        mt0 = mt;
      }

      float cosV = TMath::Prob(part.Chi2(), part.NDF());

      float values[NHistos] = { part.X(), part.Y(), part.Z(), 
                                part.Px(), part.Py(), part.Pz(), pt, part.GetP(),
                                part.GetErrX(), part.GetErrY(), part.GetErrZ(), 
                                part.GetErrPx(), part.GetErrPy(), part.GetErrPz(),
                                part.GetErrPt(), part.GetErrP(),
                                part.Chi2()/part.NDF(), topo.Chi2()/topo.NDF(), chiPrim, chiPrimXY,
                                l, dl, l/dl, part.GetR(), ct, decayL,
                                topo.GetPhi(), topo.GetTheta(), topo.GetRapidity(), mt, cosV,
                                m};
      
      for(int iSignalSet=0; iSignalSet<NSignalSets; iSignalSet++) {
        
        if((iSignalSet == 1) && !isSignal) continue;
        if((iSignalSet == 2) && !isBG) continue;

        for(int iHisto=0; iHisto<NHistos; iHisto++) {
          fHistos[particleIndex][iSignalSet][iSet][iHisto]->Fill(values[iHisto]);
        }
      }
    }

    fHistos2D[particleIndex][0]->Fill(ctau0, mass);
    fHistos3D[particleIndex][0]->Fill(y0, pt0, mass);
    fHistos3D[particleIndex][1]->Fill(y0, mt0, mass);

    nnn++;
    // if(nnn == 1000000) break;
  }

  //Store histograms
  TFile* curFile = gFile;
  TDirectory* curDirectory = gDirectory;
  
  fOutputHistoFile->cd();
  WriteHistosCurFile(fOutputHistoDir);
  
  gFile = curFile;
  gDirectory = curDirectory;
}

void StKFParticleCandidateAnalysis::RunMixedEvent() {

  KFParticle::SetField(-4.98677f);
  KFParticleSIMD::SetField(-4.98677f);

  for(int iIndex=20; iIndex<=20; iIndex++)
  {
    struct PVCoordinates {
      float m_x;
      float m_y;
      float m_z;

      bool operator==(const PVCoordinates& pv) const {
        return (pv.m_x == m_x) && (pv.m_y == m_y) && (pv.m_z == m_z);
      }
    };

    struct Event {
      std::vector<KFParticle> m_f;
      std::vector<KFParticle> m_L;
      std::vector<KFParticle> m_M;
      KFParticle pv;

      void clear() {
        m_f.clear();
        m_L.clear();
        m_M.clear();
      }
    };

    std::vector<Event> events;
    Event event;
    PVCoordinates pvCoordinates{0.f, 0.f, 0.f};

    int nnn = 0;

    const int nCandidates = fCandidateFileChain->GetEntries();

    for(int iParticle=0; iParticle < nCandidates; )
    {
      std::vector<KFParticle> particles; // 0 - mother, 1... - daughters

      fCandidateFileChain->GetEvent(iParticle);
      KFParticle pv = *fCandidate;
      iParticle++;
      fCandidateFileChain->GetEvent(iParticle);
      particles.push_back(*fCandidate);
      iParticle++;
      
      bool badDaughters = false;
      
      for(int iDaughter=0; iDaughter<particles[0].NDaughters(); iDaughter++) {
        fCandidateFileChain->GetEvent(iParticle);
        particles.push_back(*fCandidate);
        iParticle++;
        badDaughters = badDaughters || (fCandidate->GetErrZ() > 2.999f);
      }
      
      if(badDaughters) continue;

      // if(particles[0].GetMass() < 3.05) continue;

      const int particleIndex = ParticleIndex(particles[0].GetPDG());
      if(particleIndex != iIndex) continue;

      // if(pv.NDaughters() < 150) continue;

      const PVCoordinates currentPvCoordinates{pv.X(), pv.Y(), pv.Z()};

      const float pvPoint[3]{pv.X(), pv.Y(), pv.Z()};
      // particles[2].SetNonlinearMassConstraint(1.115683);
      particles[1].TransportToPoint(pvPoint);
      particles[2].TransportToPoint(pvPoint);

      if(event.m_L.size() == 0) {
        pvCoordinates = currentPvCoordinates;
        event.m_M.push_back(particles[0]);
        event.m_f.push_back(particles[1]);
        event.m_L.push_back(particles[2]);
        event.pv = pv;
      } else if( (currentPvCoordinates == pvCoordinates) ) {
        // event.m_M.push_back(particles[0]); // TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
        // event.m_f.push_back(particles[1]); // TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
        // event.m_L.push_back(particles[2]); // TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
      } else {
        events.push_back(event);
        event.clear();

        pvCoordinates = currentPvCoordinates;
        event.m_M.push_back(particles[0]);
        event.m_f.push_back(particles[1]);
        event.m_L.push_back(particles[2]);
        event.pv = pv;
      }

      if(events.size() == 1000 || iParticle >= nCandidates) {

        std::map<const KFParticle*, int> nUsed;
        for(const auto event: events) {
          for(uint32_t iP=0; iP<event.m_L.size(); iP++) {
            nUsed[&(event.m_L[iP])] = 0;
            nUsed[&(event.m_f[iP])] = 0;
          }
        }

        for(uint32_t iEvent1 = 0; iEvent1 < events.size(); iEvent1++) {
          const Event& event1 = events[iEvent1];

          for(uint32_t iL=0; iL<event1.m_L.size(); iL++) {
            const KFParticle& lambda = event1.m_L[iL];

            for(uint32_t iEvent2 = 0; iEvent2 < events.size(); iEvent2++) {
            
              if(iEvent2 == iEvent1) continue;
            
              Event& event2 = events[iEvent2];

              for(uint32_t iF=0; iF<event2.m_f.size(); iF++) {
                const KFParticle& fragment0 = event2.m_f[iF];

                if(nUsed[&fragment0] > 0) continue;

                KFParticle fragment = fragment0;

                fragment.X() += event1.pv.X() - event2.pv.X();
                fragment.Y() += event1.pv.Y() - event2.pv.Y();
                fragment.Z() += event1.pv.Z() - event2.pv.Z();

                KFParticleSIMD pvS(const_cast<KFParticle&>(event1.pv));
                KFParticleSIMD fragmentS(fragment);
                KFParticleSIMD lambdaS(const_cast<KFParticle&>(lambda));
                lambdaS.SetNonlinearMassConstraint(1.115683f);
                lambdaS.TransportToPoint(pvS.Parameters());

                if(fragment.GetDeviationFromVertex(event1.pv) > 18.f) continue;

                KFParticleSIMD part00;
                part00 += fragmentS;
                part00 += lambdaS;
                bool isOk = part00.Chi2()[0] / part00.NDF()[0] < 3.f;
                isOk &= part00.Chi2()[0] == part00.Chi2()[0];
                isOk &= part00.Chi2()[0] > 0.f;
                isOk &= std::isfinite(part00.Chi2()[0]);

                float32_v lv(1.e8f);
                float32_v dlv(1.e8f);
                mask32_v isParticleFromVertex;
                part00.GetDistanceToVertexLine(pvS, lv, dlv, &isParticleFromVertex);
                isOk &= (lv[0] < 200.f);

                if(!isOk) continue;

                KFParticle part000;
                part00.GetKFParticle(part000);
                if(part000.GetErrMass() > 0.003f) continue;

                float p1p2 = fragment.Px()*lambda.Px() + fragment.Py()*lambda.Py() + fragment.Pz()*lambda.Pz();
                float p12  = fragment.Px()*fragment.Px() + fragment.Py()*fragment.Py() + fragment.Pz()*fragment.Pz();
                float p22  = lambda.Px()*lambda.Px() + lambda.Py()*lambda.Py() + lambda.Pz()*lambda.Pz();
                if(p1p2 <= -p12 || p1p2 <= -p22) continue;

                KFParticle part0;
                part0.SetConstructMethod(2);
                KFParticle lambda0 = lambda;
                lambda0.SetProductionVertex(event1.pv);
                part0 += lambda0;
                part0.NDF() += 3;
                part0 += fragment;

                const float mass = part0.GetMass();

                // Armenteros
                {
                  float QtAlpha[2];
                  KFParticle::GetArmenterosPodolanski(fragment, lambda, QtAlpha );
                  fHistos2D[particleIndex][1]->Fill(QtAlpha[1],QtAlpha[0]);
                  fHistos2D[particleIndex][3]->Fill(QtAlpha[1],mass);
                }

                KFParticle particles[3]{part0, fragment, lambda};
                for(int iSet=0; iSet<1; iSet++)
                {
                  KFParticle part = particles[iSet];
                  KFParticle topo = particles[iSet];
                  topo.SetProductionVertex(event1.pv);
                  
                  const float chiPrim = part.GetDeviationFromVertex(event1.pv);
                  const float chiPrimXY = part.GetDeviationFromVertexXY(event1.pv);
                  float l, dl;
                  part.GetDistanceToVertexLine(event1.pv, l, dl);
                  float decayL, decayLError;
                  topo.GetDecayLength(decayL,decayLError);
                  float ct, ctError;
                  topo.GetLifeTime(ct,ctError);
                  float m, mError;
                  part.GetMass(m, mError);
                  float pt = part.GetPt();
                  float mt = sqrt(pt*pt+m*m) - m;
                  float cosV = TMath::Prob(part.Chi2(), part.NDF());

                  float values[NHistos] = { part.X(), part.Y(), part.Z(), 
                                            part.Px(), part.Py(), part.Pz(), pt, part.GetP(),
                                            part.GetErrX(), part.GetErrY(), part.GetErrZ(), 
                                            part.GetErrPx(), part.GetErrPy(), part.GetErrPz(),
                                            part.GetErrPt(), part.GetErrP(),
                                            part.Chi2()/part.NDF(), topo.Chi2()/topo.NDF(), chiPrim, chiPrimXY,
                                            l, dl, l/dl, part.GetR(), ct, decayL,
                                            topo.GetPhi(), topo.GetTheta(), topo.GetRapidity(), mt, cosV,
                                            m};
                  
                  for(int iHisto=0; iHisto<NHistos; iHisto++) {
                    fHistos[particleIndex][0][iSet][iHisto]->Fill(values[iHisto]);
                  }
                }

                nnn++;
                if(nnn % 10000 == 0) std::cout << nnn << '\n';

                nUsed[&fragment0]++;
                nUsed[&lambda]++;
                // break;
              }
              if(nUsed[&lambda] > 0) break;
            }
          }
        }
        events.clear();
      }
    }
  }

  //Store histograms
  TFile* curFile = gFile;
  TDirectory* curDirectory = gDirectory;
  
  fOutputHistoFile->cd();
  WriteHistosCurFile(fOutputHistoDir);
  
  gFile = curFile;
  gDirectory = curDirectory;
}

inline void Mix(float& a, float& b, const float sinA, const float cosA)
{
  const float x = a;
  const float y = b;
  a = x * cosA - y * sinA;
  b = x * sinA + y * cosA;
}

void RotateXY(KFParticle& particle, const float alpha)
{
  const float cosA = std::cos(alpha);
  const float sinA = std::sin(alpha);

  Mix(particle.Px(), particle.Py(), sinA, cosA);

  Mix(particle.Covariance(6), particle.Covariance(10), sinA, cosA);
  Mix(particle.Covariance(7), particle.Covariance(11), sinA, cosA);
  Mix(particle.Covariance(8), particle.Covariance(12), sinA, cosA);
  Mix(particle.Covariance(18), particle.Covariance(19), sinA, cosA);
  Mix(particle.Covariance(24), particle.Covariance(25), sinA, cosA);

  const float c9 = particle.Covariance(9);
  const float c13 = particle.Covariance(13);
  const float c14 = particle.Covariance(14);
  particle.Covariance(9)  = (c9 * cosA - c13 * sinA) * cosA - (c13 * cosA - c14 * sinA) * sinA;
  particle.Covariance(13) = (c13 * cosA + c9 * sinA) * cosA - (c13 * sinA + c14 * cosA) * sinA;
  particle.Covariance(14) = (c13 * cosA + c9 * sinA) * sinA + (c13 * sinA + c14 * cosA) * cosA;
}

void StKFParticleCandidateAnalysis::RunRotatedBG() {

  std::srand(0);

  int nnn = 0;

  for(int iParticle=0; iParticle<fCandidateFileChain->GetEntries(); )
  {
    if(nnn % 10000 == 0) std::cout << nnn << '\n';

    std::vector<KFParticle> particles1; // 0 - mother, 1... - daughters

    fCandidateFileChain->GetEvent(iParticle);
    KFParticle pv = *fCandidate;
    iParticle++;
    fCandidateFileChain->GetEvent(iParticle);
    particles1.push_back(*fCandidate);
    iParticle++;
    
    bool badDaughters = false;
    
    for(int iDaughter=0; iDaughter<particles1[0].NDaughters(); iDaughter++) {
      fCandidateFileChain->GetEvent(iParticle);
      particles1.push_back(*fCandidate);
      iParticle++;
      badDaughters = badDaughters || (fCandidate->GetErrZ() > 2.999f);
    }
    
    if(badDaughters) continue;

    // if(particles1[0].GetMass() < 3.01) continue;

    const int particleIndex = ParticleIndex(particles1[0].GetPDG());
    if(particleIndex < 18 || particleIndex > 21) continue;

    int nCombinations = 0;
    constexpr int nEventsToMix = 1;

    KFParticle part2 = particles1[2];
    part2.SetNonlinearMassConstraint(1.115683);

    const int nRotates = 1000;
    for(uint32_t iRotate=0; iRotate<nRotates; iRotate++ )
    {
      float randomAngle = double(std::rand()) / double(RAND_MAX);
      const float alpha = (360. * randomAngle + 0.) * M_PI / 180.;

      KFParticle part1 = particles1[1];
      RotateXY(part1, alpha);

      KFParticle part0;

      if(particleIndex >= 18 && particleIndex <= 21)
      {
        KFParticle& fragment = part1;
        KFParticle& lambda = part2;

        KFParticleSIMD pvS(const_cast<KFParticle&>(pv));
        KFParticleSIMD fragmentS(fragment);
        KFParticleSIMD lambdaS(const_cast<KFParticle&>(lambda));

        if(fragment.GetDeviationFromVertex(pv) > 18.f) continue;

        KFParticle part000;
        KFParticleSIMD part00;
        part00 += fragmentS;
        part00 += lambdaS;
        if(part00.Chi2()[0] / part00.NDF()[0] > 3.f) continue;
        if(part00.Chi2()[0] != part00.Chi2()[0]) continue;
        if(part00.Chi2()[0] < 0.f) continue;
        
        part00.GetKFParticle(part000);
        if(part000.GetErrMass() > 0.003f) continue;

        float p1p2 = fragment.Px()*lambda.Px() + fragment.Py()*lambda.Py() + fragment.Pz()*lambda.Pz();
        float p12  = fragment.Px()*fragment.Px() + fragment.Py()*fragment.Py() + fragment.Pz()*fragment.Pz();
        float p22  = lambda.Px()*lambda.Px() + lambda.Py()*lambda.Py() + lambda.Pz()*lambda.Pz();
        if(p1p2 <= -p12 || p1p2 < -p22) continue;

        part0.SetConstructMethod(2);
        part0.X() = pv.X();
        part0.Y() = pv.Y();
        part0.Z() = pv.Z();
        part0.Covariance(0,0) = pv.Covariance(0,0);
        part0.Covariance(1,1) = pv.Covariance(1,1);
        part0.Covariance(2,2) = pv.Covariance(2,2);
        part0.Covariance(1,0) = pv.Covariance(1,0);
        part0.Covariance(2,0) = pv.Covariance(2,0);
        part0.Covariance(2,1) = pv.Covariance(2,1);
        part0.Chi2() = 0.f;
        part0.NDF() = 0;
        part0 += fragment;
        part0 += lambda;
      }

      const float mass = part0.GetMass();

      // Armenteros
      {
        float QtAlpha[2];
        KFParticle::GetArmenterosPodolanski(part1, part2, QtAlpha );
        fHistos2D[particleIndex][1]->Fill(QtAlpha[1],QtAlpha[0],1);
        fHistos2D[particleIndex][3]->Fill(QtAlpha[1],mass,1);
      }

      KFParticle particles[3]{part0, part1, part2};
      for(int iSet=0; iSet<1; iSet++)
      {
        KFParticle part = particles[iSet];
        KFParticle topo = particles[iSet];
        topo.SetProductionVertex(pv);
        
        const float chiPrim = part.GetDeviationFromVertex(pv);
        const float chiPrimXY = part.GetDeviationFromVertexXY(pv);
        float l, dl;
        part.GetDistanceToVertexLine(pv, l, dl);
        float decayL, decayLError;
        topo.GetDecayLength(decayL,decayLError);
        float ct, ctError;
        topo.GetLifeTime(ct,ctError);
        float m, mError;
        part.GetMass(m, mError);
        float pt = part.GetPt();
        float mt = sqrt(pt*pt+m*m) - m;
        float cosV = TMath::Prob(part.Chi2(), part.NDF());

        float values[NHistos] = { part.X(), part.Y(), part.Z(), 
                                  part.Px(), part.Py(), part.Pz(), pt, part.GetP(),
                                  part.GetErrX(), part.GetErrY(), part.GetErrZ(), 
                                  part.GetErrPx(), part.GetErrPy(), part.GetErrPz(),
                                  part.GetErrPt(), part.GetErrP(),
                                  part.Chi2()/part.NDF(), topo.Chi2()/topo.NDF(), chiPrim, chiPrimXY,
                                  l, dl, l/dl, part.GetR(), ct, decayL,
                                  topo.GetPhi(), topo.GetTheta(), topo.GetRapidity(), mt, cosV,
                                  m};
        
        for(int iHisto=0; iHisto<NHistos; iHisto++) {
          fHistos[particleIndex][0][iSet][iHisto]->Fill(values[iHisto]);
        }
      }
      nCombinations++;
      if(nCombinations == nEventsToMix) break;
    }

    nnn++;
    // if(nnn == 1000000) break;
  }

  //Store histograms
  TFile* curFile = gFile;
  TDirectory* curDirectory = gDirectory;
  
  fOutputHistoFile->cd();
  WriteHistosCurFile(fOutputHistoDir);
  
  gFile = curFile;
  gDirectory = curDirectory;
}

void StKFParticleCandidateAnalysis::WriteHistosCurFile( TObject *obj ){
  if( !obj->IsFolder() ) obj->Write();
  else{
    TDirectory *cur = TDirectory::CurrentDirectory();
    TFile *currentFile = TFile::CurrentFile();

    TDirectory *sub = cur->GetDirectory(obj->GetName());
    sub->cd();
    TList *listSub = (static_cast<TDirectory*>(obj))->GetList();
    TIter it(listSub);
    while( TObject *obj1=it() ) WriteHistosCurFile(obj1);
    cur->cd();
    TFile::CurrentFile() = currentFile;
    TDirectory::CurrentDirectory() = cur;
  }
}

void StKFParticleCandidateAnalysis::CheckMath() {
  
  auto print = [](const KFParticle& p){
    for(uint32_t i=0; i<8; i++) {
      std::cout << std::scientific << std::setprecision(10) << p.GetParameter(i) << " ";
    }
    std::cout << '\n';

    for(uint32_t i=0; i<8; i++) {
      for(uint32_t j=0; j<=i; j++){
        std::cout << p.GetCovariance(i, j) << " ";
      }
      std::cout << '\n';
    }
    std::cout << '\n';
  };

  for(int iParticle=0; iParticle<fCandidateFileChain->GetEntries(); )
  {
    std::vector<KFParticle> particles; // 0 - mother, 1... - daughters

    fCandidateFileChain->GetEvent(iParticle);
    KFParticle pv = *fCandidate;
    iParticle++;
    fCandidateFileChain->GetEvent(iParticle);
    particles.push_back(*fCandidate);
    iParticle++;
    
    bool badDaughters = false;
    
    for(int iDaughter=0; iDaughter<particles[0].NDaughters(); iDaughter++) {
      fCandidateFileChain->GetEvent(iParticle);
      particles.push_back(*fCandidate);
      iParticle++;
      badDaughters = badDaughters || (fCandidate->GetErrZ() > 2.999f);
    }
    
    if(badDaughters) continue;

    const int particleIndex = ParticleIndex(particles[0].GetPDG());
    if(particleIndex < 18 || particleIndex > 21) continue;

    KFParticle& fragment = particles[1];
    KFParticle& lambda = particles[2];

    lambda.SetNonlinearMassConstraint(1.115683f);

    KFParticleSIMD pvS(const_cast<KFParticle&>(pv));
    KFParticleSIMD fragmentS(fragment);
    KFParticleSIMD lambdaS(const_cast<KFParticle&>(lambda));

    KFParticleSIMD part00;
    part00 += fragmentS;
    part00 += lambdaS;
    KFParticle part000;
    part00.GetKFParticle(part000);

    KFParticle part0;
    part0 += fragment;
    part0 += lambda;

    if(part0.Chi2() / part0.NDF() < 3.f && part000.Chi2() / part000.NDF() >= 3.f) {
      std::cout << "\nscalar\n";
      print(part0);
      std::cout << "simd\n";
      print(part000);
      std::cin.get();
    }
  }
}
