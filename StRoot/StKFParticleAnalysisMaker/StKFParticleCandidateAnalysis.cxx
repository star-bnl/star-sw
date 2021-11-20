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
  
  fPdgMap[3004] = 0;
  fPdgMap[3005] = 1;
  fPdgMap[3006] = 2;
  fPdgMap[3007] = 3;
  fPdgMap[3012] = 4;
  fPdgMap[3013] = 5;
  
  fPdgMap[3003] = 6;
  fPdgMap[3103] = 7;
  fPdgMap[3016] = 8;
  fPdgMap[3019] = 9;
  fPdgMap[3022] = 10;
  fPdgMap[3025] = 11;

  fPdgMap[3203] = 12;
  fPdgMap[3008] = 13;
  fPdgMap[3009] = 14;
  fPdgMap[3010] = 15;
  fPdgMap[3011] = 16;
  
  fPdgMap[ 3312] = 17;
  fPdgMap[-3312] = 18;
  fPdgMap[ 3334] = 19;
  fPdgMap[-3334] = 20;

  const float pi = TMath::Pi();

  TString particleName[NParticles]{ "H3L",  "H4L", "He4L", "He5L", "H3Ldppi", "H4Ltppi", "LN", "LNN", "H6L", "He6L", "He7L", "Li7L", "LLN", "H4LL", "H4LL3", "H5LL", "He6LL", "Xi-", "Xi+", "Omega-", "Omega+" };
  float minM[NSets][NParticles]{  {  2.94,   3.86,   3.86,   4.81,      2.94,      3.86, 2.02,  2.94,  5.74,   5.74,   6.67,   6.67,  3.13,   4.06,    4.06,   4.99,    5.95,  1.22,  1.22,     1.66,      1.66}, 
                                  {   0.1,    0.1,    0.1,    0.1,       0.1,       0.1,  0.1,   0.1,   0.1,    0.1,    0.1,    0.1,   0.1,    0.1,     0.1,    0.1,     0.1,   0.1,   0.1,      0.4,       0.4}, 
                                  {   2.7,    3.6,    2.7,    3.6,       1.8,       2.7,  1.8,   2.7,   5.5,    5.5,    6.5,    6.5,  2.94,   3.86,    2.94,   4.81,    4.81,   1.0,   1.0,      1.0,       1.0}, 
                                  {     0,      0,   0.93,   0.93,      0.93,      0.93,    0,     0,     0,      0,      0,      0,     0,      0,    0.93,      0,    0.93,     0,     0,        0,         0}, 
                                  {     0,      0,   1.07,   1.07,      1.07,      1.07,    0,     0,     0,      0,      0,      0,     0,      0,    1.07,      0,    1.07,     0,     0,        0,         0},
                                  {     0,      0,   3.86,   4.81,      2.94,      3.86,    0,     0,     0,      0,      0,      0,     0,      0,    4.06,      0,    5.95,     0,     0,        0,         0},
                                  {     0,      0,   2.94,   3.86,      2.01,      2.94,    0,     0,     0,      0,      0,      0,     0,      0,    3.13,      0,    4.99,     0,     0,        0,         0},
                                  {     0,      0,   3.74,   4.66,      2.81,      3.74,    0,     0,     0,      0,      0,      0,     0,      0,    3.92,      0,    5.81,     0,     0,        0,         0}
  };                                                                                                  
  float maxM[NSets][NParticles]{  {  3.19,   4.11,   4.11,   5.06,      3.19,      4.11, 2.27,  3.19,  5.95,   5.95,   6.92,   6.92,  3.38,   4.31,    4.31,   5.24,    6.20,  1.42,  1.42,     1.68,      1.68},
                                  {   0.2,    0.2,    0.2,    0.2,       0.2,       0.2,  0.1,   0.2,   0.2,    0.2,    0.2,    0.2,   0.2,    0.2,     0.2,    0.2,     0.2,   0.2,   0.2,      0.6,       0.6},
                                  {   2.9,    3.8,    2.9,    3.8,       2.0,       2.9,  2.0,   2.9,   5.7,    5.7,    6.7,    6.7,  3.19,   4.11,    3.19,   5.06,    5.06,   1.2,   1.2,      1.2,       1.2},
                                  {     0,      0,   0.95,   0.95,      0.95,      0.95,    0,     0,     0,      0,      0,      0,     0,      0,    0.95,      0,    0.95,     0,     0,        0,         0},
                                  {     0,      0,   1.32,   1.32,      1.32,      1.32,    0,     0,     0,      0,      0,      0,     0,      0,    1.32,      0,    1.32,     0,     0,        0,         0},
                                  {     0,      0,   4.11,   5.06,      3.19,      4.11,    0,     0,     0,      0,      0,      0,     0,      0,    4.31,      0,    6.20,     0,     0,        0,         0},
                                  {     0,      0,   3.19,   4.11,      2.26,      3.19,    0,     0,     0,      0,      0,      0,     0,      0,    3.38,      0,    5.24,     0,     0,        0,         0},
                                  {     0,      0,   3.99,   4.91,      3.06,      3.99,    0,     0,     0,      0,      0,      0,     0,      0,    4.17,      0,    6.06,     0,     0,        0,         0}
  };

  TString signalSet[NSignalSets]{"Total", "Signal", "BG"};
  TString setNames[NParticles][NSets]{ 
    {"mother", "pi", "He3",  "",  "",    "",   "",    ""  },
    {"mother", "pi", "He4",  "",  "",    "",   "",    ""  },
    {"mother", "pi", "He3",  "p", "ppi", "fl", "fpi", "fp"},
    {"mother", "pi", "He4",  "p", "ppi", "fl", "fpi", "fp"},
    {"mother", "pi", "d",    "p", "ppi", "fl", "fpi", "fp"},
    {"mother", "pi", "t",    "p", "ppi", "fl", "fpi", "fp"},
    {"mother", "pi", "d",    "",  "",    "",   "",    ""  },
    {"mother", "pi", "t",    "",  "",    "",   "",    ""  },
    {"mother", "pi", "He6",  "",  "",    "",   "",    ""  },
    {"mother", "pi", "Li6",  "",  "",    "",   "",    ""  },
    {"mother", "pi", "Li7",  "",  "",    "",   "",    ""  },
    {"mother", "pi", "Be7",  "",  "",    "",   "",    ""  },
    {"mother", "pi", "H3L",  "",  "",    "",   "",    ""  },
    {"mother", "pi", "He4L", "",  "",    "",   "",    ""  },
    {"mother", "pi", "H3L",  "p", "ppi", "fl", "fpi", "fp"},
    {"mother", "pi", "He5L", "",  "",    "",   "",    ""  },
    {"mother", "pi", "He5L", "p", "ppi", "fl", "fpi", "fp"},
    {"mother", "pi", "L",    "",  "",    "",   "",    ""  },
    {"mother", "pi", "L",    "",  "",    "",   "",    ""  },
    {"mother", "K",  "L",    "",  "",    "",   "",    ""  },
    {"mother", "K",  "L",    "",  "",    "",   "",    ""  },
  };
  //                         0        1        2        3        4   5   6   7   8        9        10       11       12                13                 14                15                 16
  TString f1Name[NParticles]{"^{3}He","^{4}He","^{3}He","^{4}He","d","t","d","t","^{6}He","^{6}Li","^{7}Li","^{7}Be","^{3}_{#Lambda}H","^{4}_{#Lambda}He","^{3}_{#Lambda}H","^{5}_{#Lambda}He","^{5}_{#Lambda}He",
                             "#Lambda", "#bar{#Lambda}", "#Lambda", "#bar{#Lambda}"};
  //                         0  1  2   3   4   5   6  7  8  9  10 11 12 13 14  15 16  17 18 19 20
  TString f2Name[NParticles]{"","","p","p","p","p","","","","","","","","","p","","p","","","",""};
  

  TString histoNames[NHistos-1]{"x", "y", "z", "px", "py", "pz", "pt", "p",
                                "dx", "dy", "dz", "dpx", "dpy", "dpz", "dpt", "dp",
                                "chi", "chiTopo", "chiPrim", "l", "dl", "ldl", "r", "ctau",
                                "decayL", "phi", "theta", "rapidity", "mt"};
  //                             1     2     3     4     5     6     7     8
  const int nBins[NHistos-1] {  200,  200,  500,  200,  200,  400,   50,  200,
                                100,  100,  100,  100,  100,  100,  100,  100,
                                200,  200, 1000,  200,  100, 1000,  100,  100,
                                100,  100,  100,   60,   50
  };
  const float minX[NHistos-1]{ -100, -100, -250,  -10,  -10,  -20,    0,    0,
                                  0,    0,    0,    0,    0,    0,    0,    0,
                                  0,    0,    0,    0,    0,    0,    0,    0,
                                  0,  -pi,    0,   -3,    0
  };
  const float maxX[NHistos-1]{  100,  100,  250,   10,   10,   20,    5,   20,
                                 10,   10,   10,  0.2,  0.2,  0.2,  0.2,  0.2,
                                 20,   20, 1000,  200,   10,  100,   50,  100,
                                100,   pi,   pi,    3,    5
  };

  fOutputHistoFile = new TFile("candidateHistos.root", "RECREATE");
  fOutputHistoDir = fOutputHistoFile->mkdir("Particles");
  fOutputHistoDir->cd();
  {
    for(int iParticle=0; iParticle<NParticles; iParticle++) {
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
                                nBins[23], minX[23], maxX[23],
                                nBinsM, minM[0][iParticle], maxM[0][iParticle]);
      fHistos2D[iParticle][0]->GetXaxis()->SetTitle("c#tau");
      fHistos2D[iParticle][0]->GetYaxis()->SetTitle("m [GeV/c]");
      //y-pt-M and y-mt-M
      fHistos3D[iParticle][0] = new TH3D("y-pt-m", "y-pt-m",
                                nBins[27], minX[27], maxX[27],
                                nBins[6], minX[6], maxX[6],
                                nBinsM, minM[0][iParticle], maxM[0][iParticle]);
      fHistos3D[iParticle][1] = new TH3D("y-mt-m", "y-mt-m",
                                nBins[27], minX[27], maxX[27],
                                nBins[28], minX[28], maxX[28],
                                nBinsM, minM[0][iParticle], maxM[0][iParticle]);
      //Dalitz
      if(setNames[iParticle][0].IsNull()) continue;

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
      gDirectory->cd(".."); //main folder
    }
  }
  
  gFile = curFile;
  gDirectory = curDirectory;
}

void StKFParticleCandidateAnalysis::Run() {
  //                       H3L     H4L     He4L    He5L    H3L     H4L     LN      LNN     H6L     He6L    He7L    Li7L    LLN    H4LL    H4LL    H5L     He6LL   Xi-      Xi+      Omega-   Omega+
  float peakM[NParticles] {2.9914, 3.9227, 3.9217, 4.8402, 2.9914, 3.9227, 2.4500, 2.9800, 5.7800, 5.7800, 6.7150, 6.7110, 3.169, 4.1100, 4.1100, 5.0200, 5.9700, 1.32171, 1.32171, 1.67245, 1.67245};
  float sigmaM[NParticles]{0.0018, 0.0018, 0.0013, 0.0013, 0.0013, 0.0013, 0.0018, 0.0018, 0.0018, 0.0018, 0.0018, 0.0018, 0.005, 0.0015, 0.0015, 0.0015, 0.0015, 0.00200, 0.00200, 0.00200, 0.00200};

  for(int iParticle=0; iParticle<fCandidateFileChain->GetEntries(); ) {
    std::vector<KFParticle> particles; // 0 - mother, 1... - daughters

    fCandidateFileChain->GetEvent(iParticle);
    const KFParticle pv = *fCandidate;
    iParticle++;
    fCandidateFileChain->GetEvent(iParticle);
    particles.push_back(*fCandidate);
    iParticle++;
    
    for(int iDaughter=0; iDaughter<particles[0].NDaughters(); iDaughter++) {
      fCandidateFileChain->GetEvent(iParticle);
      float pvPosition[3]{pv.X(), pv.Y(), pv.Z()};
//      fCandidate->TransportToPoint(pvPosition);
      particles.push_back(*fCandidate);
      iParticle++;
    }
    
    const int particleIndex = ParticleIndex(particles[0].GetPDG());
    if(particleIndex < 0) continue;

//     float aaa, daaa;
//     particles[0].GetDistanceToVertexLine(pv,aaa,daaa);
//     if(aaa/daaa < 20) continue;
//     if(particles[2].GetDeviationFromVertex(pv) < 18.f ) continue;

//     if(particles[1].GetDeviationFromVertex(pv) < 18.f ) continue;
//     if(particles[0].NDaughters()==3)
//       if(particles[3].GetDeviationFromVertex(pv) < 18.f ) continue;

    if(particleIndex >= 12) {
      float l, dl;
      particles[2].GetDistanceToVertexLine(pv,l,dl);
      if(l/dl < 5) continue;
      
//       float a, da;
//       particles[0].GetDistanceToVertexLine(pv,a,da);
//       
//       std::cout << "mother:   " << a << " " << da << std::endl;
//       std::cout << "daughter: " << l << " " << dl << std::endl;
//       
//       float g, dg;
//       particles[2].GetDistanceToVertexLine(particles[0],g,dg);
//       std::cout << "d->m:     " << g << " " << dg << std::endl;
//       
//       
//       KFParticle fragment = particles[2];
//       fragment.TransportToPoint(particles[0].Parameters());
//       const float pp = fragment.Px()*particles[0].Px() + fragment.Py()*particles[0].Py() + fragment.Pz()*particles[0].Pz();
//       std::cout << "pp " << pp << std::endl;
//       
//       const float dx = particles[2].X() - particles[0].X();
//       const float dy = particles[2].Y() - particles[0].Y();
//       const float dz = particles[2].Z() - particles[0].Z();
//       
//       const float cosV = dx*particles[2].Px() + dy*particles[2].Py() + dz*particles[2].Pz();
//       std::cout << "cosV " << cosV << std::endl;
//       
//       if( l < a ) std::cin.get();
      
//       float g, dg;
//       particles[2].GetDistanceToVertexLineWithDirection(particles[0],g,dg);
//       if(dg > 10000.f) continue;
      
//       particles[2].GetDistanceToVertexLineWithDirection(pv,g,dg);
//       if(dg > 3.f) continue;

//       particles[0].GetDistanceToVertexLineWithDirection(pv,g,dg);
//       if(dg > 3.f) continue;
//       
      
      KFParticle topo = particles[0];
      topo.SetProductionVertex(pv);
      if(topo.Chi2()/topo.NDF() > 3.f) continue;
      if(topo.Chi2() < 0.f) continue;
    }
    
    
//     if(particles[0].NDaughters()!=3) continue;
      
//     KFParticle fragment = particles[2];
//     fragment.SetProductionVertex(pv);
//     float a,da;
//     particles[0].GetDistanceToVertexLine(pv, a,da);
//     if(a/da > 10.f) continue;

//     KFParticle part;
//     if(particleIndex==12) {
//       part += particles[2];
//       part += particles[1];
//       
//       std::cout << "Z " << part.Z() << " " << particles[2].Z() << std::endl;
//       std::cin.get();
//     }

    const float mass = particles[0].GetMass();
    const bool isSignal = ( mass > peakM[particleIndex] - 3*sigmaM[particleIndex] ) &&
                          ( mass < peakM[particleIndex] + 3*sigmaM[particleIndex] );
    const bool isBG = (( mass > peakM[particleIndex] - 6*sigmaM[particleIndex] ) &&
                       ( mass < peakM[particleIndex] - 3*sigmaM[particleIndex] ) ) ||
                      (( mass > peakM[particleIndex] + 3*sigmaM[particleIndex] ) &&
                       ( mass < peakM[particleIndex] + 6*sigmaM[particleIndex] ) );

    if(particles[0].NDaughters()==3) {
      
      float l,dl;
//       particles[3].GetDistanceToVertexLine(pv,l,dl);
//       if(l/dl < 3) continue;
//       particles[1].GetDistanceToVertexLine(pv,l,dl);
//       if(l/dl < 3) continue;

      KFParticle ppi;
      ppi += particles[1];
      ppi += particles[3];
      particles.push_back(ppi);
      const float mppi = ppi.GetMass();

      ppi.SetNonlinearMassConstraint(1.115683);
      if( ppi.Chi2()/ppi.NDF() < 3.f && 
          particles[2].GetDeviationFromVertex(pv) < 18.f &&
          ppi.GetDeviationFromVertex(pv) < 30.f ) continue;
//       if(ppi.Chi2()/ppi.NDF() > 3) continue;

      KFParticle fl;
      fl += particles[2];
      fl += ppi;
      particles.push_back(fl);
      fl.GetDistanceToVertexLine(pv,l,dl);
//       if(l < 5.f*dl) continue;
      
      KFParticle fpi;
      fpi += particles[2];
      fpi += particles[1];
      particles.push_back(fpi);

//       KFParticle fpip;
//       fpip += particles[3];
//       fpip += fpi;
//       fpip.GetDistanceToVertexLine(pv,l,dl);
//       if(l < 5.f*dl) continue;
      
      KFParticle fp;
      fp += particles[2];
      fp += particles[3];
      particles.push_back(fp);
      fp.GetDistanceToVertexLine(pv,l,dl);
//       if(l < 5.f*dl) continue;
      
//       float mfp, dmfp;
//       fp.GetMass(mfp, dmfp);
//       if(particleIndex == 3 && mfp > 4.672) continue;

//       float p1p2 = ppi.Px()*particles[2].Px() + ppi.Py()*particles[2].Py() + ppi.Pz()*particles[2].Pz();
//       float p12  = ppi.Px()*ppi.Px() + ppi.Py()*ppi.Py() + ppi.Pz()*ppi.Pz();
//       float p22  = particles[2].Px()*particles[2].Px() + particles[2].Py()*particles[2].Py() + particles[2].Pz()*particles[2].Pz();
//      active[iPDGPos] &= simd_cast<int_m>(p1p2 > -p12);
//      active[iPDGPos] &= simd_cast<int_m>(p1p2 > -p22);
//       std::cout << p1p2 << " " << (-p12) << " " << (-p22) << std::endl;
//       if(! (p1p2 > -p12 || p1p2 > -p22) ) std::cin.get();

//       float m12, m13, m23, M, m1, m2, m3, dm;
//       fpi.GetMass(m12, dm);
//       ppi.GetMass(m13, dm);
//       fp.GetMass(m23, dm);
//       
//       particles[0].GetMass(M, dm);
//       particles[1].GetMass(m1, dm);
//       particles[2].GetMass(m2, dm);
//       particles[3].GetMass(m3, dm);
//       
//       std::cout << (M - m1) << " " << m23 << std::endl;
//       std::cout << (M - m2) << " " << m13 << std::endl;
//       std::cout << (M - m3) << " " << m12 << std::endl;
//       if( M - m1 < m23) std::cin.get();
//       if( M - m2 < m13) std::cin.get();
//       if( M - m3 < m12) std::cin.get();
      
      
      
      
      fHistos3D[particleIndex][2]->Fill(fp.GetMass(), mppi, mass);
      fHistos3D[particleIndex][3]->Fill(fpi.GetMass(), mppi, mass);
      fHistos3D[particleIndex][4]->Fill(fp.GetMass(), fpi.GetMass(), mass);
    }

    float ctau0{0.f}, y0{0.f}, pt0{0.f}, mt0{0.f};

    for(unsigned int iSet=0; iSet<particles.size(); iSet++) {
      
      KFParticle part = particles[iSet];
      KFParticle topo = part;
      
      if(particleIndex >= 17 && particleIndex <= 20 && iSet == 2)
        topo.SetProductionVertex(particles[0]);
      else
        topo.SetProductionVertex(pv);
      
      const float chiPrim = part.GetDeviationFromVertex(pv);
      float l, dl;
      part.GetDistanceToVertexLine(pv, l, dl);
      float decayL, decayLError;
      topo.GetDecayLength(decayL,decayLError);
      float ct, ctError;
      topo.GetLifeTime(ct,ctError);
      if(particleIndex >= 17 && particleIndex <= 20 && iSet == 2)
        ct += ctau0;
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
        
      float values[NHistos] = { part.X(), part.Y(), part.Z(), 
                                part.Px(), part.Py(), part.Pz(), pt, part.GetP(),
                                part.GetErrX(), part.GetErrY(), part.GetErrZ(), 
                                part.GetErrPx(), part.GetErrPy(), part.GetErrPz(),
                                part.GetErrPt(), part.GetErrP(),
                                part.Chi2()/part.NDF(), topo.Chi2()/topo.NDF(), chiPrim,
                                l, dl, l/dl, part.GetR(), ct, decayL,
                                topo.GetPhi(), topo.GetTheta(), topo.GetRapidity(), mt,
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
