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

  const float pi = TMath::Pi();

  TString particleName[NParticles]{ "H3L",  "H4L", "He4L", "He5L", "H3Ldppi", "H4Ltppi"};
  float minM[NSets][NParticles]{  {  2.94,   3.86,   3.86,   4.81,      2.94,      3.86}, 
                                  {   0.1,    0.1,    0.1,    0.1,       0.1,       0.1}, 
                                  {   2.7,    3.6,    2.7,    3.6,       1.8,       2.7}, 
                                  {     0,      0,   0.93,   0.93,      0.93,      0.93}, 
                                  {     0,      0,   1.07,   1.07,      1.07,      1.07},
                                  {     0,      0,   3.86,   4.81,      2.94,      3.86},
                                  {     0,      0,   2.94,   3.86,      2.01,      2.94},
                                  {     0,      0,   3.74,   4.66,      2.81,      3.74},
    
  };
  float maxM[NSets][NParticles]{  {  3.19,   4.11,   4.11,   5.06,      3.19,      4.11},
                                  {   0.2,    0.2,    0.2,    0.2,       0.2,       0.2},
                                  {   2.9,    3.8,    2.9,    3.8,       2.0,       2.9},
                                  {     0,      0,   0.95,   0.95,      0.95,      0.95},
                                  {     0,      0,   1.32,   1.32,      1.32,      1.32},
                                  {     0,      0,   4.11,   5.06,      3.19,      4.11},
                                  {     0,      0,   3.19,   4.11,      2.26,      3.19},
                                  {     0,      0,   3.99,   4.91,      3.06,      3.99},
    
  };

  TString signalSet[NSignalSets]{"Total", "Signal", "BG"};
  TString setNames[NParticles][NSets]{ 
    {"mother", "pi", "He3", "",  "",    "",   "",    ""  },
    {"mother", "pi", "He4", "",  "",    "",   "",    ""  },
    {"mother", "pi", "He3", "p", "ppi", "fl", "fpi", "fp"},
    {"mother", "pi", "He4", "p", "ppi", "fl", "fpi", "fp"},
    {"mother", "pi", "d",   "p", "ppi", "fl", "fpi", "fp"},
    {"mother", "pi", "t",   "p", "ppi", "fl", "fpi", "fp"}
  };
  TString f1Name[NParticles]{"^{3}He","^{4}He","^{3}He","^{4}He","d","t"};
  TString f2Name[NParticles]{"","","p","p","p","p"};
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
  float peakM[NParticles]         {2.9914, 3.9227, 3.9217, 4.8402,    2.9914,    3.9227};
  float sigmaM[NParticles]        {0.0018, 0.0018, 0.0013, 0.0013,    0.0013,    0.0013};

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
      fCandidate->TransportToPoint(pvPosition);
      particles.push_back(*fCandidate);
      iParticle++;
    }
    
    const int particleIndex = ParticleIndex(particles[0].GetPDG());
    if(particleIndex < 0) continue;

//     if(particles[1].GetDeviationFromVertex(pv) < 30.f) continue;
    
//     KFParticle fragment = particles[2];
//     fragment.SetProductionVertex(pv);
//     float a,da;
//     particles[0].GetDistanceToVertexLine(pv, a,da);
//     if(a/da > 10.f) continue;

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
//       if(l/dl < 5.f) continue;
      
      KFParticle fpi;
      fpi += particles[2];
      fpi += particles[1];
      particles.push_back(fpi);

      KFParticle fp;
      fp += particles[2];
      fp += particles[3];
      particles.push_back(fp);
      
      fHistos3D[particleIndex][2]->Fill(fp.GetMass(), mppi, mass);
      fHistos3D[particleIndex][3]->Fill(fpi.GetMass(), mppi, mass);
      fHistos3D[particleIndex][4]->Fill(fp.GetMass(), fpi.GetMass(), mass);
    }

    float ctau0{0.f}, y0{0.f}, pt0{0.f}, mt0{0.f};

    for(unsigned int iSet=0; iSet<particles.size(); iSet++) {
      
      KFParticle part = particles[iSet];
      KFParticle topo = part;
      topo.SetProductionVertex(pv);
      const float chiPrim = part.GetDeviationFromVertex(pv);
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
