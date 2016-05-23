//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  I.Kisel, I.Kulakov, M.Zyzak
// @version 1.0
// @since   20.08.13
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#ifdef DO_TPCCATRACKER_EFF_PERFORMANCE

#include "KFParticlePerformanceBase.h"

#include "TMath.h"
#include "TROOT.h"
#include "Riostream.h"
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "TProfile.h"
#include "TStyle.h"


KFParticlePerformanceBase::KFParticlePerformanceBase():
  fParteff(), fPVeff(), fPVeffMCReconstructable(), fParticles(0), fPV(0), outfileName(), histodir(0), fNEvents(0)
#ifndef KFPWITHTRACKER
  ,fHistoDir(0), fIsHistoCreated(0)
#endif
{
}

void KFParticlePerformanceBase::CreateHistos(string histoDir, TDirectory* outFile)
{
  TDirectory *curdir = gDirectory;
  if (outFile) {
    outFile->cd();
    if (histoDir != "") {
      fHistoDir = outFile->mkdir( TString(histoDir) );
      fHistoDir->cd();
    }
  } else {
    fHistoDir = TDirectory::CurrentDirectory();
  }
  {
  {
    gDirectory->mkdir("KFParticlesFinder");
    gDirectory->cd("KFParticlesFinder");
    histodir = gDirectory;
    gDirectory->mkdir("Particles");
    gDirectory->cd("Particles");
    for(int iPart=0; iPart<fParteff.nParticles; ++iPart)
      {
        gDirectory->mkdir(fParteff.partName[iPart].data());
        gDirectory->cd(fParteff.partName[iPart].data());
        {
          TString res = "res";
          TString pull = "pull";
  
          gDirectory->mkdir("DaughtersQA");
          gDirectory->cd("DaughtersQA");
          {
            TString parName[nFitQA/2] = {"X","Y","Z","Px","Py","Pz","E","M"};
            int nBins = 100;
           float xMax[nFitQA/2] = {0.15,0.15,0.03,0.01,0.01,0.06,0.06,0.01};
//             float xMax[nFitQA/2] = {2.,2.,5.,0.3,0.3,0.3,0.03,0.03};
  
            for( int iH=0; iH<nFitQA/2; iH++ ){
              hFitDaughtersQA[iPart][iH]   = new TH1F((res+parName[iH]).Data(),
                                                      (GetDirectoryPath()+res+parName[iH]).Data(), 
                                                      nBins, -xMax[iH],xMax[iH]);
              hFitDaughtersQA[iPart][iH+8] = new TH1F((pull+parName[iH]).Data(),
                                                      (GetDirectoryPath()+pull+parName[iH]).Data(), 
                                                      nBins, -6,6);
            }
          }
          gDirectory->cd(".."); //particle directory

          gDirectory->mkdir("DSToParticleQA");
          gDirectory->cd("DSToParticleQA");
          {
            TString parName[3] = {"X","Y","Z"};
            int nBins = 100;
            float xMax[3] = {0.5, 0.5, 2.};
  
            for( int iH=0; iH<3; iH++ ){
              hDSToParticleQA[iPart][iH]   = new TH1F((res+parName[iH]).Data(),
                                                      (GetDirectoryPath()+res+parName[iH]).Data(), 
                                                      nBins, -xMax[iH],xMax[iH]);
              hDSToParticleQA[iPart][iH+3] = new TH1F((pull+parName[iH]).Data(),
                                                      (GetDirectoryPath()+pull+parName[iH]).Data(), 
                                                      nBins, -6,6);
            }
            
            hDSToParticleQA[iPart][6] = new TH1F("r", (GetDirectoryPath()+TString("r")).Data(), 1000, 0.0, 20.0);
          }
          gDirectory->cd(".."); //particle directory
          
          CreateFitHistograms(hFitQA[iPart], iPart);
          CreateEfficiencyHistograms(hPartEfficiency[iPart]);
            
          gDirectory->mkdir("Parameters");
          gDirectory->cd("Parameters");
          {
            bool drawZR = (iPart<5) || (iPart==41);
            CreateParameterHistograms(hPartParam[0], hPartParam2D[0], iPart, drawZR);

            gDirectory->mkdir("Signal");
            gDirectory->cd("Signal");
            {
              CreateParameterHistograms(hPartParam[1], hPartParam2D[1], iPart, drawZR);
            }
            gDirectory->cd(".."); // particle directory / Parameters
            gDirectory->mkdir("Background");
            gDirectory->cd("Background");
            {
              CreateParameterHistograms(hPartParam[2], hPartParam2D[2], iPart, drawZR);
            }
            gDirectory->cd(".."); // particle directory
            gDirectory->mkdir("Ghost");
            gDirectory->cd("Ghost");
            {
              CreateParameterHistograms(hPartParam[3], hPartParam2D[3], iPart, drawZR);
            }
            gDirectory->cd(".."); // Parameters
            
            bool plotPrimaryHistograms = abs(fParteff.partPDG[iPart]) == 310 ||
                                         abs(fParteff.partPDG[iPart]) == 3122 ||
                                         abs(fParteff.partPDG[iPart]) == 22 ||
                                         abs(fParteff.partPDG[iPart]) == 111 ||
                                         abs(fParteff.partPDG[iPart]) == 3312 ||
                                         abs(fParteff.partPDG[iPart]) == 3334;  
                                         
            bool plotSecondaryHistograms = abs(fParteff.partPDG[iPart]) == 310 ||
                                           abs(fParteff.partPDG[iPart]) == 3122 ||
                                           abs(fParteff.partPDG[iPart]) == 22 ||
                                           abs(fParteff.partPDG[iPart]) == 111;
                                           
            if(plotPrimaryHistograms)
            {
              gDirectory->mkdir("Primary");
              gDirectory->cd("Primary");
              {
                CreateParameterSubfolder("NoConstraint (1C-Fit)", hPartParamPrimary, hPartParam2DPrimary, hFitQANoConstraint, iPart);
                CreateParameterSubfolder("MassConstraint (2C-Fit)", hPartParamPrimaryMass, hPartParam2DPrimaryMass, hFitQAMassConstraint, iPart);
                CreateParameterSubfolder("PVConstraint (3C-Fit)", hPartParamPrimaryTopo, hPartParam2DPrimaryTopo, hFitQATopoConstraint, iPart);
                CreateParameterSubfolder("PVMassConstraint (4C-Fit)", hPartParamPrimaryTopoMass, hPartParam2DPrimaryTopoMass, hFitQATopoMassConstraint, iPart);
              }
              gDirectory->cd(".."); // particle directory / Parameters
            }
            
            if(plotSecondaryHistograms)
            {
              gDirectory->mkdir("Secondary");
              gDirectory->cd("Secondary");
              {
                CreateParameterSubfolder("NoConstraint (1C-Fit)", hPartParamSecondary, hPartParam2DSecondary, 0, iPart);
                CreateParameterSubfolder("MassConstraint (2C-Fit)", hPartParamSecondaryMass, hPartParam2DSecondaryMass, 0, iPart);
              }
              gDirectory->cd(".."); // particle directory / Parameters
            }
          }
          gDirectory->cd(".."); //particle directory
        }
        gDirectory->cd(".."); //Particles
      }
    }
    gDirectory->cd(".."); //main
    gDirectory->mkdir("PrimaryVertexQA");
    gDirectory->cd("PrimaryVertexQA");
    {
      struct {
        TString name;
        TString title;
        Int_t n;
        Double_t l,r;
      } Table[nHistosPV]=
      {
        {"PVResX",  "x_{rec}-x_{mc}, cm", 100, -0.1f, 0.1f},
        {"PVResY",  "y_{rec}-y_{mc}, cm", 100, -0.1f, 0.1f},
        {"PVResZ",  "z_{rec}-z_{mc}, cm", 100, -1.f, 1.f},
        {"PVPullX", "Pull X",             100, -6.f, 6.f},
        {"PVPullY", "Pull Y",             100, -6.f, 6.f},
        {"PVPullZ", "Pull Z",             100, -6.f, 6.f},
        {"Lost",    "Lost tracks",        102, -0.01f, 1.01f}        
      };
      
      TString parName[nHistosPVParam] = {"x","y","z","r","Ntracks","Chi2","NDF","Chi2NDF","prob", "PVpurity", 
                                         "ghostTr", "triggerTr", "pileupTr", "bgTr", "dzSamePV"};
      TString parAxisName[nHistosPVParam] = {"x [cm]","y [cm]","z [cm]","r [cm]","N tracks","Chi2","NDF","Chi2NDF","prob","purity",
                                             "ghost tracks [%]", "trigger tracks [%]", "pileup tracks [%]", "bg tracks [%]", "dz [cm]"};
      int nBins[nHistosPVParam] = {1000,1000,1000,1000,1001,10000,1001,10000,100,102,102,102,102,102,1000};
      float xMin[nHistosPVParam] = {-10., -10., -100.,  0,   -0.5,    0.,   -0.5,    0., 0., -0.01, -0.01, -0.01, -0.01, -0.01, 0.};
      float xMax[nHistosPVParam] = { 10.,  10.,  100., 10, 1000.5, 1000., 1000.5, 1000., 1.,  1.01,  1.01,  1.01,  1.01,  1.01, 100.};
      
      TString parName2D[nHistosPVParam2D] = {"xy"};
      TString parXAxisName2D[nHistosPVParam2D] = {"x [cm]"};
      TString parYAxisName2D[nHistosPVParam2D] = {"y [cm]"};
      int nBinsX2D[nHistosPVParam2D] = {1000};
      float xMin2D[nHistosPVParam2D] = {-10.};
      float xMax2D[nHistosPVParam2D] = { 10.};
      int nBinsY2D[nHistosPVParam2D] = {1000};
      float yMin2D[nHistosPVParam2D] = {-10.};
      float yMax2D[nHistosPVParam2D] = { 10.};
      
      for(int iH=0; iH<nHistosPVParam; iH++)
      {
        hPVParam[iH]       = new TH1F(parName[iH].Data(),(GetDirectoryPath()+parName[iH]).Data(),
                                        nBins[iH],xMin[iH],xMax[iH]);
        hPVParam[iH]->GetXaxis()->SetTitle(parAxisName[iH].Data());
      }

      for(int iH=0; iH<nHistosPVParam2D; iH++)
      {
        hPVParam2D[iH]       = new TH2F(parName2D[iH].Data(),(GetDirectoryPath()+parName2D[iH]).Data(),
                                        nBinsX2D[iH],xMin2D[iH],xMax2D[iH],
                                        nBinsY2D[iH],yMin2D[iH],yMax2D[iH]);
        hPVParam2D[iH]->GetXaxis()->SetTitle(parXAxisName2D[iH].Data());
        hPVParam2D[iH]->GetYaxis()->SetTitle(parYAxisName2D[iH].Data());
      }
      
      gDirectory->mkdir("Efficiency");
      gDirectory->cd("Efficiency");
      {
        TString effName[nPVefficiency] = {"effVsNMCPVTracks","effVsNMCPV","effVsNMCTracks","effVsNPVTracks","effVsNPV","effVsNTracks"};
        int nBinsEff[nPVefficiency]  = { 100 , 100 ,  100 ,  100 , 100 ,  100 };
        float xMinEff[nPVefficiency] = {   0.,   0.,    0.,    0.,   0.,    0.};
        float xMaxEff[nPVefficiency] = { 100., 100., 1000.,  100., 100., 1000.};

        gDirectory->mkdir("Signal");
        gDirectory->cd("Signal");
        {
          for( int iH=0; iH<nPVefficiency; iH++ ){
            hPVefficiency[0][iH]   = new TProfile( effName[iH].Data(), (GetDirectoryPath()+effName[iH]).Data(), nBinsEff[iH], xMinEff[iH], xMaxEff[iH]);
          }
        }
        gDirectory->cd(".."); //L1

        gDirectory->mkdir("Pileup");
        gDirectory->cd("Pileup");
        {
          for( int iH=0; iH<nPVefficiency; iH++ ){
            hPVefficiency[1][iH]   = new TProfile( effName[iH].Data(), (GetDirectoryPath()+effName[iH].Data()), nBinsEff[iH], xMinEff[iH], xMaxEff[iH]);
          }
        }
        gDirectory->cd(".."); //L1
        
        gDirectory->mkdir("Signal_MCReconstructable");
        gDirectory->cd("Signal_MCReconstructable");
        {
          for( int iH=0; iH<nPVefficiency; iH++ ){
            hPVefficiency[2][iH]   = new TProfile( effName[iH].Data(), (GetDirectoryPath()+effName[iH].Data()), nBinsEff[iH], xMinEff[iH], xMaxEff[iH]);
          }
        }
        gDirectory->cd(".."); //L1
        
        gDirectory->mkdir("Pileup_MCReconstructable");
        gDirectory->cd("Pileup_MCReconstructable");
        {
          for( int iH=0; iH<nPVefficiency; iH++ ){
            hPVefficiency[3][iH]   = new TProfile( effName[iH].Data(), (GetDirectoryPath()+effName[iH].Data()), nBinsEff[iH], xMinEff[iH], xMaxEff[iH]);
          }
        }
        gDirectory->cd(".."); //L1
      }      
      gDirectory->cd(".."); //L1
      
      gDirectory->mkdir("PVTracksQA");
      gDirectory->cd("PVTracksQA");
      {
        TString resTrPV = "resTrPV";
        TString pullTrPV = "pullTrPV";
        TString parNameTrPV[nFitPVTracksQA/2] = {"X","Y","Z","Px","Py","Pz"};
        int nBinsTrPV = 100;
        float xMaxTrPV[nFitPVTracksQA/2] = {2.,2.,5.,0.3,0.3,0.3};

        for( int iH=0; iH<nFitPVTracksQA/2; iH++ ){
          hFitPVTracksQA[iH]   = new TH1F((resTrPV+parNameTrPV[iH]).Data(),
                                                  (GetDirectoryPath()+resTrPV+parNameTrPV[iH]).Data(), 
                                                  nBinsTrPV, -xMaxTrPV[iH],xMaxTrPV[iH]);
          hFitPVTracksQA[iH+nFitPVTracksQA/2] = new TH1F((pullTrPV+parNameTrPV[iH]).Data(),
                                                  (GetDirectoryPath()+pullTrPV+parNameTrPV[iH]).Data(), 
                                                  nBinsTrPV, -6,6);
        }
      }      
      gDirectory->cd(".."); //L1
      
      gDirectory->mkdir("Signal");
      gDirectory->cd("Signal");
      {
        gDirectory->mkdir("FitQA");
        gDirectory->cd("FitQA");
        {
          gDirectory->mkdir("FitQAvcNMCPVTracks");
          gDirectory->cd("FitQAvcNMCPVTracks");
          {
            for(int iHPV=0; iHPV<nHistosPV-1; ++iHPV){
              hPVFitQa2D[0][0][iHPV] = new TH2F(Table[iHPV].name.Data(),(GetDirectoryPath()+Table[iHPV].title).Data(),
                                                500, 0., 5000.,
                                                Table[iHPV].n, Table[iHPV].l, Table[iHPV].r);
            }
          }
          gDirectory->cd(".."); //FitQA

          gDirectory->mkdir("FitQAvcNPVTracks");
          gDirectory->cd("FitQAvcNPVTracks");
          {
            for(int iHPV=0; iHPV<nHistosPV-1; ++iHPV){
              hPVFitQa2D[0][1][iHPV] = new TH2F(Table[iHPV].name.Data(),(GetDirectoryPath()+Table[iHPV].title).Data(),
                                                500, 0., 5000.,
                                                Table[iHPV].n, Table[iHPV].l, Table[iHPV].r);
            }
          }
          gDirectory->cd(".."); //FitQA
          
          for(int iHPV=0; iHPV<nHistosPV; ++iHPV){
            hPVFitQa[0][iHPV] = new TH1F(Table[iHPV].name.Data(),(GetDirectoryPath()+Table[iHPV].title).Data(),
                                         Table[iHPV].n, Table[iHPV].l, Table[iHPV].r);
          }
        }
        gDirectory->cd(".."); //Signal

        for(int iH=0; iH<nHistosPVParam; iH++)
        {
          hPVParamSignal[iH] = new TH1F((parName[iH]).Data(),(GetDirectoryPath()+parName[iH]).Data(),
                                        nBins[iH],xMin[iH],xMax[iH]);
          hPVParamSignal[iH]->GetXaxis()->SetTitle(parAxisName[iH].Data());
        }
      }      
      gDirectory->cd(".."); //L1

      gDirectory->mkdir("Pileup");
      gDirectory->cd("Pileup");
      {
        gDirectory->mkdir("FitQA");
        gDirectory->cd("FitQA");
        {
          gDirectory->mkdir("FitQAvcNMCPVTracks");
          gDirectory->cd("FitQAvcNMCPVTracks");
          {
            for(int iHPV=0; iHPV<nHistosPV-1; ++iHPV){
              hPVFitQa2D[1][0][iHPV] = new TH2F(Table[iHPV].name.Data(),(GetDirectoryPath()+Table[iHPV].title).Data(),
                                                500, 0., 5000.,
                                                Table[iHPV].n, Table[iHPV].l, Table[iHPV].r);
            }
          }
          gDirectory->cd(".."); //FitQA

          gDirectory->mkdir("FitQAvcNPVTracks");
          gDirectory->cd("FitQAvcNPVTracks");
          {
            for(int iHPV=0; iHPV<nHistosPV-1; ++iHPV){
              hPVFitQa2D[1][1][iHPV] = new TH2F(Table[iHPV].name.Data(),(GetDirectoryPath()+Table[iHPV].title).Data(),
                                                500, 0., 5000.,
                                                Table[iHPV].n, Table[iHPV].l, Table[iHPV].r);
            }
          }
          gDirectory->cd(".."); //FitQA
          
          for(int iHPV=0; iHPV<nHistosPV; ++iHPV){
            hPVFitQa[1][iHPV] = new TH1F(Table[iHPV].name.Data(),(GetDirectoryPath()+Table[iHPV].title).Data(),
                                         Table[iHPV].n, Table[iHPV].l, Table[iHPV].r);
          }
        }
        gDirectory->cd(".."); //Signal
        
        for(int iH=0; iH<nHistosPVParam; iH++)
        {
          hPVParamPileup[iH] = new TH1F((parName[iH]).Data(),(GetDirectoryPath()+parName[iH]).Data(),
                                        nBins[iH],xMin[iH],xMax[iH]);
          hPVParamPileup[iH]->GetXaxis()->SetTitle(parAxisName[iH].Data());
        }
      }      
      gDirectory->cd(".."); //L1
      
      gDirectory->mkdir("Background");
      gDirectory->cd("Background");
      {
        for(int iH=0; iH<nHistosPVParam; iH++)
        {
          hPVParamBG[iH] = new TH1F((parName[iH]).Data(),(GetDirectoryPath()+parName[iH]).Data(),
                                        nBins[iH],xMin[iH],xMax[iH]);
          hPVParamBG[iH]->GetXaxis()->SetTitle(parAxisName[iH].Data());
        }
      }      
      gDirectory->cd(".."); //L1
      
      gDirectory->mkdir("Ghost");
      gDirectory->cd("Ghost");
      {
        for(int iH=0; iH<nHistosPVParam; iH++)
        {
          hPVParamGhost[iH] = new TH1F((parName[iH]).Data(),(GetDirectoryPath()+parName[iH]).Data(),
                                        nBins[iH],xMin[iH],xMax[iH]);
          hPVParamGhost[iH]->GetXaxis()->SetTitle(parAxisName[iH].Data());
        }
      }
      gDirectory->cd(".."); //L1
    }
    gDirectory->cd(".."); //L1
    gDirectory->mkdir("TrackParameters");
    gDirectory->cd("TrackParameters");
    {
      TString chi2Name = "Chi2Prim";
      for(int iPart=0; iPart < KFPartEfficiencies::nParticles; iPart++)
      {
        TString chi2NamePart = "Chi2Prim";
        chi2NamePart += "_";
        chi2NamePart += fParteff.partName[iPart].data();
        hTrackParameters[iPart] = new TH1F(chi2NamePart.Data(), (GetDirectoryPath()+chi2NamePart).Data(), 1000, 0, 100);

      }
      hTrackParameters[KFPartEfficiencies::nParticles  ] = new TH1F("Chi2Prim_total", (GetDirectoryPath()+TString("Chi2Prim_total")), 1000, 0, 100);
      hTrackParameters[KFPartEfficiencies::nParticles+1] = new TH1F("Chi2Prim_prim", (GetDirectoryPath()+TString("Chi2Prim_prim")), 1000, 0, 100);
      hTrackParameters[KFPartEfficiencies::nParticles+2] = new TH1F("Chi2Prim_sec", (GetDirectoryPath()+TString("Chi2Prim_sec")), 1000, 0, 100);
      hTrackParameters[KFPartEfficiencies::nParticles+3] = new TH1F("Chi2Prim_ghost", (GetDirectoryPath()+TString("Chi2Prim_ghost")), 1000, 0, 100);
      
      hTrackParameters[KFPartEfficiencies::nParticles+4] = new TH1F("ProbPrim_total", (GetDirectoryPath()+TString("ProbPrim_total")), 10000, 0, 1);
      hTrackParameters[KFPartEfficiencies::nParticles+5] = new TH1F("ProbPrim_prim", (GetDirectoryPath()+TString("ProbPrim_prim")), 10000, 0, 1);
      hTrackParameters[KFPartEfficiencies::nParticles+6] = new TH1F("ProbPrim_sec", (GetDirectoryPath()+TString("ProbPrim_sec")), 10000, 0, 1);
      hTrackParameters[KFPartEfficiencies::nParticles+7] = new TH1F("ProbPrim_ghost", (GetDirectoryPath()+TString("ProbPrim_ghost")), 10000, 0, 1);
    }
    gDirectory->cd(".."); //particle directory


    curdir->cd();    

    }

    SetHistoCreated();
}

void KFParticlePerformanceBase::CreateFitHistograms(TH1F* histo[nFitQA], int iPart)
{
  TString res = "res";
  TString pull = "pull";
  
  gDirectory->mkdir("FitQA");
  gDirectory->cd("FitQA");
  {
    TString parName[nFitQA/2] = {"X","Y","Z","Px","Py","Pz","E","M"};
    int nBins = 50;
    float xMax[nFitQA/2] = {0.15,0.15,1.2,0.02,0.02,0.15,0.15,0.006};
    float mult[nFitQA/2]={1.f,1.f,1.f,1.f,1.f,1.f,1.f,1.f};
    if(iPart>63 && iPart<75)
      for(int iMult=3; iMult<nFitQA/2; iMult++)
        mult[iMult] = 3;
    if(iPart>45 && iPart<64)
    {
      for(int iMult=0; iMult<3; iMult++)
        mult[iMult] = 0.03;
      for(int iMult=3; iMult<nFitQA/2; iMult++)
        mult[iMult] = 3;
    }
    if(iPart==44 || iPart==45)
    {
      mult[0] = 0.25;
      mult[1] = 0.5;
      mult[2] = 0.15;
      for(int iMult=3; iMult<nFitQA/2; iMult++)
        mult[iMult] = 4;
    }
    
    for( int iH=0; iH<nFitQA/2; iH++ ){
      histo[iH]   = new TH1F((res+parName[iH]).Data(),
                             (GetDirectoryPath()+res+parName[iH]).Data(), 
                             nBins, -mult[iH]*xMax[iH],mult[iH]*xMax[iH]);
      histo[iH+8] = new TH1F((pull+parName[iH]).Data(),
                             (GetDirectoryPath()+pull+parName[iH]).Data(), 
                             nBins, -6,6);
    }
  }
  gDirectory->cd("..");
}

void KFParticlePerformanceBase::CreateEfficiencyHistograms(TProfile* histo[3][nPartEfficiency])
{
  gDirectory->mkdir("Efficiency");
  gDirectory->cd("Efficiency");
  {//vs p, pt, y, z, c*tau, decay length, l, r
    TString partNameEff[nPartEfficiency] = {"EffVsP","EffVsPt","EffVsY","EffVsZ","EffVsCT","EffVsDL","EffVsL","EffVsR"};
    TString partAxisNameEff[nPartEfficiency] = {"p [GeV/c]","p_{t} [GeV/c]",
                                                "y", "z [cm]", "Life time c#tau [cm]", "Decay length [cm]", 
                                                "L [cm]", "Rxy [cm]"};
    int nBinsEff[nPartEfficiency]  = { 200 , 100 , 100 ,  1000 ,  100 ,  100 , 1000 , 1000  };
    float xMinEff[nPartEfficiency] = {   0.,   0.,  -6.,  -200.,    0.,    0.,    0.,    0. };
    float xMaxEff[nPartEfficiency] = {  20.,  10.,   6.,   200.,  100.,  100.,  400.,  200. };
    
    TString effTypeName[3] = {"All particles",
                              "Reconstructable daughters",
                              "Reconstructed daughters"};
    
    for(int iEff=0; iEff<3; iEff++)
    {
      gDirectory->mkdir(effTypeName[iEff].Data());
      gDirectory->cd(effTypeName[iEff].Data());
      {
        for(int iH=0; iH<nPartEfficiency; iH++)
        {
          histo[iEff][iH] = new TProfile( partNameEff[iH].Data(), (GetDirectoryPath()+partAxisNameEff[iH]).Data(), nBinsEff[iH], xMinEff[iH], xMaxEff[iH]);
          histo[iEff][iH]->GetYaxis()->SetTitle("Efficiency");                  
          histo[iEff][iH]->GetXaxis()->SetTitle(partAxisNameEff[iH].Data());
        }
      }
      gDirectory->cd("..");// particle directory / Efficiency
    }
  }
  gDirectory->cd("..");// particle directory 
}

void KFParticlePerformanceBase::CreateParameterHistograms(TH1F* histoParameters[KFPartEfficiencies::nParticles][nHistoPartParam],
                                                          TH2F *histoParameters2D[KFPartEfficiencies::nParticles][nHistoPartParam2D],
                                                          int iPart, bool drawZR)
{
  TString parName[nHistoPartParam] = {"M","p","p_{t}","y","DecayL","c#tau","chi2ndf","prob","#theta","phi","X","Y","Z","R", "L", "l/dl","Multiplicity"};
  TString parTitle[nHistoPartParam];
  TString parName2D[nHistoPartParam2D] = {"y-p_{t}", "Z-R"};
  TString parTitle2D[nHistoPartParam2D];
  for(int iParam=0; iParam<nHistoPartParam; iParam++)
  {
    TString path = GetDirectoryPath();
    parTitle[iParam] = path + parName[iParam];
    if(iParam<nHistoPartParam2D)
      parTitle2D[iParam] = path + parName2D[iParam];
  }
  
  TString parAxisName[nHistoPartParam] = {"m [GeV/c^{2}]","p [GeV/c]","p_{t} [GeV/c]",
                                          "y","Decay length [cm]","Life time c#tau [cm]",
                                          "chi2/ndf","prob","#theta [rad]",
                                          "phi [rad]","x [cm]","y [cm]","z [cm]","Rxy [cm]", "L [cm]", "L/dL","Multiplicity"};
#ifdef CBM
  int nBins[nHistoPartParam] = {1000, // M
                                  100, // p
                                  100, // pt
                                  100, // y
                                  100, // DecayL
                                  100, // ctau
                                  100, // chi2/ndf
                                  100, // prob
                                  100, // theta
                                  100, // phi
                                  200, // X
                                  200, // Y
                                  360, // Z
                                  200, // R
                                  200, // L
                                  200, // L/dL
                                fParteff.partMaxMult[iPart]+1};
  float xMin[nHistoPartParam] = { fParteff.partMHistoMin[iPart], // M
                                    0.f, // p
                                    0.f, // pt
                                    0.f, // y
                                    -5.f, // DecayL
                                    0.f, // ctau
                                    0.f, // chi2/ndf
                                    0.f, // prob
                                    -2.f, // theta
                                    -2.f, // phi
                                  -50.f, // X
                                  -50.f, // Y
                                  -10.f, // Z
                                    0.f, // R
                                    0.f, // L
                                    -1.f, // L/dL
                                    -0.5f };
  float xMax[nHistoPartParam] = { fParteff.partMHistoMax[iPart], // M
                                    10.f, // p
                                      3.f, // pt
                                      6.f, // y
                                    55.f, // DecayL
                                    30.f, // ctau
                                    20.f, // chi2/ndf
                                      1.f, // prob
                                      2.f, // theta
                                      2.f, // phi
                                    50.f, // X
                                    50.f, // Y
                                    80.f, // Z
                                    50.f, // R
                                    100.f, // L
                                    35.f, // L/dL
                                  float(fParteff.partMaxMult[iPart])+0.5f};
#else
  int nBins[nHistoPartParam] = {1000, // M
                                  100, // p
                                  100, // pt
                                  100, // y
                                  100, // DecayL
                                  100, // ctau
                                  100, // chi2/ndf
                                  100, // prob
                                  100, // theta
                                  100, // phi
                                  100, // X
                                1000, // Y
                                1000, // Z
                                1000, // R
                                1000, // L
                                1000, // L/dL
                                fParteff.partMaxMult[iPart]+1};
  float xMin[nHistoPartParam] = { fParteff.partMHistoMin[iPart], // M
                                    0.f, // p
                                    0.f, // pt
                                    -6.f, // y
                                    -5.f, // DecayL
                                    0.f, // ctau
                                    0.f, // chi2/ndf
                                    0.f, // prob
                                    -2.f, // theta
                                    -2.f, // phi
                                  -200.f, // X
                                  -200.f, // Y
                                  -200.f, // Z
                                    0.f, // R
                                    0.f, // L
                                    -1.f, // L/dL
                                    -0.5f };
  float xMax[nHistoPartParam] = { fParteff.partMHistoMax[iPart], // M
                                    10.f, // p
                                      3.f, // pt
                                      6.f, // y
                                    55.f, // DecayL
                                    30.f, // ctau
                                    20.f, // chi2/ndf
                                      1.f, // prob
                                      2.f, // theta
                                      2.f, // phi
                                    200.f, // X
                                    200.f, // Y
                                    200.f, // Z
                                    200.f, // R
                                    400.f, // L
                                    35.f, // L/dL
                                  float(fParteff.partMaxMult[iPart])+0.5f};
#endif
  for(int iH=0; iH<nHistoPartParam; iH++)
  {
    histoParameters[iPart][iH] = new TH1F(parName[iH].Data(),parTitle[iH].Data(),
                                          nBins[iH],xMin[iH],xMax[iH]);
    histoParameters[iPart][iH]->GetXaxis()->SetTitle(parAxisName[iH].Data());
  }

  histoParameters2D[iPart][0] = new TH2F(parName2D[0].Data(),parTitle2D[0].Data(),
                                    nBins[3],xMin[3],xMax[3],
                                    nBins[2],xMin[2],xMax[2]);
  histoParameters2D[iPart][0]->GetXaxis()->SetTitle("y");
  histoParameters2D[iPart][0]->GetYaxis()->SetTitle("p_{t} [GeV/c]");

  if(drawZR)
  {
    histoParameters2D[iPart][1] = new TH2F(parName2D[1].Data(),parTitle2D[1].Data(),
                                      nBins[12],xMin[12],xMax[12],
                                      nBins[13],xMin[13],xMax[13]);
    histoParameters2D[iPart][1]->GetXaxis()->SetTitle("Z [cm]");
    histoParameters2D[iPart][1]->GetYaxis()->SetTitle("R [cm]");
  }
}

void KFParticlePerformanceBase::CreateParameterSubfolder(TString folderName, 
                                                         TH1F* histoParameters[4][KFPartEfficiencies::nParticles][nHistoPartParam],
                                                         TH2F* histoParameters2D[4][KFPartEfficiencies::nParticles][nHistoPartParam2D],
                                                         TH1F* histoFit[KFPartEfficiencies::nParticles][nFitQA], int iPart)
{
  gDirectory->mkdir(folderName.Data());
  gDirectory->cd(folderName.Data());
  {
    gDirectory->mkdir("Signal");
    gDirectory->cd("Signal");
    {
      CreateParameterHistograms(histoParameters[1], histoParameters2D[1], iPart);
    }
    gDirectory->cd("..");
    gDirectory->mkdir("Background");
    gDirectory->cd("Background");
    {
      CreateParameterHistograms(histoParameters[2], histoParameters2D[2], iPart);
    }
    gDirectory->cd("..");
    gDirectory->mkdir("Ghost");
    gDirectory->cd("Ghost");
    {
      CreateParameterHistograms(histoParameters[3], histoParameters2D[3], iPart);
    }
    gDirectory->cd("..");
    
    CreateParameterHistograms(histoParameters[0], histoParameters2D[0], iPart);
    if(histoFit!=0)
      CreateFitHistograms(histoFit[iPart], iPart);
  }
  gDirectory->cd("..");
}

TString KFParticlePerformanceBase::GetDirectoryPath()
{
  TString path = gDirectory->GetPath();
  int fileNamePosition = path.Index("Finder/");
  path.Remove(0, fileNamePosition+7);
  path.ReplaceAll("Particles/", "");
  path.ReplaceAll("/Parameters", "");
  path+=" ";
  return path;
}

void KFParticlePerformanceBase::FillHistos()
{

} // void KFParticlePerformanceBase::FillHistos()
#endif //DO_TPCCATRACKER_EFF_PERFORMANCE

