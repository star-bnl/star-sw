#ifdef __CINT__

#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;

#pragma link C++ class PlotFile;
#endif

#ifndef __CINT__
#include <iostream>
#include <iomanip>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include "math.h"
#include "string.h"

#include "TROOT.h"
#include "TFile.h"

#include "TChain.h"
#include "TH1.h"
#include "TH2.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TProfile.h"
#include "TTree.h"
#include "TNtuple.h"
#include "TRandom.h"
#include "TMath.h"
#include "TVector2.h"
#include "TSystem.h"
#include "TUnixSystem.h"
#endif
using namespace std;

#include "tof.h"

int main(int argc, char **argv)
{
  if(argc !=2) exit(1);
  if(!gSystem) TUnixSystem *gSystem = new TUnixSystem();

  //----------------------------------
  // Open files and add to the chain
  //----------------------------------
  TChain *chain = new TChain("tof");
  
  //void* dir = gSystem->OpenDirectory(gSystem->expandpathname(argv[1]));
  //cout << gSystem->expandpathname(argv[1]) << endl;
  int nruns=0;
  char *file_name;
  TString Tname;
  char file_list[2500][256];
  
  cout << " execute : " << argv[0] << " " << argv[1] << endl;
//  do {
    //file_name = (char*)gSystem->GetDirEntry(dir);
    file_name = argv[1];
    Tname=file_name;
    cout << Tname.Data() << endl;
    if (file_name && Tname.Contains("root")) {
      //sprintf(file_list[nruns],"%s/%s",argv[1],file_name);
      sprintf(file_list[nruns],"./%s",file_name);
      //if(nruns>1) { 
      chain->Add(file_list[nruns]);
      cout << " read in " << file_list[nruns] << endl;
      //}
      nruns++;
    }
  //} while (file_name);

  //----------------------
  // Book the histograms
  //----------------------
  Float_t mrpcA[32] = { 0.,  0.,  0.,  0.,  0., 0., 16., 16.,
                       20., 22., 22., 22., 22., 22., 26., 26.,
                                  26., 26., 30., 30., 32., 32., 32., 32.,
                                  32., 32., 32., 32., 32., 32., 32., 32.};
  Int_t MIdStart[8] = {1, 7, 9, 10, 15, 19, 21, 33};
  TH2D *mYLocal[7];
  TH2D *mZLocal[7];
  TH2D *mYLocalPhi[7];  // vs phi
  TH2D *mZLocalPhi[7];  // vs phi
  for(int i=0;i<7;i++) {
    char name[100];
    sprintf(name,"yLocal_%d",i);
    mYLocal[i] = new TH2D(name,"",120,0.,120.,500,-5.,5.);
    sprintf(name,"zLocal_%d",i);
    mZLocal[i] = new TH2D(name,"",120,0.,120.,600,-6.,6.);
    sprintf(name,"yLocalPhi_%d",i);
    mYLocalPhi[i] = new TH2D(name,"",120,-3.,717.,500,-5.,5.);
    sprintf(name,"zLocalPhi_%d",i);
    mZLocalPhi[i] = new TH2D(name,"",120,-3.,717.,600,-6.,6.);
  }
  TH2D *mYLocalAll = new TH2D("yLocal_all","",120,0.,120.,500,-5.,5.);
  TH2D *mYLocalPhiAll = new TH2D("yLocalPhi_all","",120,0.,120.,500,-5.,5.);

  //--------------------
  // loop the chain
  //--------------------
  //  TTree *tree = (TTree *)chain->GetTree();
  int nevents = (int)chain->GetEntries();
  cout << "== total entries : " << nevents << endl;
  
  tof *t = new tof(chain);
  for(int i=0;i<nevents;i++){
    if(i%10000==0) cout << "begin " << i << "th entry...." << endl;
    t->GetEntry(i);

    float vz = t->vertexZ;
    if(fabs(vz)>30.) continue;

    int nhits = t->nTofHits;
    for(int j=0;j<nhits;j++) {
      int nHits = t->nHitsFit[j];
      if(abs(nHits)<25) continue;
//      float dcaXY = sqrt(pow(t->dcaX[j],2.)+pow(t->dcaY[j],2.));
//      if(dcaXY>1.) continue;
//      float dcaZ = t->dcaZ[j];
//      if(fabs(dcaZ)>1.) continue;
      float pt = t->pt[j];
      if(pt<0.5) continue;       /// select good quality track from TPC center


      int tray = t->tray[j];
      int module = t->module[j];
      int m_index = -1;
      for(int k=0;k<7;k++) {
        if(module>=MIdStart[k] && module <MIdStart[k+1]) {
          m_index = k;  break;
        }
      }
      if(m_index<0 || m_index>=7 ) continue;

      float phi;
      if(tray<=60) {
        phi = 72. - (tray-1)*6.;
        if(phi<-3.) phi+= 360.;
      } else {
        phi = 108 + (tray-61) * 6;
        if(phi>357.) phi-= 360.;

        phi += 360.;   // shifted by 360 for east
      }

      float yLocal = t->yLocal[j];
      float zLocal = t->zLocal[j];
      mYLocalPhi[m_index]->Fill(phi, yLocal);
      mZLocalPhi[m_index]->Fill(phi, zLocal);

      mYLocal[m_index]->Fill(tray-1, yLocal);
      mZLocal[m_index]->Fill(tray-1, zLocal);

      mYLocalAll->Fill(tray-1, yLocal);
      mYLocalPhiAll->Fill(phi, yLocal);
    }
  } // end event looping
  
  ///////////////////////
  // histograms output
  ///////////////////////
  TFile *f2 = new TFile("align.root","recreate");
  f2->cd();
  for(int i=0;i<7;i++) {
    mYLocal[i]->Write();
    mZLocal[i]->Write();
    mYLocalPhi[i]->Write();
    mZLocalPhi[i]->Write();
  }
  mYLocalAll->Write();
  mYLocalPhiAll->Write();
  f2->Close();
  
  cout<<"end of program"<<endl;
  return(0);
  exit(0);
}
