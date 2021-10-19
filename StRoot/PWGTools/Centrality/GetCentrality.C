// ROOT headers
#include "TMath.h"
#include "TRandom3.h"
#include "TCanvas.h"
#include "TF1.h"
#include "TH1D.h"
#include "TH1D.h"
#include "TGraph.h"
#include "TAxis.h"
#include "TGraph2D.h"
#include "TGraphErrors.h"
#include "TH1F.h"
#include "TH1F.h"
#include "TFile.h"
#include "TNtuple.h"

// C++ headers
#include <iostream>
#include <numeric>
#include <sstream>
#include <chrono>

//_________________
void getcentralitybins() {
  TFile *f0 = new TFile("Case3/Zr/Ratio_npp2.386_k3.889_x0.123_eff0.024.root");
  TH1D *sim = (TH1D *)f0->Get("hRefMultSim");
  TH1D *data = (TH1D *)f0->Get("hRefMult");
    
  int centralitybin[16][2];
  double integral = sim->Integral();
  for(int cent=0; cent<16; cent++){
    double distance = 1000.0;
    double fraction = 0.05*((double)cent+1.0);
    //For the most central bins, integrate the data
    if(cent<4){
      for(int i=0; i<data->GetNbinsX(); i++){
	double thisfraction = (data->Integral(i,500))/(integral);
	double thisdistance = TMath::Abs(thisfraction - fraction);
	if(thisdistance>distance){
	  if(cent==0){centralitybin[0][0]=500; centralitybin[0][1]=i-1;centralitybin[1][0]=i-2;}
	  else{centralitybin[cent][1]=i-1;centralitybin[cent+1][0]=i-2;}
	  break;
	}
	else{distance=thisdistance;}
      }
    }
    //For more peripheral bins, integrate the simulated distribution
    else{
      int newmaxbin = centralitybin[3][1]-1;
      double zeroToTwentyIntegral = data->Integral(newmaxbin+1,500);
      for(int i=0; i<newmaxbin; i++){
	double thisfraction=(sim->Integral(i,newmaxbin)+zeroToTwentyIntegral)/(integral);
	double thisdistance = TMath::Abs(thisfraction - fraction);
	if(thisdistance>distance){
	  if(cent==15){centralitybin[15][1]=i-2;}
	  else{centralitybin[cent][1]=i-1;centralitybin[cent+1][0]=i-2;}
	  break;
	}
	else{distance=thisdistance;}
      }
    }
  }
  
  std::cout<<"******* 16 Bins *******"<<std::endl;
  std::cout<<"High bins"<<std::endl;
  for(int i=0; i<16; i++){
    std::cout<<centralitybin[i][0]<<std::endl;
  }
  std::cout<<"Low bins"<<std::endl;
  for(int i=0; i<16; i++){
    std::cout<<centralitybin[i][1]<<std::endl;
  }
  std::cout<<"******* 9 Bins *******"<<std::endl;
  std::cout<<"High bins"<<std::endl;
  for(int i=0; i<9; i++){
    if(i==0 || i==1){std::cout<<centralitybin[i][0]<<std::endl;}
    else{std::cout<<centralitybin[2*i-1][0]<<std::endl;}
  }
  std::cout<<"Low bins"<<std::endl;
  for(int i=0; i<9; i++){
    if(i==0 || i==1){std::cout<<centralitybin[i][1]<<std::endl;}
    else{std::cout<<centralitybin[2*i-1][1]<<std::endl;}
  }
}
