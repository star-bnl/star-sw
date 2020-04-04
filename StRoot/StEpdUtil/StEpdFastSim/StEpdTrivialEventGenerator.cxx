#include "./StEpdTrivialEventGenerator.h"
#include "TClonesArray.h"
#include "TVector3.h"
#include "TH1D.h"
#include "TRandom3.h"
#include "TMath.h"
#include <iostream>
//using namespace std;

// --------------------------------------------------------
// this function does a binary search to invert the equation
// C(phi) = xran
// where C(phi) is the normalized integral of 1+0.5*(v1*cos(phi) + v2*cos(2phi))
double FindPhi(double v1, double v2, double xran){
  double tolerance=0.01;
  double lo=0.0;
  double hi=2.0*TMath::Pi();
  double mid;
  do{
    mid = (hi+lo)/2.0;
    double Cvalue = (mid/2.0 + v1*sin(mid) + 0.5*v2*sin(2.0*mid))/TMath::Pi();
    if (Cvalue > xran){hi=mid;}
    else {lo=mid;}
  }    while(hi-lo>tolerance);
  return mid;
}


// --------------------------------------------------------
StEpdTrivialEventGenerator::StEpdTrivialEventGenerator(TH1D* DnDeta, TH1D* V1versusEta, TH1D* V2versusEta){
  if (DnDeta==0) std::cout << "You gave me no dNdeta histogram.  Um, okay, but you're getting nothing but empty events-- have fun!\n\n";
  mDnDeta = DnDeta;
  mV1versusEta = V1versusEta;
  mV2versusEta = V2versusEta;

  // check that the user made the x-axes the same.... please!
  int nbins = mDnDeta->GetXaxis()->GetNbins();
  float low = mDnDeta->GetXaxis()->GetBinLowEdge(1);
  float hi  = mDnDeta->GetXaxis()->GetBinUpEdge(1);
  if ((mV1versusEta->GetXaxis()->GetNbins() != nbins)
      || (mV2versusEta->GetXaxis()->GetNbins() != nbins)
      || (fabs(mV1versusEta->GetXaxis()->GetBinLowEdge(1)- low) > 0.000001)
      || (fabs(mV2versusEta->GetXaxis()->GetBinLowEdge(1)- low) > 0.000001)
      || (fabs(mV1versusEta->GetXaxis()->GetBinUpEdge(1)- hi) > 0.000001)
      || (fabs(mV2versusEta->GetXaxis()->GetBinUpEdge(1)- hi) > 0.000001))
      //      || (mV2versusEta->GetXaxis()->GetBinLowEdge(1) != low))
    //      || (mV1versusEta->GetXaxis()->GetBinUpEdge(1) != hi)
    //      || (mV2versusEta->GetXaxis()->GetBinUpEdge(1) != hi))
    {
      std::cout << "Dude, come on.  Give me the same x-axis.  I'm killing the v1 and v2 histograms.  Your events will be isotropic.\n";
      std::cout << low << " " << mV1versusEta->GetXaxis()->GetBinLowEdge(1) << " " << mV2versusEta->GetXaxis()->GetBinLowEdge(1) << "\n";
      mV1versusEta=0;
      mV2versusEta=0;
    }
  mRan = new TRandom3();
  mRan->SetSeed();
  mTracks = new TClonesArray("TVector3",1000);
}

// --------------------------------------------------------
StEpdTrivialEventGenerator::~StEpdTrivialEventGenerator(){
  /* no op */
}

// --------------------------------------------------------
TClonesArray* StEpdTrivialEventGenerator::Momenta(){
  if (mDnDeta==0) return 0;

  mTracks->Clear();

  // first, sample the dNdEta distribution.
  // each bin has an average, and we sample a Poissonian with that average
  
  for (int ibin=1; ibin<=mDnDeta->GetXaxis()->GetNbins(); ibin++){
    double ave = mDnDeta->GetBinContent(ibin)*mDnDeta->GetXaxis()->GetBinWidth(ibin);
    if (ave<0.00001) continue;
    int n = mRan->Poisson(ave);    
    for (int i=0; i<n; i++){
      double eta = mRan->Uniform(mDnDeta->GetXaxis()->GetBinLowEdge(ibin),mDnDeta->GetXaxis()->GetBinUpEdge(ibin));
      double v1 = (mV1versusEta==0)?0.0:mV1versusEta->GetBinContent(ibin);
      double v2 = (mV2versusEta==0)?0.0:mV2versusEta->GetBinContent(ibin);
      double phi = FindPhi(v1,v2,mRan->Uniform());
      double pt = 1.0;  // irrelevant, EPD only cares about eta and phi, and the simulator makes everything a MIP
      TVector3* v = (TVector3*)mTracks->ConstructedAt(mTracks->GetEntriesFast());
      v->SetPtEtaPhi(pt,eta,phi);
    }
    
  }
  
  return mTracks;
}



