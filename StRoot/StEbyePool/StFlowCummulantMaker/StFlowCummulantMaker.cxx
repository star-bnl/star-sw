////////////////////////////////////////////////////////////////////////////
//
// $Id: StFlowCummulantMaker.cxx,v 1.2 2003/09/02 17:57:58 perev Exp $
//
// Authors: Raimond Snellings and Art Poskanzer, LBNL, Aug 1999
//
////////////////////////////////////////////////////////////////////////////
//
// Description:  Maker to analyze Flow using the FlowTags and/or StFlowEvent
//
////////////////////////////////////////////////////////////////////////////
//
// $Log: StFlowCummulantMaker.cxx,v $
// Revision 1.2  2003/09/02 17:57:58  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.1  2001/01/31 19:48:17  snelling
// Cummulant calculation for the q-vector based on Ollitraults paper
//
//  
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include <stdlib.h>
#include <math.h>
#include "StMaker.h"
#include "StFlowCummulantMaker.h"
#include "StFlowMaker/StFlowMaker.h"
#include "StFlowMaker/StFlowEvent.h"
#include "StFlowTagMaker/StFlowTagMaker.h"
#include "StFlowMaker/StFlowConstants.h"
#include "StFlowMaker/StFlowSelection.h"
#include "StFlowMaker/StFlowCutTrack.h"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "TVector2.h"
#include "TFile.h"
#include "TString.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
#include "TProfile.h"
#include "TProfile2D.h"
#include "TF1.h"
#include "TOrdCollection.h"
#include "StMessMgr.h"
#define PR(x) cout << "##### FlowAnalysis: " << (#x) << " = " << (x) << endl;

ClassImp(StFlowCummulantMaker)

//-----------------------------------------------------------------------

StFlowCummulantMaker::StFlowCummulantMaker(const Char_t* name): StMaker(name),
  MakerName(name) {
  pFlowSelect = new StFlowSelection();
}

StFlowCummulantMaker::StFlowCummulantMaker(const Char_t* name,
					 const StFlowSelection& flowSelect) :
  StMaker(name), MakerName(name) {
  pFlowSelect = new StFlowSelection(flowSelect); //copy constructor
}

//-----------------------------------------------------------------------

StFlowCummulantMaker::~StFlowCummulantMaker() {
}

//-----------------------------------------------------------------------

Int_t StFlowCummulantMaker::Make() {
  // Make histograms

  // Get a pointer to StFlowEvent
  StFlowMaker* pFlowMaker = NULL;
  TString* makerName = new TString("Flow");
  makerName->Append(pFlowSelect->Number());
  pFlowMaker = (StFlowMaker*)GetMaker(makerName->Data());
  delete makerName;
  if (pFlowMaker) pFlowEvent = pFlowMaker->FlowEventPointer();
  if (pFlowEvent && pFlowSelect->Select(pFlowEvent)) {     // event selected
    // Event quantities
    if (pFlowEvent) {
      cout << "Calling FillFromFlowEvent: " << endl;
      FillFromFlowEvent();                   // get event quantities
    } else {
      gMessMgr->Info("##### FlowAnalysis: FlowEvent pointer null");
      return kStOK;
    }
    
    if (Debug()) StMaker::PrintInfo();
  }
  
  return kStOK;
}

//-----------------------------------------------------------------------

Int_t StFlowCummulantMaker::Init() {

  nevents = 0;
  mTotalMultInt = 0;
  zmax = 0.3;
  r0 = zmax /::sqrt((Float_t)count);
  
  for (int i = 0; i < count; i++) {
    rz[i] = r0*::sqrt((Float_t)count);
    for (int j = 0; j < 2*count; j++) {
      Float_t phi = 2. * acos(0.) * (Float_t)(j+1) / (Float_t)(i+1);
      xz[i][j] = rz[i]*cos(phi);
      yz[i][j] = rz[i]*sin(phi);
      g0[i][j] = 0.;
      cout << "phi: " << phi << endl;
      cout << "g0: " << g0[i][j] << endl;
      cout << "xz: " << xz[i][j] << endl;
      cout << "yz: " << yz[i][j] << endl;
    }
  }


  gMessMgr->SetLimit("##### FlowAnalysis", 2);
  gMessMgr->Info("##### FlowAnalysis: $Id: StFlowCummulantMaker.cxx,v 1.2 2003/09/02 17:57:58 perev Exp $");

  return StMaker::Init();
}

//-----------------------------------------------------------------------

void StFlowCummulantMaker::FillFromFlowEvent() {
  // Get event quantities from StFlowEvent

  pFlowSelect->SetSelection(0);
  pFlowSelect->SetHarmonic(1);
  pFlowSelect->SetSubevent(-1);
  // full event quantities

  nevents = nevents + 1;
  cout << " FlowAnalisysMaker: nevents: " << nevents << endl;
  mTotalMultInt = mTotalMultInt + pFlowEvent->Mult(pFlowSelect);
  Double_t sqx = pFlowEvent->Q(pFlowSelect).X(); 
  Double_t sqy = pFlowEvent->Q(pFlowSelect).Y();

  //Olli
  for (Int_t i = 0; i < count; i++) {
    for (Int_t j = 0; j < 2*count; j++) {
      g0[i][j] = g0[i][j] + exp(2.*xz[i][j]*sqx-2.*yz[i][j]*sqy);
    }
  }

}


//-----------------------------------------------------------------------

Int_t StFlowCummulantMaker::Finish() {
  // Calculates resolution and mean flow values
  // Fits q distribution and outputs phiWgt values

  // Ollitraults stuff
  Float_t mAvMult = (Float_t)mTotalMultInt / (Float_t)nevents;

  cout << "Average multiplicity: " << mAvMult << endl;
  cout << "n events: " << nevents << endl;

  for (int i = 0; i < count; i++) {
    cumul[i] = 0.;
    for (int j = 0; j < 2*count; j++) {
      g0[i][j] = g0[i][j] / (Float_t)nevents;
      cout << "g0: " << g0[i][j] << endl;
      cumul[i] = cumul[i] + ::log(g0[i][j]) / 2. * (Float_t)count;
      cout << "cumul: " << cumul[i] << endl;
    }
  }
  //     Interpolation formulas to obtain the first three terms in 
  //     the power series expansion of the generating function of 
  //     cumulants, defined in Eq.(82), and Eq.(D4). 
  //     The corresponding coefficients are then related  to the 
  //     integrated flow by Eqs.(83) which, unfortunately, contain 
  //     typos (coeff are wrong!). 
  //     The statistical error (due to the finite number of events) 
  //     and the systematic error (due to nonflow correlations)
  //     on v are also estimated using Eqs.(83). 
  //     First three coefficients (this is Eq.(D5))     
  Float_t a1=(3.*cumul[1]-3./2.*cumul[2]+1./3.*cumul[3])/r0*r0;
  cout << "a1: " << a1 << endl;
  Float_t a2=(-5./2.*cumul[1]+2.*cumul[2]-1./2.*cumul[3])/r0*r0*r0*r0;
  cout << "a2: " << a2 << endl;
  Float_t a3=(1./2.*cumul[1]-1./2.*cumul[2]+1./6.*cumul[3])/r0*r0*r0*r0*r0*r0;
  cout << "a3: " << a3 << endl;

  Float_t epsta = 1./::sqrt((Float_t)nevents);
  cout << "epsta: " << epsta << endl;
  cout << " " << endl;
  cout << " Q   <v>  stat. err.     syst. err. " << endl;

  Float_t q2 = a1;
  q2 = q2 -1.;
  if ( q2 <= 0. ) {
    cout << "order two formula not applicable" << endl;
  } else {
    Float_t qo2 = ::sqrt(q2);
    Float_t vo2 = ::sqrt(q2)/::sqrt(mAvMult);
    Float_t erst2 = ::sqrt(q2+epsta)/::sqrt(mAvMult)-vo2;
    Float_t ersy2 = ::sqrt(q2+1.)/::sqrt(mAvMult)-vo2;
    cout << qo2 << " " << vo2 << " " << erst2 << " " << ersy2 << endl;
  }
  //     Result of the method to order 4 (second of Eqs.(83))
  //     (2 particle nonflow correlations eliminated)
  Float_t q4 = -4. * a2;
  //     subtraction of autocorrelations
  q4 = q4-1. / mAvMult;
  if (q4 <= 0.) {
    cout << "order four formula not applicable" << endl;
  } else {
    Float_t qo4 = ::pow(q4, 1./4.);
    Float_t vo4 = ::pow(q4, 1./4.)/::sqrt(mAvMult);
    Float_t erst4 = ::pow(q4 + 4.*epsta, 1./4.)/::sqrt(mAvMult) - vo4;
    Float_t ersy4 = ::pow(q4 + 1./mAvMult, 1./4.)/::sqrt(mAvMult) - vo4;
    cout << qo4 << " " << vo4 << " " << erst4 << " " << ersy4 << endl;
  }
  //     Result of the method to order 6 (third of Eqs.(83))
  //     (4 particle nonflow correlations eliminated)
  Float_t q6 = 9. * a3;
  //     subtraction of autocorrelations
  q6 = q6 - 1. / mAvMult*mAvMult;   
  if (q6 <= 0.) {
    cout << "order six formula not applicable" << endl;
  } else {
    Float_t qo6 = ::pow(q6, 1./6.);
    Float_t vo6 = ::pow(q6, 1./6.) / ::sqrt(mAvMult);
    Float_t erst6 = ::pow(q6+10.*epsta, 1./6.) / ::sqrt(mAvMult) - vo6;
    Float_t ersy6 = ::pow(q6 + 2./mAvMult*mAvMult, 1./6.)/::sqrt(mAvMult) - vo6;
    cout << qo6 << " " << vo6 << " " << erst6 << " " << ersy6 << endl;
  }

  // Print the selection object details
  pFlowSelect->PrintList();

  delete pFlowSelect;

  cout << endl;
  gMessMgr->Summary(3);
  cout << endl;



  return StMaker::Finish();
}





