/***************************************************************************
 *
 * $Id: StEbyeScaTagsMaker.cxx,v 1.4 1999/05/01 00:56:59 fisyak Exp $
 *
 * Author: Jeff Reid, UW, Feb 1999
 ***************************************************************************
 *
 * Description:  Maker to fill the SCA EbyE Tags
 *
 ***************************************************************************
 *
 * $Log: StEbyeScaTagsMaker.cxx,v $
 * Revision 1.4  1999/05/01 00:56:59  fisyak
 * Change Clear function to defualt
 *
 * Revision 1.3  1999/03/30 20:32:23  wenaus
 * Update for new Maker; explicit StGlobalTrack include
 *
 * Revision 1.2  1999/03/20 21:02:26  perev
 * new maker schema
 *
 * Revision 1.1.1.1  1999/02/21 21:08:03  jgreid
 * Ebye SCA Tag Filler
 *
 **************************************************************************/
#include "StEbyeScaTagsMaker.h"
#include "StRoot/StEventReaderMaker/StEventReaderMaker.h"
#include "StChain/StChain.h"
#include "StEvent/StRun.hh"
#include "StEvent/StEvent.hh"
#include "StEvent/StGlobalTrack.hh"
#include "SystemOfUnits.h"

// define values for temperature calculation
#define PI_MASS 0.139569
#define NBINS 50
#define PI 3.1415926

ClassImp(StEbyeScaTagsMaker)

StEbyeScaTagsMaker::StEbyeScaTagsMaker(const Char_t *name, const Char_t *title) : StMaker(name, title) {
  drawinit = kFALSE;
  theTag = 0;
}

StEbyeScaTagsMaker::~StEbyeScaTagsMaker() {
}

Int_t StEbyeScaTagsMaker::Make() {
  StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
  if (! evMaker->event()) return kStOK; // If no event, we're done
  StEvent& ev = *(evMaker->event());
  StRun& run = *(evMaker->run());

  // OK, we've got the event. Do what thou wilst.

  // Create and fill the tag
  if (theTag) delete theTag;
  theTag = new ScaTag_st;
  fillTag(ev,*theTag);

  return kStOK;
}

void StEbyeScaTagsMaker::PrintInfo() {
  cout << "$Id: StEbyeScaTagsMaker.cxx,v 1.4 1999/05/01 00:56:59 fisyak Exp $" << endl;
  if (Debug()) StMaker::PrintInfo();
}


ScaTag_st* StEbyeScaTagsMaker::tag() {
    return theTag;
}

void StEbyeScaTagsMaker::fillTag(StEvent& event, ScaTag_st& scaTag) {
  // get field from somewhere!
  // **  Is this correct?  **
  float bField = 0.5*tesla;

  // Initialize Iterator, loop variables
  StTrackCollection* tracks = event.trackCollection();
  StTrackIterator itr = tracks->begin();
  StTrackIterator lastTrack = tracks->end();
  StGlobalTrack *currentTrack = 0;
  
  double mt_histo[NBINS];
  
  /* Reset pt, mt, eta & phi  histograms  */
  memset (&mt_histo, 0, sizeof(double)*NBINS);

  // hard-wire some numbers temporarily
  float mt_min  = 0;
  float mt_max  = 1.5;
 
  float eta_min = -1;
  float eta_max = 1;

  // define variables
  float deta1 = eta_max - eta_min;

  float mt_binsize  = (mt_max - mt_min)/NBINS;
  float dmt         = mt_binsize;
  float mtweight1   = 1./(deta1*dmt);

  float pt, mt;
  float dip;
  float theta, eta;
  int imtbin;

  float trackCount = 0.0;
  float meanPt = 0.0;
  float meanPtSquared = 0.0;
  float meanEta = 0.0;
  float meanEtaSquared = 0.0;

  // ** track loop **
  for (; itr != lastTrack; itr++) {
    currentTrack = *itr;

    // get the momentum of the current track
    pt = currentTrack->helix().momentum(bField).perp();

    mt = sqrt(pt*pt + PI_MASS*PI_MASS)-PI_MASS;
    imtbin  = (mt - mt_min)/mt_binsize;

    dip = currentTrack->helix().dipAngle();
    theta = (PI/2.0)-atan(dip);
    eta = -log(tan(theta/2.0));

    // ** rapidity cut [cut #2] 
    if ((eta > eta_min) && (eta < eta_max)) {

      // ** cut out extreme pt values [cut #1]
      if ((pt > 0) && (pt < 20.0)) {

        /* dN/mt*dy*dmt histogram */
        if (0<=imtbin && imtbin<NBINS) mt_histo[imtbin] += mtweight1/mt; 

        // calculate number of particles that make the cuts, and the first two pt moments
        trackCount++;
        meanPtSquared += pt*pt;
        meanPt += pt;

	meanEtaSquared += eta*eta;
	meanEta += eta;

      } // [cut #1]

    } // [cut #2]

  } // ** end of track loop **
  meanPtSquared /= trackCount;
  meanPt /= trackCount;
  meanEtaSquared /= trackCount;
  meanEta /= trackCount;

  // fill the chargedParicles_Means array in the sca Tag

  // 0 - event multiplicity
  scaTag.chargedParticles_Means[0] = trackCount;
  // 1 - eventwise mean transverse momentum
  scaTag.chargedParticles_Means[1] = meanPt;
  // 2 - eventwise mean transverse momentum squared
  scaTag.chargedParticles_Means[2] = meanPtSquared;
  // 3 - eventwise mean rapidity
  scaTag.chargedParticles_Means[3] = meanEta;
  // 4 - eventwise mean rapidity squared
  scaTag.chargedParticles_Means[4] = meanEtaSquared;
  // 5 - estimated temperature of the event
  //     (based on slope fit to 1/mt dN/dmt)
  scaTag.chargedParticles_Means[5] = mtInverseSlope(mt_histo, 0, NBINS);

  cout << trackCount << " " << meanPt/GeV << " " << meanPtSquared/(GeV*GeV) << endl;
}

float StEbyeScaTagsMaker::mtInverseSlope(double *mthisto, int ibegin, int istop) {
  
  float mtx, mt_binsize, invslope;
  float s=0, sx=0, sy=0, sxx=0, sxy=0, delta=0;
  int   index;

  // hard wire these numbers temporarily
  float mt_min  = 0;
  float mt_max  = 1.5;
 
  float eta_min = -1;
  float eta_max = 1;

  mt_binsize  = (mt_max - mt_min)/NBINS;

  /*  Do a Linear Least Square fit to  log(dN/mt*dy*dmt) = -mt/T  */
  for  (index=ibegin; index<istop;  index++) {
    if (!mthisto[index])
      continue;
    mtx  = mt_binsize*(float)index + mt_binsize/2.;
    sx  += mtx;
    sy  += log(mthisto[index]);
    sxx += mtx*mtx;
    sxy += log(mthisto[index])*mtx;
    s++;
  }
  delta    = s*sxx - sx*sx;
  invslope = fabs ((s*sxy - sx*sy)/delta);
  invslope = 1./invslope;

  return invslope;
}

void StEbyeScaTagsMaker::printTag(ostream& os) {
    os << "--- Event-by-Event SCA Tag Table ---" << endl; 
    if (!theTag) os << "(tag is empty)" << endl;
    else {
      os <<  "N = " << theTag->chargedParticles_Means[0] << endl;
      os <<  "<pt> = " << theTag->chargedParticles_Means[1] << endl;
      os <<  "<pt^2> = " << theTag->chargedParticles_Means[2] << endl;
      os <<  "<y> = " << theTag->chargedParticles_Means[3] << endl;
      os <<  "<y^2> = " << theTag->chargedParticles_Means[4] << endl;
      os <<  "T = " << theTag->chargedParticles_Means[5] << endl;
      os <<  "...and more to be filled later" << endl;
    }
}

Int_t StEbyeScaTagsMaker::Init() {
  return StMaker::Init();
}

void StEbyeScaTagsMaker::Clear(Option_t *opt) {
  if (theTag) {
    delete theTag;
    theTag = 0;
  }
  StMaker::Clear();
}

Int_t StEbyeScaTagsMaker::Finish() {
  return kStOK;
}
