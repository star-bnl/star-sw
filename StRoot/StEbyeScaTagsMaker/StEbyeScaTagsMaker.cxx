/***************************************************************************
 *
 * $Id: StEbyeScaTagsMaker.cxx,v 1.11 2000/02/04 22:44:30 jgreid Exp $
 *
 * Author: Jeff Reid, UW, Feb 1999
 ***************************************************************************
 *
 * Description:  Maker to fill the SCA EbyE Tags
 *
 ***************************************************************************
 *
 * $Log: StEbyeScaTagsMaker.cxx,v $
 * Revision 1.11  2000/02/04 22:44:30  jgreid
 * added functionality for ScaTags to be picked up by the TagDB after filling
 *
 * Revision 1.10  2000/01/25 15:31:15  fisyak
 * Add namespace for CC5
 *
 * Revision 1.9  2000/01/04 19:50:20  jgreid
 * modified to access tracks through the track node scheme
 *
 * Revision 1.8  1999/09/24 01:22:57  fisyak
 * Reduced Include Path
 *
 * Revision 1.7  1999/07/15 13:57:00  perev
 * cleanup
 *
 * Revision 1.6  1999/06/27 22:45:27  fisyak
 * Merge StRootEvent and StEvent
 *
 * Revision 1.5  1999/05/27 17:19:24  jgreid
 * fixed rapidity calculation bug and added additional QC cuts
 *
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
#include "StChain.h"
#include "StRun.h"
#include "StEventTypes.h"
#include "StGlobalTrack.h"

#include "SystemOfUnits.h"
#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

// define values for temperature calculation
#define PI_MASS 0.139569
#define NBINS 50

ClassImp(StEbyeScaTagsMaker)

StEbyeScaTagsMaker::StEbyeScaTagsMaker(const Char_t *name, const Char_t *title) : StMaker(name, title) {
}

StEbyeScaTagsMaker::~StEbyeScaTagsMaker() {
}

Int_t StEbyeScaTagsMaker::Make() {
#if 0
  StEventReaderMaker* evMaker = (StEventReaderMaker*) gStChain->Maker("events");
  if (! event()) return kStOK; // If no event, we're done
  StEvent& ev = *(evMaker->event());
#endif
  StEvent* mEvent = (StEvent *) GetInputDS("StEvent");
  if (!mEvent) return kStOK; // If no event, we're done
  StEvent& ev = *mEvent;
  StRun *run = (StRun *) GetInputDS("StRun");

  // OK, we've got the event. Do what thou wilst.

  // tag filling based on StFlowTagMaker
  // Create a new tag
  mTagHeader = NULL;
  mTag = NULL;
  
  // instantiate new St_ScaTag class
  mTagHeader = new St_ScaTag("ScaTag",1); // table header
  // set the size of the table
  mTagHeader->SetNRows(1);
  // add ScaTag table to the root .data directory
  AddData(mTagHeader,".data");
  // get a pointer to the c-struct containing the variables
  mTag = mTagHeader->GetTable(); // table structure

  //and fill the tag
  fillTag(ev);

  return kStOK;
}

ScaTag_st* StEbyeScaTagsMaker::tag() {
    return mTag;
}

void StEbyeScaTagsMaker::fillTag(StEvent& event) {

  double mt_histo[NBINS];
  
  /* Reset pt, mt, eta & phi  histograms  */
  memset (&mt_histo, 0, sizeof(double)*NBINS);

  // hard-wire some numbers temporarily
  float mt_min  = 0;
  float mt_max  = 1.5;
 
  float eta_min = -1;
  float eta_max = 1;

  float dcaX_min = -2;
  float dcaX_max = 2;

  float dcaY_min = dcaX_min;
  float dcaY_max = dcaX_max;

  // define variables
  float deta1 = eta_max - eta_min;

  float mt_binsize  = (mt_max - mt_min)/NBINS;
  float dmt         = mt_binsize;
  float mtweight1   = 1./(deta1*dmt);

  float pt, mt;
  float charge;

  float dip;
  float theta, eta;

  float nFound, nMax;

  int imtbin;

  float trackCount = 0.0;
  float meanPt = 0.0;
  float meanPtSquared = 0.0;
  float meanEta = 0.0;
  float meanEtaSquared = 0.0;

  double s;
  StThreeVectorD dca, p;

  double dcaX, dcaY, dcaZ, dcaM;
  
  // temporarily use the first (currently only) primary vertex
  StVertex *primeVertex = event.primaryVertex();

  StThreeVectorD origin(0,0,0);
  StThreeVectorD primaryVertexPosition;

  // uncomment the next line (and 'outFile' << line below) to APPEND output to a file
  //  !! If this file already exists it will just add the new data to the end !!
  // ofstream outFile("EbyeSca.out",ios::app);

  // Loop over 'event' vertices and their primary tracks
  // ** track loop **
  if (primeVertex) {
    primaryVertexPosition = primeVertex->position();
    const StSPtrVecTrackNode& theNodes = event.trackNodes();
    for (unsigned int k=0; k<theNodes.size(); k++) {

      // get the momentum of the current track
      pt = theNodes[k]->track(global)->geometry()->momentum().perp();

      // get the charge of the current track
      charge = theNodes[k]->track(global)->geometry()->charge();

      // get Nfound & Nmax
      nFound = theNodes[k]->track(global)->fitTraits().numberOfFitPoints();
      nMax = theNodes[k]->track(global)->numberOfPossiblePoints();

      // calculate distance of closest approach to the primary vertex position
      s = theNodes[k]->track(global)->geometry()->helix().pathLength(primaryVertexPosition);
      p = theNodes[k]->track(global)->geometry()->helix().at(s);
      dca = p-primaryVertexPosition;
      dcaX = dca.x()/centimeter;
      dcaY = dca.y()/centimeter;
      dcaZ = dca.z()/centimeter;
      dcaM = (abs(dca))/centimeter;

      // calculate mt (needed for temperature calculation)
      mt = sqrt(pt*pt + PI_MASS*PI_MASS)-PI_MASS;
      imtbin  = (int) ((mt - mt_min)/mt_binsize);

      // calculate eta
      dip = theNodes[k]->track(global)->geometry()->dipAngle();
      theta = (M_PI/2.0)-dip;
      eta = -log(tan(theta/2.0));

      // ** transverse DCA cut [cut #3] 
      if (((dcaX > dcaX_min) && (dcaX < dcaX_max)) && ((dcaY > dcaY_min) && (dcaY < dcaY_max))) {
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

      } // [cut #3]

    } // ** end of track loop **

    meanPtSquared /= trackCount;
    meanPt /= trackCount;
    meanEtaSquared /= trackCount;
    meanEta /= trackCount;

    // fill the chargedParicles_Means array in the sca Tag

    // 0 - event multiplicity
    mTag->chargedParticles_Means[0] = trackCount;
    // 1 - eventwise mean transverse momentum
    mTag->chargedParticles_Means[1] = meanPt;
    // 2 - eventwise mean transverse momentum squared
    mTag->chargedParticles_Means[2] = meanPtSquared;
    // 3 - eventwise mean rapidity
    mTag->chargedParticles_Means[3] = meanEta;
    // 4 - eventwise mean rapidity squared
    mTag->chargedParticles_Means[4] = meanEtaSquared;
    // 5 - estimated temperature of the event
    //     (based on slope fit to 1/mt dN/dmt)
    mTag->chargedParticles_Means[5] = mtInverseSlope(mt_histo, 0, NBINS);

    //uncomment the next line to send the analysis results to cout
    //cout << trackCount << " " << meanPt/GeV << " " << meanPtSquared/(GeV*GeV) << endl;

    //uncomment the next line (and declaration of outFile above) to append results to a file
    //outFile << trackCount << " " << meanPt/GeV << " " << meanPtSquared/(GeV*GeV) << endl;
  }

}

float StEbyeScaTagsMaker::mtInverseSlope(double *mthisto, int ibegin, int istop) {
  
  float mtx, mt_binsize, invslope;
  float s=0, sx=0, sy=0, sxx=0, sxy=0, delta=0;
  int   index;

  // hard wire these numbers temporarily
  float mt_min  = 0;
  float mt_max  = 1.5;
 
  //float eta_min = -1;
  //float eta_max = 1;

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
    if (!mTag) os << "(tag is empty)" << endl;
    else {
      os <<  "N = " << mTag->chargedParticles_Means[0] << endl;
      os <<  "<pt> = " << mTag->chargedParticles_Means[1] << endl;
      os <<  "<pt^2> = " << mTag->chargedParticles_Means[2] << endl;
      os <<  "<y> = " << mTag->chargedParticles_Means[3] << endl;
      os <<  "<y^2> = " << mTag->chargedParticles_Means[4] << endl;
      os <<  "T = " << mTag->chargedParticles_Means[5] << endl;
      os <<  "...and more to be filled later" << endl;
    }
}

Int_t StEbyeScaTagsMaker::Init() {
  return StMaker::Init();
}

void StEbyeScaTagsMaker::Clear(Option_t *opt) {
  //if (mTag) {
  //  delete mTag;
  //  mTag = NULL;
  //}
  StMaker::Clear();
}

Int_t StEbyeScaTagsMaker::Finish() {
  return kStOK;
}
