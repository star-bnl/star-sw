/***************************************************************************
 *
 * $Id: StEbyeScaTagsMaker.cxx,v 1.21 2000/06/27 17:47:26 jseger Exp $
 *
 * Author: Jeff Reid, UW, Feb 1999
 ***************************************************************************
 *
 * Description:  Maker to fill the SCA EbyE Tags
 *
 ***************************************************************************
 *
 * $Log: StEbyeScaTagsMaker.cxx,v $
 * Revision 1.21  2000/06/27 17:47:26  jseger
 * Removed default production of Ebyesca.out file
 *
 * Revision 1.20  2000/06/06 17:55:39  jgreid
 * Don't use exit(0)lsls Us kStWarn instead!
 *
 * Revision 1.19  2000/06/05 13:42:50  jgreid
 * change to exit(0) on primary vertex failure
 *
 * Revision 1.18  2000/05/24 13:36:30  fisyak
 * Add cast to make  Solaris happy
 *
 * Revision 1.17  2000/02/29 23:04:01  jgreid
 * bug fix
 *
 * Revision 1.16  2000/02/25 20:04:59  jgreid
 * changed to primary tracks, added abs(y) calculation
 *
 * Revision 1.15  2000/02/21 19:07:18  jgreid
 * added return value to fillTag()
 *
 * Revision 1.14  2000/02/17 19:46:52  jgreid
 * bug fix
 *
 * Revision 1.13  2000/02/17 19:44:59  jgreid
 * added separate global variable calculations for different charge species
 *
 * Revision 1.12  2000/02/15 21:21:15  jgreid
 * added multiple primary vertex handling
 *
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
  StRun *run;
  run = (StRun *) GetInputDS("StRun");

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
  if (fillTag(ev) != kStErr) {
    return kStOK;
  } else {
    return kStErr;
  }
}

ScaTag_st* StEbyeScaTagsMaker::tag() {
    return mTag;
}

Int_t StEbyeScaTagsMaker::fillTag(StEvent& event) {

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

  float trackCountPlus = 0.0;
  float meanPtPlus = 0.0;
  float meanPtSquaredPlus = 0.0;
  float meanEtaPlus = 0.0;
  float meanEtaSquaredPlus = 0.0;
  float totalAbsEtaPlus = 0.0;

  float trackCountMinus = 0.0;
  float meanPtMinus = 0.0;
  float meanPtSquaredMinus = 0.0;
  float meanEtaMinus = 0.0;
  float meanEtaSquaredMinus = 0.0;
  float totalAbsEtaMinus = 0.0;

  double s;
  StThreeVectorD dca, p;

  double dcaX, dcaY, dcaZ, dcaM;
  
  // temporarily use the first (currently only) primary vertex
  StVertex *primeVertex;
  const StTrack *currentTrack;

  StThreeVectorD origin(0,0,0);
  StThreeVectorD primaryVertexPosition;

  // The tags are filled and put in .data for tag collection by the infrastructure
  //  for debugging and/or test analysis runs the user can uncomment the ascii
  //  file output code and look at the analysis results directly 

  // uncomment the next line (and 'outFile' << line below) to APPEND output to a file
  //  !! If this file already exists it will just add the new data to the end !!
  //ofstream outFile("EbyeSca.out",ios::app);

  // Number of primary vertices
  Int_t npvtx = event.numberOfPrimaryVertices();

  // If there is no found vertex exit with no error
  //   so that the DST can be written
  if (npvtx == 0) return kStWarn;
  
  // loop over these and choose the one with the most daughters,
  //  or default to primaryVertex(0) if there is only one
  primeVertex = event.primaryVertex(0);
  for (Int_t i = 1 ; i < npvtx ; i++) {
    if (event.primaryVertex(i)->numberOfDaughters() > primeVertex->numberOfDaughters())
      primeVertex = event.primaryVertex(i);
  } 

  // Loop over 'event' vertices and their primary tracks
  // ** track loop **
  if (primeVertex) {
    primaryVertexPosition = primeVertex->position();
    const StSPtrVecTrackNode& theNodes = event.trackNodes();
    for (unsigned int k=0; k<theNodes.size(); k++) {

      currentTrack = theNodes[k]->track(primary);
      if (currentTrack) {

        // cut out tracks marked as bad [cut #1]
	if (currentTrack->flag() > 0) {

          // cut out tracks with bad goodness of fit [cut #2]
          if (currentTrack->fitTraits().chi2() < 3) {

            // get the momentum of the current track
            pt = currentTrack->geometry()->momentum().perp();

            // get the charge of the current track
            charge = currentTrack->geometry()->charge();

            // get Nfound & Nmax
            nFound = currentTrack->fitTraits().numberOfFitPoints();
            nMax = currentTrack->numberOfPossiblePoints();

            // ** nFound/nMax cut [cut #3] 
            if ( (nFound/nMax) > 0.5 ) {

              // calculate distance of closest approach to the primary vertex position
              s = currentTrack->geometry()->helix().pathLength(primaryVertexPosition);
              p = currentTrack->geometry()->helix().at(s);
              dca = p-primaryVertexPosition;
              dcaX = dca.x()/centimeter;
              dcaY = dca.y()/centimeter;
              dcaZ = dca.z()/centimeter;
              dcaM = (abs(dca))/centimeter;

              // calculate mt (needed for temperature calculation)
              mt = sqrt(pt*pt + PI_MASS*PI_MASS)-PI_MASS;
              imtbin  = (int) ((mt - mt_min)/mt_binsize);

              // calculate eta
              dip = currentTrack->geometry()->dipAngle();
              theta = (M_PI/2.0)-dip;
              eta = -log(tan(theta/2.0));

              // ** transverse DCA cut [cut #4] 
              if (((dcaX > dcaX_min) && (dcaX < dcaX_max)) && ((dcaY > dcaY_min) && (dcaY < dcaY_max))) {

                // ** rapidity cut [cut #5] 
                if ((eta > eta_min) && (eta < eta_max)) {

                  // ** cut out extreme pt values [cut #6]
                  if ((pt > 0) && (pt < 20.0)) {

                    /* dN/mt*dy*dmt histogram */
                    if (0<=imtbin && imtbin<NBINS) mt_histo[imtbin] += mtweight1/mt; 

	            if (charge > 0) {
  
                      // calculate number of + particles that make the cuts, and the first two pt moments
                      trackCountPlus++;
                      meanPtSquaredPlus += pt*pt;
                      meanPtPlus += pt;

  	              meanEtaSquaredPlus += eta*eta;
	              meanEtaPlus += eta;
                      // be sure to us floating point absolute value!
                      totalAbsEtaPlus += fabs(eta);

	            } else if (charge < 0) {
  
                      // calculate number of - particles that make the cuts, and the first two pt moments
                      trackCountMinus++;
                      meanPtSquaredMinus += pt*pt;
                      meanPtMinus += pt;

  	              meanEtaSquaredMinus += eta*eta;
	              meanEtaMinus += eta;
                      // be sure to us floating point absolute value!
                      totalAbsEtaMinus += fabs(eta);

	            }

	          } // [cut #6]
	        } // [cut #5]
              } // [cut #4]
            } // [cut #3]
          } // [cut #2]
        } // [cut #1]

      } // if (currentTrack)

    } // ** end of track loop **

    meanPtSquaredPlus /= trackCountPlus;
    meanPtPlus /= trackCountPlus;
    meanEtaSquaredPlus /= trackCountPlus;
    meanEtaPlus /= trackCountPlus;

    meanPtSquaredMinus /= trackCountMinus;
    meanPtMinus /= trackCountMinus;
    meanEtaSquaredMinus /= trackCountMinus;
    meanEtaMinus /= trackCountMinus;

    // fill the chargedParicles_Means array in the sca Tag

    // 0,1 - event multiplicities
    mTag->chargedParticles_Means[0] = trackCountPlus;
    mTag->chargedParticles_Means[1] = trackCountMinus;
    // 2,3 - eventwise mean transverse momenta
    mTag->chargedParticles_Means[2] = meanPtPlus;
    mTag->chargedParticles_Means[3] = meanPtMinus;
    // 4,5 - eventwise mean transverse momenta squared
    mTag->chargedParticles_Means[4] = meanPtSquaredPlus;
    mTag->chargedParticles_Means[5] = meanPtSquaredMinus;
    // 6,7 - eventwise mean rapidities
    mTag->chargedParticles_Means[6] = meanEtaPlus;
    mTag->chargedParticles_Means[7] = meanEtaMinus;
    // 8,9 - eventwise mean rapidities squared
    mTag->chargedParticles_Means[8] = meanEtaSquaredPlus;
    mTag->chargedParticles_Means[9] = meanEtaSquaredMinus;
    // 10,11 - total abs(rapidity)
    mTag->chargedParticles_Means[10] = totalAbsEtaPlus;
    mTag->chargedParticles_Means[11] = totalAbsEtaMinus;
    // 12 - estimated temperature of the event
    //     (based on slope fit to 1/mt dN/dmt)
    mTag->chargedParticles_Means[12] = mtInverseSlope(mt_histo, 0, NBINS);

    //uncomment the next line to send the analysis results to cout
    //    for charge > 0 ...
    //cout << trackCountPlus << " " << meanPtPlus/GeV << " " << meanPtSquaredPlus/(GeV*GeV);
    //    for charge < 0 ...
    //cout << trackCountMinus << " " << meanPtMinus/GeV << " " << meanPtSquaredMinus/(GeV*GeV);
    //    for charge > 0 ...
    //cout << meanEtaPlus << " " << meanEtaSquaredPlus << " " << totalAbsEtaPlus;
    //    for charge < 0 ...
    //cout << meanEtaMinus << " " << meanEtaSquaredMinus << " " << totalAbsEtaMinus << endl;


    //uncomment the next line (and declaration of outFile above) to append results to a file
    //    for charge > 0 ...
    //outFile << trackCountPlus << " " << meanPtPlus/GeV << " " << meanPtSquaredPlus/(GeV*GeV);
    //    for charge < 0 ...
    //outFile << trackCountMinus << " " << meanPtMinus/GeV << " " << meanPtSquaredMinus/(GeV*GeV);
    //    for charge > 0 ...
    //outFile << meanEtaPlus << " " << meanEtaSquaredPlus << " " << totalAbsEtaPlus;
    //    for charge < 0 ...
    //outFile << meanEtaMinus << " " << meanEtaSquaredMinus << " " << totalAbsEtaMinus << endl;
  }

  return kStOk;

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
      os <<  "N_Plus = " << mTag->chargedParticles_Means[0] << endl;
      os <<  "N_Minus = " << mTag->chargedParticles_Means[1] << endl;
      os <<  "<pt_Plus> = " << mTag->chargedParticles_Means[2] << endl;
      os <<  "<pt>_Minus = " << mTag->chargedParticles_Means[3] << endl;
      os <<  "<pt^2>_Plus = " << mTag->chargedParticles_Means[4] << endl;
      os <<  "<pt^2>_Minus = " << mTag->chargedParticles_Means[5] << endl;
      os <<  "<y>_Plus = " << mTag->chargedParticles_Means[6] << endl;
      os <<  "<y>_Minus = " << mTag->chargedParticles_Means[7] << endl;
      os <<  "<y^2>_Plus = " << mTag->chargedParticles_Means[8] << endl;
      os <<  "<y^2>_Minus = " << mTag->chargedParticles_Means[9] << endl;
      os <<  "abs(y_Plus) = " << mTag->chargedParticles_Means[10] << endl;
      os <<  "abs(y_Minus) = " << mTag->chargedParticles_Means[11] << endl;
      os <<  "T = " << mTag->chargedParticles_Means[12] << endl;
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
