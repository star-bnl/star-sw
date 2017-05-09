/***************************************************************************
 *
 * $Id: StMinuitVertexFinder.cxx,v 1.59 2017/05/09 12:29:40 smirnovd Exp $
 *
 * Author: Thomas Ullrich, Feb 2002
 ***************************************************************************
 *
 * Description: 
 *
 **************************************************************************/
#include "StMinuitVertexFinder.h"
#include "StEventTypes.h"
#include "StEnumerations.h"
#include "StGlobals.hh"
#include "SystemOfUnits.h"
#include "StCtbMatcher.h"
#include "StMessMgr.h"
#include <math.h>
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StDcaGeometry.h"
#include "St_VertexCutsC.h"
#include "StMaker.h"
std::vector<StPhysicalHelixD>   StMinuitVertexFinder::mHelices;
std::vector<UShort_t>           StMinuitVertexFinder::mHelixFlags;
std::vector<Double_t >          StMinuitVertexFinder::mZImpact;
Bool_t                     StMinuitVertexFinder::requireCTB;
Int_t                      StMinuitVertexFinder::nCTBHits;
//==========================================================
//==========================================================
void StMinuitVertexFinder::Clear(){
  StGenericVertexFinder::Clear();
  mStatusMin    = 0;
  mNSeed = 0;
}

void
StMinuitVertexFinder::setExternalSeed(const StThreeVectorD& s)
{
    mExternalSeedPresent = kTRUE;
    mExternalSeed = s;
}


StMinuitVertexFinder::StMinuitVertexFinder(VertexFit_t fitMode) :
  StGenericVertexFinder(SeedFinder_t::MinuitVF, fitMode)
{
  mExternalSeedPresent = kFALSE;
  mRequireCTB        = kFALSE;
  requireCTB         = kFALSE;
  mUseITTF           = kFALSE;
  mUseOldBEMCRank    = kFALSE;
  mLowerSplitVtxRank = kFALSE;
  mVertexOrderMethod = orderByRanking; // change ordering by ranking
  mMinTrack  = -1;
  mCTBSum = 0;
}
 

StMinuitVertexFinder::~StMinuitVertexFinder()
{
   LOG_WARN << "Skipping delete Minuit in StMinuitVertexFinder::~StMinuitVertexFinder()" << endm;
   mHelices.clear();
   mHelixFlags.clear();
   mZImpact.clear();
}
//________________________________________________________________________________
void StMinuitVertexFinder::InitRun(int run_number, const St_db_Maker* db_maker)
{
  StGenericVertexFinder::InitRun(run_number, db_maker);

  St_VertexCutsC *cuts = St_VertexCutsC::instance();
  mMinNumberOfFitPointsOnTrack = cuts->MinNumberOfFitPointsOnTrack();
  mDcaZMax                     = cuts->DcaZMax();     // Note: best to use integer numbers
  mMinTrack                    = (mMinTrack<0 ? cuts->MinTrack() : mMinTrack);
  mRImpactMax                  = cuts->RImpactMax();
  mZMin                        = cuts->ZMin();        // note: best to use integer numbers
  mZMax                        = cuts->ZMax();        // note: best to use integer numbers
  if (mZMin == mZMax) {
    // historical defaults
    mZMin = -200.0;
    mZMax =  200.0;
  }
  LOG_INFO << "Set cuts: MinNumberOfFitPointsOnTrack = " << mMinNumberOfFitPointsOnTrack
	   << " DcaZMax = " << mDcaZMax
	   << " MinTrack = " << mMinTrack  
	   << " RImpactMax = " << mRImpactMax
	   << " ZMin = " << mZMin
	   << " ZMax = " << mZMax
	   << endm;
}
//________________________________________________________________________________


void
StMinuitVertexFinder::setFlagBase(){
  if(mUseITTF){
    mFlagBase = 8000;
  } else {
    mFlagBase = 1000;
  }
}

Int_t StMinuitVertexFinder::findSeeds() {
  mNSeed = 0;

  int zIdxMax = (int) (mZMax - mZMin); 
  Int_t zImpactArr[zIdxMax]; // simple array to 'histogram' zImpacts
  for (Int_t i=0; i < zIdxMax; i++)
    zImpactArr[i]=0;

  Int_t nTrk = mZImpact.size();
  for (Int_t iTrk=0; iTrk < nTrk; iTrk++) {
    if ((mZImpact[iTrk] > mZMin) &&
        (mZImpact[iTrk] < mZMax))
      zImpactArr[int(mZImpact[iTrk]-mZMin)]++;
  }

  // Search for maxima using sliding 3-bin window
  Int_t nOldBin = 0;
  Int_t slope = 0;
  Int_t nBinZ = 3;
  for (Int_t iBin=0; iBin < zIdxMax - nBinZ; iBin++) {
    Int_t nTrkBin = 0;
    for (Int_t iBin2=0; iBin2 < nBinZ; iBin2++) {
      nTrkBin += zImpactArr[iBin + iBin2];
    }
    if (nTrkBin > nOldBin)
      slope = 1;
    else if (nTrkBin < nOldBin) {
      if (slope == 1) {
	if (mNSeed < maxSeed) {
	  Float_t seed_z = mZMin + iBin + (Float_t)nBinZ / 2 - 1;
	  Double_t meanZ = 0;
	  Int_t nTrkZ = 0;
	  for (Int_t iTrk = 0; iTrk < nTrk; iTrk ++ ) {
	    if ( fabs(mZImpact[iTrk] - seed_z ) < mDcaZMax ) {
	      meanZ += mZImpact[iTrk];
	      nTrkZ++;
	    }
	  }
	  if (nTrkZ >= mMinTrack) {
	    if (mDebugLevel) {
	      LOG_INFO << "Seed " << mNSeed << ", z " << seed_z << " nTrk " << nTrkZ << " meanZ/nTrkZ " << meanZ/nTrkZ << endm;
            }
	    seed_z = meanZ/nTrkZ;
	    mSeedZ[mNSeed] = seed_z;
	    mNSeed++;
	  }
	}
	else {
	  LOG_WARN << "Too many vertex seeds, limit=" << maxSeed << endm;
	}	
      }
      slope = -1;
    }
    if (mDebugLevel > 1) {
      LOG_INFO << "iBin " << iBin << " nTrkBin " << nTrkBin << " nOldBin " << nOldBin << ", slope " << slope << " mNSeed " << mNSeed << endm; 
    }
    nOldBin = nTrkBin;
  }

  LOG_INFO << "Found " << mNSeed << " seeds" << endm;
  return mNSeed;
}

void StMinuitVertexFinder::fillBemcHits(StEvent *event){
  static Int_t nMod = 120;
  static Int_t nEta = 20;
  static Int_t nSub = 2;
  static Float_t mEmcThres = 0.15;
  for (Int_t m=0; m < nMod; m++) 
    for (Int_t e=0; e < nEta; e++) 
      for (Int_t s=0; s < nSub; s++) 
	mBemcHit[m][e][s]=0;

  Int_t n_emc_hit=0;
  if (event->emcCollection() && event->emcCollection()->detector(kBarrelEmcTowerId)) {
    StEmcDetector* bemcDet = event->emcCollection()->detector(kBarrelEmcTowerId);
    for (Int_t iMod=0; iMod < nMod; iMod++) {
      if (!bemcDet->module(iMod)) 
        continue;
      StEmcModule *mod = bemcDet->module(iMod);
      StSPtrVecEmcRawHit&  hits = mod->hits();
      for (StEmcRawHitIterator hitIter = hits.begin(); hitIter != hits.end(); hitIter++) {
        StEmcRawHit *hit = *hitIter;
        if (hit->energy() > mEmcThres) {
          mBemcHit[hit->module()-1][hit->eta()-1][hit->sub()-1]=1;
          n_emc_hit++; 
        }
      }
    }
  }
  if (mDebugLevel) {
    LOG_INFO << "Found " << n_emc_hit << " emc hits" << endm;
  }
}

Int_t  
StMinuitVertexFinder::matchTrack2BEMC(const StTrack *track){
  static const Double_t rBemc = 242; // middle of tower
  static StEmcGeom *bemcGeom = StEmcGeom::getEmcGeom("bemc");
  //static Double_t rBemc = bemcGeom->Radius(); // front face??
  //LOG_INFO << "rBemc: " << rBemc << endm;

  if (track->outerGeometry()==0) {
    if (mDebugLevel) { // Happens only rarely
      LOG_INFO << "No outer track geom" << endm;
    }
    return 0;
  }

  StPhysicalHelixD helix = track->outerGeometry()->helix();

  if (!helix.valid()) {
    if (mDebugLevel) { // Happens only rarely
      LOG_INFO << "Invalid helix" << endm;
    }
    return 0;
  }
     
  pairD  d2;
  d2 = helix.pathLength(rBemc);
  if (d2.first > 99999999 && d2.second > 99999999)
    return 0;
  Double_t path=d2.second;
  if (d2.first >= 0 && d2.first < d2.second)
    path = d2.first;
  else if(d2.first>=0 || d2.second<=0) {
    LOG_WARN << "Unexpected solution for track crossing Cyl:" 
			<< " track mom = " 
			<< track->geometry()->momentum().mag() 
			<< ", d2.first=" << d2.first 
			<< ", second=" << d2.second <<", trying first" << endm;
    path=d2.first;
  }
  StThreeVectorD posCyl = helix.at(path);

  Float_t phi=posCyl.phi();
  Float_t eta=posCyl.pseudoRapidity();

  if (fabs(eta) < 1) {
    Int_t mod, ieta, sub;
    if (bemcGeom->getBin(phi, eta, mod, ieta, sub) == 0) {
      // There is some edge effect leading to sub=-1. 
      // Strange, but leave in for now// HOW CAN IT be: sub == -1????
      if (sub > 0 && mBemcHit[mod-1][ieta-1][sub-1]) {
	return 1;
      }
    }
  }
  return 0;
}

Int_t StMinuitVertexFinder::checkCrossMembrane(const StTrack *track) {
  Int_t n_pnt_neg=0, n_pnt_pos=0;
  
  StPtrVecHit hits = track->detectorInfo()->hits(kTpcId);
  for (StHitIterator hitIter = hits.begin(); hitIter != hits.end(); hitIter++) {
    if ((*hitIter)->position().z() < 0)
      n_pnt_neg++;
    if ((*hitIter)->position().z() > 0)
      n_pnt_pos++;
  }
  return (n_pnt_pos > 5 && n_pnt_neg > 5);
}

void StMinuitVertexFinder::calculateRanks() {    
  
  // Calculation of veretx ranks to select 'best' (i.e. triggered)
  // vertex
  // Three ranks are used, based on average dip, number of BEMC matches 
  // and tarcks crossing central membrane
  // Each rank is normalised to be 0 for 'average' valid vertices and
  // has a sigma of 1. 
  //
  // Values are limited to [-5,1] for each rank
  //
  // Note that the average dip angle ranking is naturally peaked to 1,
  // while the others are peaked at 1 (intentionally) due to rounding.

  // A fancier way would be to calculate something more like 
  // a likelihood based on the expected distributions. 
  // That's left as an excercise to the reader.

  // Added by Ant: split vertices have 3 rank units deducted near the
  // end of function (if mLowerSplitVtxRank). See hypernews post:
  // http://www.star.bnl.gov/HyperNews-star/get/vertex/127.html 

  Int_t nBemcMatchTot = 0;
  Int_t nVtxTrackTot = 0;
  for (Int_t iVertex=0; iVertex < size(); iVertex++) {
    StPrimaryVertex *primV = getVertex(iVertex);
    nVtxTrackTot += primV->numTracksUsedInFinder();
    nBemcMatchTot += primV->numMatchesWithBEMC();
  }

  mBestRank = -999;
  mBestVtx  = 0;
  for (Int_t iVertex=0; iVertex < size(); iVertex++) {
    StPrimaryVertex *primV = getVertex(iVertex);
    // expected values based on Cu+Cu data
    Float_t avg_dip_expected = -0.0033*primV->position().z();
    Float_t n_bemc_expected = 0;
    if (nVtxTrackTot) {
      if (mUseOldBEMCRank) 
        n_bemc_expected = (1-0.25*(1-(float)primV->numTracksUsedInFinder()/nVtxTrackTot))*nBemcMatchTot; 
      else
        n_bemc_expected = (float)primV->numTracksUsedInFinder()/nVtxTrackTot*nBemcMatchTot; 
    }

    Float_t n_cross_expected = fabs(primV->position().z())*0.0020*primV->numTracksUsedInFinder(); // old coeff 0.0016 with dca 3 and 10 points on track

    if (mDebugLevel) {
      LOG_INFO << "vertex z " << primV->position().z() << " dip expected " << avg_dip_expected << " bemc " << n_bemc_expected << " cross " << n_cross_expected << endm;
    }

    Float_t rank_avg_dip = 1 - fabs(primV->meanDip() - avg_dip_expected)*sqrt((float)primV->numTracksUsedInFinder())/0.67;  // Sigma was 0.8 for old cuts

    Float_t rank_bemc = 0;
    if (n_bemc_expected >= 1) { 
      //Float_t sigma = 0.12*n_bemc_match_tot;
      Float_t sigma = 0.5*sqrt(n_bemc_expected);

      // limit sigma to avoid large weights at small multiplicity
      sigma = ( sigma < 0.75 ? 0.75 : sigma);

      rank_bemc = (primV->numMatchesWithBEMC() - n_bemc_expected)/sigma;
      if (mUseOldBEMCRank)
        rank_bemc += 0.5; // distribution is asymmetric; add 0.5 
    }
    
    Float_t rank_cross = 0;
    if ( n_cross_expected >= 1 ) {
      Float_t sigma=1.1*sqrt(n_cross_expected);
      rank_cross = (primV->numTracksCrossingCentralMembrane() - n_cross_expected)/sigma;
    }

    // Handle possible overflows
    rank_avg_dip = ( rank_avg_dip < -5 ? -5 : rank_avg_dip );
    rank_bemc    = ( rank_bemc    < -5 ? -5 : (rank_bemc  > 1 ? 1 : rank_bemc) );
    rank_cross   = ( rank_cross   < -5 ? -5 : (rank_cross > 1 ? 1 : rank_cross) );

    if (mDebugLevel) {
      LOG_INFO << "rankings: " << rank_avg_dip << " " << rank_bemc << " " << rank_cross << endm;
    }

    //Give split vertices a lower rank...   	
    if (mLowerSplitVtxRank && mCTBSum > 6000.0*primV->numTracksUsedInFinder()/80. + 2000)
      primV->setRanking(rank_cross+rank_bemc+rank_avg_dip-3); 
    else
      primV->setRanking(rank_cross+rank_bemc+rank_avg_dip); 

    if (primV->ranking() > mBestRank) {
      mBestRank = primV->ranking();
      mBestVtx = primV;
    }
  }
}


int StMinuitVertexFinder::fit(StEvent* event)
{
    setFlagBase();

    // get CTB info
    StCtbTriggerDetector* ctbDet = 0;
    std::vector<ctbHit> ctbHits;

    StTriggerDetectorCollection* trigCol = event->triggerDetectorCollection();
    mCTBSum = 0;
    if(trigCol){
      ctbDet = &(trigCol->ctb());

      for (UInt_t slat = 0; slat < ctbDet->numberOfSlats(); slat++) {
	for (UInt_t tray = 0; tray < ctbDet->numberOfTrays(); tray++) {
	  ctbHit curHit;
	  curHit.adc = ctbDet->mips(tray,slat,0);
	  if(curHit.adc > 0){
	    mCTBSum += curHit.adc;
	    ctb_get_slat_from_data(slat, tray, curHit.phi, curHit.eta);
	    ctbHits.push_back(curHit);
	  }
	}
      }
    }

    // Loop over all global tracks (TPC) and store the refering helices and
    // their estimated DCA resolution in vectors. Quality cuts are applied (see
    // StMinuitVertexFinder::accept()). The helices and the sigma are used in
    // fcn to calculate the fit potential which gets minimized by Minuit.
    mDCAs.clear();
    mHelices.clear();
    mHelixFlags.clear();
    mZImpact.clear();

    fillBemcHits(event);

    Int_t n_ctb_match_tot = 0;
    Int_t n_bemc_match_tot = 0;
    Int_t n_cross_tot = 0;

    for (const StTrackNode* stTrack : event->trackNodes())
    {
      StGlobalTrack* g = ( StGlobalTrack*) stTrack->track(global);
      if (!accept(g)) continue;
      StDcaGeometry* gDCA = g->dcaGeometry();
      if (! gDCA) continue;
      if (TMath::Abs(gDCA->impact()) >  mRImpactMax) continue;
      mDCAs.push_back(gDCA);
      // 	  StPhysicalHelixD helix = gDCA->helix(); 
      // 	  mHelices.push_back(helix);
      mHelices.push_back(g->geometry()->helix());
      mHelixFlags.push_back(1);
      Double_t z_lin = gDCA->z();
      mZImpact.push_back(z_lin);

      Bool_t shouldHitCTB = kFALSE;
      Double_t etaInCTBFrame = -999;
      bool ctb_match =  EtaAndPhiToOrriginAtCTB(g,&ctbHits,shouldHitCTB,etaInCTBFrame);
      if (ctb_match) {
        mHelixFlags[mHelixFlags.size()-1] |= kFlagCTBMatch;
        n_ctb_match_tot++;
      }

      if (matchTrack2BEMC(g)) {
        mHelixFlags[mHelixFlags.size()-1] |= kFlagBEMCMatch;
        n_bemc_match_tot++;
      }

      if (checkCrossMembrane(g)) {
        mHelixFlags[mHelixFlags.size()-1] |= kFlagCrossMembrane;
        n_cross_tot++;
      }
    }

    if (mDebugLevel) {
      LOG_INFO << "Found " << n_ctb_match_tot << " ctb matches, " << n_bemc_match_tot << " bemc matches, " << n_cross_tot << " tracks crossing central membrane" << endm; 
    }

    // In case there are no tracks left we better quit
    if (mHelices.empty()) {
	LOG_WARN << "StMinuitVertexFinder::fit: no tracks to fit." << endm;
	mStatusMin = -1;
	return 0;
    }

    LOG_INFO << "StMinuitVertexFinder::fit size of helix vector: " << mHelices.size() << endm;

    // Set some global pars
    if (mRequireCTB) requireCTB = kTRUE;
    
    // Reset and clear Minuit parameters mStatusMin
    mMinuit->mnexcm("CLEar", 0, 0, mStatusMin);

    // Make sure the global pointer points to valid object so Minuit uses correct data
    StGenericVertexFinder::sSelf = this;
    
    // Set parameters and start values. We do constrain the parameters since it
    // harms the fit quality (see Minuit documentation).
    //
    // Initialize the seed with a z value which is not one of the discrete
    // values which it can tend to, implies zero not allowed. Also need
    // different initialization when vertex constraint.

    static Double_t step[3] = {0.03, 0.03, 0.03};

    // Scan z to find best seed for the actual fit.
    // Skip this step if an external seed is given.
    if (!mExternalSeedPresent) {
      findSeeds();
    }
    else {
      mNSeed = 1;
    }

    Float_t old_vtx_z = -999;
    Double_t seed_z = -999;
    Double_t chisquare = 0;

    for (Int_t iSeed = 0; iSeed < mNSeed; iSeed++) {

      // Reset and clear Minuit parameters mStatusMin
      mMinuit->mnexcm("CLEar", 0, 0, mStatusMin);

      seed_z= mSeedZ[iSeed]; 

      if (mExternalSeedPresent)
	seed_z = mExternalSeed.z();
      if (mDebugLevel) {
	LOG_INFO << "Vertex seed = " << seed_z << endm;
      }
      
      if (mVertexFitMode == VertexFit_t::Beamline1D){ 
	mMinuit->mnparm(0, "z", seed_z, step[2], 0, 0, mStatusMin);
      }
      else {
	mMinuit->mnparm(0, "x",      0, step[0], 0, 0, mStatusMin);
	mMinuit->mnparm(1, "y",      0, step[1], 0, 0, mStatusMin);
	mMinuit->mnparm(2, "z", seed_z, step[2], 0, 0, mStatusMin);
      }

      Int_t done = 0;
      Int_t iter = 0;

      Int_t n_trk_vtx = 0;
      Int_t n_helix = mHelices.size();
      do {  
	// For most vertices one pass is fine, but multiple passes can be done
	n_trk_vtx = 0;
	for (Int_t i=0; i < n_helix; i++) {
	  if (fabs(mZImpact[i]-seed_z) < mDcaZMax) {
	    mHelixFlags[i] |= kFlagDcaz;
	    n_trk_vtx++;
	  }
	  else
	    mHelixFlags[i] &= ~kFlagDcaz;
	}
      	
	if (mDebugLevel) {
	  LOG_INFO << n_trk_vtx << " tracks within dcaZ cut (iter " << iter <<" )" << endm;
        }
	if (n_trk_vtx < mMinTrack) {
	  if (mDebugLevel) {
	    LOG_INFO << "Less than mMinTrack (=" << mMinTrack << ") tracks, skipping vtx" << endm;
          }
	  continue;
	}
	mMinuit->mnexcm("MINImize", 0, 0, mStatusMin);
	done = 1;

	// Check fit result
	if (mStatusMin) {
	  LOG_WARN << "StMinuitVertexFinder::fit: error in Minuit::mnexcm(), check status flag. ( iter=" << iter << ")" << endm;
	  done = 0; // refit
	}

	Double_t fedm, errdef;
	Int_t npari, nparx;

	mMinuit->mnstat(chisquare, fedm, errdef, npari, nparx, mStatusMin);

	if (mStatusMin != 3) {
	  LOG_INFO << "Warning: Minuit Status: " << mStatusMin << ", func val " << chisquare<< endm;
	  done = 0;  // refit
	}
	mMinuit->mnhess();

	Double_t new_z, zerr;
	if (mVertexFitMode == VertexFit_t::Beamline1D) {
	  mMinuit->GetParameter(0, new_z, zerr); 
	}
	else {
	  mMinuit->GetParameter(2, new_z, zerr); 
	}

	if (fabs(new_z - seed_z) > 1) // refit if vertex shifted
	  done = 0;

	Int_t n_trk = 0;
	for (Int_t i=0; i < n_helix; i++) {
	  if ( fabs(mZImpact[i] - new_z) < mDcaZMax ) {
	    n_trk++;
	  }
	}
	if ( 10 * abs(n_trk - n_trk_vtx) >= n_trk_vtx ) // refit if number of track changed by more than 10%
	  done = 0;

	iter++;
	seed_z = new_z; // seed for next iteration
      } while (!done && iter < 5 && n_trk_vtx >= mMinTrack);

      if (n_trk_vtx < mMinTrack)
	continue;

      if (!done) { 
	LOG_WARN << "Vertex unstable: no convergence after " << iter << " iterations. Skipping vertex " << endm;
	continue;
      }

      if (!mExternalSeedPresent && fabs(seed_z-mSeedZ[iSeed]) > mDcaZMax) {
	LOG_WARN << "Vertex walks during fits: seed was " << mSeedZ[iSeed] << ", vertex at " << seed_z << endm;
      }

      if (fabs(seed_z - old_vtx_z) < mDcaZMax) {
        if (mDebugLevel) {
	  LOG_INFO << "Vertices too close (<mDcaZMax). Skipping" << endm;
        }
	continue;
      }

      // Store vertex
      StThreeVectorD XVertex;
      Float_t cov[6];
      memset(cov,0,sizeof(cov));
    
      Double_t val, verr;
      if (mVertexFitMode == VertexFit_t::Beamline1D) {
	mMinuit->GetParameter(0, val, verr); 
	XVertex.setZ(val);  cov[5]=verr*verr;
	
	// LSB Really error in x and y should come from error on constraint
	// At least this way it is clear that those were fixed paramters
	XVertex.setX(beamX(val));  cov[0]=0.1; // non-zero error values needed for Sti
	XVertex.setY(beamY(val));  cov[2]=0.1;
      }
      else {
	XVertex = StThreeVectorD(mMinuit->fU[0],mMinuit->fU[1],mMinuit->fU[2]);
	Double_t emat[9];
	/* 0 1 2
	   3 4 5
           6 7 8 */
	mMinuit->mnemat(emat,3);
	cov[0] = emat[0];
	cov[1] = emat[3];
	cov[2] = emat[4];
	cov[3] = emat[6];
	cov[4] = emat[7];
	cov[5] = emat[8];
      }
      StPrimaryVertex primV;
      primV.setPosition(XVertex);
      primV.setChiSquared(chisquare);  // this is not really a chisquare, but anyways
      primV.setCovariantMatrix(cov); 
      primV.setVertexFinderId(minuitVertexFinder);
      primV.setFlag(1); // was not set earlier by this vertex finder ?? Jan
      primV.setRanking(333);
      primV.setNumTracksUsedInFinder(n_trk_vtx);

      Int_t n_ctb_match = 0;
      Int_t n_bemc_match = 0;
      Int_t n_cross = 0;
      n_trk_vtx = 0;

      Double_t mean_dip = 0;
      Double_t sum_pt = 0;
      for (UInt_t i=0; i < mHelixFlags.size(); i++) {
	if (!(mHelixFlags[i] & kFlagDcaz))
	  continue;
	n_trk_vtx++;
	if (mHelixFlags[i] & kFlagCTBMatch)
	  n_ctb_match++;
	if (mHelixFlags[i] & kFlagBEMCMatch)
	  n_bemc_match++;
	if (mHelixFlags[i] & kFlagCrossMembrane)
	  n_cross++;
	mean_dip += mHelices[i].dipAngle();
	sum_pt += mHelices[i].momentum(0).perp();
      }
      
      mean_dip /= n_trk_vtx;

      if (mDebugLevel) {
	LOG_INFO << "check n_trk_vtx " << n_trk_vtx << ", found "
	         << n_ctb_match << " ctb matches, "
		 << n_bemc_match << " bemc matches, "
		 << n_cross << " tracks crossing central membrane\n"
	         << "mean dip " << mean_dip << endm;
      }
      primV.setNumMatchesWithCTB(n_ctb_match);      
      primV.setNumMatchesWithBEMC(n_bemc_match);
      primV.setNumTracksCrossingCentralMembrane(n_cross);
      primV.setMeanDip(mean_dip);
      primV.setSumOfTrackPt(sum_pt);

      //..... add vertex to the list
      addVertex(primV);

      old_vtx_z = XVertex.z();
    }

    calculateRanks();

    //  Reset the flag which tells us about external
    //  seeds. This needs to be provided for every event.
    mExternalSeedPresent = kFALSE;
    
    requireCTB = kFALSE;

    return 1;
}


//________________________________________________________________________________
double StMinuitVertexFinder::CalcChi2DCAs(const StThreeVectorD &vtx) {
  Double_t f = 0;
  Double_t e;
  nCTBHits = 0;
  if (fabs(vtx.x())> 10) return 1e6;
  if (fabs(vtx.y())> 10) return 1e6;
  if (fabs(vtx.z())>300) return 1e6;
  for (UInt_t i=0; i<mDCAs.size(); i++) {

    if ( !(mHelixFlags[i] & kFlagDcaz) || (requireCTB && !(mHelixFlags[i] & kFlagCTBMatch)) )
       continue;

    const StDcaGeometry* gDCA = mDCAs[i];
    if (! gDCA) continue;
    const StPhysicalHelixD helix = gDCA->helix();
    e = helix.distance(vtx, kFALSE);  // false: don't do multiple loops
    //VP version
    //VP	Double_t chi2     = e*e/(errMatrix[0] + errMatrix[2]);
    Double_t err2;
    Double_t chi2 = gDCA->thelix().Dca(&(vtx.x()),&err2);
    chi2*=chi2/err2;
    //EndVP
    static double scale = 100;
    f += scale*(1. - TMath::Exp(-chi2/scale)); // robust potential
    //	f -= scale*TMath::Exp(-chi2/scale); // robust potential
    if((mHelixFlags[i] & kFlagCTBMatch) && e<3.0) nCTBHits++;
  }
  return f;
}


/**
 * Accept only tracks which fulfill certain quality criteria.
 */
bool StMinuitVertexFinder::accept(StTrack* track) const
{
  return (track &&
          track->flag() >= 0 &&
          track->fitTraits().numberOfFitPoints() >= mMinNumberOfFitPointsOnTrack &&
          !track->topologyMap().trackFtpc() &&
          finite(track->length()) &&  //LSB another temporary check
          track->geometry()->helix().valid());
}


/// Use mMinuit print level 
void StMinuitVertexFinder::setPrintLevel(Int_t level) 
{
  mMinuit->SetPrintLevel(level);
}


void StMinuitVertexFinder::printInfo(ostream& os) const
{
    os << "StMinuitVertexFinder - Statistics:" << endl;
    os << "Number of vertices found ......." << size() << endl;
    os << "Rank of best vertex ............" << mBestRank << endl;
    if(mBestVtx) {
      os << "Properties of best vertex:" << endl;
      os << "position ..................... " << mBestVtx->position() << endl;
      os << "position errors .............. " << mBestVtx->positionError()<< endl;
      os << "# of used tracks ............. " << mBestVtx->numTracksUsedInFinder() << endl;
      os << "Chisquare .................... " << mBestVtx->chiSquared() << endl;
    }
    os << "min # of fit points for tracks . " << mMinNumberOfFitPointsOnTrack << endl;
}


Int_t  StMinuitVertexFinder::NCtbMatches() { 
  return nCTBHits;
}
