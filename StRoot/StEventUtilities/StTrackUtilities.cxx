#include "StTrackUtilities.h"
#include "StMaker.h"
#include "StEvent/StEvent.h"
#include "StEvent/StTrack.h"
#include "StEvent/StTrackNode.h"
#include "StEvent/StGlobalTrack.h"
#include "StEvent/StPrimaryTrack.h"
#include "StEvent/StTrackMassFit.h"
#include "StEvent/StPrimaryVertex.h"
#include "StEvent/StTrackDetectorInfo.h"
#include "StEvent/StHelixModel.h"
#include "StEvent/StTpcHit.h"
#include "StarMagField/StarMagField.h"
#include "StuFixTopoMap.h"
StTrackUtilities* StTrackUtilities::fgInstance = 0;
//________________________________________________________________________________
Bool_t StTrackUtilities::StFixTopoMap(StTrack* track) {
  return ::StuFixTopoMap(track);
}
//________________________________________________________________________________
StTrackUtilities* StTrackUtilities::instance() {
  if (! fgInstance) fgInstance = new StTrackUtilities;
  return fgInstance;
}
//_____________________________________________________________________________
void StTrackUtilities::FillPrimaryTracks() {
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  UInt_t NpVX = pEvent->numberOfPrimaryVertices();
  for (UInt_t i = 0; i < NpVX; i++) {
    StPrimaryVertex *vx = pEvent->primaryVertex(i);
    UInt_t nDaughters = vx->numberOfDaughters();
    for (UInt_t j = 0; j < nDaughters; j++) {
      StPrimaryTrack *pTrack = (StPrimaryTrack *) vx->daughter(j);
      if (! pTrack) continue;
      FillPrimaryTrack(pTrack);
    }  
#if 0
    CalculateRank(vx);
#endif
  }
}
//_____________________________________________________________________________
void StTrackUtilities::FillPrimaryTrack(StPrimaryTrack *pTrack) {
  if (! pTrack) return;
  StTrackNode *node = pTrack->node();
  if (! node) return;
  StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
  if (!pEvent) return;
  StGlobalTrack  *gTrack = static_cast<StGlobalTrack *>(node->track(global));
  if (! gTrack) return;
  StTrackMassFit *mfp = (StTrackMassFit *) node->track(massFitAtVx);
  if (! mfp) return;
  const KFParticle &P = *mfp->kfParticle();
  pTrack->setKey( gTrack->key());
  pTrack->setFlagExtension( gTrack->flagExtension());
  pTrack->setIdTruth(gTrack->idTruth(),gTrack->qaTruth());
  StTrackDetectorInfo* detInfo = new StTrackDetectorInfo(*gTrack->detectorInfo());
  pTrack->setDetectorInfo(detInfo);
  StSPtrVecTrackDetectorInfo& detInfoVec = pEvent->trackDetectorInfo(); 
  detInfoVec.push_back(detInfo);
  StHelixModel *gOut = (StHelixModel *)gTrack->outerGeometry();
  StTrackGeometry* geometry = new StHelixModel(gOut->charge(), gOut->psi(), gOut->curvature(), 
					       gOut->dipAngle(), gOut->origin(), gOut->momentum(), gOut->helicity());
  pTrack->setOuterGeometry(geometry);
  //
  
  StThreeVectorF origin(P.GetX(),P.GetY(),P.GetZ());
  StThreeVectorF field;
  StarMagField::Instance()->BField(origin.xyz(), field.xyz());
  static const Double_t EC = 2.99792458e-4;
  StThreeVectorF p(P.GetPx(), P.GetPy(), P.GetPz());
  Double_t hz = EC*field.z();
  Double_t qovepT = P.GetQ()/P.GetPt();
  Double_t curvature = - hz*qovepT;
  Double_t helicity = (curvature < 0) ? -1 : 1;
  geometry = new StHelixModel(P.GetQ(),
			      p.phi(),
			      fabs(curvature), 
			      TMath::PiOver2() - p.theta(),
			      origin, 
			      p,
			      helicity);
  pTrack->setGeometry(geometry);
  Double_t tlen = mfp->length();
  pTrack->setLength(tlen);// someone removed this, grrrr!!!!
  StuFixTopoMap(pTrack);
  FillFlags(pTrack);
  pTrack->setIdTruth();

  return;
}
///_____________________________________________________________________________
/// data members from StEvent/StTrack.h
///  The track flag (mFlag accessed via flag() method) definitions with ITTF 
///(flag definition in EGR era can be found at  http://www.star.bnl.gov/STAR/html/all_l/html/dst_track_flags.html)
///
///  mFlag=zxyy, where  z = 1 for pile up track in TPC (otherwise 0) 
///                     x indicates the detectors included in the fit and 
///                    yy indicates the status of the fit. 
///  Positive mFlag values are good fits, negative values are bad fits. 
///
///  The first digit indicates which detectors were used in the refit: 
///
///      x=1 -> TPC only 
///      x=3 -> TPC       + primary vertex 
///      x=5 -> SVT + TPC 
///      x=6 -> SVT + TPC + primary vertex 
///      x=7 -> FTPC only 
///      x=8 -> FTPC      + primary 
///      x=9 -> TPC beam background tracks            
///
///  The last two digits indicate the status of the refit: 
///       = +x01 -> good track 
///
///       = -x01 -> Bad fit, outlier removal eliminated too many points 
///       = -x02 -> Bad fit, not enough points to fit 
///       = -x03 -> Bad fit, too many fit iterations 
///       = -x04 -> Bad Fit, too many outlier removal iterations 
///       = -x06 -> Bad fit, outlier could not be identified 
///       = -x10 -> Bad fit, not enough points to start 
///
///       = -x11 -> Short track pointing to EEMC
///       = -x12 -> Short track pointing to ETOF

void StTrackUtilities::FillFlags(StTrack* gTrack) 
{
  Int_t flag = 0;
  if (gTrack->type()==global) {
    flag = 101; //change: make sure flag is ok
  }
  else if (gTrack->type()==primary) {
    flag = 301;
  }
  StTrackFitTraits& fitTrait = gTrack->fitTraits();
  //Int_t tpcFitPoints = fitTrait.numberOfFitPoints(kTpcId);
  Int_t svtFitPoints = fitTrait.numberOfFitPoints(kSvtId);
  Int_t ssdFitPoints = fitTrait.numberOfFitPoints(kSsdId) +  fitTrait.numberOfFitPoints(kSstId);
  Int_t pxlFitPoints = fitTrait.numberOfFitPoints(kPxlId);
  Int_t istFitPoints = fitTrait.numberOfFitPoints(kIstId);
  //  Int_t totFitPoints = fitTrait.numberOfFitPoints();
  /// In the flagging scheme, I will put in the cases for
  /// TPC only, and TPC+SVT (plus their respective cases with vertex)
  /// Ftpc case has their own code and SSD doesn't have a flag...

  // first case is default above, tpc only = 101 and tpc+vertex = 301
  // next case is:
  // if the track has svt points, it will be an svt+tpc track
  // (we assume that the ittf tracks start from tpc, so we don't
  // use the "svt only" case.)
  if (svtFitPoints+ssdFitPoints+pxlFitPoints+istFitPoints>0) {
      if (gTrack->type()==global) {
	flag = 501; //svt+tpc
      }
      else if (gTrack->type()==primary) {
	flag = 601;  //svt+tpc+primary
      }
  }
  const StTrackDetectorInfo *dinfo = gTrack->detectorInfo();
  if (dinfo) {
    Int_t NoTpcFitPoints = dinfo->numberOfPoints(kTpcId);
    Int_t NoFtpcWestId   = dinfo->numberOfPoints(kFtpcWestId);
    Int_t NoFtpcEastId   = dinfo->numberOfPoints(kFtpcEastId);
    // Check that it could be TPC pile-up track, i.e. in the same half TPC (West East) 
    // there are more than 2 hits with wrong Z -position
    if (NoTpcFitPoints >= 11) {
      const StPtrVecHit& hits = dinfo->hits(kTpcId);
      Int_t Nhits = hits.size();
      Int_t NoWrongSignZ = 0;
      Int_t NoPositiveSignZ = 0;
      Int_t NoNegativeSignZ = 0;
      Int_t NoPromptHits = 0;
      Double_t zE = -200, zW = 200;
      Int_t    rE = 0, rW = 0;
      Int_t   nW = 0, nE = 0;
      for (Int_t i = 0; i < Nhits; i++) {
	const StTpcHit *hit = (StTpcHit *) hits[i];
	Double_t z = hit->position().z();
	Int_t sector = hit->sector();
	if (sector <= 12) nW++;
	else              nE++;
	Int_t row    = hit->padrow();
	if ((z < -1.0 && sector <= 12) ||
	    (z >  1.0 && sector >  12)) NoWrongSignZ++;
	else {
	  if (z < -1.0) {NoNegativeSignZ++; if (z > zE) {zE = z; rE = row;}}
	  if (z >  1.0) {NoPositiveSignZ++; if (z < zW) {zW = z; rW = row;}}
	}
	if (TMath::Abs(209.4 - TMath::Abs(z)) < 3.0) NoPromptHits++;
      }
      if (NoWrongSignZ >= 2)                             gTrack->setPostCrossingTrack();
      else {
	if (NoPromptHits == 1)                           gTrack->setPromptTrack();
	if (NoPositiveSignZ >= 2 && NoNegativeSignZ >=2) {
	  if (zW - zE < 10 ||
	      TMath::Abs(rW - rE) < 3) 
	    gTrack->setMembraneCrossingTrack();
	}
      }
      if (nW >  0 && nE == 0) gTrack->setWestTpcOnly();
      if (nW == 0 && nE >  0) gTrack->setEastTpcOnly();
    }
    if (NoTpcFitPoints < 11 && NoFtpcWestId < 5 && NoFtpcEastId < 5) { 
      // hardcoded number correspondant to  __MIN_HITS_TPC__ 11 in StMuFilter.cxx
      //keep most sig. digit, set last digit to 2, and set negative sign
      gTrack->setRejected();
      flag = - ((flag/100)*100 + 2); // -x02 

      // Deciding which short tracks to keep based on event time.
      // Hardcoded times are not optimal, and will need revisiting
      // when EEMC is turned back on after BES-II, eTOF stays or goes?
      Int_t evtTime = 0;
      StEvent* pEvent = (StEvent*) StMaker::GetChain()->GetInputDS("StEvent");
      if ( pEvent) {
	evtTime = pEvent->time();
      }
      bool doShort2EMC = (evtTime < 1538352000 || evtTime > 1633046400); // t < 2018-10-01 or t > 2021-10-01
      bool doShort2ETOF = (evtTime > 1525910400 && evtTime < 1633046400); // 2018-05-10 < t < 2021-10-01

      if ((doShort2EMC || doShort2ETOF) && gTrack->geometry()) {
	const StThreeVectorF &momentum = gTrack->geometry()->momentum();
	const float eta = momentum.pseudoRapidity();
	if (TMath::Abs(eta) > 0.5) {
	  const StTrackDetectorInfo *dinfo = gTrack->detectorInfo();
	  const StPtrVecHit& hits = dinfo->hits();
	  Int_t Nhits = hits.size();
	  Bool_t ShortTrack2EMC = kFALSE;
	  Bool_t ShortTrack2ETOF = kFALSE;
	  for (Int_t i = 0; i < Nhits; i++) {
	    const StHit *hit = hits[i];
	    if (doShort2EMC && eta > 0.5 && hit->position().z() > 150.0) {
	      ShortTrack2EMC = kTRUE;
	      break;
	    }
	    if (doShort2ETOF && eta < -0.5 && hit->position().z() < -150.0) {
	      ShortTrack2ETOF = kTRUE;
	      break;
	    }
	  }
	  if (ShortTrack2EMC) {
	    gTrack->setShortTrack2EMC();
	    flag = (TMath::Abs(flag)/100)*100+11; // +x11 
	  } else if (ShortTrack2ETOF) {
	    gTrack->setShortTrack2ETOF();
	    flag = (TMath::Abs(flag)/100)*100+12; // +x12 
	  }
	}
      }
    }
  }
  
  gTrack->setFlag( flag);
  if (gTrack->type()==global) {
#if 0
    // Match with fast detectors
    StPhysicalHelixD hlx = gTrack->outerGeometry()->helix();
    StiTrack2FastDetector t;
    mFastDetectorMatcher->matchTrack2FastDetectors(&hlx,&t);
    if (t.btofBin > 0) {
      if (t.mBtof > 0) gTrack->setToFMatched();
      else             gTrack->setToFNotMatched();
    }
    if (t.ctbBin > 0) {
      if (t.mCtb  > 0) gTrack->setCtbMatched();
      else             gTrack->setCtbNotMatched();
    }
    if (t.bemcBin > 0 || t.eemcBin > 0) {
      Int_t W = 0;
      if (t.bemcBin > 0) {
	W = StBemcHitList::instance()->getFired(t.bemcBin);
	if (W > 0) gTrack->setBemcMatched();
	else      gTrack->setBemcNotMatched();
      } else if (t.eemcBin > 0) {
	W = StEemcHitList::instance()->getFired(t.eemcBin);
	if (W > 0) gTrack->setEemcMatched();
	else      gTrack->setEemcNotMatched();
      }
      if (W > 0) {
	UInt_t fext = gTrack->flagExtension();
	if (W > 7) W = 7;
	fext &= ~7;
	fext += W;
	gTrack->setFlagExtension(fext);
      }
    }
#endif
  } else if (gTrack->type()==primary) {
    StTrackNode *n = gTrack->node();
    assert(n);
    StTrack *t = n->track(global);
    assert(t);
    gTrack->setFlagExtension(t->flagExtension());
  }
}
