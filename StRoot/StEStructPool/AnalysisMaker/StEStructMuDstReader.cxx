/**********************************************************************
 *
 * $Id: StEStructMuDstReader.cxx,v 1.16 2009/05/08 00:04:22 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  event reader class for common MuDsts
 *               Uses the StMuDstMaker for real reading
 *
 ***********************************************************************/
#include "StEStructMuDstReader.h"

#include "StEStructEventCuts.h"
#include "StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"


#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StDetectorId.h"


StEStructMuDstReader::StEStructMuDstReader() {
    mhasdEdxCuts  = 0;
    mMaker        = 0;
    mECuts        = 0;
    mTCuts        = 0;
    mInChain      = false;
    mAmDone       = false;
    mUseGlobalTracks=false;
    mPileup = new Pileup();
};
StEStructMuDstReader::StEStructMuDstReader(StMuDstMaker* maker,
                                           StEStructEventCuts* ecuts,
                                           StEStructTrackCuts* tcuts) {
    mhasdEdxCuts  = 0;
    mMaker        = maker;
    setEventCuts(ecuts);
    setTrackCuts(tcuts);
    mInChain      = false;
    mAmDone       = false;
    mUseGlobalTracks=false;
    mPileup = new Pileup();
};

//-------------------------------------------------------------------------
StEStructMuDstReader::~StEStructMuDstReader() {
    delete mPileup;
};

void StEStructMuDstReader::setMuDstMaker(StMuDstMaker* maker, bool inChain){ 
  mInChain=inChain;
  mMaker=maker; 
};

void StEStructMuDstReader::setEventCuts(StEStructEventCuts* ecuts) { mECuts=ecuts; };
void StEStructMuDstReader::setTrackCuts(StEStructTrackCuts* tcuts) {
    mTCuts=tcuts;
    if ( !mTCuts->goodElectron(100.0) ) {      
        mhasdEdxCuts = 1;
        dEdxBefore = new TH2F("dEdxNoCut","dEdxNoCut",150,0,1.5,150,0,0.000015);
        dEdxAfter  = new TH2F("dEdxCut","dEdxCut",150,0,1.5,150,0,0.000015);
        mTCuts->addCutHists(dEdxBefore, dEdxAfter, "dEdXPlots");
    }
};
void StEStructMuDstReader::setUseGlobalTracks(bool global) {
    mUseGlobalTracks=global;
};

inline bool StEStructMuDstReader::setInChain(bool inChain) {
    mInChain = inChain;
    return mInChain;
};
inline bool StEStructMuDstReader::InChain(){ return mInChain; };
bool StEStructMuDstReader::hasMaker() { return (mMaker) ? true : false ; }
bool StEStructMuDstReader::hasEventCuts() { return (mECuts) ? true : false ; }
bool StEStructMuDstReader::hasTrackCuts() { return (mTCuts) ? true : false ; }


//-------------------------------------------------------------------------
StEStructEvent* StEStructMuDstReader::next() {

  StEStructEvent* retVal=NULL;
  if(!mMaker) return retVal;
  if(!mInChain){            // no chain to call Make() 
     int iret=mMaker->Make();
     if(iret){
        mAmDone=true;
        return retVal;
     }
  }
  if(!mMaker->muDst()) return retVal;
  return fillEvent();
}
     
//-------------------------------------------------------------------------
// Feb 14, 2006 djp Expunging all references to refMult that is stored in
//                  the MuDst. centrality cut will now always be on total
//                  number of tracks passing track cuts for MuDst. For
//                  Hijing this cut can be on number of tracks passing cuts
//                  or impact parameter.
// Apr 12, 2005 djp Jeff put centrality cuts into the event cuts and
//                  cut on refMult. I am changing this to use the
//                  centrality bin returned by the centrality object.
//                  This allows me to use total number of tracks passing cuts
//                  if I choose.
//  The problem is that to use all tracks passing cuts we have to count tracks
//  before we determine the centrality bin. Probably this amount of time is
//  small compared to rest of analysis.
StEStructEvent* StEStructMuDstReader::fillEvent(){

    StEStructEvent* retVal = NULL;
    StMuDst* muDst=mMaker->muDst();
    StMuEvent* muEvent=muDst->event();
    if(!muEvent) return retVal;

    retVal=new StEStructEvent();
       
    unsigned int tword=muEvent->l0Trigger().triggerWord();
    mNumGoodTracks = 0;

    float x;
    float y;
    float z;
    bool useEvent = true;

    // Note: Recommended primary vertex cuts are made in StEStructEventCuts.h
    x = muEvent->eventSummary().primaryVertexPosition().x();
    y = muEvent->eventSummary().primaryVertexPosition().y();
    z = muEvent->eventSummary().primaryVertexPosition().z();

    if ((fabs(x) < 1e-5) && (fabs(y) < 1e-5) && (fabs(z) < 1e-5)) {
        useEvent = false;
    } else if ( !mECuts->goodTrigger(muDst)  ||
                !mECuts->goodPrimaryVertexZ(z) ) {
        useEvent = false;
    }

    int nTracks = countGoodTracks();

//    Feb2006 rjp ........
//      currently for MuDst's all we have for centrality is NTracks 
//      if/when this changes and for EventGenerator readers, we will
//      wrap these via .... if(cent->getCentType()==ESNTracks){....
//

    retVal->SetCentrality((double)nTracks);
    if(!mECuts->goodCentrality(retVal->Centrality())) useEvent=false;

    retVal->SetRefMult( muEvent->refMult() );
    retVal->SetctbMult( muEvent->ctbMultiplicity() );
    retVal->SetNumPrim( muEvent->eventSummary().numberOfGoodPrimaryTracks() );    

    if(useEvent){

      retVal->SetEventID(muEvent->eventNumber());
      retVal->SetRunID(muEvent->runNumber());
      retVal->SetVertex(x,y,z);
      retVal->SetZDCe((float)muEvent->zdcAdcAttentuatedSumEast());
      retVal->SetZDCw((float)muEvent->zdcAdcAttentuatedSumWest());
      retVal->SetBField((float)muEvent->magneticField());

      if (!fillTracks(retVal)) useEvent=false; 
        
    }

    mECuts->fillHistogram(mECuts->triggerWordName(),(float)tword,useEvent);
    mECuts->fillHistogram(mECuts->primaryVertexZName(),z,useEvent);
    mECuts->fillHistogram(mECuts->centralityName(),(float)nTracks,useEvent);

    if (!useEvent) {
        delete retVal;
        retVal=NULL;
    } else {
        // Only check for pileup if event passes other cuts.
        // (I am interested in fraction of "good" events with pileup.)
        double zDist;  // Distance from z to nearest pileup vertex.
        int    mPile;  // Multiplicity of nearest pileup vertex.
        int    nPile;  // Total number of pileup vertices found (multiple of 2).
        nPile = mPileup->nearest(muDst,z,&zDist,&mPile);
        if (nPile > 0 && !mECuts->goodZVertSep(zDist)) {
            delete retVal;
            retVal   = NULL;
            useEvent = false;
        }
        mECuts->fillHistogram(mECuts->zVertSepName(),zDist,useEvent);
    }

    if (retVal) retVal->FillChargeCollections(); //creates track list by charge
    
    return retVal;
} 
 

//-------------------------------------------------------------
bool StEStructMuDstReader::fillTracks(StEStructEvent* estructEvent) {

    //
    // create a single EbyE track, check cuts,
    // fill variables, add to StEStructEvent tracks which
    // does a copy.
    //

    StMuDst* muDst=mMaker->muDst();
    int numTracks;
    if (mUseGlobalTracks) {
        numTracks = muDst->globalTracks()->GetEntries();
    } else {
        numTracks = muDst->primaryTracks()->GetEntries();
    }
    if (0==numTracks) {
        return false;
    }

    StEStructTrack* eTrack = new StEStructTrack();
    for(int i=0; i<numTracks; i++) {
        bool useTrack=true;
        eTrack->SetInComplete();
        StMuTrack* track;
        if (mUseGlobalTracks) {
            track = muDst->globalTracks(i);
        } else {
            track = muDst->primaryTracks(i);
        }

        if (mhasdEdxCuts) {
            dEdxBefore->Fill((track->p()).mag(),track->dEdx());
        }

        useTrack = isTrackGoodToUse( track ); //this includes track kine cuts
        mTCuts->fillHistograms(useTrack);

        if (useTrack) {
            if (mhasdEdxCuts) {
                dEdxAfter->Fill((track->p()).mag(),track->dEdx());
            }
            fillEStructTrack(eTrack,track);
            estructEvent->AddTrack(eTrack);
        }
    }
    delete eTrack;
    return true;
}
//-------------------------------------------------------------
// This method checks all track cuts.
// No histogramming or copying data around.
bool StEStructMuDstReader::isTrackGood(StMuTrack* track) {

    bool useTrack=true;

    // Do eta cut first so my ThisCut can use the eta value.
    useTrack = (mTCuts->goodEta(track->eta()) && useTrack);
    useTrack = (mTCuts->goodFlag(track->flag()) && useTrack);
    useTrack = (mTCuts->goodCharge(track->charge()) && useTrack);
    useTrack = (mTCuts->goodNFitPoints(track->nHitsFit()) && useTrack);
    useTrack = (mTCuts->goodNFitNMax((float)((float)track->nHitsFit()/(float)track->nHitsPoss())) && useTrack);
    if (mUseGlobalTracks) {
        useTrack = (mTCuts->goodGlobalDCA(track->dca().magnitude()) && useTrack);
    } else {
        useTrack = (mTCuts->goodGlobalDCA(track->dcaGlobal().magnitude()) && useTrack);
    }
    useTrack = (mTCuts->goodChi2(track->chi2()) && useTrack);
    useTrack = (mTCuts->goodPhi(track->phi()) && useTrack);
    if(track->pt() < 0.15) useTrack = false;  // basic pt cut, ranges checked in isTrackGoodToUse

//>>>>> delta p_t / p_t cut. Make sure charge sign is well defined.
// Need P08 or later for this code.
//    StMuDst* muDst=mMaker->muDst();
//    int ic = track->index2Cov();
//    if (ic > 0) {
//        static StDcaGeometry *cov = muDst->covGlobTracks(ic);
//        const float *err =  cov->errMatrix();
//        float dpTOverpT = sqrt(err[9])/track->pt(); //dpT/pT
//        useTrack = (mTCuts->gooddPtByPt(dpTOverpT) && useTrack);
//    }
//>>>>>

    //--> But add a quick electron removal... for selected p ranges
    //    Note I only want to do this if I am defining an electron dEdx cut.
    if (mhasdEdxCuts && mTCuts->goodElectron( track->nSigmaElectron() ) && useTrack )  {      
      float p = (track->p()).mag();
      if( (p>0.2 && p<0.45) || (p>0.7 && p<0.8) ) {
	useTrack=false;
	//cout << "  cut e:  p = " << p << ", nsig = " << track->nSigmaElectron() << endl;
      }
    }
    //--> end of electron pid

    return useTrack;
};

//----includes call to isTrackGood ... and adds some more ....
bool StEStructMuDstReader::isTrackGoodToUse(StMuTrack* track){

  bool useTrack=isTrackGood(track);
  if(useTrack){

    float pt = track->pt();
    useTrack = (mTCuts->goodPt(pt) && useTrack);
    float _r=pt/0.139;
    float yt=log(sqrt(1+_r*_r)+_r);
    useTrack = (mTCuts->goodYt(yt) && useTrack);
    float mt = sqrt(pt*pt + 0.139*0.139);
    float xt = 1 - exp( -1*(mt-0.139)/0.4 );
    useTrack = (mTCuts->goodXt(xt) && useTrack);

  }
    return useTrack;
};

//-------------------------------------------------------------------------
//-------------------------------------------------------------
// This method counts all good track.
// No histogramming or copying data around.
int StEStructMuDstReader::countGoodTracks() {
    mNumGoodTracks=0;
    StMuDst* muDst=mMaker->muDst();
    int numTracks;
    if (mUseGlobalTracks) {
        numTracks = muDst->globalTracks()->GetEntries();
    } else {
        numTracks = muDst->primaryTracks()->GetEntries();
    }
    if (0==numTracks) {
        return 0;
    }
    for(int i=0; i<numTracks; i++) {
        if (mUseGlobalTracks) {
            if (isTrackGood(muDst->globalTracks(i))) {
                mNumGoodTracks++;
            }
        } else {
            if (isTrackGood(muDst->primaryTracks(i))) {
                mNumGoodTracks++;
            }
        }
    }
    return mNumGoodTracks;
}

//-------------------------------------------------------------------------
void StEStructMuDstReader::fillEStructTrack(StEStructTrack* eTrack,StMuTrack* mTrack){

  StThreeVectorF p=mTrack->p();  
  eTrack->SetPx(p.x());
  eTrack->SetPy(p.y());
  eTrack->SetPz(p.z());

  StThreeVectorF b=mTrack->dca();
  eTrack->SetBx(b.x());
  eTrack->SetBy(b.y());
  eTrack->SetBz(b.z());

  StThreeVectorF gb;
  // In some productions we have dcaGlobal() = (0,0,0) for global tracks.
  // dca() seems to be useful though.
  if (mUseGlobalTracks) {
      gb = mTrack->dca();
  } else {
      gb = mTrack->dcaGlobal();
  }
  if (0 == gb.x() && 0 == gb.y() && 0 == gb.z()) {
      printf("Found track with dca = (0,0,0)\n");
  }
  eTrack->SetBxGlobal(gb.x());
  eTrack->SetByGlobal(gb.y());
  eTrack->SetBzGlobal(gb.z());

  // Note: For global tracks eta and phi are calculated at DCA to primary vertex.
  //       (Not sure how far back this goes. Need to check if using globals with old data)
  eTrack->SetEta(mTrack->eta());
  eTrack->SetPhi(mTrack->phi());

  eTrack->SetDedx(mTrack->dEdx());
  eTrack->SetChi2(mTrack->chi2());

  //
  // -> note in my analysis I chose nSigma instead of prob.
  //
  eTrack->SetPIDe(mTrack->nSigmaElectron()); 
  eTrack->SetPIDpi(mTrack->nSigmaPion());
  eTrack->SetPIDp(mTrack->nSigmaProton());
  eTrack->SetPIDk(mTrack->nSigmaKaon());
  eTrack->SetPIDd(10000.);

  eTrack->SetNFitPoints(mTrack->nHitsFit());
  eTrack->SetNFoundPoints(mTrack->nHits());
  eTrack->SetNMaxPoints(mTrack->nHitsPoss());

  StTrackTopologyMap map = mTrack->topologyMap();

  eTrack->SetTopologyMapTPCNHits(map.numberOfHits(kTpcId));
  eTrack->SetTopologyMapData(0,map.data(0));
  eTrack->SetTopologyMapData(1,map.data(1));

  eTrack->SetDetectorID(1); //TPC
  eTrack->SetFlag(mTrack->flag());
  eTrack->SetCharge(mTrack->charge());

  eTrack->SetHelix(mTrack->helix());
  // It seems that the calculation of entrance, mid and exit points still works
  // when we use the outerHelix instead of helix (which should be helix parameters
  // at the point closest to the primary vertex). I believe the two are different
  // to account for multiple scattering and energy loss. Expect properties of the
  // track within the TPC to be described better by outerHelix.
  //>>>>>Make this modification with the next set of updates (where I expect we will
  //     be using global tracks instead of primary tracks.)
//  eTrack->SetHelix(mTrack->outerHelix());
}; 
  

/***********************************************************************
 *
 * $Log: StEStructMuDstReader.cxx,v $
 * Revision 1.16  2009/05/08 00:04:22  prindle
 * Just putting Yuri's TMath back in
 *
 * Revision 1.15  2008/12/02 23:35:34  prindle
 * Added code for pileup rejection in EventCuts and MuDstReader.
 * Modified trigger selections for some data sets in EventCuts.
 *
 * Revision 1.14  2008/05/01 23:35:57  prindle
 *   Found that for global tracks we sometimes have global dca = (0,0,0)
 * Now use dca() when we are using global tracks.
 *
 * Revision 1.13  2008/03/19 22:01:59  prindle
 * Updated some dataset definitions.
 *
 * Revision 1.12  2007/11/26 19:52:24  prindle
 * Add cucu62, cucu200 2007ib production datasets.
 * Included vertex cuts for case of ranked vertices. (Pass muDst pointer to EventCuts)
 * Add n^(1/4) histograms to QAHists
 *
 * Revision 1.11  2007/05/27 22:43:35  msd
 * Added new centrality plots to Empty analysis
 *
 * Revision 1.10  2007/01/26 17:09:29  msd
 * Minor bug fix in AnalysisMaker, cleaned up EmptyAnalysis
 *
 * Revision 1.9  2006/04/11 17:50:45  prindle
 *   Remove inChain from constructor arguments (no longer used in macro)
 *
 * Revision 1.8  2006/04/06 00:54:01  prindle
 *   Tried to rationalize the way centrality is defined.
 *   Now the reader gives a float to StEStructEvent and this float is
 * what is being used to define centrality. When we need a centrality
 * bin index we pass this number into the centrality singleton object.
 *
 * Revision 1.7  2006/04/04 22:05:06  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.6  2006/02/22 22:03:23  prindle
 * Removed all references to multRef
 *
 * Revision 1.5  2005/09/14 17:08:34  msd
 * Fixed compiler warnings, a few tweaks and upgrades
 *
 * Revision 1.4  2005/09/07 20:18:42  prindle
 *   AnalysisMaker: Keep track of currentAnalysis (for use in doEStruct macro)
 *   EventCuts.h:   Added trigger cuts including cucu and year 4.
 *   MuDstReader:   Added dE/dx histograms. Re-arranged code to count tracks
 *                    before making centrality cut.
 *   TrackCuts:     Random changes. Moved some variables from private to public.o
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
