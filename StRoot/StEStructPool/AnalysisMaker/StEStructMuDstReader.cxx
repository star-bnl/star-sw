/**********************************************************************
 *
 * $Id: StEStructMuDstReader.cxx,v 1.20 2012/11/16 21:19:07 prindle Exp $
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
#include "StBTofPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuBTofPidTraits.h"


#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StDetectorId.h"


StEStructMuDstReader::StEStructMuDstReader() {
    mhasdEdxCuts  = 0;
    mhasToFCuts   = 0;
    mhasPrimaryCuts   = 0;
    mhasVertexRadiusCuts   = 0;
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
    mhasToFCuts   = 0;
    mhasPrimaryCuts   = 0;
    mhasVertexRadiusCuts   = 0;
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

void StEStructMuDstReader::setEventCuts(StEStructEventCuts* ecuts) {
    mECuts=ecuts;
    if (!mECuts->doFillHists()) {
        return;
    }
    // I want to create histograms for trigger ids here.
    // However, I need an StMuEvent which I think I don't have until fillEvent.
    // Create histograms there.
    // Histograms for ToF cuts.
    //>>>>>>>>>> Need to figure out where to fill these, and how to write them out.
    if ( mECuts->hasToFFractionCut() ) {
        mhasToFCuts = 1;
        ToFBefore = new TH2F("dEdxToFNoCut2D","dEdxToFNoCut2D",50,1,1000,50,1,750);
        ToFAfter  = new TH2F("dEdxToFCut2D","dEdxToFCut2D",50,1,1000,50,1,750);
        mECuts->addCutHists(ToFBefore, ToFAfter, "ToFPlots2D");
    }
    if ( mECuts->hasPrimaryFractionCut() ) {
        mhasPrimaryCuts = 1;
        PrimaryBefore = new TH2F("PrimVGlobNoCut2D","PrimVGlobNoCut2D",120,1,1200,200,1,2000);
        PrimaryAfter  = new TH2F("PrimVGlobCut2D","PrimVGlobCut2D",120,1,1200,200,1,2000);
        mECuts->addCutHists(PrimaryBefore, PrimaryAfter, "PrimaryPlots2D");
    }
    if ( mECuts->hasPrimaryVertexRadiusCut() ) {
        mhasVertexRadiusCuts = 1;
        VRadiusBefore = new TH2F("VertexRadiusNoCut2D","VertexRadiusNoCut2D",50,-10,10,50,-10,10);
        VRadiusAfter  = new TH2F("VertexRadiusCut2D","VertexRadiusCut2D",50,-10,10,50,-10,10);
        mECuts->addCutHists(VRadiusBefore, VRadiusAfter, "VertexRadiusPlots2D");
    }
};
void StEStructMuDstReader::setTrackCuts(StEStructTrackCuts* tcuts) {
    mTCuts=tcuts;
    if (!mTCuts->doFillHists()) {
        return;
    }
    if ( mTCuts->hasElectronCut() ) {      
        mhasdEdxCuts = 1;
        dEdxBefore = new TH2F("dEdxNoCut2D","dEdxNoCut2D",150,-1.5,1.5,150,0,0.000015);
        dEdxAfter  = new TH2F("dEdxCut2D","dEdxCut2D",150,-1.5,1.5,150,0,0.000015);
        mTCuts->addCutHists(dEdxBefore, dEdxAfter, "dEdxPlots2D");
    }
    dEdxBetaBefore = new TH3F("dEdxBetaNoCut3D","dEdxBetaNoCut3D",100,-2.0,2.0,100,0.0000015,0.000006,100,0.55,1.05);
    dEdxBetaAfter  = new TH3F("dEdxBetaCut3D","dEdxBetaCut3D",100,-2.0,2.0,100,0.0000015,0.000006,100,0.55,1.05);
    mTCuts->addCutHists(dEdxBetaBefore, dEdxBetaAfter, "dEdxBetaPlots3D");
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

    mNumGoodTracks = 0;

    float x;
    float y;
    float z;
    bool useEvent = true;

    // Note: Recommended primary vertex cuts are made in StEStructEventCuts.h
    x = muEvent->eventSummary().primaryVertexPosition().x();
    y = muEvent->eventSummary().primaryVertexPosition().y();
    z = muEvent->eventSummary().primaryVertexPosition().z();
    int nMultVertex = 0;
    if (muEvent->eventSummary().numberOfVertices()>1) {
        nMultVertex++;
    }

    // goodTrigger (currently) does a topological cut on vertex shape of the "current" primary vertex.
    // Need to require tracks are associated with this vertex.
    // Unless someone redefines the currentVertexIndex it will be set to 0 and the dca cuts should
    // remove all tracks we don't want.
    int good = 0;
    mPrimaryVertexId = muDst->currentVertexIndex();
    if ((fabs(x) < 1e-5) && (fabs(y) < 1e-5) && (fabs(z) < 1e-5)) {
        useEvent = false;
    } else if ( !mECuts->goodTrigger(muDst)  ||
                !mECuts->goodPrimaryVertexZ(z) ) {
        useEvent = false;
    } else {
        good++;
    }
    if (!mECuts->goodPrimaryVertexRadius(sqrt(x*x+y*y))) {
        useEvent = false;
    }
    StBTofHeader* btofHeader = muDst->btofHeader();
    float vpdZ = 0;
    if (btofHeader) {
        vpdZ = btofHeader->vpdVz();
        if (!mECuts->goodVPDVertex(z-vpdZ)) {
            useEvent = false;
        }
    }

    int ndEdx = 0;
    int nToF  = 0;
    int nTracks = countGoodTracks(&ndEdx, &nToF);
    if (!mECuts->goodToFFraction(ndEdx, nToF)) {
        useEvent = false;
    }
    int nGlobal  = muDst->globalTracks()->GetEntries();
    int nPrimary = muDst->primaryTracks()->GetEntries();
    if (!mECuts->goodPrimaryFraction(nPrimary, nGlobal)) {
        useEvent = false;
    }

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
      // I think this zdc coincidence rate is one number for the event
      // Don't know how to get rate at time of event.
      // Actually, in old runs this was true. Now it should be updated every
      // 10 or 15 seconds throughout the run.
      retVal->SetZDCCoincidence((float)muEvent->runInfo().zdcCoincidenceRate());
      retVal->SetBField((float)muEvent->magneticField());

      if (!fillTracks(retVal)) useEvent=false; 
        
    }

    // Fill (before) trigger information for this event
    const StTriggerId& l0Nom = muEvent->triggerIdCollection().nominal();
    int nTrig = 0;
    for (unsigned int idx=0;idx<l0Nom.maxTriggerIds();idx++) {
        if (0 == l0Nom.triggerId(idx)) {
            break;
        }
        nTrig++;
    }
    for (int idx=0;idx<nTrig;idx++) {
        mECuts->fillHistogram(mECuts->triggerWordName(),l0Nom.triggerId(idx),useEvent);
        for (int idy=idx+1;idy<nTrig;idy++) {
            mECuts->fillHistogram(mECuts->triggerWordName(),l0Nom.triggerId(idx),l0Nom.triggerId(idy),useEvent);
        }
    }
    mECuts->fillHistogram(mECuts->primaryVertexZName(),z,useEvent);
    if (btofHeader) {
        mECuts->fillHistogram(mECuts->primaryVPDVertexName(),z-vpdZ,useEvent);
    }
    mECuts->fillHistogram(mECuts->centralityName(),(float)nTracks,useEvent);
    mECuts->fillHistogram(mECuts->goodtoffractionName(),(float)ndEdx,(float)nToF,useEvent);

    mECuts->fillHistogram("ToFPlots",(double)ndEdx,(double)nToF,useEvent);
    mECuts->fillHistogram("PrimaryPlots",(double)nPrimary,(double)nGlobal,useEvent);
    mECuts->fillHistogram("VertexRadiusPlots",x,y,useEvent);

    // Two pileup cuts using my vertex finding.
    // First we look for vertices which have tracks from only one side of TPC.
    // Then we look for matching pileup vertices where one of them reconstructs near the primary vertex.
    if (useEvent && mECuts->hasZVertMatchCut()) {
        double zDist = 0;  // Distance from z to nearest pileup half vertex.
        int nPile = mPileup->find(muDst);
        if ((nPile > 0) && (6 == mPileup->flag(0))) {
            double zd1 = z - mPileup->z(0,2);
            double zd2 = z - mPileup->z(0,3);
            zDist = (fabs(zd1) < fabs(zd2)) ? zd1 : zd2;
            if (!mECuts->goodZVertMatch(zDist)) {
                useEvent = false;
            }
            mECuts->fillHistogram(mECuts->zVertMatchName(),zDist,useEvent);
        }
    }

    if (useEvent && mECuts->hasZVertSepCut()) {
        double zDist;  // Distance from z to nearest pileup vertex.
        int    mPile;  // Multiplicity of nearest pileup vertex.
        int    nPile;  // Total number of pileup vertices found (multiple of 2).
        nPile = mPileup->nearest(muDst,z,&zDist,&mPile);
        if (nPile > 0 && !mECuts->goodZVertSep(zDist)) {
            useEvent = false;
        }
        mECuts->fillHistogram(mECuts->zVertSepName(),zDist,useEvent);
    }

    if (!useEvent) {
        delete retVal;
        retVal=NULL;
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
        // Even global tracks are associated with a particular primary vertex (for calculating DCA).
// This check seems way too slow. See if we can get dca to calculate wrt mPrimaryVertex.
//        if (track->vertexIndex() != mPrimaryVertexId) {
//            continue;
//        }

        float ptot = track->charge() * track->p().magnitude();
        StMuBTofPidTraits dProb = track->btofPidTraits();
        float beta = dProb.beta();

        useTrack = isTrackGoodToUse( track ); //this includes track kine cuts
        mTCuts->fillHistograms(useTrack);

        mTCuts->fillHistogram("dEdxPlots",ptot,track->dEdx(),useTrack);
        if (beta != 0) {
            mTCuts->fillHistogram("dEdxBetaPlots",ptot,track->dEdx(),beta,useTrack);
        }

        if (useTrack) {
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

    // When using global tracks use the outerHelix to calculate eta, phi and dca.
    Float_t mDCA;
    if (mUseGlobalTracks) {
        StMuDst* muDst=mMaker->muDst();
        StMuEvent* muEvent=muDst->event();
        const StThreeVectorF &pvert = muEvent->eventSummary().primaryVertexPosition();
        StPhysicalHelixD helix = track->outerHelix();
        double dist = helix.pathLength(pvert,false);
        const StThreeVectorF &pos = helix.at(dist) - pvert;
        const StThreeVectorF &dir = helix.cat(dist);

        mEta = dir.pseudoRapidity();
        mPhi = dir.phi();
        mDCA = pos.mag();
    } else {
        mEta = track->eta();
        mPhi = track->phi();
        mDCA = track->dcaGlobal(mPrimaryVertexId).magnitude();
    }

    // Do eta cut first so my ThisCut can use the eta value.
    useTrack = (mTCuts->goodEta(mEta) && useTrack);
    useTrack = (mTCuts->goodFlag(track->flag()) && useTrack);
    useTrack = (mTCuts->goodCharge(track->charge()) && useTrack);
    useTrack = (mTCuts->goodNFitPoints(track->nHitsFit()) && useTrack);
    useTrack = (mTCuts->goodNFitNMax((float)((float)track->nHitsFit()/(float)track->nHitsPoss())) && useTrack);
    useTrack = (mTCuts->goodGlobalDCA(mDCA) && useTrack);
    useTrack = (mTCuts->goodChi2(track->chi2()) && useTrack);
    useTrack = (mTCuts->goodPhi(mPhi) && useTrack);
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
        }
    }
    //--> end of electron pid

    //  Try a mass cut using TOF to reject electrons.
    //  Expect TOF efficiency to be of order 60% but if we can get one of two that isn't bad.
    // Do some QA plots before implementing. Probably end up with 1/beta and dEdx 2D cut.
    //if (mTCuts->goodTOFEMass( track->Mass() ) && useTrack )  {      
    //  useTrack=false;
    //}

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
int StEStructMuDstReader::countGoodTracks(int *ndEdx, int *nToF) {
    mNumGoodTracks = 0;
    int ndedx = 0;
    int ntof  = 0;
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
    // Want to compare pp analysis with |\eta| < 0.5 and |\eta| < 1.0
    // countGoodTracks is used for centrality, so we change \eta cuts here
    // (then change back at and)
    // >>>>>Expect this code to normally be commented out.<<<<<
//    double etaMin = mTCuts->meta[0];
//    double etaMax = mTCuts->meta[1];
//    mTCuts->meta[0] = -1;
//    mTCuts->meta[1] = +1;

    for(int i=0; i<numTracks; i++) {
        if (mUseGlobalTracks) {
            if (isTrackGood(muDst->globalTracks(i))) {
                mNumGoodTracks++;
                if ((fabs(muDst->globalTracks(i)->nSigmaPion()) < 2) ||
                    (fabs(muDst->globalTracks(i)->nSigmaKaon()) < 2) ||
                    (fabs(muDst->globalTracks(i)->nSigmaProton()) < 2)) {
                    ndedx++;
                }
                StMuBTofPidTraits dProb = muDst->globalTracks(i)->btofPidTraits();
                if ((fabs(dProb.sigmaPion()) < 2) ||
                    (fabs(dProb.sigmaKaon()) < 2) ||
                    (fabs(dProb.sigmaProton()) < 2)) {
                    ntof++;
                }
            }
        } else {
            if (isTrackGood(muDst->primaryTracks(i))) {
                mNumGoodTracks++;
                if ((fabs(muDst->primaryTracks(i)->nSigmaPion()) < 2) ||
                    (fabs(muDst->primaryTracks(i)->nSigmaKaon()) < 2) ||
                    (fabs(muDst->primaryTracks(i)->nSigmaProton()) < 2)) {
                    ndedx++;
                }
                StMuBTofPidTraits dProb = muDst->primaryTracks(i)->btofPidTraits();
                if ((fabs(dProb.sigmaPion()) < 2) ||
                    (fabs(dProb.sigmaKaon()) < 2) ||
                    (fabs(dProb.sigmaProton()) < 2)) {
                    ntof++;
                }
            }
        }
    }
    *ndEdx = ndedx;
    *nToF  = ntof;
//    mTCuts->meta[0] = etaMin;
//    mTCuts->meta[1] = etaMax;
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
//      gb = mTrack->dca();
      gb = mTrack->dcaGlobal();
  } else {
      gb = mTrack->dcaGlobal();
  }
  if (0 == gb.x() && 0 == gb.y() && 0 == gb.z()) {
      printf("Found track with dca = (0,0,0)\n");
  }
  eTrack->SetBxGlobal(gb.x());
  eTrack->SetByGlobal(gb.y());
  eTrack->SetBzGlobal(gb.z());

  eTrack->SetEta(mEta);
  eTrack->SetPhi(mPhi);
  eTrack->SetDedx(mTrack->dEdx());
  eTrack->SetChi2(mTrack->chi2());
  if (mUseGlobalTracks) {
      eTrack->SetHelix(mTrack->outerHelix());
  } else {
      eTrack->SetHelix(mTrack->helix());
  }

  //
  // -> note in my analysis I chose nSigma instead of prob.
  //
  eTrack->SetPIDe_dEdx(mTrack->nSigmaElectron()); 
  eTrack->SetPIDpi_dEdx(mTrack->nSigmaPion());
  eTrack->SetPIDp_dEdx(mTrack->nSigmaProton());
  eTrack->SetPIDk_dEdx(mTrack->nSigmaKaon());
  eTrack->SetPIDd_dEdx(10000.);


  StMuBTofPidTraits dProb = mTrack->btofPidTraits();
  eTrack->SetPIDe_ToF(dProb.sigmaElectron());
  eTrack->SetPIDpi_ToF(dProb.sigmaPion());
  eTrack->SetPIDp_ToF(dProb.sigmaProton());
  eTrack->SetPIDk_ToF(dProb.sigmaKaon());
  eTrack->SetPIDd_ToF(999);
  eTrack->SetBeta(dProb.beta());

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
}; 
  

/***********************************************************************
 *
 * $Log: StEStructMuDstReader.cxx,v $
 * Revision 1.20  2012/11/16 21:19:07  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.19  2011/08/02 20:31:25  prindle
 *   Change string handling
 *   Added event cuts for VPD, good fraction of global tracks are primary, vertex
 *   found only from tracks on single side of TPC, good fraction of primary tracks have TOF hits..
 *   Added methods to check if cuts imposed
 *   Added 2010 200GeV and 62 GeV, 2011 19 GeV AuAu datasets, 200 GeV pp2pp 2009 dataset.
 *   Added TOF vs. dEdx vs. p_t histograms
 *   Fix participant histograms in QAHists.
 *   Added TOFEMass cut in TrackCuts although I think we want to supersede this.
 *
 * Revision 1.18  2010/09/02 21:20:09  prindle
 *   Cuts:   Add flag to not fill histograms. Important when scanning files for sorting.
 *   EventCuts: Add radius cut on vertex, ToF fraction cut. Merge 2004 AuAu 200 GeV datasets.
 *              Add 7, 11 and 39 GeV dataset selections
 *   MuDstReader: Add 2D histograms for vertex radius and ToF fraction cuts.
 *                Modify countGoodTracks to return number of dEdx and ToF pid identified tracks.
 *                Include code to set track pid information from Dst.
 *   QAHists: New ToF QA hists. Modify dEdx to include signed momentum.
 *
 * Revision 1.17  2010/03/02 21:43:38  prindle
 *   Use outerHelix() for global tracks
 *   Add sensible triggerId histograms
 *   Starting to add support to sort events (available for Hijing)
 *
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
