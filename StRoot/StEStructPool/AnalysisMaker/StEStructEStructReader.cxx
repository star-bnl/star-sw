/**********************************************************************
 *
 * $Id: StEStructEStructReader.cxx,v 1.1 2013/05/04 23:45:43 prindle Exp $
 *
 * Author: Duncan Prindle
 *
 **********************************************************************
 *
 * Description:  event reader class for reading EStruct event format files
 *               Essentially duplicated from StEstructMuDstReader.
 *
 ***********************************************************************/
#include "StEStructEStructReader.h"

#include "StEStructEventCuts.h"
#include "StEStructTrackCuts.h"
#include "StEStructPool/EventMaker/StEStructEvent.h"
#include "StEStructPool/EventMaker/StEStructTrack.h"
#include "StEStructPool/EventMaker/StEStructCentrality.h"

#include "StDetectorId.h"
#include "TFile.h"
#include "TList.h"


StEStructEStructReader::StEStructEStructReader() {
    mhasdEdxCuts  = 0;
    mhasToFCuts   = 0;
    mhasPrimaryCuts   = 0;
    mhasVertexRadiusCuts   = 0;
    mFile        = 0;
    mkeyList     = 0;
    miEvent      = 0;
    mnEvents     = 0;
    mECuts        = 0;
    mTCuts        = 0;
    mInChain      = false;
    mAmDone       = false;
    mUseGlobalTracks=false;
    mPileup = new Pileup();
};
StEStructEStructReader::StEStructEStructReader(char* estructFormatFile,
                                               StEStructEventCuts* ecuts,
                                               StEStructTrackCuts* tcuts) {
    mhasdEdxCuts  = 0;
    mhasToFCuts   = 0;
    mhasPrimaryCuts   = 0;
    mhasVertexRadiusCuts   = 0;
    mFile        = new TFile(estructFormatFile);
    miEvent      = 0;
    mkeyList     = mFile->GetListOfKeys();
    mnEvents     = mkeyList->GetEntries();
    setEventCuts(ecuts);
    setTrackCuts(tcuts);
    mInChain      = false;
    mAmDone       = false;
    mUseGlobalTracks=false;
    mPileup = new Pileup();
};

//-------------------------------------------------------------------------
StEStructEStructReader::~StEStructEStructReader() {
    delete mPileup;
    delete mFile;
};

void StEStructEStructReader::setEventCuts(StEStructEventCuts* ecuts) {
    mECuts=ecuts;
    if (!mECuts->doFillHists()) {
        return;
    }
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
void StEStructEStructReader::setTrackCuts(StEStructTrackCuts* tcuts) {
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

//-------------------------------------------------------------------------
StEStructEvent* StEStructEStructReader::next() {

    StEStructEvent* retVal=NULL;
    if(!mkeyList) return retVal;

    mEventIn = (StEStructEvent *) mFile->Get(mkeyList->At(miEvent)->GetName());
    miEvent++;
    mEventIn->FillChargeCollections();

    return fillEvent();
}
StEStructEvent* StEStructEStructReader::fillEvent(){

    StEStructEvent* retVal = new StEStructEvent();
    mNumGoodTracks = 0;

    float x;
    float y;
    float z;
    bool useEvent = true;

    // Note: Recommended primary vertex cuts are made in StEStructEventCuts.h
    x = mEventIn->Vx();
    y = mEventIn->Vy();
    z = mEventIn->Vz();

    // In StEStructMuDstReader we invoke mECuts->goodTrigger(muDst).
    // Comments suggest this is a topological cut. Skip it for now.
    // >>>>>>Check: If it is really required we need to figure out how to invoke it. I think is has been superseded.
    int good = 0;
    if ((fabs(x) < 1e-5) && (fabs(y) < 1e-5) && (fabs(z) < 1e-5)) {
        useEvent = false;
    } else if ( !mECuts->goodPrimaryVertexZ(z) ) {
        useEvent = false;
    } else {
        good++;
    }
    if (!mECuts->goodPrimaryVertexRadius(sqrt(x*x+y*y))) {
        useEvent = false;
    }

    // In StEStructMuDstReader we cut on distance between VPD Z vertex and
    // vertex determined by tracking. Didn't store VPD information in EStruct format.
    // >>>>>>Check: If we really need to tighten this cut we need to store VPD in EStruct.

    int ndEdx = 0;
    int nToF  = 0;
    int nTracks = countGoodTracks(&ndEdx, &nToF);
    if (!mECuts->goodToFFraction(ndEdx, nToF)) {
        useEvent = false;
    }

    // >>>>>>Check: Are you going to redefine primary and global tracks?
    // >>>>>>Check: If so do that before this cut (probably in countGoodTracks) and implement cut here.
    //int nGlobal  = muDst->globalTracks()->GetEntries();
    //int nPrimary = muDst->primaryTracks()->GetEntries();
    //if (!mECuts->goodPrimaryFraction(nPrimary, nGlobal)) {
    //    useEvent = false;
    //}

    retVal->SetCentrality((double)nTracks);
    if(!mECuts->goodCentrality(retVal->Centrality())) useEvent=false;

    retVal->SetRefMult( mEventIn->RefMult() );
    retVal->SetctbMult( mEventIn->ctbMult() );
    retVal->SetNumPrim( mEventIn->NumPrim() );    

    if(useEvent){

      retVal->SetEventID(mEventIn->EventID());
      retVal->SetRunID(mEventIn->RunID());
      retVal->SetVertex(x,y,z);
      retVal->SetZDCe((float)mEventIn->ZDCe());
      retVal->SetZDCw((float)mEventIn->ZDCw());
      // I think this zdc coincidence rate is one number for the event
      // Don't know how to get rate at time of event.
      // Actually, in old runs this was true. Now it should be updated every
      // 10 or 15 seconds throughout the run.
      retVal->SetZDCCoincidence((float)mEventIn->ZDCCoincidence());
      retVal->SetBField((float)mEventIn->BField());

      if (!fillTracks(retVal)) useEvent=false; 

    }

    mECuts->fillHistogram(mECuts->primaryVertexZName(),z,useEvent);
    // >>>>>>Check: Don't think we have VPD information in StEStructEvent.
    //if (btofHeader) {
    //    mECuts->fillHistogram(mECuts->primaryVPDVertexName(),z-vpdZ,useEvent);
    //}
    mECuts->fillHistogram(mECuts->centralityName(),(float)nTracks,useEvent);
    mECuts->fillHistogram(mECuts->goodtoffractionName(),(float)ndEdx,(float)nToF,useEvent);

    mECuts->fillHistogram("ToFPlots",(double)ndEdx,(double)nToF,useEvent);
    //mECuts->fillHistogram("PrimaryPlots",(double)nPrimary,(double)nGlobal,useEvent);
    mECuts->fillHistogram("VertexRadiusPlots",x,y,useEvent);

    // >>>>>>Check: My pileup finder relies on global tracks. If you really want to tighten up pileup cuts
    // >>>>>>Check: here we should probably store some information from the pileup code in the StEStructEvent.
    // Two pileup cuts using my vertex finding.
    // First we look for vertices which have tracks from only one side of TPC.
    // Then we look for matching pileup vertices where one of them reconstructs near the primary vertex.
    //if (useEvent && mECuts->hasZVertMatchCut()) {
    //    double zDist = 0;  // Distance from z to nearest pileup half vertex.
    //    int nPile = mPileup->find(muDst);
    //    if ((nPile > 0) && (6 == mPileup->flag(0))) {
    //        double zd1 = z - mPileup->z(0,2);
    //        double zd2 = z - mPileup->z(0,3);
    //        zDist = (fabs(zd1) < fabs(zd2)) ? zd1 : zd2;
    //        if (!mECuts->goodZVertMatch(zDist)) {
    //            useEvent = false;
    //        }
    //        mECuts->fillHistogram(mECuts->zVertMatchName(),zDist,useEvent);
    //    }
    //}

    //if (useEvent && mECuts->hasZVertSepCut()) {
    //    double zDist;  // Distance from z to nearest pileup vertex.
    //    int    mPile;  // Multiplicity of nearest pileup vertex.
    //    int    nPile;  // Total number of pileup vertices found (multiple of 2).
    //    nPile = mPileup->nearest(muDst,z,&zDist,&mPile);
    //    if (nPile > 0 && !mECuts->goodZVertSep(zDist)) {
    //        useEvent = false;
    //    }
    //    mECuts->fillHistogram(mECuts->zVertSepName(),zDist,useEvent);
    //}

    if (!useEvent) {
        delete retVal;
        retVal=NULL;
    }
    if (retVal) retVal->FillChargeCollections(); //creates track list by charge
    
    return retVal;
}


//-------------------------------------------------------------
bool StEStructEStructReader::fillTracks(StEStructEvent* estructEvent) {

    //
    // create a single EStruct track, check cuts,
    // fill variables, add to StEStructEvent tracks which
    // does a copy.
    //

    int numTracks = mEventIn->Ntrack();
    if (0==numTracks) {
        return false;
    }

    for (int ich=0;ich<2;ich++) {
        StEStructTrackCollection *tc;
        if (ich==0) {
            tc=mEventIn->TrackCollectionP();
        } else {
            tc=mEventIn->TrackCollectionM();
        }
        for (StEStructTrackIterator iter = tc->begin(); iter != tc->end(); iter++) {
            StEStructTrack* track = (*iter);

            bool useTrack=true;
            float ptot = track->Charge() * track->Ptot();
            float beta = track->beta();

            useTrack = isTrackGoodToUse( track ); //this includes track kine cuts
            mTCuts->fillHistograms(useTrack);

            mTCuts->fillHistogram("dEdxPlots",ptot,track->Dedx(),useTrack);
            if (beta != 0) {
                mTCuts->fillHistogram("dEdxBetaPlots",ptot,track->Dedx(),beta,useTrack);
        }    

            if (useTrack) {
                estructEvent->AddTrack(track);
            }
        }
    }
    return true;
}
//-------------------------------------------------------------
// This method checks all track cuts.
// No histogramming or copying data around.
bool StEStructEStructReader::isTrackGood(StEStructTrack* track) {

    bool useTrack=true;

    // When using global tracks use the outerHelix to calculate eta, phi and dca.
    // >>>>>> Not sure it makes sense to allow global track cuts for EStruct format events.
    // >>>>>> Need to look into this if that is required.
    Float_t mDCA;
    mEta = track->Eta();
    mPhi = track->Phi();
    mDCA = track->DcaGlobal();

    // Do eta cut first so my ThisCut can use the eta value.
    useTrack = (mTCuts->goodEta(mEta) && useTrack);
    useTrack = (mTCuts->goodFlag(track->Flag()) && useTrack);
    useTrack = (mTCuts->goodCharge(track->Charge()) && useTrack);
    useTrack = (mTCuts->goodNFitPoints(track->NFitPoints()) && useTrack);
    useTrack = (mTCuts->goodNFitNMax((float)((float)track->NFitPoints()/(float)track->NMaxPoints())) && useTrack);
    useTrack = (mTCuts->goodGlobalDCA(mDCA) && useTrack);
    useTrack = (mTCuts->goodChi2(track->Chi2()) && useTrack);
    useTrack = (mTCuts->goodPhi(mPhi) && useTrack);
    if(track->Pt() < 0.15) useTrack = false;  // basic pt cut, ranges checked in isTrackGoodToUse

    //--> But add a quick electron removal... for selected p ranges
    //    Note I only want to do this if I am defining an electron dEdx cut.
    if (mhasdEdxCuts && mTCuts->goodElectron( track->PIDe_dEdx() ) && useTrack )  {
        float p = track->Ptot();
        if( (p>0.2 && p<0.45) || (p>0.7 && p<0.8) ) {
	        useTrack=false;
        }
    }
    //--> end of electron pid

    return useTrack;
};

//----includes call to isTrackGood ... and adds some more ....
bool StEStructEStructReader::isTrackGoodToUse(StEStructTrack* track){

  bool useTrack=isTrackGood(track);
  if(useTrack){

    float pt = track->Pt();
    useTrack = (mTCuts->goodPt(pt) && useTrack);
    float yt = track->Yt();
    useTrack = (mTCuts->goodYt(yt) && useTrack);
    float xt = track->Xt();
    useTrack = (mTCuts->goodXt(xt) && useTrack);

  }
    return useTrack;
};

//-------------------------------------------------------------------------
//-------------------------------------------------------------
// This method counts all good track.
// No histogramming or copying data around.
int StEStructEStructReader::countGoodTracks(int *ndEdx, int *nToF) {
    mNumGoodTracks = 0;
    int ndedx = 0;
    int ntof  = 0;

    int numTracks = mEventIn->Ntrack();
    // >>>>>> If we need to redefine global and primary tracks we need to do more work.
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

    for (int ich=0;ich<2;ich++) {
        StEStructTrackCollection *tc;
        if (ich==0) {
            tc=mEventIn->TrackCollectionP();
        } else {
            tc=mEventIn->TrackCollectionM();
        }
        for (StEStructTrackIterator iter = tc->begin(); iter != tc->end(); iter++) {
            StEStructTrack* track = (*iter);
            if (isTrackGood(track)) {
                mNumGoodTracks++;
                if ((fabs(track->PIDpi_dEdx()) < 2) ||
                    (fabs(track->PIDk_dEdx()) < 2) ||
                    (fabs(track->PIDp_dEdx()) < 2)) {
                    ndedx++;
                }
                if ((fabs(track->PIDpi_ToF()) < 2) ||
                    (fabs(track->PIDpi_ToF()) < 2) ||
                    (fabs(track->PIDpi_ToF()) < 2)) {
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
void StEStructEStructReader::fillEStructTrack(StEStructTrack* eTrack,StEStructTrack* mTrack){

  eTrack->SetPx(mTrack->Px());
  eTrack->SetPy(mTrack->Py());
  eTrack->SetPz(mTrack->Pz());

  eTrack->SetBx(mTrack->Bx());
  eTrack->SetBy(mTrack->By());
  eTrack->SetBz(mTrack->Bz());

  if (0 == mTrack->BxGlobal() && 0 == mTrack->ByGlobal() && 0 == mTrack->BzGlobal()) {
      printf("Found track with dca = (0,0,0)\n");
  }
  eTrack->SetBxGlobal(mTrack->BxGlobal());
  eTrack->SetByGlobal(mTrack->ByGlobal());
  eTrack->SetBzGlobal(mTrack->BzGlobal());

  eTrack->SetEta(mEta);
  eTrack->SetPhi(mPhi);
  eTrack->SetDedx(mTrack->Dedx());
  eTrack->SetChi2(mTrack->Chi2());
  eTrack->SetHelix(mTrack->Helix());

  //
  // -> note in my analysis I chose nSigma instead of prob.
  //
  eTrack->SetPIDe_dEdx(mTrack->PIDe_dEdx()); 
  eTrack->SetPIDpi_dEdx(mTrack->PIDpi_dEdx());
  eTrack->SetPIDp_dEdx(mTrack->PIDp_dEdx());
  eTrack->SetPIDk_dEdx(mTrack->PIDk_dEdx());
  eTrack->SetPIDd_dEdx(10000.);


  eTrack->SetPIDe_ToF(mTrack->PIDe_ToF());
  eTrack->SetPIDpi_ToF(mTrack->PIDpi_ToF());
  eTrack->SetPIDp_ToF(mTrack->PIDp_ToF());
  eTrack->SetPIDk_ToF(mTrack->PIDk_ToF());
  eTrack->SetPIDd_ToF(999);
  eTrack->SetBeta(mTrack->beta());

  eTrack->SetNFitPoints(mTrack->NFitPoints());
  eTrack->SetNFoundPoints(mTrack->NFoundPoints());
  eTrack->SetNMaxPoints(mTrack->NMaxPoints());

  eTrack->SetTopologyMapTPCNHits(mTrack->TopologyMapTPCNHits());
  eTrack->SetTopologyMapData(0,mTrack->TopologyMapData(0));
  eTrack->SetTopologyMapData(1,mTrack->TopologyMapData(1));

  eTrack->SetDetectorID(1); //TPC
  eTrack->SetFlag(mTrack->Flag());
  eTrack->SetCharge(mTrack->Charge());
};


/***********************************************************************
 *
 * $Log: StEStructEStructReader.cxx,v $
 * Revision 1.1  2013/05/04 23:45:43  prindle
 * Code to read EStruct format files. Modified from StEStructMuDstReader.
 *
 *
 * Revision 1.0  2013/05/02 18:20:32  prindle
 * Copy of StEStructMuDstReader specialized for reading EStruct format files..
 *
 *********************************************************************/
