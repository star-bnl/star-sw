/***************************************************************************
 *
 * $Id: StTofMaker.cxx,v 1.2 2001/04/26 18:39:54 wzhang Exp $
 *
 * Author: Wei-Ming Zhang, April 2001
 *
 ***************************************************************************
 *
 * Description: TOF offline software
 *              StTofMaker.h - ROOT/STAR Maker for offline chain.
 ***************************************************************************
 *
 * $Log: StTofMaker.cxx,v $
 * Revision 1.2  2001/04/26 18:39:54  wzhang
 * Changed MCInfo() to mcInfo()
 *
 * Revision 1.1  2001/04/24 20:27:08  wzhang
 * First release
 *
 *
 **************************************************************************/

#include "StTofMaker.h"
#include "StChain.h"
#include "stdlib.h"
#include "StMessMgr.h"
#include "StEventTypes.h"
#include "SystemOfUnits.h"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "StTofDatabase.h"
#include "StTofCross.h"
#include "StTofMC.h"
//#include "StTofPidTraits.h"
//#include "StTofPidAlgorithm.h"
#include "StParticleDefinition.hh"

#if !defined(ST_NO_NAMESPACES)
 using namespace units;
#endif
#include "TFile.h"
#include "TH1.h"
#include "TH2.h"

ClassImp(StTofMaker)

//_____________________________________________________________________________

StTofMaker::StTofMaker(const char *name):StMaker(name) { drawinit=kFALSE; }

//_____________________________________________________________________________

StTofMaker::~StTofMaker(){}

//_____________________________________________________________________________

Int_t StTofMaker::Init(){
    int iInit=0;
    mTofCross = new StTofCross();
    if(mTofCross != NULL) iInit = kStOK;

    mTofDb = new StTofDatabase();
//    mTofDb = StTofDatabase::getDb();
    mTofDb->fillDb();
    
// create Histograms  
    mHitPerTrk    = new TH1F ("Hits Per Track","HitPerTrk",10,0.,10.0);
    mTrkDist      = new TH1F ("Track Dist","TrkDist",10,0.,10.0);
    mHitDist      = new TH1F ("Hit Dist","HitDist",10,0.,10.0);
    mZ            = new TH1F ("Z coordinate","z",200,0.,200.0);
    mZ1            = new TH1F ("Z coordinate1","z1",200,0.,200.0);
    mCur1          = new TH1F ("Curvature1","Curv1",100,0.,0.2);
    mCur2          = new TH1F ("Curvature2","Curv2",100,0.,0.2);
    mCur3          = new TH1F ("Curvature3","Curv3",100,0.,0.2);
    mMom1          = new TH1F ("Momentum1","Mom1",100,0.,5.0);
    mMom2          = new TH1F ("Momentum2","Mom2",100,0.,5.0);
    mMom3          = new TH1F ("Momentum3","Mom3",100,0.,5.0);
    mPz            = new TH1F ("MomentumZ","Pz",100,0.,5.0);
    mPz1            = new TH1F ("MomentumZ1","Pz1",100,0.,5.0);

    return StMaker::Init();
}

//_____________________________________________________________________________

Int_t StTofMaker::Make(){
    int iMake=kStOK;

    mEvent = (StEvent *) GetInputDS("StEvent");
   
// check collections
        mTofCollectionPresent = 0;
        mSlatCollectionPresent = 0;
        mHitCollectionPresent = 0;

    if (mEvent) {
        mTheTofCollection = mEvent->tofCollection();
        if(mTheTofCollection) {
            mTofCollectionPresent = 1;
            cout << "StEvent tofCollection Exists" << endl;

            if(mTheTofCollection->slatsPresent()) {
                cout << "StEvent TofSlatCollection Exists" << endl;
                mSlatCollectionPresent = 1;
            }
            else 
                cout << "** StEvent TofSlatCollection DOES NOT Exist" << endl;

            if(mTheTofCollection->hitsPresent()) {
                cout << "StEvent TofHitCollection Exists" << endl;
                mSlatCollectionPresent = 1;
            }
            else 
                cout << "** StEvent TofHitCollection DOES NOT Exist" << endl;
        }
        else {
            gMessMgr->Warning()
                 << "** StEvent tofCollection DOES NOT Exist" << endm;
            cout << "** StEvent TofSlatCollection DOES NOT Exist" << endl;
            cout << "** StEvent TofHitCollection DOES NOT Exist" << endl;
        }
    }
    else {
            gMessMgr->Warning()
             << " ***** WARNING IN STTOFMAKER *****" << endm;
        cout << " *****StEvent structure does not exist" << endl;
        cout << " *** Try make one yourself" << endl;
//        mEvent = new StEvent();
// AddData is a member of StMaker --- WMZ ---
        //AddData(new St_ObjectSet("StTofEvent", mEvent));
    }

// build mSlatCollection first

    mSlatCollection = new StTofSlatCollection();

// slatCollection  not available
    if(mSlatCollectionPresent != 1) {
      StTofMC tofMC;

// get data from xdf files
       if(m_Mode == 2) {
          St_DataSet *geantData = GetInputDS("geant");
          if (geantData) {//          Geant input exists

// vectors for manipulating MCSlats, instances of StTofMCSlat
            tofMCSlatVector MCSlatVec, slatTempVec, slatErasedVec;
            tofMCSlatVecIter  slatTempIter, slatErasedIter;
            MCSlatVec.clear();

// Geant data
            St_DataSetIter geantIter(geantData);
            StafStG2TTrack   *g2tTrackData = 
                             (StafStG2TTrack *)geantIter("g2t_track");
            StTofG2TTrack* g2tTrackPtr = new StTofG2TTrack;

// work on g2t tracks first
            if (g2tTrackData) {
               g2tTrackPtr = g2tTrackData->GetTable();
               cout << "g2t track # = " << g2tTrackData->GetNRows() << endl;
               for (int i = 0; i<g2tTrackData->GetNRows(); i++, g2tTrackPtr++) {
//                if(i%40 == 0) {
//                   cout << "Track info = "  << g2tTrackPtr->ge_pid << " "
//                   << g2tTrackPtr->id << " " << g2tTrackPtr->n_tof_hit << " "
//                   << g2tTrackPtr->hit_tof_p << " "<< g2tTrackPtr->p[1] << " "
//                   << g2tTrackPtr->p[2] << " " << g2tTrackPtr->p[3] << " "
//                   << g2tTrackPtr->eta  << endl;
//                }
               }
               for(int i = 0; i<g2tTrackData->GetNRows(); i++, g2tTrackPtr--) {}
            }
/*  comment out CTB stuff
// work on g2t CTB hits which are treated as TOF hits (volume_id is not used)
            StafStG2THit *g2tCtbHitData = 
                                 (StafStG2THit*) geantIter("g2t_ctb_hit");
            if (g2tCtbHitData) {
               StTofG2THit* g2tCtbHitPtr = g2tCtbHitData->GetTable();

               cout << "g2t Hit # = " << g2tCtbHitData->GetNRows() << endl;
               for(int i = 0; i< g2tCtbHitData->GetNRows(); i++, g2tCtbHitPtr++)
               {

                  StThreeVectorD crossPoint = StThreeVectorD(g2tCtbHitPtr->x[0],
                                      g2tCtbHitPtr->x[1], g2tCtbHitPtr->x[2]);  
                  int slatId = mTofCross->tofSlatCrossId(crossPoint, mTofDb);

// convert g2t hit to SlatMCSlat
                  if(slatId > 0)
                      MCSlatVec.push_back(tofMC.slatResponse(slatId, 
                                     g2tCtbHitPtr, g2tTrackPtr, mTofDb));
                  if(i%50 ==0) {
                      cout << "CTB Hit " <<  g2tCtbHitPtr->id << " "
                      << slatId << " " << g2tCtbHitPtr->track_p << " "
                      << g2tCtbHitPtr->tof << " " << g2tCtbHitPtr->de << " "
                      << g2tCtbHitPtr->ds << " " << g2tCtbHitPtr->s_track << " "
                      << g2tCtbHitPtr->x[0] <<" "<< g2tCtbHitPtr->x[1] << endl; 
                  }
               }
            }
*/
// work on g2t TOF hits
            StafStG2THit *g2tTofHitData = 
                                (StafStG2THit*) geantIter("g2t_tof_hit");
            if (g2tTofHitData) {
               StTofG2THit* g2tTofHitPtr = g2tTofHitData->GetTable();

               cout << "g2t hit # = " << g2tTofHitData->GetNRows() << endl;
               for(int i = 0; i< g2tTofHitData->GetNRows(); i++, g2tTofHitPtr++)
               {
                  StThreeVectorD crossPoint = StThreeVectorD(g2tTofHitPtr->x[0],
                                      g2tTofHitPtr->x[1], g2tTofHitPtr->x[2]);  
//                int slatId1 = mTofCross->tofSlatCrossId(crossPoint, mTofDb);
// get slatId using volume_id instead of crossPoint
                  int slatId = 
                     mTofCross->tofSlatCrossId(g2tTofHitPtr->volume_id, mTofDb);
//                  cout << " Volume and Point slatIds  = " 
//                       << slatId << " " << slatId1 << endl; 

// convert g2t hit to SlatMCSlat
                  if(slatId > 0)
                  MCSlatVec.push_back(tofMC.slatResponse(slatId, g2tTofHitPtr, 
                                                  g2tTrackPtr, mTofDb));
//                  cout << "TOF Hit " << g2tTofHitPtr->id << " "
//                     << slatId << " " << g2tTofHitPtr->track_p << " "
//                     << g2tTofHitPtr->tof << " " << g2tTofHitPtr->de << " "
//                     << g2tTofHitPtr->ds << " "<< g2tTofHitPtr->s_track << " "
//                     << g2tTofHitPtr->x[0] <<" "<< g2tTofHitPtr->x[1] <<endl; 
               }
            }

// build tofMC.mMCSlatVec from MCSlatVec which may have entries with same slatId
            slatTempVec = MCSlatVec;
            slatErasedVec = slatTempVec;

            while (slatTempVec.size() != 0) {
               unsigned short fastTdc;
               int nFired=0, accumNPhe=0;
               float accumDe=0.0, accumDs=0.0, fastTof=0.0;
               fastTof = slatTempIter->mcInfo().mTof;
               fastTdc = slatTempIter->tdc();

               slatTempIter=slatTempVec.begin();
               slatErasedIter=slatErasedVec.begin();

               while(slatErasedIter!= slatErasedVec.end()) {
                  if(slatTempIter->slatIndex() == slatErasedIter->slatIndex()) {
                     nFired++;
                     accumDe = accumDe + slatErasedIter->mcInfo().mDe;
                     accumDs = accumDs + slatErasedIter->mcInfo().mDs;
                     accumNPhe = accumNPhe + slatErasedIter->mcInfo().mNPhe;
                     fastTof = min(fastTof, slatErasedIter->mcInfo().mTof);
                     fastTdc = min(fastTdc, slatErasedIter->tdc());

                     slatErasedVec.erase(slatErasedIter);
                     slatErasedIter--;
                  }
                  slatErasedIter++;
               }

// modify contents of MCSlat with mult-hit and add it to tofMC.mMCSlatVec 
               StTofMCSlat MCSlat = *slatTempIter;
               MCSlat.setNHits(nFired);
               MCSlat.setNPhe(accumNPhe);
               MCSlat.setDe(accumDe);
               MCSlat.setDs(accumDs);
               MCSlat.setTof(fastTof);
               MCSlat.setTdc(fastTdc);
               tofMC.push_back(MCSlat);

               slatTempVec = slatErasedVec;
           
           } // end of while 

           cout << " Old and New MCSlatvec sizes = " << MCSlatVec.size()  
                << " " << tofMC.size()   << endl;

// fill mSlatCollection with tofMC 
           for (size_t ii = 0; ii < tofMC.size(); ii++) {
              StTofMCSlat* MCSlatPtr = new StTofMCSlat();
              *MCSlatPtr = tofMC.getSlat(ii);
              mSlatCollection->push_back(MCSlatPtr); 
           }
/*
// print out to check
           for(size_t i = 0; i < MCSlatVec.size(); i++) cout << MCSlatVec[i];
           cout << " **************************" << endl;
           for(size_t i = 0; i < tofMC.size(); i++) cout << tofMC.getSlat(i);
*/
         } // if xdf data exists
       }   // if mod = 999 for xdf data  

// Daq-reader
       else if (m_Mode == 1) {  /* TOF-DAQ-READER member */ }
       else {
          cout << "Wrong Mode = " << m_Mode << "! " <<
               " It should be either 1 (DAQ) or 2(MC)" << endl;
          return iMake;
       }
    }  // mSlatCollection not available

// mSlatCollection avalable
    else if (mSlatCollectionPresent == 1) {  
       if(mHitCollectionPresent == 1) {
          gMessMgr->Warning()
          << "** StEvent has TOF data. Nothing is done in StTofMaker!" << endm;
           return iMake;
       }

// get tofSlatVec from mTheTofCollection 
       else  {
//         slatVector tofSlatVec = mTheTofCollection->getTofSlats(); 
         StSPtrVecTofSlat tofSlatVec = mTheTofCollection->tofSlats(); 
         for (size_t ii = 0; ii < tofSlatVec.size(); ii++) {
              mSlatCollection->push_back(tofSlatVec[ii]); 
         }
       }
    }

// extrapolate StTracks to find hits on slats 

    float tofCylRadius = mTofDb->tofParam().r +
                         mTofDb->tofParam().counter_thickness;
/*
   eventSlatHitVec stores fired slats for an event. a slat could be stored 
   multiple times if it is crossed by more than 1 tracks. 
*/
    tofSlatHitVector tempVec, erasedVec, eventSlatHitVec;
    tofSlatHitVectorIter tempIter, erasedIter;
    eventSlatHitVec.clear();

// loop over nodes, using global tracks;
    StSPtrVecTrackNode& nodes = mEvent->trackNodes();
    cout << "StEvent Node # =" << nodes.size() << endl;
    for (size_t iNode = 0; iNode < nodes.size(); iNode++){
       tofSlatHitVector slatHitVec;
       slatHitVec.clear(); 
       mTrkDist->Fill(1.0);
       StTrack* aTrack = nodes[iNode]->track(global);

/* ---- samples to access various variables
       int         nVert = event-> numberOfPrimaryVertices();
       double      magField  = event->summary()->magneticField();
       StPtrVecHit hits = aTrack->detectorInfo()->hits(kTpcId);
       int         nHit = hits.size();
       float       px = track->geometry()->momentum().x();
*/
// extrapolate a track-helix to the TOF cylinder (its r = mean middle radius)
       StPhysicalHelixD aHelix = aTrack->geometry()->helix();
       pairD pairLength = aHelix.pathLength(tofCylRadius);

// check pairLengths from extrapolation.
       if(( pairLength.first > -500.0 && pairLength.first < 500.0) &&    
          (pairLength.second > -500.0 && pairLength.second < 500.0) ) {
          mTrkDist->Fill(2.0);
          mMom1->Fill(aTrack->geometry()->momentum().mag());
          mCur1->Fill(aTrack->geometry()->curvature());

// choose an appropriate path length from two solutions at the TOF cylinder
         double pathLengthToCyl;
         if(aHelix.at(pairLength.first)*aTrack->geometry()->momentum() >= 0)  
            pathLengthToCyl = pairLength.first; 
         else
            pathLengthToCyl = pairLength.second; 

         StThreeVectorD hitAtCyl = aHelix.at(pathLengthToCyl);
         mZ->Fill(fabs(hitAtCyl.z()));
         mPz->Fill(fabs(aTrack->geometry()->momentum().z()));

// z in the range of TOF cylinder
         if(fabs(hitAtCyl.z()) <= 232.02) {
           mTrkDist->Fill(3.0);

// pz > 100 MeV
           if (fabs(aTrack->geometry()->momentum().z()) >= 0.1) {
             mTrkDist->Fill(4.0);

// look for slats crossed by the track (mHitDist stays here temporarily)
             slatHitVec = 
             mTofCross->tofHelixToSlat(mHitDist, aHelix, pathLengthToCyl, mTofDb); 
             mHitPerTrk->Fill((float) slatHitVec.size()); 
             if(slatHitVec.size() == 0) {
               mMom3->Fill(aTrack->geometry()->momentum().mag());
               mCur3->Fill(aTrack->geometry()->curvature());
               mPz1->Fill(fabs(aTrack->geometry()->momentum().z()));
               mZ1->Fill(fabs(hitAtCyl.z()));

             }
           }
         }
       }
       else {                      // bad extrapolation
         mMom2->Fill(aTrack->geometry()->momentum().mag());
         mCur2->Fill(aTrack->geometry()->curvature());
      }

// accumulate fired slats to a vector for an event 
      for (size_t ii = 0; ii < slatHitVec.size(); ii++) { 
          slatHitVec[ii].trackIdVec.push_back(iNode);
          eventSlatHitVec.push_back(slatHitVec[ii]);
      }
    }  // end of loop over tracks in an event 

    tofSlatHitVector newSlatHitVec;
    StructSlatHit     slatHit;
    tempVec = eventSlatHitVec;
    erasedVec = tempVec;
/*
   build newSlatHitVec from eventSlatHitVec  
   note: 1, newSlatHitVec does not allow multiple apprearance of a slat.     
         2, newSlatHitVec has more info than tofSlatCollection to be built!
*/
    while (tempVec.size() != 0) {
         int nTracks = 0;
         StThreeVectorD position(0.0, 0.0, 0.0); 
         tempIter=tempVec.begin();
         erasedIter=erasedVec.begin();
         idVector trackIdVec;
         while(erasedIter!= erasedVec.end()) {
            if(tempIter->slatIndex == erasedIter->slatIndex) {
              trackIdVec.push_back(tempIter->trackIdVec.back());
              nTracks++;
              if(nTracks != 1) cout <<"dbl Id = " <<tempIter->slatIndex <<endl;
              position = position + erasedIter->hitPosition;
              erasedVec.erase(erasedIter);
              erasedIter--;
            }
            erasedIter++;
         }

// average position for a slat crossed by more thant 1 tracks
         position = position/nTracks;

// fill slatHit and add it to newSlatHitVec
         slatHit.slatIndex = tempIter->slatIndex;
         slatHit.hitPosition = position;
         slatHit.trackIdVec = trackIdVec;

         newSlatHitVec.push_back(slatHit);
         tempVec = erasedVec;
    }
//  
    cout << " Old and New eventSlatHitVec (whole TOF system) sizes = " 
         << eventSlatHitVec.size() << " " << newSlatHitVec.size()   << endl;

// build tofHitCollection from newSlatHitVec and mSlatCollection

    mHitCollection = new StTofHitCollection();

// nCheckHit is the numbers of slats, each of which is fired by simulated hits 
// and is traced by extrapolations of tracks.
 
    int nCheckHit = 0;

// check if MC data
    StTofMCSlat* pointer =
         dynamic_cast<StTofMCSlat*>(mSlatCollection->getSlat(0));
    if(pointer) {
      for (size_t ii = 0; ii < newSlatHitVec.size(); ii++) {
         for (size_t jj = 0; jj < mSlatCollection->size(); jj++) {
           StTofMCSlat* MCSlatPtr =
               dynamic_cast<StTofMCSlat*>(mSlatCollection->getSlat(jj));
            if (newSlatHitVec[ii].slatIndex ==MCSlatPtr->slatIndex()) {
               nCheckHit++;
               StTofMCHit *MCHitPtr = new StTofMCHit(); 

               MCHitPtr->setSlatIndex(MCSlatPtr->slatIndex());
               MCHitPtr->setTrkId(MCSlatPtr->mcInfo().mTrkId);
               MCHitPtr->setGId(MCSlatPtr->mcInfo().mGId);
               MCHitPtr->setFlightTime(MCSlatPtr->mcInfo().mTime);
               MCHitPtr->setPosition(newSlatHitVec[ii].hitPosition);
               MCHitPtr->setHardwarePosition(kTofId);
// setNumberOfMips is out temporarily for now (numberOfMips shows slatIndex!) 
               MCHitPtr->setNumberOfMips(MCSlatPtr->adc()/100);
               mHitCollection->push_back(MCHitPtr); 

// add TOF hits to StTracks
               for (size_t iNode = 0; iNode < nodes.size(); iNode++){
                  StTrack* aTrack = nodes[iNode]->track(global);
                  for(size_t kk = 0; kk< newSlatHitVec[ii].trackIdVec.size(); 
                                                                       kk++){
                     if(iNode == (size_t) newSlatHitVec[ii].trackIdVec[kk]) {

// addHit would update trackReferenceCount in hit (see StTrackDetectorInfo.cxx)
                        aTrack->detectorInfo()->addHit(MCHitPtr); 
//                      cout << *MCHitPtr;
                     }
                  }
               }
            }
         }
      }
    }

//  not MC data
    else { 
      for (size_t ii = 0; ii < newSlatHitVec.size(); ii++) {
         for (size_t jj = 0; jj < mSlatCollection->size(); jj++) {
             StTofSlat* slatPtr = mSlatCollection->getSlat(jj);
            if (newSlatHitVec[ii].slatIndex ==slatPtr->slatIndex()){
               nCheckHit++;
               StTofHit *hitPtr = new StTofHit(); 
               hitPtr->setSlatIndex(slatPtr->slatIndex());
               hitPtr->setFlightTime(0.1);
               hitPtr->setPosition(newSlatHitVec[ii].hitPosition);
               hitPtr->setHardwarePosition(kTofId);
               mHitCollection->push_back(hitPtr); 

// add TOF hits to tracks
               for (size_t iNode = 0; iNode < nodes.size(); iNode++){
                 StTrack* aTrack = nodes[iNode]->track(global);
                  for(size_t kk = 0; jj<newSlatHitVec[ii].trackIdVec.size(); 
                                                                       jj++){
                     if(iNode == (size_t) newSlatHitVec[ii].trackIdVec[kk]) {

// addHit would update trackReferenceCount in hit (see StTrackDetectorInfo.cxx)
                        aTrack->detectorInfo()->addHit(hitPtr); 
                     }
                  }
               }
            }
         } 
      }
    }
    cout << " Number of slats fired by hits and traced by Tracks  = " 
         << nCheckHit << endl;
    
// fill StEvent with mSlatCollection and mHitCollection 
    if(mEvent) this->fillStEvent();

//  if(mEvent) this->fillPidTraits();

    return iMake;
}
              
//_________________________________________________________________________

void StTofMaker::fillStEvent() {

    if(mTofCollectionPresent != 1) mTheTofCollection = new StTofCollection();

// fill mTheTofCollection with slats 
    if(mSlatCollectionPresent != 1) {
      cout << "size of mSlatCollection = " << mSlatCollection->size() << endl;
      for(size_t jj = 0; jj < mSlatCollection->size(); jj++) {
/*
         StTofMCSlat* p = 
                   dynamic_cast<StTofMCSlat*>(mSlatCollection->getSlat(jj));
         if(p)  cout << *p;
*/
         mTheTofCollection->addSlat(mSlatCollection->getSlat(jj));
      }
    }

// fill mTheTofCollection with hits
    cout << "size of mHitCollection = " << mHitCollection->size() << endl;
    for (size_t jj = 0; jj < mHitCollection->size(); jj ++) {
//       StTofMCHit* p = dynamic_cast<StTofMCHit*>(mHitCollection->getHit(jj));
//         if(p) cout << *p;

         mTheTofCollection->addHit(mHitCollection->getHit(jj));
    }

// fill mEvent
    mEvent->setTofCollection(mTheTofCollection);

// verify existence of tofCollection in StEvent (mEvent) 
    mTheTofCollection = 0;
    mTheTofCollection = mEvent->tofCollection();
    if(mTheTofCollection) {
            cout << "StEvent tofCollection Exists" << endl;
            mTofCollectionPresent = 1;
            if(mTheTofCollection->slatsPresent()) {
                cout << "StEvent TofSlatCollection Exists" << endl;
                mSlatCollectionPresent = 1;
            }
            else {
                cout << "** StEvent TofSlatCollection DOES NOT Exist" << endl;
            }
            if(mTheTofCollection->hitsPresent()) {
                cout << "StEvent TofHitCollection Exists" << endl;
                mSlatCollectionPresent = 1;
            }
            else {
                cout << "** StEvent TofHitCollection DOES NOT Exist" << endl;
            }
    }
    else {
            cout << "** StEvent tofCollection DOES NOT Exist" << endl;
            cout << "** StEvent TofSlatCollection DOES NOT Exist" << endl;
            cout << "** StEvent TofHitCollection DOES NOT Exist" << endl;
    }
}
/*
void StTofMaker::fillPidTraits() {
// verify if TOF hits have added to StTracks
    StSPtrVecTrackNode& nodes = mEvent->trackNodes();
    for (size_t iNode = 0; iNode < nodes.size(); iNode++){
       StSPtrVecTrackNode& nodes = mEvent->trackNodes();
       StTrack* aTrack = nodes[iNode]->track(global);
       StPtrVecHit checkHit =  aTrack->detectorInfo()->hits(kTofId);
       if(checkHit.size() != 0) 
           cout << "# of TPC hits & lastPoint= " 
                << aTrack->detectorInfo()->numberOfPoints(kTpcId) << " "
                << aTrack->detectorInfo()->lastPoint() << endl;

       for (size_t ii = 0; ii < checkHit.size(); ii++) {
          StPtrVecTrack gvec, pvec;
          gvec = checkHit[ii]->relatedTracks(mEvent->trackNodes(), global);
          pvec = checkHit[ii]->relatedTracks(mEvent->trackNodes(), primary);

          cout << "gvec and pvec sizes = " << gvec.size() << " " 
               << pvec.size() << " ";
               << " --Check Hit Id = " <<  " " 
               << (float) checkHit[ii]->charge()  << " " 
               << (int) checkHit[ii]->trackReferenceCount()  << " " 
               << (int )checkHit[ii]->detector() << endl; 

          if(checkHit[ii]->trackReferenceCount() == 1) {

// calculate pathlength required by tofPidTraits
             StPhysicalHelixD aHelix = aTrack->geometry()->helix();
             double pathLength = aHelix.pathLength(checkHit[ii]->position());
             if(pathLength > -500.0 && pathLength < 500.0) {  

// fill tofPidTraits, then add to StTrack
                StTofHit* p = dynamic_cast<StTofHit*>(checkHit[ii]);
                StTofPidTraits* tofPidTraits = 
                    new StTofPidTraits(kTofId, 
                    aTrack->detectorInfo()->numberOfPoints(kTpcId),
                    p->numberOfMips(), p->flightTime(), 0, pathLength,
                    (StThreeVectorD) aTrack->detectorInfo()->lastPoint());

                aTrack->addPidTraits(tofPidTraits);
             }
          } 
       }
    }

// check if tofPidTraits is loaded and if tofPidAlgorithm works
    for (size_t iNode = 0; iNode < nodes.size(); iNode++){
       StSPtrVecTrackNode& nodes = mEvent->trackNodes();
       StTrack* aTrack = nodes[iNode]->track(global);

       if(aTrack->pidTraits(kTofId).size() != 0) {
         StTofPidAlgorithm tofPidAlgorithm;
         const StParticleDefinition* parDef=aTrack->pidTraits(tofPidAlgorithm);

         cout << " particle = " << parDef->name() << endl;
         cout << " Algor tof = " << tofPidAlgorithm.traits()->tof() << " ";
         cout << " Algor len = "<< tofPidAlgorithm.traits()->pathLength()<<endl;
       }
    }
}
*/
//_____________________________________________________________________________

Int_t StTofMaker::Finish(){

  TFile theFile("myctf.root","RECREATE","mystudy");
  theFile.cd();

  mHitPerTrk->Write();
  mTrkDist->Write();
  mHitDist->Write();
  mMom1->Write();
  mMom2->Write();
  mMom3->Write();
  mPz->Write();
  mPz1->Write();
  mZ->Write();
  mZ1->Write();
  mCur1->Write();
  mCur2->Write();
  mCur3->Write();
  return kStOK;
}
