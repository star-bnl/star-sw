//////////////////////////////////////////////////////////////////////
//
// $Id: StJets.cxx,v 1.14 2008/03/24 22:43:18 tai Exp $
// $Log: StJets.cxx,v $
// Revision 1.14  2008/03/24 22:43:18  tai
// added particles_() to make test easier.
//
// Revision 1.13  2008/01/28 03:37:25  staszak
// A number of updates: i) Murad's use2006cuts function which extends tracks to SMD, and includes DCA cuts, ii) emulated L2 results now included and data L2 results restructured, iii) StJet zVertex and detEta now filled
//
// Revision 1.12  2008/01/23 20:18:38  staszak
// Courtesy of Adam - changes to use the STAR Logger, and compression added to output jet trees
//
// Revision 1.11  2007/05/17 14:33:22  mmiller
// Added Murad's dca update.
//
// Revision 1.10  2007/01/17 16:43:46  mmiller
// Added StMuTrack info on track charge, dedx, and hit information to StJets.h.  Updated exampleFastJetAna() accordingly.
//
// Revision 1.9  2006/03/06 20:03:06  mmiller
// Added extra protection agains events with SumEmcEnergy>200 GeV (flag as corrupt, return w/o jet finding).  Also added nDylanPoints() and sumEmcE() methods to StJets.
//
// Revision 1.8  2005/08/19 21:34:14  jeromel
// TClonesArray to TObjArray safe change
//
// Revision 1.7  2005/03/23 14:59:20  mmiller
// Update to add PythiaAssociator, correct EMC simulation path
//
// Revision 1.6  2005/01/27 18:39:03  mmiller
// Added some extra accessors to StJet object to keep track of Et from TPC, BTOW, ETOW, etc.
//
// Revision 1.5  2004/09/22 15:46:21  mmiller
// Added a double check to verify that jet 4p is equal to the vector sum of
// the particles 4-p.  Removed troublesome access methods to StJets.  See
// StJetReader::exampleEventAna() for access to jet-particles.
//
// Revision 1.4  2004/09/20 23:15:51  mmiller
// Fixed bug in retreiving emc towers for jet, introduced
// TrackToJetIndex inherits from TLorentzVector now.  See StJetReader::exampleAna
// for example of how to retreive the corrected 4-momenta used for barrel towers.
//
// Revision 1.3  2004/09/14 17:27:15  mmiller
// Fixed bug (lack of StFourPMaker::Clear()).
//
// Revision 1.2  2004/09/10 18:13:53  mmiller
// Two fixes:
// 1) add StDetectorId to the TTree to allow sorting of jet particles into
// StMuTrack and BemcTowers.  See StJetReader::exampleEventAna() for usage
//
// 2) removed a continue line in StJetMaker::Make that created a non-synch between
// the jet tree and the MuDst
//
// Revision 1.1  2004/07/08 15:41:04  mmiller
// First release of StJetMaker.  Mike's commit of full clean of StJetFinder, StJetMaker, and StSpinMaker.  builds and runs in dev.
//
// Revision 1.8  2004/05/06 22:55:27  thenry
// This works better.
//
// Revision 1.7  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.6  2003/05/20 20:46:04  thenry
// Accidentally left debug output line in last commit.
//
// Revision 1.5  2003/05/20 20:22:44  thenry
// Moved body of jetTrackIndices to cxx.
//
// Revision 1.4  2003/05/20 19:17:42  thenry
// Fixed problem with jet value accessor functions (always returned -999. fixed),
// now return useful values.
//
// Revision 1.3  2003/05/15 17:48:26  thenry
// Previous versions of StJets expected only primary TPC tracks to be used by
// the jet maker.  That changed with the introduction of EMC points.
// Therefore, a bug existed in jetParticles, because this function
// assumed that all the TrackToJetIndices were valid primary TPC track indices.
// This bug has been fixed, so that if the TrackToJetIndex is greater than
// the number of primary tracks, that index is skipped in the construction
// of the StJets::TrackVec.  Therefore, the StJets::jetParticles function NOW
// does exactly what it did before, completely ignoring EMC Points, even when
// they contribute to the jet.
//
// In addition, a new function was added: jetTrackIndices(), which returns a
// vector of integers corresponding to TPC track indices with the addition of
// (EMC Point index + number TPC primary tracks)).  This function then allows
// us to determine which tracks and which points (their indexes at least) are
// part of each jet, even if we do not have a correctly filled StppEvent*.
//
// Revision 1.2  2003/05/09 19:28:13  thenry
// No changes.
//
// Revision 1.1  2002/12/04 20:28:07  thenry
// StppuDstMaker was modified to allow multiple jet analysis modules to be
// run simultaneosly with various parameters while the Maker loads the events
// and analyses them.  Four different jet analyzers exist:
//
// Konstanin's Analyzers:
//     Kt type: StppKonstKtJetAnalyzer
//     Cone type: StppKonstConeJetAnalyzer
//
// Mike's Analyzers:
//     Kt type: StppMikeKtJetAnalyzer
//     Cone type: StppMikeConeJetAnalyzer
//
// These modules all require the StJetFinder modules.
//
//
// Revision 1.0  2002/09/05 Thomas Henry adapted from Akio Ogawa
//
//////////////////////////////////////////////////////////////////////

//std
#include "Stiostream.h"

#include "StMessMgr.h"

//StMuDst
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"

//StJetFinder
#include "StJetFinder/StProtoJet.h"

//useful for extrapolations
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StFourPMakers/StMuEmcPosition.h"

//StSpinMaker
#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StJets.h"

ClassImp(StJets)
ClassImp(TrackToJetIndex)

int* global_index;

TrackToJetIndex::TrackToJetIndex(int ji, int ti, StDetectorId id) 
: mJetIndex(ji), mTrackIndex(ti) , mDetId(id) , mCharge(0), mNhits(0), mNhitsPoss(0), mNhitsDedx(0), mNhitsFit(0), mNsigmaPion(0.), mTdca(0.), mTdcaxy(0.)
{
}

StJets::StJets()
: mJets( new TClonesArray("StJet",100)), mTrackToJetIndices( new TClonesArray("TrackToJetIndex",200)) 
{
    mEventId = mEventNumber = mRunId = mRunNumber = 0;
    mDylanPoints = 0;
    mSumEmcE = 0.;
	
}

StJets::~StJets()
{
    mJets->Delete();
    delete mJets;
    mJets = 0;
	
    mTrackToJetIndices->Delete();
    delete mTrackToJetIndices;
    mTrackToJetIndices = 0;
}

void StJets::Clear(bool clearAll)
{
    mJets->Clear();
    mTrackToJetIndices->Clear();
    mDylanPoints = 0;
    mSumEmcE = 0.;
    LOG_DEBUG << "Cleared the Jets" <<endm;
}

void StJets::addProtoJet(StProtoJet& pj, const StMuDst* muDst)
{

  StMuEmcPosition*  mMuPosition = new StMuEmcPosition();

  //jetIndex == number of jets + 1, i.e., where to insert
  int jetIndex = mJets->GetLast()+1;
  
  StProtoJet::FourVecList &trackList = pj.list(); // Get the tracks too.
	
  //Make it here and update info as we go through tracks:
  StJet tempJet( pj.e(), pj.px(), pj.py(), pj.pz(), 0, 0 );
  tempJet.jetEt = pj.eT();
  tempJet.jetPt = tempJet.Pt();
  tempJet.jetEta = tempJet.Eta();
  tempJet.jetPhi = tempJet.Phi();
  
  StMuEvent* event = muDst->event();
  StThreeVectorF vPos = event->primaryVertexPosition();
  tempJet.zVertex = vPos.z();

  for(StProtoJet::FourVecList::iterator it2=trackList.begin(); it2!=trackList.end(); ++it2)  {
    StMuTrackFourVec *track = dynamic_cast<StMuTrackFourVec*>(*it2);
    if (!track) {
      cout <<"StJets::addProtoJet(). ERROR:\tcast to StMuTrackFourVecFailed.  no action"<<endl;
      return;
    }
    int muTrackIndex = track->getIndex();
    if (muTrackIndex <0) {
      cout <<"Error, muTrackIndex<0. abort()"<<endl;
      abort();
    }
    else {
      
      //cout <<"here's the track:\t"<<*track<<endl;
      
      //add to trackToJetIndices
      int addAt = mTrackToJetIndices->GetLast()+1;
      TrackToJetIndex t2j( jetIndex, muTrackIndex, track->detectorId() );
      t2j.SetPxPyPzE(track->px(), track->py(), track->pz(), track->e() );
      
      //and cache some properties if it really came from a StMuTrack:
      StMuTrack* muTrack = track->particle();
      if (muTrack) {  //this will fail for calorimeter towers ;)

	//----------------------------------------------
	double bField = 0.5; //to put it in Tesla
	double rad=238.6;//geom->Radius()+5.;
	StThreeVectorD momentumAt,positionAt;
	bool tok = mMuPosition->trackOnEmc(&positionAt, &momentumAt,
					   muTrack, bField, rad );
	t2j.setCharge( muTrack->charge() );
	t2j.setNhits( muTrack->nHits() );
	t2j.setNhitsPoss( muTrack->nHitsPoss() );
	t2j.setNhitsDedx( muTrack->nHitsDedx() );
	t2j.setNhitsFit( muTrack->nHitsFit() );
	t2j.setNsigmaPion( muTrack->nSigmaPion() );
	t2j.setTdca ( muTrack->dcaGlobal().mag() ); //jan 27, 2007
	t2j.setTdcaz ( muTrack->dcaZ() ); //jan 27, 2007
	t2j.setTdcaxy ( muTrack->dcaD() ); //jan 27, 2007
	t2j.setetaext ( positionAt.pseudoRapidity() );
	t2j.setphiext ( positionAt.phi() );
      }
     
      //cout <<"here's the t2j:\t"<<t2j<<endl;
      
      new ( (*mTrackToJetIndices)[addAt]) TrackToJetIndex( t2j );
      
      //ok, get track/tower properties here:
      StDetectorId mDetId = track->detectorId();
      if (mDetId==kTpcId) {
	tempJet.nTracks++;
	tempJet.tpcEtSum += track->eT();
      }
      else if (mDetId==kBarrelEmcTowerId) {
	tempJet.nBtowers++;
	tempJet.btowEtSum += track->eT();
      }
      else if (mDetId==kEndcapEmcTowerId) {
	tempJet.nEtowers++;
	tempJet.etowEtSum += track->eT();
      }
    }
  }
  //add in the jet container
  new((*mJets)[jetIndex]) StJet( tempJet );
}

vector<TrackToJetIndex*> StJets::particles(int jetIndex)
{
    int size = mTrackToJetIndices->GetLast()+1;
    vector<TrackToJetIndex*> vec;
    
    for (int i=0; i<size; ++i) {
		TrackToJetIndex* id = static_cast<TrackToJetIndex*>( (*mTrackToJetIndices)[i] );
		if (id->jetIndex()==jetIndex) {
			vec.push_back(id);
		}
    }
    return vec;
}

TObjArray StJets::particles_(int jetIndex)
{
  TObjArray ret;

  vector<TrackToJetIndex*> vec = particles(jetIndex);
  for (vector<TrackToJetIndex*>::iterator iter = vec.begin(); iter != vec.end(); ++iter)
    ret.Add(*iter);

  return ret;
} 

//right now it's a linear search, even though the JetsToTrackIndices is ordered by jetIndex
StJets::TrackVec StJets::jetParticles(StMuDst* event, int jetIndex)
{
    TrackVec vec;
    int size = mTrackToJetIndices->GetLast()+1;
	
    TObjArray& tracks = *( event->primaryTracks() );
    Int_t maxNumTracks = tracks.GetLast()+1;
    
    for (int i=0; i<size; ++i) {
		TrackToJetIndex* id = static_cast<TrackToJetIndex*>( (*mTrackToJetIndices)[i] );
		int trackIndex = id->trackIndex();
		StDetectorId detId = id->detectorId();
		
		if (detId != kTpcId) continue; 
		
		if (trackIndex >= maxNumTracks) { //this should never happen!
			cout <<"StJets::jetParticles() ERROR:\tid==kTpcId but index out of bounds.  abort()"<<endl;
			cout <<"index\t"<<trackIndex<<"\tmaxNumTracks:\t"<<maxNumTracks<<endl;
			abort();
		}
		if (id->jetIndex() == jetIndex ) {
			StMuTrack* track = static_cast<StMuTrack*>( tracks[trackIndex] );
			vec.push_back( track );
		}
    }
    
    return vec;
}


bool StJets::inBounds(int i)
{
    return (i>0 && i<nJets());
}

double StJets::e(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->E() : -999.;
}

double StJets::et(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->et() : -999.;
}

double StJets::p(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->P() : -999.;
}

double StJets::pt(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->Pt() : -999.;
}

double StJets::phi(int i)
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->Phi() : -999.;
}

double StJets::eta(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->Eta() : -999.;
}

int StJets::nCell(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->nCell : -999;
}

int StJets::charge(int i) 
{
    StJet* j = dynamic_cast<StJet*>(mJets->UncheckedAt(i));
    return (j) ? j->charge : -999;
}

void StJets::setMuDst(const StMuDst* muDst)
{
    assert(muDst);
    StMuEvent* ev = muDst->event();
    
    mEventId = ev->eventId();
    mEventNumber = ev->eventNumber();
    mRunId = ev->runId();
    mRunNumber = ev->runNumber(); 
}

bool StJets::isSameEvent(const StMuDst* muDst)
{
    assert(muDst);
    StMuEvent* ev = muDst->event();
	
    //cout <<"\n\n TEST!!!\t"<<mEventId<<"\t"<<mEventNumber<<"\t"<<mRunId<<"\t"<<mRunNumber<<endl;
	
    
    return mEventId == ev->eventId()
		&& mEventNumber == ev->eventNumber()
		&& mRunId == ev->runId()
		&& mRunNumber == ev->runNumber();
}
