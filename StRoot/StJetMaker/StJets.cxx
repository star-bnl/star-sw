//////////////////////////////////////////////////////////////////////
//
// $Id: StJets.cxx,v 1.4 2004/09/20 23:15:51 mmiller Exp $
// $Log: StJets.cxx,v $
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

//StMuDst
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"

//StJetFinder
#include "StJetFinder/StProtoJet.h"

//StSpinMaker
#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StJets.h"

ClassImp(StJets)
ClassImp(TrackToJetIndex)

int* global_index;

StJets::StJets()
    : mJets( new TClonesArray("StJet",100)), mTrackToJetIndices( new TClonesArray("TrackToJetIndex",200)) 
{
    mEventId = mEventNumber = mRunId = mRunNumber = 0;
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
    cout << "Cleared the Jets" <<endl;
}

void StJets::addProtoJet(StProtoJet& pj)
{
    //jetIndex == number of jets + 1, i.e., where to insert
    int jetIndex = mJets->GetLast()+1;
    
    StProtoJet::FourVecList &trackList = pj.list(); // Get the tracks too.
    
    // We need to add up the charged tracks to get charge and nCell
    int nCell = 0;
    int charge = 0;
    
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
	    //cout <<"here's the t2j:\t"<<t2j<<endl;

	    new ( (*mTrackToJetIndices)[addAt]) TrackToJetIndex( t2j );

	    //((TrackToJetIndex*)(*mTrackToJetIndices)[addAt])->setTrackIndex(muTrackIndex);
	    //((TrackToJetIndex*)(*mTrackToJetIndices)[addAt])->setJetIndex(jetIndex);
	}
	if(track->particle())
	    if( track->charge() ) {  // If charge != 0, increment the number of cp
                nCell++;
	}
	if(track->particle())
	    charge += track->particle()->charge();
    }
    //add in the jet container
    new((*mJets)[jetIndex]) StJet( pj.e(), pj.px(), pj.py(), pj.pz(), nCell, charge );
}

void StJets::print()
{
    /*
      for(Int_t i = 0; i < numJets(); i++)
      printf("Jet#%d: Et=%6.3f  Eta=%6.3f  Phi=%6.3f  P=%6.3f  Pt=%6.3f  E=%6.3f  nCell= %4d\n",
      i,et(i),eta(i),phi(i),p(i),pt(i),e(i),nCell(i));
    */
}


const TLorentzVector* StJets::trackToJetIndex(int jetIndex, int trackIndex)
{
    int size = mTrackToJetIndices->GetLast()+1;
    TrackToJetIndex* t2j=0;

    //cout <<"\n search for jetIndex:\t"<<jetIndex<<"\ttrackIndex:\t"<<trackIndex<<endl;
    for (int i=0; i<size; ++i) {
	TrackToJetIndex* id = static_cast<TrackToJetIndex*>( (*mTrackToJetIndices)[i] );
	//cout <<"\tjet:\t"<<id->jetIndex()<<"\ttrack:\t"<<id->trackIndex()<<endl;
	if (id->jetIndex()==jetIndex && id->trackIndex()==trackIndex) {
	    t2j = id;
	    break;
	}
    }
    return t2j;
}

const TLorentzVector* StJets::trackToJetIndex(StMuDst* event, int jetIndex, const StMuTrack* track)
{
    int trackIndex = -1;
    TClonesArray& tracks = *(event->primaryTracks());
    int ntracks = tracks.GetLast()+1;
    for (int i=0; i<ntracks; ++i) {
	const StMuTrack* temp = static_cast<const StMuTrack*>( tracks[i] );
	if (temp == track) {
	    trackIndex = i;
	    break;
	}
    }
    return trackToJetIndex(jetIndex, trackIndex);
}

vector<int> StJets::jetBemcTowerIndices(int jetIndex)
{
    vector<int> vec;
    int size = mTrackToJetIndices->GetLast()+1;
        
    for (int i=0; i<size; ++i) {
	TrackToJetIndex* id = static_cast<TrackToJetIndex*>( (*mTrackToJetIndices)[i] );
	StDetectorId detId = id->detectorId();
	
	if (detId!=kBarrelEmcTowerId || id->jetIndex()!=jetIndex) continue;
	vec.push_back(id->trackIndex());
    }
    return vec;
}

//right now it's a linear search, even though the JetsToTrackIndices is ordered by jetIndex
StJets::TrackVec StJets::jetParticles(StMuDst* event, int jetIndex)
{
    TrackVec vec;
    int size = mTrackToJetIndices->GetLast()+1;

    TClonesArray& tracks = *( event->primaryTracks() );
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
	    //cout <<"mom_track:\t"<<track->momentum()<<endl;
	    //cout <<"mom_check:\t"<<id->Px()<<"\t"<<id->Py()<<"\t"<<id->Pz()<<endl;
	}
    }
    
    return vec;
}


bool StJets::inBounds(int i)
{
    return (i>0 && i<nJets());
}

/*
  StJet* StJets::jet(int i)
  {
  //this is readable, but fast, optimized compiler will take care of it
  TClonesArray& tj = *mJets;
  TObject* temp = tj[i];
  return ( inBounds(i)==true ) ? static_cast<StJet*>( temp ) : 0;
  }
*/

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
