//////////////////////////////////////////////////////////////////////
//
// $Id: StJets.h,v 1.5 2003/09/02 17:59:01 perev Exp $
// $Log: StJets.h,v $
// Revision 1.5  2003/09/02 17:59:01  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.4  2003/05/20 20:22:45  thenry
// Moved body of jetTrackIndices to cxx.
//
// Revision 1.3  2003/05/15 17:48:27  thenry
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
// Revision 1.2  2003/04/01 23:45:04  thenry
// Added jet track accessor functions:
// numTracks, tracksPt, tracksPhi, tracksEta
//
// Revision 1.1  2002/12/04 20:28:08  thenry
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
// Revision 1.0  2002/02/11 20:30:48  Henry
// Adapted from StJet.h by Akio Ogawa
//
//////////////////////////////////////////////////////////////////////
//
// StJets
//
// Branch for multiple jets and an array of track->jet indices
//
//////////////////////////////////////////////////////////////////////
#ifndef StJets_h
#define StJets_h

#include <vector>
using std::vector;
#include <cmath>
#include "TObject.h"
#include "TClonesArray.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

class StProtoJet;
class StppEvent;
class StJet;

class TrackToJetIndex : public TObject
{
public:
    TrackToJetIndex(int ji=-1, int ti=-1) : mJetIndex(ji), mTrackIndex(ti) {};
    virtual ~TrackToJetIndex() {};
    
    void setJetIndex(int n) {mJetIndex=n;}
    int jetIndex() const {return mJetIndex;}
    
    void setTrackIndex(int n) {mTrackIndex=n;}
    int trackIndex() const {return mTrackIndex;}
    
private:
    int mJetIndex;
    int mTrackIndex;
    
    ClassDef(TrackToJetIndex,1)
};

class StJets : public TObject
{
public:
    typedef vector<StMuTrack*> TrackVec;

    StJets();
    virtual ~StJets();
    
    void Clear(bool clearTracks = false);
    void Clear(const char *opt){TObject::Clear(opt);}//WarnOff
    void  print();

    ///add a jet to the container
    void addProtoJet(StProtoJet& pj);

    ///Access to the jets in this event. jets::size = jets()->GetLast()+1
    int nJets() {return mJets->GetLast()+1;}
    TClonesArray* jets() {return mJets;}

    ///Access to a container of the charged-tracks associated with a jet
    TrackVec jetParticles(StppEvent* event, int jetIndex);

    vector<int> jetTrackIndices(int jetIndex);

public:
    ///User Interface as per Thomas H's request.  Access jet kinematics based on index:
    
    double e(int) ;
    double et(int) ;
    double p(int) ;
    double pt(int) ;
    double phi(int) ;
    double eta(int) ;
    int nCell(int) ;
    int charge(int) ;

    int numTracks(int i, StppEvent* event) 
	{ return jetParticles(event, i).size(); };
    double tracksPt(int i, StppEvent* event, int index) 
	{ return jetParticles(event, i)[index]->pt(); };
    double tracksPhi(int i, StppEvent* event, int index) 
	{ return jetParticles(event, i)[index]->phi(); };
    double tracksEta(int i, StppEvent* event, int index) 
	{ return jetParticles(event, i)[index]->eta(); };

    
private:
    bool inBounds(int);
    StJet* jet(int);
    
    TClonesArray* mJets;
    TClonesArray* mTrackToJetIndices;
    
    ClassDef(StJets,1)
};

//non-members ---------------------

#endif
