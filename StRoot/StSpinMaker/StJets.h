//////////////////////////////////////////////////////////////////////
//
// $Id: StJets.h,v 1.1 2002/12/04 20:28:08 thenry Exp $
// $Log: StJets.h,v $
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

class StProtoJet;
class StppEvent;
class StJet;

class StJets : public TObject
{
public:
    typedef vector<StMuTrack*> TrackVec;

    StJets();
    virtual ~StJets();
    
    void Clear(bool clearTracks = false);
    void  print();

    ///add a jet to the container
    void addProtoJet(StProtoJet& pj);

    ///Access to the jets in this event. jets::size = jets()->GetLast()+1
    int nJets() {return mJets->GetLast()+1;}
    TClonesArray* jets() {return mJets;}

    ///Access to a container of the charged-tracks associated with a jet
    TrackVec jetParticles(StppEvent* event, int jetIndex);

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
    
private:
    bool inBounds(int);
    StJet* jet(int);
    
    TClonesArray* mJets;
    TClonesArray* mTrackToJetIndices;
    
    ClassDef(StJets,1)
};

//non-members ---------------------

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


#endif
