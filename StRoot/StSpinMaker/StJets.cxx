//////////////////////////////////////////////////////////////////////
//
// $Id: StJets.cxx,v 1.2 2003/05/09 19:28:13 thenry Exp $
// $Log: StJets.cxx,v $
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
#include <iostream>

//StJetFinder
#include "StJetFinder/StProtoJet.h"

//StSpinMaker
#include "StppEvent.h"
#include "StMuTrackFourVec.h"
#include "StJet.h"
#include "StJets.h"

ClassImp(StJets)
ClassImp(TrackToJetIndex)

StJets::StJets()
    : mJets( new TClonesArray("StJet",100)), mTrackToJetIndices( new TClonesArray("TrackToJetIndex",200)) 
{
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
	if (muTrackIndex >=0) {
	    //add to trackToJetIndices
	    int addAt = mTrackToJetIndices->GetLast()+1;
	    new ( (*mTrackToJetIndices)[addAt]) TrackToJetIndex( jetIndex, muTrackIndex);
	}
	if(track->particle())
	    if( track->charge() ) {  // If charge != 0, increment the number of cp
                nCell++;
	}
	if(track->particle())
	    charge += track->particle()->charge();
	
	//add in the jet container
	new((*mJets)[jetIndex]) StJet( pj.e(), pj.px(), pj.py(), pj.pz(), nCell, charge );
    }
}

void StJets::print()
{
    /*
      for(Int_t i = 0; i < numJets(); i++)
      printf("Jet#%d: Et=%6.3f  Eta=%6.3f  Phi=%6.3f  P=%6.3f  Pt=%6.3f  E=%6.3f  nCell= %4d\n",
      i,et(i),eta(i),phi(i),p(i),pt(i),e(i),nCell(i));
    */
}

//right now it's a linear search, even though the JetsToTrackIndices is ordered by jetIndex
StJets::TrackVec StJets::jetParticles(StppEvent* event, int jetIndex)
{
    TrackVec vec;
    int size = mTrackToJetIndices->GetLast()+1;
    
    TClonesArray& tracks = *(event->tracks);
    
    for (int i=0; i<size; ++i) {
	TrackToJetIndex* id = static_cast<TrackToJetIndex*>( (*mTrackToJetIndices)[i] );
	int trackIndex = id->trackIndex();
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

StJet* StJets::jet(int i) 
{
    //this is readable, but fast, optimized compiler will take care of it
    TClonesArray& tj = *mJets;
    TObject* temp = tj[i];
    return ( inBounds(i)==true ) ? static_cast<StJet*>( temp ) : 0;
}

double StJets::e(int i) 
{
    StJet* j = jet(i);
    return (j) ? j->E() : -999.;
}

double StJets::et(int i) 
{
    StJet* j = jet(i);
    return (j) ? j->et() : -999.;
}

double StJets::p(int i) 
{
    StJet* j = jet(i);
    return (j) ? j->P() : -999.;
}

double StJets::pt(int i) 
{
    StJet* j = jet(i);
    return (j) ? j->Pt() : -999.;
}

double StJets::phi(int i)
{
    StJet* j = jet(i);
    return (j) ? j->Phi() : -999.;
}

double StJets::eta(int i) 
{
    StJet* j = jet(i);
    return (j) ? j->Eta() : -999.;
}

int StJets::nCell(int i) 
{
    StJet* j = jet(i);
    return (j) ? j->nCell : -999;
}

int StJets::charge(int i) 
{
    StJet* j = jet(i);
    return (j) ? j->charge : -999;
}
