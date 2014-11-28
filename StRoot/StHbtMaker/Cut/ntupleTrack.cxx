/***************************************************************************
 *
 * $Id: 12 July 2000
 *
 * Author: Dominik Flierl, flierl@bnl.gov
 ***************************************************************************
 *
 * Description: 
 *    fill track information into an ntuple in order to determine the cuts
 *
 ***************************************************************************/
 

#include "StHbtMaker/Cut/ntupleTrack.h"

#ifdef __ROOT__
ClassImp(ntupleTrack)
//______________________________
ntupleTrack::ntupleTrack()
{
    // create tree
    mTree = new TTree("TrackTree","A Tree with all tracks in order to set the cuts");
    // create Branch
    mTree->Branch("track",&mtrack,"charge/I:nhits/I:dca/F:pt/F:p/F:px/F:py/F:pz/F:prapidity/F");            
}
//_____________________________
ntupleTrack::~ntupleTrack()
{
    // clean up
    delete mTree;
}
//________________________________
bool ntupleTrack::Pass(const StHbtTrack* track)
{
    // extract track info and fill tree    
    mtrack.charge  = track->Charge() ;
    mtrack.nhits   = track->NHits() ;
    mtrack.dca     = track->DCAz() ;
    mtrack.pt      = track->P().perp() ;
    mtrack.p       = track->P().magnitude() ;
    mtrack.px      = track->P().x() ;
    mtrack.py      = track->P().y() ;
    mtrack.pz      = track->P().z() ;
    mtrack.prapidity = track->P().pseudoRapidity() ;
    mTree->Fill();
    return true;
}
//________________________________
StHbtString ntupleTrack::Report()
{
    StHbtString t;
    char Ctemp[100];
    sprintf(Ctemp,"No actual track cut applied ! This cut class just fills a tree.\n");
    t+=Ctemp;
    return t;
};

#endif // ifdef ROOT

