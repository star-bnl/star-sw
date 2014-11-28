/***************************************************************************
 *
 * $Id: 12 July 2000
 *
 * Author: Dominik Flierl, flierl@bnl.gov
 ***************************************************************************
 *
 * Description: 
 *    fill event information into an ntuple in order to determine the cuts
 *
 ***************************************************************************/
 

#include "StHbtMaker/Cut/ntupleEvent.h"

#ifdef __ROOT__
ClassImp(ntupleEvent)

//______________________________
ntupleEvent::ntupleEvent()
{
    // create tree
    mTree = new TTree("EventTree","A Tree with all Event in order to set the cuts");
    // create Branch
    mTree->Branch("track",&mEvent,"ctbMult/I:numOfTpcHits/I:numOfTracks/I:numOfGoodTracks/I:vertexZ/F:vertexX/F:vertexY/F");            
}
//_____________________________
ntupleEvent::~ntupleEvent()
{
    // clean up
    delete mTree;
}
//________________________________
bool ntupleEvent::Pass(const StHbtEvent* event)
{
    // extract event info and fill tree    
    mEvent.ctbMult         = event->CtbMult() ;
    mEvent.numOfTpcHits    = event->NumberOfTpcHits() ;
    mEvent.numOfTracks     = event->NumberOfTracks() ;
    mEvent.numOfGoodTracks = event->NumberOfGoodTracks() ;
    mEvent.vertexZ         = event->PrimVertPos().z() ;
    mEvent.vertexX         = event->PrimVertPos().x() ;
    mEvent.vertexY         = event->PrimVertPos().y() ;
    mTree->Fill();
    return true;
}
//________________________________
StHbtString ntupleEvent::Report()
{
    StHbtString t;
    char Ctemp[100];
    sprintf(Ctemp,"No actual event cut applied ! This cut class just fills a tree.\n");
    t+=Ctemp;
    return t;
};

#endif // ifdef ROOT

