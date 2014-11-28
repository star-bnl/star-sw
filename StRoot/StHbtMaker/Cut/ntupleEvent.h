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
#ifndef ntupleEvent_hh
#define ntupleEvent_hh

#ifdef __ROOT__
#include "StHbtMaker/Infrastructure/StHbtTFile.hh"
#include "StHbtMaker/Base/StHbtEventCut.h"

class ntupleEvent : public StHbtEventCut {
     // define ntuple content in a nested struct
     struct mEvent_t 
     {
	 int ctbMult;
	 int numOfTpcHits;
	 int numOfTracks;
	 int numOfGoodTracks;
	 float vertexZ;
	 float vertexX;
	 float vertexY;
     };

public:
     ntupleEvent() ;
     ~ntupleEvent() ;
     
     // this one has to be provided
     StHbtString Report() ;

     // called for every event, returns in this case always true
     bool Pass(const StHbtEvent*);

     // access the tree from 
     StHbtTree* getNtupleEvent() { return mTree; } ;

private:
     // my tree
     StHbtTree* mTree ;
     // my tree consists of mEvents
     mEvent_t mEvent ;
   
     ClassDef(ntupleEvent, 0)
} ;

#endif //_ROOT_
#endif // ntupleEvent_hh
