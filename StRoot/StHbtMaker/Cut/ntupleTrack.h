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

#ifndef ntupleTrack_hh
#define ntupleTrack_hh

#include "StHbtMaker/Infrastructure/StHbtTFile.hh"
#include "StHbtMaker/Base/StHbtTrackCut.h"
#ifdef __ROOT__

class ntupleTrack : public StHbtTrackCut
{
    // define ntuple content in a nested struct
    struct mTrack_t 
    {
	int charge;
	int nhits;
	float dca;
	float pt;
	float p;
	float px;
	float py;
	float pz;
	float prapidity;
    };

 private:   
    // my tree
    StHbtTree* mTree ;
    // my tree consists of mtracks
    mTrack_t mtrack ;
    
 public:
    ntupleTrack();
    ~ntupleTrack();
    
    // called for every track, returns in this case always true
    bool Pass(const StHbtTrack*);
    
    // this one has to be provided
    StHbtString Report(); 
    
    // access the tree from 
    StHbtTree* GetNtupleTrack() { return mTree; } ;
    
    ClassDef(ntupleTrack, 0)
};


#endif // ifdeff ROOT
#endif // ifdeff ntupleTrack_hh
