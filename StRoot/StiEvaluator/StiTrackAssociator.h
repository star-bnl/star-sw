//StiTrackAssociator.h
//M.L. Miller (Yale Software)
//11/01

#ifndef StiTrackAssociator_HH
#define StiTrackAssociator_HH

#include <map>
using namespace std;

//We need this for struct trackPing
#include "StAssociationMaker/StAssociationMaker.h"
#include "StAssociationMaker/StTrackPairInfo.hh"

class StiHit;
class StHit;
class StTpcHit;
class StSvtHit;
class StiKalmanTrack;

class StiTrackAssociator
{
public:

    //Must pass valid pointer to constructor
    StiTrackAssociator( StAssociationMaker* );
    virtual ~StiTrackAssociator();
 
    ///Encapsulate the 3-way association (ITTF <--> MC <--> Global)
    typedef pair <StTrackPairInfo*, trackPing> AssocPair;
    
    ///The 3-way association for a given ITTF track
    AssocPair associate(StiKalmanTrack*);
    
protected:
    StiTrackAssociator(); //Not implemented
    
protected:
    void loopOnHits(const StiKalmanTrack*);
    void associate(const StiHit*);
    void associate(const StTpcHit*);
    void associate(const StSvtHit*);
    trackPing* chooseBestAssociation();
    StTrackPairInfo* findBestTrackPair(trackPing* );


protected:
    StAssociationMaker* mAssociationMaker;
    
    typedef map<StMcTrack*, trackPing> TrackPingMap;
    TrackPingMap mMap;
};

#endif
