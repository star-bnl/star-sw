#ifndef StiTrackFinder_H
#define StiTrackFinder_H 1

#include "StiConstants.h"

#include "StiObjectFactoryInterface.h"

class StiToolkit;
class StiSeedFinder;
class StiKalmanTrackFactory;
class StiTrackFilter;
class StiTrackFitter;
class StiDetectorContainer;
class StiHitContainer;
class StiTrackContainer;
class StiDynamicTrackFilter;
//class StMagUtilities;

class StiTrackFinder 
{
public:
    
    //_c-tor/d-tor__________________________________________________
    StiTrackFinder(StiToolkit * userToolkit);
    virtual ~StiTrackFinder();
    
    //_action methods_______________________________________________
    virtual void findTracks()=0; 
    virtual void fitTracks()=0; 
		virtual void extendTracksToVertex(StiHit* vertex)=0;
		virtual void findNextTrack()=0;
		virtual void fitNextTrack()=0;
		virtual void findNextTrackSegment()=0;
		virtual void reset()=0;
    virtual bool isValid(bool debug=false) const = 0;
    

protected:

    // Local cache of pointers - none of the following are owned
		// by this class.
		StiToolkit                * toolkit;
    StiTrackFilter            * trackFilter;
    StiTrackFitter            * trackFitter;
    StiSeedFinder             * trackSeedFinder;
    StiObjectFactoryInterface<StiKalmanTrackNode> * trackNodeFactory;
		StiObjectFactoryInterface<StiKalmanTrack> * trackFactory;
    //StMagUtilities            * magField;
    StiDetectorContainer      * detectorContainer;
    StiHitContainer           * hitContainer;
    StiTrackContainer         * trackContainer;
};


#endif
