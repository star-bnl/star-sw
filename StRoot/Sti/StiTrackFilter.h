#ifndef StiTrackFilter_H
#define StiTrackFilter_H 1

#include <iostream.h>
#include <stdlib.h>

class StiTrack;

class StiTrackFilter 
{
    /** 
     * Base class defining a track filtering mechanism
     *
     * This class does not implement a particular filter but rather simply
     * define an interface for track filtering classes which should inherit from
     * it. 
     *
     * Usage: Derived classes should use the following methods to 
     * implement the functionality of the filter.
     * "setDefaults()"           shall be overloaded to set the default values of the filter.
     * "reset()"                 shall be overloaded to set the filter counters to zero
     * "accept(StiTrack * track) shall be overloaded to determine whether given
     *                           tracks pass the filter requirements.
     * "incrementAccepted"       shall be called to count accepted tracks
     * "incrementRejected"       shall be called to count rejected tracks
     * "getAnalyzedTrackCount()  shall be called to obtain the number of tracks analyzed
     *                           by the filter.
     * "getAcceptedTrackCount()  shall be called to obtain the number of tracks accepted
     *                           by the filter. 
     *
     * Recommandation:
     * In order to make full use of the interface, it is recommanded derived classes 
     * implementing the "accept" method should include the following code to perform
     * proper accounting of the track accpeted and analyzed.
     *
     * Example:
     * bool accept(StiTrack * track)
     * {
     *    bool accepted = false;
     *    // your code here to possibly change the "accepted" state variable
     *    if (accepted)
     *      incrementAccepted(); return accepted;
     *    else
     *      incrementRejected(); return accepted;
     **/
public:

    StiTrackFilter();
  
    //virtual void setDefaults();
    virtual bool accept(StiTrack * track)
    {
	return true;
    }

    bool filter(StiTrack * track)
    {
	analyzedTrackCount++;
	bool acc = accept(track);
	if (acc) acceptedTrackCount++;
	return acc;
    }

    void reset();
    
    void incrementAccepted()  
    {
	analyzedTrackCount++;
	acceptedTrackCount++;
    }
    void incrementRejected()  
    {
	analyzedTrackCount++;
    }

    int  getAnalyzedTrackCount();
    int  getAcceptedTrackCount();

protected:

    int analyzedTrackCount;
    int acceptedTrackCount;

};

#endif
