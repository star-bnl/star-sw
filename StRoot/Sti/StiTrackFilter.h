#ifndef StiTrackFilter_H
#define StiTrackFilter_H 1

#include <iostream.h>
#include <stdlib.h>
#include "StiObjectFactoryInterface.h"

class StiTrack;
/*! Base class defining a track filtering mechanism
<p>
Abstract base class defining a track filtering mechanism. This class cannot be 
instantiated given it features pure virtual methods. As such it provides the
basic elements of a track filter without actually doing filtering itself - the
method <b>bool accept( StiTrack * track)</b> which does the filtering is 
pure virtual, i.e. must be implemented in a derived class to satisfy the 
specific needs of an application.
<p>
The class features two protected data members 
<b>analyzedTrackCount</b> and <b>acceptedTrackCount</b> corresponding respectively
to the number of tracks analyzed and accepted by this filter. The values can 
reset to zero by a call to the <b>void reset()</b> method. They can be accessed 
with the <b>int  getAnalyzedTrackCount()</b> and <b>int  getAcceptedTrackCount()</b>
accessor methods respectively. Note that these two counters are not incremented 
by the <b>bool accept(StiTrack * track)</b> method. Users wishing to the use 
these variables for accounting should use the inlined method <b>filter(StiTrack * track)</b>
which internally calls the "accept" method and also increments <b>analyzedTrackCount</b>
for all tracks analyzed, and <b>acceptedTrackCount</b> for tracks for which 
"accept" return true.
<p>
Note: The <b>accept</b> method is declared "const" to emphasize that it does 
not increment counters but only returns a bool value representative of the
acceptability of this track.
*/
class StiTrackFilter 
{
public:

    StiTrackFilter();
  
    virtual void initialize();
    virtual bool accept(StiTrack * track) const =0;

    bool filter(StiTrack * track);
    void reset();
    
    int  getAnalyzedTrackCount() const;
    int  getAcceptedTrackCount() const;

protected:
    
    /// Number of tracks analyzed since last reset.
    int analyzedTrackCount;
    /// Number of tracks found acceptable since last reset.
    int acceptedTrackCount;

};

/*! Filter the given track and return true it is found acceptable.
<p>
Determine whether the given track is found acceptable
by a call to the <b>accept</b> method. Increments 
<b>analyzedTrackCount</b> by one for all tracks analyzed. 
Increments <b>acceptedTrackCount</b> by one if 
<b>accept</b> returns true.
*/
inline bool StiTrackFilter::filter(StiTrack * track)
{
  analyzedTrackCount++;
  bool acc = accept(track);
  if (acc) acceptedTrackCount++;
  return acc;
}

/// Reset counters to zero
inline void StiTrackFilter::reset()
{
  analyzedTrackCount = 0;
  acceptedTrackCount = 0;
}

/// Returns the number of tracks analyzed by this filter since last reset.
inline int StiTrackFilter::getAnalyzedTrackCount() const
{
  return analyzedTrackCount;
}

/// Returns the number of tracks accpeted by this filter since last reset.
inline int StiTrackFilter::getAcceptedTrackCount() const
{
  return acceptedTrackCount;
}

/*! StiTrackFilter factory
 */
class StiTrackFilterFactory : public StiObjectFactoryInterface<StiTrackFilter>
{
public:
    ///This is the only constructor available.
    StiTrackFilterFactory(const string& newName, 
			  int original=-1, int 
			  incremental=-1, 
			  int maxInc=-1);
    ///Default destructor.
    virtual ~StiTrackFilterFactory();
    
protected:
    ///Return a pointer to a new StiTrackFilter object on the heap.
    virtual void * makeNewObject() const
      {
	return 0;
      }
    
private:
    StiTrackFilterFactory(); //Not implemented
};

#endif
