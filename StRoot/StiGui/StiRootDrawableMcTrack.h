#ifndef StiRootDrawableMcTrack_HH
#define StiRootDrawableMcTrack_HH 1

#include "Sti/StiMcTrack.h"
#include "Sti/StiObjectFactoryInterface.h"
#include "StiGui/StiRootDrawableTrack.h"

/*! \class StiRootDrawableMcTrack
  Work class used to display Monte Carlo tracks with ROOT.
  Instances of this class hold a pointer to the actual track.
  \author Claude A Pruneau
*/
class StiRootDrawableMcTrack : public StiMcTrack, public StiRootDrawableTrack
{
public:
    StiRootDrawableMcTrack();
    virtual ~StiRootDrawableMcTrack();
    virtual void fillHitsForDrawing();
    virtual void reset();
 protected:
    void getNewState();
};

/*! StiRootDrawableMcTrack factory
 */
class StiRootDrawableMcTrackFactory : public StiMcTrackFactory
{
public:
    ///This is the only constructor available.
    StiRootDrawableMcTrackFactory(const string& newName, 
		      int original=-1, int 
		      incremental=-1, 
		      int maxInc=-1);
    ///Default destructor.
    virtual ~StiRootDrawableMcTrackFactory();
    
protected:
    ///Return a pointer to a new StiMcTrack object on the heap.
    virtual void* makeNewObject() const
      {
	return new StiRootDrawableMcTrack();
      }
    
private:
    StiRootDrawableMcTrackFactory(); // no imp
};

#endif
