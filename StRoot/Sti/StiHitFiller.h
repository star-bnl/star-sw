//StiHitFiller.h
//M.L. Miller (Yale Software)
//04/01

/*! \class StiHitFiller
  StiHitFiller is a utility class meant to interface between StEvent and
  StiHitContainer.  StiHitFiller has the sole purpose of transfering the
  hit collection from StEvent into the StiHitContainer.  This process
  consists of loopin on the StEvent hit collection, transforming each
  StHit to an StiHit, and then adding this hit to the StiHitContainer.

  \author M.L. Miller (Yale Software)

  \note Only hits that come from a detector whose enumeration has been added
  to StiHitFiller (via addDetector(StDetectorId)) will be added to the 
  StiHitContainer instance.

 */

/*! \example StiHitContainer_ex.cxx */

#ifndef StiHitFiller_HH
#define StiHitFiller_HH

#include "StTimer.hh"

#include <iostream.h>
#include <vector>
#include <map>
#include "../pams/global/inc/StDetectorId.h" //for detector enumerations

#include "StiHit.h"
#include "StiObjectFactoryInterface.h"

using std::vector;
using std::map;

class StiHitContainer;
class StiGeometryTransform;
class StTpcCoordinateTransform;
class StEvent;
class StPrimaryVertex;
class Messenger;

class StiHitFiller
{
public:

    ///Define a collection of enumerated constants
    typedef vector<StDetectorId> det_id_vector;

    ///Default constructor.
    StiHitFiller();

    ///Default destructor.
    virtual ~StiHitFiller();

    ///Add detectors from which hits will be used.
    void addDetector(StDetectorId);

    ///Set a pointer to the next StEvent object.
    void setEvent(StEvent*);

    ///Fill the hits for a given event.
    void fillHits(StiHitContainer*, StiObjectFactoryInterface<StiHit>*);

    ///Friend out a streamer for the class.
    friend ostream& operator<<(ostream&, const StiHitFiller&);

private:
    void fillTpcHits(StiHitContainer*, StiObjectFactoryInterface<StiHit>*);
    void fillSvtHits(StiHitContainer*, StiObjectFactoryInterface<StiHit>*);
    void fillPrimaryVertices(StiHitContainer*,
			     StiObjectFactoryInterface<StiHit>*);
    
private:
    Messenger& mMessenger;
    StiGeometryTransform* mtranslator;
    //StTpcCoordinateTransform* mtpctransformer;
    
    StEvent* mevent;
    det_id_vector mvec;
    StTimer mtimer;
};

//inlines

/*! \note setEvent must be called before fillHits() for every event.
 */
inline void StiHitFiller::setEvent(StEvent* val)
{
    mevent=val;
}

ostream& operator<<(ostream&, const StiHitFiller&);

#endif
