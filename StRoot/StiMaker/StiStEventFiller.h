//StiStEventFiller.h
//Manuel Calderon (Yale Software ;) )
//3/02

#ifndef StiStEventFiller_HH
#define StiStEventFiller_HH

//Doxygen class header...
/*! \class StiStEventFiller
    StiStEventFiller is a utilitity class meant to properly convert StiTrack
    objects into StTrack (Global/Primary) objects and hang these on the StEvent
    Track-node.

    \author Manuel Calderon (Yale Software... You can't escape)
    \note Any notes here...
    \warning Any warnings here ...
 */

class StEvent;
class StiTrackContainer;

class StiStEventFiller
{
public:
    StiStEventFiller();
    virtual ~StiStEventFiller();

    ///Fill the event from the track store.
    StEvent* fillEvent(StEvent*, StiTrackContainer*);

private:
    StEvent* mEvent;
    StiTrackContainer* mTrackStore;
};

#endif
