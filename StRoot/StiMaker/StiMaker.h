//StiMaker.h

#ifndef StiMaker_HH
#define StiMaker_HH

#include <string>
using std::string;

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
#include "Sti/StiKalmanTrackFinder.h"

class TH1D;
class StEvent;
class StMcEvent;
class StiHit;
class StiTrack;
class StiStEventFiller;
class StiTrackContainer;
class StiEvaluableTrack;
class StiTrackSeedFinder;
class StiTrackFinder;
class StiKalmanTrackFinder;
class StiTrackSeedFinder;
class StiKalmanTrackNode;
class StiKalmanTrack;
class StMcEventMaker;
class StAssociationMaker;
class StiTrackMerger;
class StiToolkit;
class StiTrackingPlots;
class StiMakerParameters;
class StiVertexFinder;
class EventDisplay;
template<class FILTERED> class EditableFilter;


class StiMaker : public StMaker 
{
 public:
    
    StiMaker(const char* name = "StiMaker");
    virtual ~StiMaker();
    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t InitDetectors();
    virtual Int_t InitRun(int);
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 2.8 2003/04/11 16:52:01 pruneau Exp $ built "__DATE__" "__TIME__; return cvs;}	

    void setMcEventMaker(StMcEventMaker*);
    void setAssociationMaker(StAssociationMaker*);
    void setParameters(StiMakerParameters * pars);
    StiMakerParameters * getParameters();

    void setEventDisplay(EventDisplay* eventDisplay) 
      { _eventDisplay = eventDisplay;}
    EventDisplay* getEventDisplay() 
      { return _eventDisplay; }

private:
    StiMakerParameters * _pars;
    bool                 eventIsFinished;
    bool                 _initialized;
    StiToolkit  *        _toolkit;
    StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder> * _hitLoader;
    StiTrackSeedFinder *  _seedFinder;
    StiTrackFinder *      _tracker;
    StiStEventFiller *    _eventFiller;
    StiTrackContainer *   _trackContainer;
    StiVertexFinder*      _vertexFinder;
    StMcEventMaker*       mMcEventMaker; //!
    StAssociationMaker*   mAssociationMaker; //!
    StiTrackingPlots*     _recPlotter;
    StiTrackingPlots*     _mcPlotter;
    EventDisplay *        _eventDisplay;
    EditableFilter<StiTrack> * _loaderTrackFilter;
    EditableFilter<StiHit>   * _loaderHitFilter;
    
    ClassDef(StiMaker, 1)
};

//inlines

inline void StiMaker::setMcEventMaker(StMcEventMaker* val)
{
    mMcEventMaker = val;
}

inline void StiMaker::setAssociationMaker(StAssociationMaker* val)
{
	
    mAssociationMaker = val;
}


#endif
