//StiMaker.h

#ifndef StiMaker_HH
#define StiMaker_HH

#include <string>
using std::string;

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiKalmanTrackFitter.h"

class StEvent;
class StMcEvent;
class StiHit;
class StiTrack;
class StiStEventFiller;
class StiTrackContainer;
class StiTrackFinder;
class StiKalmanTrackFinder;
class StiKalmanTrackFitter;
class StiKalmanTrackNode;
class StiKalmanTrack;
class StMcEventMaker;
class StAssociationMaker;
class StiTrackMerger;
class StiToolkit;
class StiMakerParameters;
class StiVertexFinder;
class EventDisplay;
class StiResidualCalculator;

class StiTrackingPlots;
class RadLengthPlots;

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
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 2.14 2004/02/21 19:17:39 pruneau Exp $ built "__DATE__" "__TIME__; return cvs;}	

    void setMcEventMaker(StMcEventMaker*);
    void setAssociationMaker(StAssociationMaker*);
    void setParameters(StiMakerParameters * pars);
    StiMakerParameters * getParameters();

    void setEventDisplay(EventDisplay* eventDisplay) 
      { _eventDisplay = eventDisplay;}
    EventDisplay* getEventDisplay() 
      { return _eventDisplay; }
    StiToolkit * getToolkit();
		void load(TDataSet *);

private:
    StiMakerParameters * _pars;
    bool                 eventIsFinished;
    bool                 _initialized;
    StiToolkit  *        _toolkit;
    StiHitLoader<StEvent,StMcEvent,StiDetectorBuilder> * _hitLoader;
    StiTrackFinder *       _seedFinder;
    StiKalmanTrackFinder * _tracker;
    StiKalmanTrackFitter * _fitter;
    StiStEventFiller *    _eventFiller;
    StiTrackContainer *   _trackContainer;
    StiVertexFinder*      _vertexFinder;
    StMcEventMaker*       mMcEventMaker; //!
    StAssociationMaker*   mAssociationMaker; //!
    StiTrackingPlots*     _recPlotter;
    StiTrackingPlots*     _mcPlotter;
    RadLengthPlots *      _radLength;
    StiResidualCalculator * _residualCalculator;
    EventDisplay *        _eventDisplay;
    EditableFilter<StiTrack> * _loaderTrackFilter;
    EditableFilter<StiHit>   * _loaderHitFilter;
    
    ClassDef(StiMaker,0)
};

//inlines

inline StiToolkit * StiMaker::getToolkit()
{
  return _toolkit;
}

inline void StiMaker::setMcEventMaker(StMcEventMaker* val)
{
    mMcEventMaker = val;
}

inline void StiMaker::setAssociationMaker(StAssociationMaker* val)
{
	
    mAssociationMaker = val;
}


#endif
