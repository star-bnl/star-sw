//StiMaker.h

#ifndef StiMaker_HH
#define StiMaker_HH

#include <string>
using std::string;

#include "StMaker.h"
#include "StEvent/StEnumerations.h"
#include "Sti/StiKalmanTrackFinder.h"
#include "Sti/StiKalmanTrackFitter.h"

class TFile;
class TTree;
class StiPullEvent;
class StEvent;
class StiHit;
class StiTrack;
class StiStEventFiller;
class StiTrackContainer;
class StiTrackFinder;
class StiKalmanTrackFinder;
class StiKalmanTrackFitter;
class StiKalmanTrackNode;
class StiKalmanTrack;
class StiTrackMerger;
class StiToolkit;
class StiVertexFinder;
class EventDisplay;
class StiResidualCalculator;
template<class FILTERED> class EditableFilter;


class StiMaker : public StMaker 
{
 public:
    
    StiMaker(const char* name = "StiMaker");
    virtual ~StiMaker();
    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
            Int_t InitDetectors();
            Int_t InitPulls();
            Int_t FillPulls();
    virtual Int_t InitRun(int);
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiMaker.h,v 2.21 2006/04/07 18:00:30 perev Exp $ built "__DATE__" "__TIME__; return cvs;}	


    void setEventDisplay(EventDisplay* eventDisplay) 
      { _eventDisplay = eventDisplay;}
    EventDisplay* getEventDisplay() 
      { return _eventDisplay; }
    StiToolkit * getToolkit();
		void load(TDataSet *);

private:

		double runField;
    bool                 eventIsFinished;
    bool                 _initialized;
    StiToolkit  *        _toolkit;
    StiHitLoader<StEvent,StiDetectorBuilder> * _hitLoader;
    StiTrackFinder *       _seedFinder;
    StiKalmanTrackFinder * _tracker;
    StiKalmanTrackFitter * _fitter;
    StiStEventFiller *    _eventFiller;
    StiTrackContainer *   _trackContainer;
    StiVertexFinder*      _vertexFinder;
    StiResidualCalculator * _residualCalculator;
    EventDisplay *        _eventDisplay;
    EditableFilter<StiTrack> * _loaderTrackFilter;
    EditableFilter<StiHit>   * _loaderHitFilter;

    TFile 		*mPullFile;
    StiPullEvent 	*mPullEvent;
    TTree 	        *mPullTTree;
    
    ClassDef(StiMaker,0)
};

//inlines

inline StiToolkit * StiMaker::getToolkit()
{
  return _toolkit;
}
#endif
