#ifndef EventDisplay_H_INCLUDED
#define EventDisplay_H_INCLUDED

#include "Sti/Base/SubjectObserver.h"
#include "Sti/Base/EditableParameters.h"
#include "Sti/Base/Named.h"
#include "Sti/Base/Messenger.h"
#include "Sti/Base/Described.h"
#include "Sti/Base/EditableParameter.h"
#include "Sti/Base/EditableFilter.h"
#include "Sti/StiToolkit.h"

#include "StiGui/MenuGroup.h"
#include "StiGui/StiDrawable.h"
#include "StiGui/DrawingPolicy.h"
#include "StiGui/StiDetectorView.h"
#include "StiGui/DefaultDrawingPolicy.h"
#include "StiGui/StiRootDrawableHits.h"
#include "TGClient.h"
#include "TGFrame.h"
#include "TGWindow.h"
#include "TCanvas.h"
#include "TVolume.h"
#include "StChain.h"
#include "StIOMaker/StIOMaker.h"
class StiDetector;
class StiHit;
class StiTrack;
class StiMcTrack;
class StiDefaultTrackFilter;
class StiDetectorContainer;
class StiHitContainer;
class StiTrackContainer;
class EventDisplayParameters;

class EventDisplay : public Named, public Described, public TGMainFrame, public Observer
{
 public:
EventDisplay(const string& name, const string & description, StiToolkit * toolkit, const TGWindow *p, UInt_t w, UInt_t h);
  virtual ~EventDisplay();
  void initialize();
  void createMenu();
  void createCanvasFrame();
  void createFilters();
  void createPolicies();
  Bool_t ProcessMessage(Long_t msg, Long_t option1, Long_t option2);
  void update();
  void draw();
  void draw(StiDetectorContainer * detectorContainer);
  void draw(StiTrackContainer    * container,
	    Filter<StiTrack>     * filter, 
	    DrawingPolicy<StiDrawable> * policy);
  void draw(StiHitContainer * container,
	    Filter<StiHit>  * filter, 
	    DrawingPolicy<StiDrawable> * policy,
	    StiRootDrawableHits & usedHits,
	    StiRootDrawableHits & unusedHits);
  void reset();

  void setStChain(StChain * chain);
  void setIoMaker(StIOMaker * ioMaker);

  TGWindow                 * getWindow();
  TGClient                 * getClient();
  StChain                  * getChain();
  StIOMaker                * getIoMaker();
  StiToolkit               * getToolkit();
  StiDetectorContainer     * getDetectorContainer();
  StiHitContainer          * getHitContainer();
  StiHitContainer          * getMcHitContainer();
  StiTrackContainer        * getTrackContainer();
  StiTrackContainer        * getMcTrackContainer();
  EditableFilter<StiHit>   * getHitFilter();
  EditableFilter<StiHit>   * getMcHitFilter();
  EditableFilter<StiTrack> * getTrackFilter(); 
  EditableFilter<StiTrack> * getMcTrackFilter();   
  EventDisplayParameters   * getOptions();
  virtual void getNewState();

 protected:
  bool         _initialized;
  StiToolkit * _toolkit;
  TGClient   * _client; 
  TCanvas    * _canvas;
  TVolume    * _node;
  MenuGroup  * _fileMenuGroup;
  MenuGroup  * _optionMenuGroup;
  MenuGroup  * _viewMenuGroup;
  MenuGroup  * _navigationMenuGroup;
  MenuGroup  * _trackingMenuGroup;
  MenuGroup  * _printMenuGroup;
  MenuGroup  * _helpMenuGroup;
  StChain                  * _chain;
  StIOMaker                * _ioMaker;
  StiDetectorContainer     * _detectorContainer;
  StiHitContainer          * _hitContainer;
  StiHitContainer          * _mcHitContainer;
  StiTrackContainer        * _trackContainer;
  StiTrackContainer        * _mcTrackContainer;
  EditableFilter<StiHit>   * _hitFilter;
  EditableFilter<StiHit>   * _mcHitFilter;
  EditableFilter<StiTrack> * _trackFilter; 
  EditableFilter<StiTrack> * _mcTrackFilter; 

  DefaultDrawingPolicy * _hitDrawingPolicy;
  DefaultDrawingPolicy * _mcHitDrawingPolicy;
  DefaultDrawingPolicy * _trackDrawingPolicy;
  DefaultDrawingPolicy * _mcTrackDrawingPolicy;
  DefaultDrawingPolicy * _defaultHitDrawingPolicy;
  DefaultDrawingPolicy * _defaultMcHitDrawingPolicy;
  DefaultDrawingPolicy * _defaultTrackDrawingPolicy;
  DefaultDrawingPolicy * _defaultMcTrackDrawingPolicy;
  // _hitDrawingPolicies( new DrawingPolicies("HitDrawingPolicies") ),
  //  _trackDrawingPolicies( new DrawingPolicies("TrackDrawingPolicies") ),
  //  _mcTrackDrawingPolicies( new DrawingPolicies("McTrackDrawingPolicies") ),
  //vector<StiDetectorView*> _detectorViews; 
  EventDisplayParameters * _options;
  Messenger & _messenger;  
  StiRootDrawableHits _usedHits;
  StiRootDrawableHits _unusedHits;
  StiRootDrawableHits _mcUsedHits;
  StiRootDrawableHits _mcUnusedHits;
  ClassDef(EventDisplay,1)
};

#endif

