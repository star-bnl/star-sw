#ifndef eventCutMonitor_ReactionPlane_hh
#define eventCutMonitor_ReactionPlane_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class eventCutMonitor_ReactionPlane : public StHbtCutMonitor{

private:
  StHbt1DHisto*  mScaler; 
  StHbt1DHisto*  mVertexX; 
  StHbt1DHisto*  mVertexY; 
  StHbt1DHisto*  mVertexZ; 
  StHbt1DHisto*  mReactionPlane; 
  StHbt1DHisto*  mReactionPlaneError; 
  StHbt2DHisto*  mMultReactionPlaneError; 

public:
  eventCutMonitor_ReactionPlane();
  eventCutMonitor_ReactionPlane(const char* TitCutMoni, const char* title, int nbins=24 ,double min=-360., double max=+360);
  virtual ~eventCutMonitor_ReactionPlane();


  virtual StHbtString Report(); 
  virtual void Fill(const StHbtEvent* event);
  virtual void Finish();
  StHbt1DHisto* Scaler() {return mScaler;}
  StHbt1DHisto* VertexX() {return mVertexX;}
  StHbt1DHisto* VertexY() {return mVertexY;}
  StHbt1DHisto* VertexZ() {return mVertexZ;}
  StHbt1DHisto* ReactionPlane() {return mReactionPlane;}
  StHbt1DHisto* ReactionPlaneError() {return mReactionPlaneError;}
  StHbt2DHisto* MultReactionPlaneError() {return mMultReactionPlaneError;}
  
#ifdef __ROOT__ 
 ClassDef(eventCutMonitor_ReactionPlane, 1)
#endif
};

#endif
