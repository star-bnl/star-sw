/***************************************************************************
 *
 * $Id: mercedesEventCutMonitor.h,v 1.2 2003/02/02 23:50:43 magestro Exp $
 *
 * Author: Mercedes Lopez Noriega , Ohio State, mercedes@pacific.mps.ohio-state.edu
 *
 ***************************************************************************
 *
 * Event Cut Monitor that plots just the tracks that passed 
 * mercedesStarStandardEventCut in the multiplicity plot
 * Plots: EventMultiplicity (just # of tracks that passed the event cut),
 * ZVertexPosition, and 2D plot: EventMultiplicity vs Tracks (# of tracks that 
 * passed the cuts vs # of tracks in the event) 
 * 
 ***************************************************************************
 *
 * $Log: mercedesEventCutMonitor.h,v $
 * Revision 1.2  2003/02/02 23:50:43  magestro
 * Added dummy Fill() functions to take care of compiler warning related to overloaded virtual functions
 *
 * Revision 1.1  2002/04/09 15:14:34  mercedes
 * Event Cut Monitor for mercedesStarStandardEventCut
 *
 *
 **************************************************************************/


#ifndef mercedesEventCutMonitor_hh
#define mercedesEventCutMonitor_hh

#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "StHbtMaker/Infrastructure/StHbtEvent.hh"
#include "StHbtMaker/Base/StHbtCutMonitor.hh"

class mercedesEventCutMonitor : public StHbtCutMonitor{

private:

  StHbt1DHisto* mEventMultHisto; 
  StHbt1DHisto* mZVertexPosHisto;
  StHbt2DHisto* mEventMultvsTracks;

public:
  mercedesEventCutMonitor();
  virtual ~mercedesEventCutMonitor();

  virtual StHbtString Report(); 
  virtual void Fill(const StHbtEvent* event);
  virtual void Finish();
  
  
  // These dummy Fill() functions were introduced to remove a compiler
  //   warning related to overloaded base-class Fill() functions being 
  //   hidden by a single version of Fill() in this derived class
  void Fill(const StHbtParticleCollection* d) {;}
  void Fill(const StHbtEvent *d1, const StHbtParticleCollection* d2) {;}
  void Fill(const StHbtPair* d) {;}
  void Fill(const StHbtKink* d) {;}
  void Fill(const StHbtV0* d) {;}
  void Fill(const StHbtTrack* d) {;}

  StHbt1DHisto* EventMultHisto() {return mEventMultHisto;}
  StHbt1DHisto* ZVertexPosHisto() {return mZVertexPosHisto;}
  StHbt2DHisto* EventMultvsTracks() {return mEventMultvsTracks;}
  
#ifdef __ROOT__ 
 ClassDef(mercedesEventCutMonitor, 1)
#endif
};

#endif
