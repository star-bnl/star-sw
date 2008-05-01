/**********************************************************************
 *
 * $Id: StEStructMuDstReader.h,v 1.8 2008/05/01 23:35:57 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  event reader class for common MuDsts
 *               Uses the StMuDstMaker for real reading
 *
 ***********************************************************************/
#ifndef __STEBEEMUEventREADER__H
#define __STEBEEMUEventREADER__H

#include "TH2F.h"
#include "StEStructEventReader.h"

class StMuDstMaker;
class StMuTrack;

class StEStructEventCuts;
class StEStructTrackCuts;
class StEStructTrack;


class StEStructMuDstReader : public StEStructEventReader {

public:

  StMuDstMaker* mMaker; //!
  StEStructEventCuts* mECuts; //!
  StEStructTrackCuts* mTCuts; //!
  bool mInChain;
  bool mAmDone;
  bool mUseGlobalTracks;
  int  mNumGoodTracks;//!
  int  mhasdEdxCuts;
  TH2F*  dEdxBefore;
  TH2F*  dEdxAfter;

  StEStructMuDstReader();
  StEStructMuDstReader(StMuDstMaker* maker,
                       StEStructEventCuts* ecuts,
                       StEStructTrackCuts* tcuts);
  virtual ~StEStructMuDstReader();

  void setMuDstMaker(StMuDstMaker* MuDstMaker, bool inChain=true);
  void setEventCuts(StEStructEventCuts* cuts);
  void setTrackCuts(StEStructTrackCuts* cuts);
  void setUseGlobalTracks(bool global=false);
  bool setInChain(bool inChain);
  bool InChain();
  bool hasMaker();
  bool hasEventCuts();
  bool hasTrackCuts();

  virtual StEStructEvent* next();
  virtual bool         done();

  StEStructEvent* fillEvent();
  bool fillTracks(StEStructEvent* estructEvent);
  bool isTrackGood(StMuTrack* track);
  bool isTrackGoodToUse(StMuTrack* track);
  int  countGoodTracks();
  void fillEStructTrack(StEStructTrack* eTrack, StMuTrack* mTrack);
  
  ClassDef(StEStructMuDstReader,1)

};

inline bool StEStructMuDstReader::done(){ return mAmDone; };

#endif  

/***********************************************************************
 *
 * $Log: StEStructMuDstReader.h,v $
 * Revision 1.8  2008/05/01 23:35:57  prindle
 * Found that for global tracks we sometimes have global dca = (0,0,0)
 * Now use dca() when we are using global tracks.
 *
 * Revision 1.7  2006/04/11 17:50:48  prindle
 *   Remove inChain from constructor arguments (no longer used in macro)
 *
 * Revision 1.6  2006/04/04 22:05:06  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.5  2006/02/22 22:03:24  prindle
 * Removed all references to multRef
 *
 * Revision 1.4  2005/11/22 14:40:04  msd
 * Changed default of useAllTracks
 *
 * Revision 1.3  2005/09/14 17:08:35  msd
 * Fixed compiler warnings, a few tweaks and upgrades
 *
 * Revision 1.2  2005/09/07 20:18:43  prindle
 *   AnalysisMaker: Keep track of currentAnalysis (for use in doEStruct macro)
 *   EventCuts.h:   Added trigger cuts including cucu and year 4.
 *   MuDstReader:   Added dE/dx histograms. Re-arranged code to count tracks
 *                    before making centrality cut.
 *   TrackCuts:     Random changes. Moved some variables from private to public.o
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
