/**********************************************************************
 *
 * $Id: StEStructMuDstReader.h,v 1.1 2003/10/15 18:20:32 porter Exp $
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

#include "StEStructEventReader.h"

class StMuDstMaker;
class StMuTrack;

class StEStructEventCuts;
class StEStructTrackCuts;
class StEStructTrack;


class StEStructMuDstReader : public StEStructEventReader {

protected:

  StMuDstMaker* mMaker; //!
  StEStructEventCuts* mECuts; //!
  StEStructTrackCuts* mTCuts; //!
  bool mInChain;
  bool mAmDone;
  int  mrefMult;//!

public:

  StEStructMuDstReader();
  StEStructMuDstReader(StMuDstMaker* maker, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts, bool inChain=true);
  virtual ~StEStructMuDstReader();

  void setMuDstMaker(StMuDstMaker* MuDstMaker, bool inChain=true);
  void setEventCuts(StEStructEventCuts* cuts);
  void setTrackCuts(StEStructTrackCuts* cuts);
  bool hasMaker();
  bool hasEventCuts();
  bool hasTrackCuts();

  virtual StEStructEvent* next();
  virtual bool         done();

  StEStructEvent* fillEvent();
  bool fillTracks(StEStructEvent* estructEvent);
  void fillEStructTrack(StEStructTrack* eTrack, StMuTrack* mTrack);
  
  ClassDef(StEStructMuDstReader,1)

};

inline bool StEStructMuDstReader::done(){ return mAmDone; };

#endif  

/***********************************************************************
 *
 * $Log: StEStructMuDstReader.h,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
