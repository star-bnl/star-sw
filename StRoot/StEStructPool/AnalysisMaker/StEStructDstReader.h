/**********************************************************************
 *
 * $Id: StEStructDstReader.h,v 1.1 2003/10/15 18:20:32 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Reader class using the StEStructEventMaker
 *
 ***********************************************************************/
#ifndef __STEStructDSTREADER__H
#define __STEStructDSTREADER__H

#include "StEStructEventReader.h"

class StEStructEventMaker;
class StEStructEventCuts;
class StEStructTrackCuts;

class StEStructDstReader : public StEStructEventReader {

  StEStructEventMaker* mMaker;
  StEStructEventCuts*  mECuts; 
  StEStructTrackCuts*  mTCuts;

  bool  mInChain;
  bool  mAmDone; 
  int   mrefMult;

  bool  checkEvent(StEStructEvent* e);
  int  getNumberOfTracks(StEStructEvent* e);

public:

  StEStructDstReader();
  StEStructDstReader(StEStructEventMaker* maker, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts, bool inChain=true);
  virtual ~StEStructDstReader();


  void setEventMaker(StEStructEventMaker* eventMaker, bool inChain=true);
  void setEventCuts(StEStructEventCuts* cuts);
  void setTrackCuts(StEStructTrackCuts* cuts);
  bool hasMaker();
  bool hasEventCuts();
  bool hasTrackCuts();

  virtual StEStructEvent* next();
  virtual bool done();

  ClassDef(StEStructDstReader,1)

};

inline bool StEStructDstReader::done(){ return mAmDone; };

#endif

/***********************************************************************
 *
 * $Log: StEStructDstReader.h,v $
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/




