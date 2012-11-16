/**********************************************************************
 *
 * $Id: StEStructDstReader.h,v 1.3 2012/11/16 21:19:06 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Reader class using the StEStructEventMaker
 *
 ***********************************************************************/
#ifndef __STESTRUCTDSTREADER__H
#define __STESTRUCTDSTREADER__H

#include "StEStructEventReader.h"

class StEStructEventMaker;
class StEStructEventCuts;
class StEStructTrackCuts;

class StEStructDstReader : public StEStructEventReader {

  StEStructEventMaker* mMaker;

  bool  mInChain;
  bool  mAmDone; 
  int   mnumTracks;

  bool  checkEvent(StEStructEvent* e);
  int  getNumberOfTracks(StEStructEvent* e);

public:

  StEStructDstReader();
  StEStructDstReader(StEStructEventMaker* maker, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts, bool inChain=true);
  virtual ~StEStructDstReader();


  void setEventMaker(StEStructEventMaker* eventMaker, bool inChain=true);
  bool hasMaker();

  virtual StEStructEvent* next();
  virtual bool done();

  ClassDef(StEStructDstReader,1)

};

inline bool StEStructDstReader::done(){ return mAmDone; };

#endif

/***********************************************************************
 *
 * $Log: StEStructDstReader.h,v $
 * Revision 1.3  2012/11/16 21:19:06  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.2  2006/02/22 22:03:15  prindle
 * Removed all references to multRef
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/




