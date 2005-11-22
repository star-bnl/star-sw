/**********************************************************************
 *
 * $Id: StEStructMuDstReader.h,v 1.4 2005/11/22 14:40:04 msd Exp $
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
  bool mUseAllTracks;
  int  mCentBin;
  int  mNumGoodTracks;//!
  int  mhasdEdxCuts;
  TH2F*  dEdxBefore;
  TH2F*  dEdxAfter;

  StEStructMuDstReader();
  StEStructMuDstReader(StMuDstMaker* maker,
                       StEStructEventCuts* ecuts,
                       StEStructTrackCuts* tcuts,
                       bool inChain = true,
                       bool useAllTracks = true,
                       int  centBin = 0);
  virtual ~StEStructMuDstReader();

  void setMuDstMaker(StMuDstMaker* MuDstMaker, bool inChain=true);
  void setEventCuts(StEStructEventCuts* cuts);
  void setTrackCuts(StEStructTrackCuts* cuts);
  bool setUseAllTracks(bool useAllTracks);
  int  setCentBin(int centBin);
  bool hasMaker();
  bool hasEventCuts();
  bool hasTrackCuts();

  virtual StEStructEvent* next();
  virtual bool         done();

  StEStructEvent* fillEvent();
  bool fillTracks(StEStructEvent* estructEvent);
  bool isTrackGood(StMuTrack* track);
  int  countGoodTracks();
  void fillEStructTrack(StEStructTrack* eTrack, StMuTrack* mTrack);
  
  ClassDef(StEStructMuDstReader,1)

};

inline bool StEStructMuDstReader::done(){ return mAmDone; };
inline bool StEStructMuDstReader::setUseAllTracks(bool useAllTracks) {
    mUseAllTracks = useAllTracks;
    return mUseAllTracks;
};
inline int StEStructMuDstReader::setCentBin(int centBin) {
    mCentBin = centBin;
    return mCentBin;
};

#endif  

/***********************************************************************
 *
 * $Log: StEStructMuDstReader.h,v $
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
