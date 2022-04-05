/**********************************************************************
 *
 * $Id: StEStructEStructReader.h,v 1.1 2013/05/04 23:45:44 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  event reader class for EStruct format files.
 *               Real reading is done in macro.
 *
 ***********************************************************************/
#ifndef __STESTRUCTESTRUCTEVENTREADER__H
#define __STESTRUCTESTRUCTEVENTREADER__H

#include "TH1F.h"
#include "TH2F.h"
#include "TH3F.h"
#include "StBTofHeader.h"
#include "StEStructEventReader.h"
#include "StEStructPool/Pileup/Pileup.h"

class StEStructEventCuts;
class StEStructTrackCuts;
class StEStructTrack;


class StEStructEStructReader : public StEStructEventReader {

public:

  bool mInChain;
  bool mAmDone;
  bool mUseGlobalTracks;
  TFile *mFile;
  int  miEvent;
  TList *mkeyList;
  int mnEvents;
  StEStructEvent *mEventIn;
  int  mPrimaryVertexId;
  int  mNumGoodTracks;//!
  int  mhasdEdxCuts;
  int  mhasToFCuts;
  int  mhasPrimaryCuts;
  int  mhasVertexRadiusCuts;
  TH3F*  dEdxBetaBefore;
  TH3F*  dEdxBetaAfter;
  TH2F*  dEdxBefore;
  TH2F*  dEdxAfter;
  TH2F*  ToFBefore;
  TH2F*  ToFAfter;
  TH2F*  PrimaryBefore;
  TH2F*  PrimaryAfter;
  TH2F*  VRadiusBefore;
  TH2F*  VRadiusAfter;

  Float_t mEta;
  Float_t mPhi;

  Pileup      *mPileup;

  StEStructEStructReader();
  StEStructEStructReader(char* estructFileName,
                       StEStructEventCuts* ecuts,
                       StEStructTrackCuts* tcuts);
  virtual ~StEStructEStructReader();

  virtual StEStructEvent* next();
  virtual bool         done();

  void setEventCuts(StEStructEventCuts* cuts);
  void setTrackCuts(StEStructTrackCuts* cuts);
  StEStructEvent* fillEvent();
  bool fillTracks(StEStructEvent* estructEvent);
  bool isTrackGood(StEStructTrack* track);
  bool isTrackGoodToUse(StEStructTrack* track);
  int  countGoodTracks(int *ndEdx, int *nToF);
  void fillEStructTrack(StEStructTrack* destTrack, StEStructTrack* srcTrack);

  ClassDef(StEStructEStructReader,1)

};

inline bool StEStructEStructReader::done(){ return mAmDone; };

#endif  

/***********************************************************************
 *
 * $Log: StEStructEStructReader.h,v $
 * Revision 1.1  2013/05/04 23:45:44  prindle
 * Code to read EStruct format files. Modified from StEStructMuDstReader.
 *
 *
 * Revision 1.0  2013/05/02 18:20:32  prindle
 * Copy of StEStructMuDstReader specialized for reading EStruct format files..
 *
 *********************************************************************/
