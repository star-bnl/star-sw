/**********************************************************************
 *
 * $Id: StEStructHijing.h,v 1.1 2005/09/14 17:28:17 msd Exp $
 *
 * Author: Chunhui Han
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for (T)Hijing event generator
 *
 **********************************************************************/
#ifndef __STESTRUCTHIJING__H
#define __STESTRUCTHIJING__H

#include "StEStructPool/AnalysisMaker/StEStructEventReader.h"

#include "TROOT.h"
#include "../THijing/THijing.h"

class StEStructEventCuts;
class StEStructTrackCuts;

class StEStructHijing : public StEStructEventReader {

  THijing* mHijing;
  StEStructEventCuts* mECuts;
  StEStructTrackCuts* mTCuts;
  bool mInChain;
  bool mAmDone;
  bool mUseAllTracks;
  int mCentBin;
  int mRefMult;
  int mEventsToDo;

  void fillTracks(StEStructEvent* estructEvent);
  bool isTrackGood(int i);
  int  countGoodTracks();

  int mEventCount;

 public:

  StEStructHijing();
  StEStructHijing(THijing* hijing,
                  StEStructEventCuts* ecuts,
                  StEStructTrackCuts* tcuts,
                  bool inChain = true,
                  bool useAllTracks = false,
                  int  centBin = 0,
                  int  eventsToDo = 100);

  virtual ~StEStructHijing(){};
  void setHijingReader(THijing* hijing);
  void setEventCuts(StEStructEventCuts* cuts);
  void setTrackCuts(StEStructTrackCuts* cuts);
  bool hasGenerator();
  bool hasEventCuts();
  bool hasTrackCuts();
  bool measureable(int pid);
  float* globalDCA(float* p, float* v);

  virtual StEStructEvent* next();
  virtual bool         done();

  ClassDef(StEStructHijing,1)
};

inline bool StEStructHijing::done(){ return mAmDone; };

inline bool StEStructHijing::measureable(int pid){
  bool retVal=false;
  if(pid<0)pid*=-1;

  switch(pid){

  case 211:
    {   // charged pion
       retVal=true;
       break;
    }
  case 321:
    {   // charged kaon
      retVal=true;
      break;
    }
  case 2212:
    {   // proton
      retVal=true;
      break;
    }
  case 11:
    {   // electron
      retVal=true;
      break;
    }
  default:
    {
      break;
    }
  }
  return retVal;
}

inline float* StEStructHijing::globalDCA(float* p, float* v){

  // assumes primaryVertex at origin 
  float* r=new float[4];
  r[0]=r[1]=r[2]=r[3]=0;

  // a is the component of v vector that is parallel to p.
  float a = v[0] * p[0] + v[1] * p[1] + v[2] * p[2] ;
  a = a / sqrt(p[0]*p[0] + p[1]*p[1] + p[2]*p[2]);
  // r is the component of v vector perpendicular to p.
  r[3] = sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2] - a*a);
  // Since only the magnitude of globalDCA is used,
  // leave r[0], r[1], r[2] to be 0 here.
  return r;
}

#endif

/**********************************************************************
 *
 *
 *********************************************************************/
