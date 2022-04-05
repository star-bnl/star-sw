/**********************************************************************
 *
 * $Id: StEStructFlat.h,v 1.5 2012/11/16 21:23:18 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for flat event generator
 *
 **********************************************************************/
#ifndef __STESTRUCTFLAT__H
#define __STESTRUCTFLAT__H

#include <cmath>
#include "StEStructPool/AnalysisMaker/StEStructEventReader.h"

#include "TROOT.h"

class StEStructEventCuts;
class StEStructTrackCuts;


class StEStructFlat : public StEStructEventReader {

  StEStructEvent*     mFlatEvent;
  bool mInChain;
  bool mAmDone;
  int  mCentBin;
  int  mnumTracks;
  int  mEventsToDo;

  void   fillTracks(StEStructEvent* estructEvent);
  double maxRadius(double eta, double pt, double vz);
  bool   isTrackGood(float *v, float *p, float eta);
  int    countGoodTracks();

  int mEventCount;
  bool mgRand2Good;
  double mgRand2;

 public:

  StEStructFlat();
  StEStructFlat( StEStructEventCuts* ecuts,
                 StEStructTrackCuts* tcuts,
                 bool inChain,
                 int  centBin,
                 int  eventsToDo);

  virtual ~StEStructFlat(){};
  void setSeed(int iseed);
  bool hasGenerator();
  bool measureable(int pid);
  float* globalDCA(float* p, float* v);

  virtual StEStructEvent* next();
  virtual bool         done();
  void    generateEvent();
  double  gRand48();

  ClassDef(StEStructFlat,1)
};


inline bool StEStructFlat::done(){ return mAmDone; };

inline bool StEStructFlat::measureable(int pid){
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

inline float* StEStructFlat::globalDCA(float* p, float* v){

  // assumes primaryVertex at origin 
  float* r=new float[4];
  r[0]=r[1]=r[2]=r[3]=0;

  if(fabs(p[2])<0.01){ // then 2D
    if(fabs(p[0])<0.01){ 
      r[3]=r[1]=v[1]; // p=py only
      return r;
    }
    float a = p[1]/p[0];
    float x = -1.0*(a*v[1])/(a*a+1);
    float y=a*x+v[1];
    r[0]=x; r[1]=y;
    r[3]=sqrt(x*x+y*y);
    return r;
  }
  float ax = p[0]/p[2];
  float ay = p[1]/p[2];
  float z  = -1.0*(ax*v[0]+ay*v[1])/(ax*ax+ay*ay+1);
  float x  = ax*z+v[0];
  float y  = ay*z+v[1];
  r[0]=x; r[1]=y; r[2]=z;
  r[3]=sqrt(x*x+y*y+z*z);
  return r;
}


#endif

/**********************************************************************
 *
 * $Log: StEStructFlat.h,v $
 * Revision 1.5  2012/11/16 21:23:18  prindle
 * EventCuts and TrackCuts were moved to EventReader. Remove that code from
 * these readers.
 *
 * Revision 1.4  2006/02/22 22:05:36  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.3  2005/09/23 23:37:22  prindle
 *
 *   Starting to add vertex distribution and track acceptance dependance on
 * number of possible hits.
 *   Make Pythia interface look like Hijing interface so it now works within
 * my Fluctuation and Correlation framework.
 *
 * Revision 1.2  2005/09/07 20:22:51  prindle
 *
 *
 *     Flat: Random changes to eta and phi distributions (which don't have to be flat).
 *
 * Revision 1.1  2003/11/21 23:48:00  prindle
 * Include my toy event generator in cvs
 *
 *
 *********************************************************************/
