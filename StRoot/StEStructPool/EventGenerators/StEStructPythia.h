/**********************************************************************
 *
 * $Id: StEStructPythia.h,v 1.2 2004/02/26 20:20:06 chunhuih Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for (T)Pythia event generator
 *
 **********************************************************************/
#ifndef __STESTRUCTPYTHIA__H
#define __STESTRUCTPYTHIA__H

#include "StEStructPool/AnalysisMaker/StEStructEventReader.h"

#include "TROOT.h"
#include "TPythia6.h"

class StEStructEventCuts;
class StEStructTrackCuts;


class StEStructPythia : public StEStructEventReader {

  TPythia6* mpythia;
  int meventCount;
  int meventsToDo;
  bool mAmDone;
  int mrefMult;

  StEStructEventCuts* mECuts;
  StEStructTrackCuts* mTCuts;

  void fillTracks(StEStructEvent* estructEvent);

 public:

  StEStructPythia();
  StEStructPythia(int nevents, TPythia6* pythia, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts);

  virtual ~StEStructPythia(){};
  void setEventCuts(StEStructEventCuts* cuts);
  void setTrackCuts(StEStructTrackCuts* cuts);
  bool hasGenerator();
  bool hasEventCuts();
  bool hasTrackCuts();
  bool measureable(int pid);
  float* globalDCA(float* p, float* v);

  virtual StEStructEvent* next();
  virtual bool         done();
  virtual StEStructEvent* generateEvent();

  ClassDef(StEStructPythia,1)
};


inline bool StEStructPythia::done(){ return mAmDone; };

inline bool StEStructPythia::measureable(int pid){
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

inline float* StEStructPythia::globalDCA(float* p, float* v){

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

  /*  
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
      */
}


#endif

/**********************************************************************
 *
 * $Log: StEStructPythia.h,v $
 * Revision 1.2  2004/02/26 20:20:06  chunhuih
 * Fixed the method globalDCA().
 * Assign the globalDCA, but leave its x, y, z coordinates zero.
 *
 * Revision 1.1  2003/11/21 06:25:00  porter
 * Pythia event generater as an StEStructEventReader
 *
 *
 *********************************************************************/
