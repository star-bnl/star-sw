/**********************************************************************
 *
 * $Id: StEStructQJ.h,v 1.3 2012/11/16 21:23:19 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for flat event generator
 *
 **********************************************************************/
#ifndef __STESTRUCTQJ__H
#define __STESTRUCTQJ__H

#include <cmath>
#include <iostream>
#include "StEStructPool/AnalysisMaker/StEStructEventReader.h"

#include "TROOT.h"

class StEStructEventCuts;
class StEStructTrackCuts;


class StEStructQJ : public StEStructEventReader {

  int    meventCount;
  int    meventsToDo;
  bool   mAmDone;
  int    mnumTracks;
  double impact;

  char  *fileListName;
  std::ifstream *fList;
  std::ifstream *inFile;

  void fillTracks(StEStructEvent* estructEvent);

 public:

  StEStructQJ(char *dir);
  StEStructQJ(char *dir, int nevents, StEStructEventCuts* ecuts, StEStructTrackCuts* tcuts);

  virtual ~StEStructQJ(){};
  void setSeed(int iseed);
  bool hasGenerator();
  bool measureable(int pid);
  float* globalDCA(float* p, float* v);

  virtual StEStructEvent* next();
  virtual bool         done();
  virtual StEStructEvent* generateEvent();
  int     getNumTracks();

  ClassDef(StEStructQJ,1)
};


inline bool StEStructQJ::done(){ return mAmDone; };

inline bool StEStructQJ::measureable(int pid){
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

inline float* StEStructQJ::globalDCA(float* p, float* v){

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
 * $Log: StEStructQJ.h,v $
 * Revision 1.3  2012/11/16 21:23:19  prindle
 * EventCuts and TrackCuts were moved to EventReader. Remove that code from
 * these readers.
 *
 * Revision 1.2  2006/02/22 22:05:41  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.1  2004/03/02 21:51:00  prindle
 *
 *   I forgot to cvs add my EventGenerator readers.
 *
 * Revision 1.1  2003/11/21 23:48:00  prindle
 * Include my toy event generator in cvs
 *
 *
 *********************************************************************/
