/**********************************************************************
 *
 * $Id: StEStructTherminator.h,v 1.1 2007/11/26 20:08:25 prindle Exp $
 *
 * Author: Chunhui Han
 *
 **********************************************************************
 *
 * Description:  EStructEventReader wrapper for Therminator event generator
 *
 **********************************************************************/
#ifndef __STESTRUCTTHERMINATOR__H
#define __STESTRUCTTHERMINATOR__H

#include "StEStructPool/AnalysisMaker/StEStructEventReader.h"
#include "Therminator.h"

#include "TROOT.h"

class StEStructEventCuts;
class StEStructTrackCuts;

class StEStructTherminator : public StEStructEventReader {

  Therminator* mTherminator;
  StEStructEventCuts* mECuts;
  StEStructTrackCuts* mTCuts;
  bool mInChain;
  bool mAmDone;
  int   mnumTracks;
  int   mEventsToDo;

  void fillTracks(StEStructEvent* estructEvent);
  bool isTrackGood(int i);
  int  countGoodTracks();

  int mEventCount;

 public:

  StEStructTherminator();
  StEStructTherminator(Therminator *therminator,
                       StEStructEventCuts* ecuts,
                       StEStructTrackCuts* tcuts,
                       int  eventsToDo = 100);

  virtual ~StEStructTherminator() {};
  void setTherminatorReader(Therminator *therminator);  // This method appears to be un-used.
  void setEventCuts(StEStructEventCuts* cuts);
  void setTrackCuts(StEStructTrackCuts* cuts);
  bool hasGenerator();
  bool hasEventCuts();
  bool hasTrackCuts();
  bool setInChain(bool inChain);
  bool InChain();
  bool measureable(int pid);
  float* globalDCA(float* p, float* v);

  virtual StEStructEvent* next();
  virtual bool         done();

  ClassDef(StEStructTherminator,1)
};

inline bool StEStructTherminator::done(){ return mAmDone; };
inline bool StEStructTherminator::setInChain(bool inChain) {
    mInChain = inChain;
    return mInChain;
};
inline bool StEStructTherminator::InChain(){ return mInChain; };


inline bool StEStructTherminator::measureable(int pid){
  bool retVal=false;

  switch(pid){

  // Not sure if Therminator can produce these, but they are
  // defined by the pdg (I think, see www.slac.stanford.edu/BFROOT/www/Computing/Environment/NewUser/htmlbug/node51.html)
  // because GEANT can make them and they are measurable so I
  // include them here. djp   Sept. 12, 2005
  case 95:
    {   // deuteron
       retVal=true;
       break;
    }
  case 96:
    {   // triton
      retVal=true;
      break;
    }
  case 97:
    {   // Helium 4
      retVal=true;
      break;
    }
  default:
    {
      break;
    }
  }


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
  case 13:
    {   // muon
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

inline float* StEStructTherminator::globalDCA(float* p, float* v){

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
