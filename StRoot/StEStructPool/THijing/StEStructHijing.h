/**********************************************************************
 *
 * $Id: StEStructHijing.h,v 1.9 2012/11/16 21:28:32 prindle Exp $
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
#include "TMath.h"
#include "../THijing/THijing.h"

class StEStructHijing : public StEStructEventReader {

 public:

  THijing* mHijing;
  bool mInChain;
  bool mAmDone;
  bool museImpactParameter;
  float mImpact;
  int   mnumTracks;
  int   mEventsToDo;
  int   *mTrackList;
  double *rTrackList;

  int  filterTracks();
  int  filterTrackArea();
  void fillTracks(StEStructEvent* estructEvent);
  bool isTrackGood(int i);
  int  countGoodTracks();

  int mEventCount;

  StEStructHijing();
  StEStructHijing(THijing* hijing,
                  StEStructEventCuts* ecuts,
                  StEStructTrackCuts* tcuts,
                  bool useImpactParameter = true,
                  int  eventsToDo = 100);

  virtual ~StEStructHijing(){};
  void setHijingReader(THijing* hijing);  // This method appears to be un-used.
  bool hasGenerator();
  bool setInChain(bool inChain);
  bool InChain();
  bool measureable(int pid);
  float* globalDCA(float* p, float* v);

  virtual StEStructEvent* next();
  virtual bool         done();

  virtual double getImpact();       // impact parameter
  virtual double getBinary();       // N binary collisions
  virtual double getParticipants(); // N participants


  ClassDef(StEStructHijing,1)
};

inline bool StEStructHijing::done(){ return mAmDone; };
inline bool StEStructHijing::setInChain(bool inChain) {
    mInChain = inChain;
    return mInChain;
};
inline bool StEStructHijing::InChain(){ return mInChain; };

inline double StEStructHijing::getImpact(){ return mHijing->GetImpactParameter(); };
inline double StEStructHijing::getBinary(){ return mHijing->GetBinaryCollisions(); };
inline double StEStructHijing::getParticipants(){ return mHijing->GetParticipants(); };


inline bool StEStructHijing::measureable(int pid){
  bool retVal=false;

  switch(pid){

  // I don't think Hijing can produce these, but they are
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
