/**********************************************************************
 *
 * $Id: StEStructPythia.h,v 1.11 2012/11/16 21:23:18 prindle Exp $
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
#include "TMath.h"
#include "TPythia6.h"

class StEStructEventCuts;
class StEStructTrackCuts;


class StEStructPythia : public StEStructEventReader {

  TPythia6* mPythia;
  bool mInChain;
  bool mAmDone;
  bool museAllTracks;
  int mnumTracks;
  int mEventsToDo;
  bool mstarTrigger;


  void fillTracks(StEStructEvent* estructEvent);
  bool isTrackGood(int i);
  int  countGoodTracks();

  int mEventCount;

 public:

  StEStructPythia();
  StEStructPythia(TPythia6* pythia,
                  StEStructEventCuts* ecuts,
                  StEStructTrackCuts* tcuts,
                  bool useAllTracks,
                  int  eventsToDo);

  virtual ~StEStructPythia(){};
  bool hasGenerator();
  bool setInChain(bool inChain);
  bool InChain();
  bool measureable(int pid);
  float* globalDCA(float* p, float* v);

  virtual StEStructEvent* next();
  virtual bool         done();

  virtual double getNPartonic();


  ClassDef(StEStructPythia,1)
};


inline bool StEStructPythia::done(){ return mAmDone; };
inline bool StEStructPythia::setInChain(bool inChain) {
    mInChain = inChain;
    return mInChain;
};
inline bool StEStructPythia::InChain(){ return mInChain; };

inline bool StEStructPythia::measureable(int pid){
  bool retVal=false;

  switch(pid){

  // I don't think Pythia can produce these, but they are
  // defined by the pdg (I think, see www.slac.stanford.edu/BFROOT/www/Computing/Environment/NewUser/htmlbug/node51.html)
  // because GEANT can make them and they are measurable so I
  // include them here. djp   Sept. 13, 2005
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
 * Revision 1.11  2012/11/16 21:23:18  prindle
 * EventCuts and TrackCuts were moved to EventReader. Remove that code from
 * these readers.
 *
 * Revision 1.10  2010/03/02 21:46:24  prindle
 *   Option to use getNPartonic as a centrality measure
 *
 * Revision 1.9  2009/02/03 14:30:23  fisyak
 * Add missing includes for ROOT 5.22
 *
 * Revision 1.8  2006/04/11 17:51:41  prindle
 *   Remove inChain from constructor arguments (no longer used in macro)
 *
 * Revision 1.7  2006/04/04 22:11:27  porter
 * StEStructPythia now uses StEtructCentrality for selection
 *
 * Revision 1.6  2006/02/22 22:05:38  prindle
 * Removed all references to multRef (?)
 *
 * Revision 1.5  2005/09/23 23:37:25  prindle
 *
 *   Starting to add vertex distribution and track acceptance dependance on
 * number of possible hits.
 *   Make Pythia interface look like Hijing interface so it now works within
 * my Fluctuation and Correlation framework.
 *
 * Revision 1.4  2004/09/24 01:43:12  prindle
 * Add call to define centrality by multiplicity.
 *
 * Revision 1.3  2004/06/25 03:13:01  porter
 * added simple trigger selection implemented like BBC-AND plus CTB
 *
 * Revision 1.2  2004/02/26 20:20:06  chunhuih
 *
 * Fixed the method globalDCA().
 * Assign the globalDCA, but leave its x, y, z coordinates zero.
 *
 * Revision 1.1  2003/11/21 06:25:00  porter
 * Pythia event generater as an StEStructEventReader
 *
 *
 *********************************************************************/
