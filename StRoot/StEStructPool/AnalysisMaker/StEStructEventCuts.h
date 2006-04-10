/**********************************************************************
 *
 * $Id: StEStructEventCuts.h,v 1.7 2006/04/10 23:40:40 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  Cut class for event level quantities
 *
 *
 ***********************************************************************/
#ifndef __STEBYEEVENTCUTS__H
#define __STEBYEEVENTCUTS__H


#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuL3EventSummary.h"
#include "StEStructCuts.h"
#include "Stiostream.h"

class StEStructEventCuts : public StEStructCuts {

protected:

   CutName mtWordName;
   CutName mpVertexZName;
   CutName mcentralityName;  


  char         mRunPeriod[1024];
  bool         mtrgByRunPeriod;
  unsigned int mtWord[2];  
  float        mpVertexZ[2]; 
  float        mcentrality[2];
  
  void init();
  void initCuts();
  void initNames();

public:

  StEStructEventCuts();
  StEStructEventCuts(const char* cutFileName);
  virtual ~StEStructEventCuts();

  virtual bool loadBaseCuts(const char* name, const char** vals, int nvals);
  virtual void loadUserCuts(const char* name, const char** vals, int nvals);
  virtual void printCutStats(ostream& ofs);


  bool goodTrigger(StMuEvent* muEvent);
  bool goodPrimaryVertexZ( float z );
  bool goodCentrality( float n);

  char* triggerWordName(){ return (char*)mtWordName.name; };
  char* primaryVertexZName() { return (char*) mpVertexZName.name; };
  char* centralityName() { return (char*) mcentralityName.name; };


  ClassDef(StEStructEventCuts,1)
    
};

inline void StEStructEventCuts::loadUserCuts(const char* name, const char** vals, int nvals){}

inline bool StEStructEventCuts::goodTrigger(StMuEvent* muEvent){

  if(mtrgByRunPeriod){

    if (!strcmp("CuCu62GeVProductionMinBias2005",mRunPeriod)) {
        if (muEvent->triggerIdCollection().nominal().isTrigger(76007) ||
            muEvent->triggerIdCollection().nominal().isTrigger(76011)) {
            return true;
        }
    } else if (!strcmp("CuCu200GeVProductionMinBias2005",mRunPeriod)) {
        if (muEvent->triggerIdCollection().nominal().isTrigger(66007)) {
            return true;
        }
    } else if (!strcmp("AuAu200GeVMinBias2004",mRunPeriod)) {
        if (muEvent->triggerIdCollection().nominal().isTrigger(25007) ||
            muEvent->triggerIdCollection().nominal().isTrigger(15007) ||
            muEvent->triggerIdCollection().nominal().isTrigger(15003)) {
            return true;
        }
    } else if (!strcmp("AuAu62GeVMinBias2004",mRunPeriod)) {
        if (((muEvent->triggerIdCollection().nominal().isTrigger(35004) ||
              muEvent->triggerIdCollection().nominal().isTrigger(35007))
            ||
            ((muEvent->triggerIdCollection().nominal().isTrigger(35001) ||
              muEvent->triggerIdCollection().nominal().isTrigger(35009) )
              && muEvent->ctbMultiplicity()>15))) {
            return true;
        }
    } else if (!strcmp("AuAu200GeVMinBias2001",mRunPeriod)) {
        StMuL3EventSummary l3 = muEvent->l3EventSummary();
        if (!(l3.unbiasedTrigger())) {
            return false;
        }
        unsigned int t = muEvent->l0Trigger().triggerWord();
        if ( 0x1000 == t ) {
            return true;
        }
    } else if(!strcmp("ppMinBias",mRunPeriod)){
      if(muEvent->triggerIdCollection().nominal().isTrigger(8192)) return true;
    }

  } else {
        unsigned int t = muEvent->l0Trigger().triggerWord();
        mvalues[mtWordName.idx] = (float)t;
        return ( (mtWord[0]==mtWord[1] && mtWord[0]==0) ||
                 (t>=mtWord[0] && t<=mtWord[1])  ) ;
  }

    return false;
}

inline bool StEStructEventCuts::goodPrimaryVertexZ(float z) {
  mvalues[mpVertexZName.idx] = z;
  if (mpVertexZ[0]==mpVertexZ[1] && mpVertexZ[0]==0) {
    return true;
  }
  return (z>=mpVertexZ[0] && z<=mpVertexZ[1]);
}

inline bool StEStructEventCuts::goodCentrality(float c){
  mvalues[mcentralityName.idx] = c;
  return (  (mcentrality[0]==mcentrality[1] && mcentrality[0]==0) ||
            (c>=mcentrality[0] && c<=mcentrality[1]) );
}

#endif

/***********************************************************************
 *
 * $Log: StEStructEventCuts.h,v $
 * Revision 1.7  2006/04/10 23:40:40  porter
 * Fixed the minbias trigger definition for the CuCu 200 2005 run
 *
 * Revision 1.6  2006/04/04 22:05:05  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.5  2006/02/22 22:03:18  prindle
 * Removed all references to multRef
 *
 * Revision 1.4  2005/09/14 17:08:34  msd
 * Fixed compiler warnings, a few tweaks and upgrades
 *
 * Revision 1.3  2005/09/07 20:18:40  prindle
 *   AnalysisMaker: Keep track of currentAnalysis (for use in doEStruct macro)
 *   EventCuts.h:   Added trigger cuts including cucu and year 4.
 *   MuDstReader:   Added dE/dx histograms. Re-arranged code to count tracks
 *                    before making centrality cut.
 *   TrackCuts:     Random changes. Moved some variables from private to public.o
 *
 * Revision 1.2  2004/09/24 01:41:42  prindle
 * Allow for cuts to be defined by character strings. I use this to select trigger
 * cuts appropriate for run periods
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/








