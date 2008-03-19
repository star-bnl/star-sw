/**********************************************************************
 *
 * $Id: StEStructEventCuts.h,v 1.12 2008/03/19 22:01:59 prindle Exp $
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


#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
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


  bool goodTrigger(StMuDst* muDst);
  bool goodPrimaryVertexZ( float z );
  bool goodCentrality( float n);

  char* triggerWordName(){ return (char*)mtWordName.name; };
  char* primaryVertexZName() { return (char*) mpVertexZName.name; };
  char* centralityName() { return (char*) mcentralityName.name; };


  ClassDef(StEStructEventCuts,1)
    
};

inline void StEStructEventCuts::loadUserCuts(const char* name, const char** vals, int nvals){}

inline bool StEStructEventCuts::goodTrigger(StMuDst* muDst) {

  StMuEvent* muEvent = muDst->event();
  if (mtrgByRunPeriod) {

    if (!strcmp("CuCu22GeVProductionMinBiasP05if",mRunPeriod)) {
        // Accept all triggers.
        return true;
    } else if (!strcmp("CuCu62GeVProductionMinBias2005",mRunPeriod)) {
        if (muEvent->triggerIdCollection().nominal().isTrigger(76007) ||
            muEvent->triggerIdCollection().nominal().isTrigger(76011)) {
            return true;
        }
    } else if (!strcmp("CuCu200GeVProductionMinBias2005",mRunPeriod)) {
        if (muEvent->triggerIdCollection().nominal().isTrigger(66007)) {
            return true;
        }
    } else if (!strcmp("CuCu62GeVProductionMinBias2007ic",mRunPeriod)) {
        // Assume vertex characteristics essentially the same
        // as the 200GeV CuCu data.
        if (muEvent->runNumber()<6069077) {
            if (!muEvent->triggerIdCollection().nominal().isTrigger(76002) &&
                !muEvent->triggerIdCollection().nominal().isTrigger(76011)) {
                return false;
            }
        } else {
            if (!muEvent->triggerIdCollection().nominal().isTrigger(76007) &&
                !muEvent->triggerIdCollection().nominal().isTrigger(76011)) {
                return false;
            }
        }
        bool keep = false;
        if (muDst->event()->refMult() >= 17) {
            if (fabs(muDst->primaryVertex()->meanDip())/muDst->event()->ctbMultiplicity() < (0.8/800)) {
                 keep = true;
            }
        } else {
            if (muDst->primaryVertex()->ranking()>-2.5) {
                keep = true;
            }
        }
        return keep;
    } else if (!strcmp("CuCu200GeVProductionMinBias2007ic",mRunPeriod)) {
        if (!muEvent->triggerIdCollection().nominal().isTrigger(66007)) {
            return false;
        }
        bool keep = false;
        if (muDst->event()->refMult() >= 17) {
            if (fabs(muDst->primaryVertex()->meanDip())/muDst->event()->ctbMultiplicity() < (0.8/800)) {
                 keep = true;
            }
        } else {
            if (muDst->primaryVertex()->ranking()>-2.5) {
                keep = true;
            }
        }
        return keep;
    } else if (!strcmp("2007LowLuminosity",mRunPeriod)) {
        // This is 200GeV AuAu data from 2007
        // First triggerId has killer bits on, second killer bits off.
        // I don't know what killer bits are. Are they important?
        // Trigger page suggests ID 200001 should be in mbvpd.
        // Multiplicity distribution of that id is cleary wrong.
        if (!muEvent->triggerIdCollection().nominal().isTrigger(200001) &&
            !muEvent->triggerIdCollection().nominal().isTrigger(200003) &&
            !muEvent->triggerIdCollection().nominal().isTrigger(200013) &&
            !muEvent->triggerIdCollection().nominal().isTrigger(200020)) {
            return false;
        }
        // Not sure these vertex criteria have been optimized.
        bool keep = false;
        if (muDst->event()->refMult() >= 17) {
            if (fabs(muDst->primaryVertex()->meanDip())/muDst->event()->ctbMultiplicity() < (0.8/800)) {
                 keep = true;
            }
        } else {
            if (muDst->primaryVertex()->ranking()>-2.5) {
                keep = true;
            }
        }
        return keep;
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
    } else if (!strcmp("AuAu200GeVCentral2001",mRunPeriod)) {
        StMuL3EventSummary l3 = muEvent->l3EventSummary();
        if (!(l3.unbiasedTrigger())) {
	        return false;
        }
        unsigned int t = muEvent->l0Trigger().triggerWord();
        if ( 0x1100 == t ) {
	        return true;
       }
    } else if (!strcmp("dAu200GeVMinBias2003",mRunPeriod)) {
        if (muEvent->triggerIdCollection().nominal().isTrigger(2001) ||
	        muEvent->triggerIdCollection().nominal().isTrigger(2003)) {
            return true;
        }
    } else if(!strcmp("ppMinBias",mRunPeriod)){
        if(muEvent->triggerIdCollection().nominal().isTrigger(8192)) {
            return true;
        }
    } else if(!strcmp("ppProductionMinBias2005",mRunPeriod)){
        if(muEvent->triggerIdCollection().nominal().isTrigger(96011) ||
           muEvent->triggerIdCollection().nominal().isTrigger(106011)) {
               return true;
        }
    } else if(!strcmp("pp400MinBias2005",mRunPeriod)){
        // Not 100% sure of these triggers. Small data set, probably not worth spending much time on.
        if(muEvent->triggerIdCollection().nominal().isTrigger(96011) ||
           muEvent->triggerIdCollection().nominal().isTrigger(106011)) {
               return true;
        }
    } else if(!strcmp("pp2006MinBias2006",mRunPeriod)){
        if(muEvent->triggerIdCollection().nominal().isTrigger(117001)) {
               return true;
        }
    } else if(!strcmp("ppProductionMB622006",mRunPeriod)){
        if(muEvent->triggerIdCollection().nominal().isTrigger(147001)) {
               return true;
        }
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
 * Revision 1.12  2008/03/19 22:01:59  prindle
 * Updated some dataset definitions.
 *
 * Revision 1.11  2007/11/27 23:00:02  prindle
 * Added cucu22 GeV data set as possible input
 *
 * Revision 1.10  2007/11/26 19:52:24  prindle
 * Add cucu62, cucu200 2007ib production datasets.
 * Included vertex cuts for case of ranked vertices. (Pass muDst pointer to EventCuts)
 * Add n^(1/4) histograms to QAHists
 *
 * Revision 1.9  2006/04/27 22:20:07  prindle
 * Some changes in trigger names for run periods.
 * Changed a couple of the Hijing QA histograms.
 *
 * Revision 1.8  2006/04/25 21:02:51  msd
 * Added AuAu200GeVCentral2001 and dAu200GeVMinBias2003
 *
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








