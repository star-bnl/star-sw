/**********************************************************************
 *
 * $Id: StEStructEventCuts.h,v 1.16 2011/08/02 20:31:25 prindle Exp $
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
#include "TH1F.h"
#include "TH2F.h"

class StEStructEventCuts : public StEStructCuts {

protected:

   CutName mtWordName;
   CutName mpVertexZName;
   CutName mpVPDVertexName;
   CutName mpVertexRadiusName;
   CutName mcentralityName;  
   CutName mgoodtoffractionName;  
   CutName mgoodprimaryfractionName;  
   CutName mZVertSepName;  
   CutName mZVertMatchName;  


  char         mRunPeriod[1024];
  bool         mtrgByRunPeriod;
  unsigned int mtWord[2];  
  float        mpVertexZ[2]; 
  float        mpVPDVertex[2]; 
  float        mpVertexRadius[2]; 
  float        mcentrality[2];
  float        mgoodtoffraction[2];
  float        mgoodprimaryfraction[2];
  float        mZVertSep[2];
  float        mZVertMatch[2];
  int badTrigger;

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
  bool goodVertexTopology(StMuDst* muDst);
  bool goodPrimaryVertexZ( float z );
  bool goodVPDVertex( float dz );
  bool goodPrimaryVertexRadius( float r );
  bool hasPrimaryVertexRadiusCut();
  bool goodCentrality( float n);
  bool goodToFFraction(int ndEdx, int nToF);
  bool hasToFFractionCut();
  bool goodPrimaryFraction(int ndPrimary, int nGlobal);
  bool hasPrimaryFractionCut();
  bool goodZVertSep(float dz);
  bool hasZVertSepCut();
  bool goodZVertMatch(float dz);
  bool hasZVertMatchCut();

  char* triggerWordName(){ return (char*)mtWordName.name; };
  char* primaryVertexZName() { return (char*) mpVertexZName.name; };
  char* primaryVPDVertexName() { return (char*) mpVPDVertexName.name; };
  char* primaryVertexRadiusName() { return (char*) mpVertexRadiusName.name; };
  char* centralityName() { return (char*) mcentralityName.name; };
  char* goodtoffractionName() { return (char*) mgoodtoffractionName.name; };
  char* goodprimaryfractionName() { return (char*) mgoodprimaryfractionName.name; };
  char* zVertSepName() { return (char*) mZVertSepName.name; };
  char* zVertMatchName() { return (char*) mZVertMatchName.name; };


  ClassDef(StEStructEventCuts,1)
    
};

inline void StEStructEventCuts::loadUserCuts(const char* name, const char** vals, int nvals){}

inline bool StEStructEventCuts::goodPrimaryVertexZ(float z) {
  mvalues[mpVertexZName.idx] = z;
  if (mpVertexZ[0]==mpVertexZ[1] && mpVertexZ[0]==0) {
    return true;
  }
  return (z>=mpVertexZ[0] && z<=mpVertexZ[1]);
}

inline bool StEStructEventCuts::goodVPDVertex(float dz) {
  mvalues[mpVPDVertexName.idx] = dz;
  if (mpVPDVertex[0]==mpVPDVertex[1] && mpVPDVertex[0]==0) {
    return true;
  }
  return (dz>=mpVPDVertex[0] && dz<=mpVPDVertex[1]);
}

inline bool StEStructEventCuts::goodPrimaryVertexRadius(float r) {
  mvalues[mpVertexRadiusName.idx] = r;
  if (mpVertexRadius[0]==mpVertexRadius[1] && mpVertexRadius[0]==0) {
    return true;
  }
  return (r>=mpVertexRadius[0] && r<=mpVertexRadius[1]);
}
inline bool StEStructEventCuts::hasPrimaryVertexRadiusCut() {
  return (mpVertexRadius[1]!=0);
}

inline bool StEStructEventCuts::goodCentrality(float c){
  mvalues[mcentralityName.idx] = c;
  return (  (mcentrality[0]==mcentrality[1] && mcentrality[0]==0) ||
            (c>=mcentrality[0] && c<=mcentrality[1]) );
}

inline bool StEStructEventCuts::goodToFFraction(int ndEdx, int nToF) {
    if (mgoodtoffraction[0]==mgoodtoffraction[1] && mgoodtoffraction[0]==0) {
        return true;
    }
    // Need to check this cut at 2 is reasonable. May cause problem at low multiplicity
    if (nToF < 2) {
        return false;
    }
    float sToF = nToF - ndEdx * mgoodtoffraction[0];
    if ((fabs(sToF) < mgoodtoffraction[1]) &&
        (sToF > -0.5*ndEdx)) {
        return true;
    }
    return false;
}
inline bool StEStructEventCuts::hasToFFractionCut() {
    return (mgoodtoffraction[0]!=mgoodtoffraction[1]);
}

inline bool StEStructEventCuts::goodPrimaryFraction(int nPrimary, int nGlobal) {
    if (mgoodprimaryfraction[0]==mgoodprimaryfraction[1] && mgoodprimaryfraction[0]==0) {
        return true;
    }
    float dPrim = nGlobal/mgoodprimaryfraction[0] - nPrimary;
    if (dPrim < mgoodprimaryfraction[1]) {
        return true;
    }
    return false;
}
inline bool StEStructEventCuts::hasPrimaryFractionCut() {
    return (mgoodprimaryfraction[0]!=mgoodprimaryfraction[1]);
}

inline bool StEStructEventCuts::goodZVertSep(float dz){
  mvalues[mZVertSepName.idx] = dz;
  return (  (mZVertSep[0]==mZVertSep[1] && mZVertSep[0]==0) ||
            (dz<mZVertSep[0] || mZVertSep[1]<dz) );
}
inline bool StEStructEventCuts::hasZVertSepCut() {
    return (mZVertSep[0]!=mZVertSep[1]);
}

inline bool StEStructEventCuts::goodZVertMatch(float dz){
  // If dz is between cuts we want to reject vertex
  mvalues[mZVertMatchName.idx] = dz;
  return (  (mZVertMatch[0]==mZVertMatch[1] && mZVertMatch[0]==0) ||
            (dz<mZVertMatch[0] || mZVertMatch[1]<dz) );
}
inline bool StEStructEventCuts::hasZVertMatchCut() {
    return (mZVertMatch[0]!=mZVertMatch[1]);
}

#endif

/***********************************************************************
 *
 * $Log: StEStructEventCuts.h,v $
 * Revision 1.16  2011/08/02 20:31:25  prindle
 *   Change string handling
 *   Added event cuts for VPD, good fraction of global tracks are primary, vertex
 *   found only from tracks on single side of TPC, good fraction of primary tracks have TOF hits..
 *   Added methods to check if cuts imposed
 *   Added 2010 200GeV and 62 GeV, 2011 19 GeV AuAu datasets, 200 GeV pp2pp 2009 dataset.
 *   Added TOF vs. dEdx vs. p_t histograms
 *   Fix participant histograms in QAHists.
 *   Added TOFEMass cut in TrackCuts although I think we want to supersede this.
 *
 * Revision 1.15  2010/09/02 21:20:09  prindle
 *   Cuts:   Add flag to not fill histograms. Important when scanning files for sorting.
 *   EventCuts: Add radius cut on vertex, ToF fraction cut. Merge 2004 AuAu 200 GeV datasets.
 *              Add 7, 11 and 39 GeV dataset selections
 *   MuDstReader: Add 2D histograms for vertex radius and ToF fraction cuts.
 *                Modify countGoodTracks to return number of dEdx and ToF pid identified tracks.
 *                Include code to set track pid information from Dst.
 *   QAHists: New ToF QA hists. Modify dEdx to include signed momentum.
 *
 * Revision 1.14  2010/03/02 21:43:38  prindle
 *   Use outerHelix() for global tracks
 *   Add sensible triggerId histograms
 *   Starting to add support to sort events (available for Hijing)
 *
 * Revision 1.13  2008/12/02 23:35:34  prindle
 * Added code for pileup rejection in EventCuts and MuDstReader.
 * Modified trigger selections for some data sets in EventCuts.
 *
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








