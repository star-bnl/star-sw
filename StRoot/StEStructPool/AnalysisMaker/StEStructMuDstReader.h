/**********************************************************************
 *
 * $Id: StEStructMuDstReader.h,v 1.13 2012/11/16 21:19:07 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  event reader class for common MuDsts
 *               Uses the StMuDstMaker for real reading
 *
 ***********************************************************************/
#ifndef __STESTRUCTMUEVENTREADER__H
#define __STESTRUCTMUEVENTREADER__H

#include "TH1F.h"
#include "TH2F.h"
#include "TH3F.h"
#include "StBTofHeader.h"
#include "StEStructEventReader.h"
#include "StEStructPool/Pileup/Pileup.h"

class StMuDstMaker;
class StMuTrack;

class StEStructEventCuts;
class StEStructTrackCuts;
class StEStructTrack;


class StEStructMuDstReader : public StEStructEventReader {

public:

  StMuDstMaker* mMaker; //!
  bool mInChain;
  bool mAmDone;
  bool mUseGlobalTracks;
  int  mPrimaryVertexId;
  int  mNumGoodTracks;//!
  int  mhasdEdxCuts;
  int  mhasToFCuts;
  int  mhasPrimaryCuts;
  int  mhasVertexRadiusCuts;
  TH3F*  dEdxBetaBefore;
  TH3F*  dEdxBetaAfter;
  TH2F*  dEdxBefore;
  TH2F*  dEdxAfter;
  TH2F*  ToFBefore;
  TH2F*  ToFAfter;
  TH2F*  PrimaryBefore;
  TH2F*  PrimaryAfter;
  TH2F*  VRadiusBefore;
  TH2F*  VRadiusAfter;

  Float_t mEta;
  Float_t mPhi;

  Pileup      *mPileup;

  StEStructMuDstReader();
  StEStructMuDstReader(StMuDstMaker* maker,
                       StEStructEventCuts* ecuts,
                       StEStructTrackCuts* tcuts);
  virtual ~StEStructMuDstReader();

  void setMuDstMaker(StMuDstMaker* MuDstMaker, bool inChain=true);
  void setUseGlobalTracks(bool global=false);
  bool setInChain(bool inChain);
  bool InChain();
  bool hasMaker();

  virtual StEStructEvent* next();
  virtual bool         done();

  void setEventCuts(StEStructEventCuts* cuts);
  void setTrackCuts(StEStructTrackCuts* cuts);
  StEStructEvent* fillEvent();
  bool fillTracks(StEStructEvent* estructEvent);
  bool isTrackGood(StMuTrack* track);
  bool isTrackGoodToUse(StMuTrack* track);
  int  countGoodTracks(int *ndEdx, int *nToF);
  void fillEStructTrack(StEStructTrack* eTrack, StMuTrack* mTrack);
  
  ClassDef(StEStructMuDstReader,1)

};

inline bool StEStructMuDstReader::done(){ return mAmDone; };

#endif  

/***********************************************************************
 *
 * $Log: StEStructMuDstReader.h,v $
 * Revision 1.13  2012/11/16 21:19:07  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.12  2011/08/02 20:31:25  prindle
 *   Change string handling
 *   Added event cuts for VPD, good fraction of global tracks are primary, vertex
 *   found only from tracks on single side of TPC, good fraction of primary tracks have TOF hits..
 *   Added methods to check if cuts imposed
 *   Added 2010 200GeV and 62 GeV, 2011 19 GeV AuAu datasets, 200 GeV pp2pp 2009 dataset.
 *   Added TOF vs. dEdx vs. p_t histograms
 *   Fix participant histograms in QAHists.
 *   Added TOFEMass cut in TrackCuts although I think we want to supersede this.
 *
 * Revision 1.11  2010/09/02 21:20:09  prindle
 *   Cuts:   Add flag to not fill histograms. Important when scanning files for sorting.
 *   EventCuts: Add radius cut on vertex, ToF fraction cut. Merge 2004 AuAu 200 GeV datasets.
 *              Add 7, 11 and 39 GeV dataset selections
 *   MuDstReader: Add 2D histograms for vertex radius and ToF fraction cuts.
 *                Modify countGoodTracks to return number of dEdx and ToF pid identified tracks.
 *                Include code to set track pid information from Dst.
 *   QAHists: New ToF QA hists. Modify dEdx to include signed momentum.
 *
 * Revision 1.10  2010/03/02 21:43:38  prindle
 *   Use outerHelix() for global tracks
 *   Add sensible triggerId histograms
 *   Starting to add support to sort events (available for Hijing)
 *
 * Revision 1.9  2008/12/02 23:35:35  prindle
 * Added code for pileup rejection in EventCuts and MuDstReader.
 * Modified trigger selections for some data sets in EventCuts.
 *
 * Revision 1.8  2008/05/01 23:35:57  prindle
 *   Found that for global tracks we sometimes have global dca = (0,0,0)
 * Now use dca() when we are using global tracks.
 *
 * Revision 1.7  2006/04/11 17:50:48  prindle
 *   Remove inChain from constructor arguments (no longer used in macro)
 *
 * Revision 1.6  2006/04/04 22:05:06  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 * Revision 1.5  2006/02/22 22:03:24  prindle
 * Removed all references to multRef
 *
 * Revision 1.4  2005/11/22 14:40:04  msd
 * Changed default of useAllTracks
 *
 * Revision 1.3  2005/09/14 17:08:35  msd
 * Fixed compiler warnings, a few tweaks and upgrades
 *
 * Revision 1.2  2005/09/07 20:18:43  prindle
 *   AnalysisMaker: Keep track of currentAnalysis (for use in doEStruct macro)
 *   EventCuts.h:   Added trigger cuts including cucu and year 4.
 *   MuDstReader:   Added dE/dx histograms. Re-arranged code to count tracks
 *                    before making centrality cut.
 *   TrackCuts:     Random changes. Moved some variables from private to public.o
 *
 * Revision 1.1  2003/10/15 18:20:32  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/
