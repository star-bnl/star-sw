/**********************************************************************
 *
 * $Id: StEStructQAHists.h,v 1.8 2012/11/16 21:19:07 prindle Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description:  base class for QA histogramming
 *
 ***********************************************************************/
#ifndef _STEStructQAHists_H
#define _STEStructQAHists_H

#include "StEStructEventReader.h"
#include "TROOT.h"

class TH1D;
class TH1F;
class TH2F;
class StEStructTrack;

class StEStructQAHists : public TObject {

 protected:

  int  mEType; //event type: 0=data, 1=hij, 2=pyth, 3... organize as you need

  TH1D * mCents[2];
  TH1D * mptCents[3];
  TH1D * mTotMult;
  TH1D * mRefMult;
  TH1D * mPosMult;
  TH1D * mNegMult;
  TH1D * mTotMult4;
  TH1D * mPosMult4;
  TH1D * mNegMult4;

  // --- a base set for aa event generators (e.g. hijing)

  TH1D * aaGenImpact;
  TH2F * aaGen[2];
  TH1D ** aaGenBin;
  TH1D ** aaGenPart;

  // --- a base set for aa event generators (e.g. hijing)

  TH1F ** ppELines;
  TH1F ** ppALines;

  //--- a basic track set

  bool mhasAIndex;
  int mntBins;
  TH1F ** mHEta;
  TH1F ** mHPhi;
  TH1F ** mHPt;
  TH1F ** mHYt;
  TH1F ** mHMass;
  TH2F ** mHdEdxPtot;
  TH2F ** mHToFPtot;
  TH2F ** mHEtaPt;

  void initBaseHistograms();
  void fillBaseHistograms(StEStructEvent* event, StEStructEventReader* reader);
  void writeBaseHistograms(TFile* tf);

 public:

  StEStructQAHists(int itype=0);
  virtual ~StEStructQAHists();

  virtual void initHistograms();
  virtual void fillHistograms(StEStructEvent* event, StEStructEventReader* reader);
  virtual void writeHistograms(TFile* tf);

  virtual void initTrackHistograms(int numBins, int analysisIndex=-1);
  virtual void fillTrackHistograms(StEStructTrack* track, int ibin);
  virtual void writeTrackHistograms(TFile* tf);

//  virtual void initPairHistograms(int numBins, int analysisIndex=-1);
//  virtual void fillPairHistograms(StEStructTrack* track1, StEStructTrack* track2, int ibin, int after);
//  virtual void writePairHistograms(TFile* tf);

  ClassDef(StEStructQAHists,1)
};

#endif

/**********************************************************************
 *
 * $Log: StEStructQAHists.h,v $
 * Revision 1.8  2012/11/16 21:19:07  prindle
 * Moved EventCuts, TrackCuts to EventReader. Affects most readers.
 * Added support to write and read EStructEvents.
 * Cuts: 3D histo support, switch to control filling of histogram for reading EStructEvents
 * EventCuts: A few new cuts
 * MuDstReader: Add 2D to some histograms, treat ToFCut, PrimaryCuts, VertexRadius histograms like other cut histograms.
 * QAHists: Add refMult
 * TrackCuts: Add some hijing cuts.
 *
 * Revision 1.7  2011/08/02 20:31:25  prindle
 *   Change string handling
 *   Added event cuts for VPD, good fraction of global tracks are primary, vertex
 *   found only from tracks on single side of TPC, good fraction of primary tracks have TOF hits..
 *   Added methods to check if cuts imposed
 *   Added 2010 200GeV and 62 GeV, 2011 19 GeV AuAu datasets, 200 GeV pp2pp 2009 dataset.
 *   Added TOF vs. dEdx vs. p_t histograms
 *   Fix participant histograms in QAHists.
 *   Added TOFEMass cut in TrackCuts although I think we want to supersede this.
 *
 * Revision 1.6  2010/09/02 21:20:09  prindle
 *   Cuts:   Add flag to not fill histograms. Important when scanning files for sorting.
 *   EventCuts: Add radius cut on vertex, ToF fraction cut. Merge 2004 AuAu 200 GeV datasets.
 *              Add 7, 11 and 39 GeV dataset selections
 *   MuDstReader: Add 2D histograms for vertex radius and ToF fraction cuts.
 *                Modify countGoodTracks to return number of dEdx and ToF pid identified tracks.
 *                Include code to set track pid information from Dst.
 *   QAHists: New ToF QA hists. Modify dEdx to include signed momentum.
 *
 * Revision 1.5  2010/06/23 22:29:50  prindle
 *   Hadd typo of 2004B instead of B2004 in EventCuts.cxx
 *   Added a couple of histograms in QAHists.
 *
 * Revision 1.4  2008/03/19 22:02:00  prindle
 * Updated some dataset definitions.
 *
 * Revision 1.3  2007/11/26 19:52:25  prindle
 * Add cucu62, cucu200 2007ib production datasets.
 * Included vertex cuts for case of ranked vertices. (Pass muDst pointer to EventCuts)
 * Add n^(1/4) histograms to QAHists
 *
 * Revision 1.2  2006/04/27 22:20:12  prindle
 * Some changes in trigger names for run periods.
 * Changed a couple of the Hijing QA histograms.
 *
 * Revision 1.1  2006/04/04 22:05:06  porter
 * a handful of changes:
 *  - changed the StEStructAnalysisMaker to contain 1 reader not a list of readers
 *  - added StEStructQAHists object to contain histograms that did exist in macros or elsewhere
 *  - made centrality event cut taken from StEStructCentrality singleton
 *  - put in  ability to get any max,min val from the cut class - one must call setRange in class
 *
 *
 *********************************************************************/
