/*********************************************************************
 *
 *  $Id: StEStructMCReader.h,v 1.3 2004/03/03 23:17:10 chunhuih Exp $
 *
 *  Author: Chunhui Han
 *
 *********************************************************************
 *
 *  Discription:
 *    Read STAR standard MC rootuples, and apply track and
 *    event cuts on it. The output is StEStructEvent.
 *
 *    Modified from the automatically generated files from ROOT.
 *
 ********************************************************************
 *
 *  $Log: StEStructMCReader.h,v $
 *  Revision 1.3  2004/03/03 23:17:10  chunhuih
 *  added mNentries to store the total entries in the rootuple, to reduce
 *  redundant code.
 *
 *  Revision 1.2  2004/03/02 21:35:10  chunhuih
 *
 *  added impact parameter information to the StEStructEvent
 *
 *  Revision 1.1  2004/02/26 20:05:33  chunhuih
 *
 *  initial import
 *
 *
 ********************************************************************/

//////////////////////////////////////////////////////////
//   This class has been automatically generated 
//     (Thu Feb 12 19:19:06 2004 by ROOT version3.10/01)
//   from TTree h999/HEPEVNT
//   found on file: evgen.1.root
//////////////////////////////////////////////////////////

#ifndef StEStructMCReader_h
#define StEStructMCReader_h

#include "StEStructPool/AnalysisMaker/StEStructEventReader.h"

#include <TROOT.h>
#include <TChain.h>
#include <TFile.h>
#include <TH1F.h>
class StEStructEventCuts;
class StEStructTrackCuts;

class StEStructMCReader : public StEStructEventReader {
 private:
  TTree          *fChain;   //!pointer to the analyzed TTree or TChain
  Int_t           fCurrent; //!current Tree number in a TChain
  //Declaration of leaves types
  Int_t           itrac;
  Int_t           istat;
  Int_t           ipdg;
  Int_t           moth1;
  Int_t           moth2;
  Int_t           idau1;
  Int_t           idau2;
  Float_t         Pxyz[3];
  Float_t         ener;
  Float_t         mass;
  Float_t         Vxyz[3];
  Float_t         Vtime;

//List of branches
  TBranch        *b_itrac;   //!
  TBranch        *b_istat;   //!
  TBranch        *b_ipdg;   //!
  TBranch        *b_moth1;   //!
  TBranch        *b_moth2;   //!
  TBranch        *b_idau1;   //!
  TBranch        *b_idau2;   //!
  TBranch        *b_Pxyz;   //!
  TBranch        *b_ener;   //!
  TBranch        *b_mass;   //!
  TBranch        *b_Vxyz;   //!
  TBranch        *b_Vtime;   //!

  int meventsToDo;
  int meventCount;
  int mloopIndex;
  bool mAmDone;
  int mrefMult;
  int mNentries;

  StEStructEventCuts *mECuts;
  StEStructTrackCuts *mTCuts;

  void fillTracks(StEStructEvent *estructEvent);
  int getTotalEventCount();
  int getCharge(int);

  const int mIPMAX;

 public:
  StEStructMCReader(TTree *tree=0);
  StEStructMCReader(int nevents, TTree *tree = 0, StEStructEventCuts *ecuts = 0, StEStructTrackCuts *tcuts = 0);
  StEStructMCReader(int nevents, char *fileListFile, StEStructEventCuts *ecuts = 0, StEStructTrackCuts *tcuts = 0);
  ~StEStructMCReader();

  void setEventCuts(StEStructEventCuts *cuts);
  void setTrackCuts(StEStructTrackCuts *cuts);
  bool hasTree();
  bool hasEventCuts();
  bool hasTrackCuts();
  bool measureable(int pid);
  float *globalDCA(float *p, float *v);

  virtual StEStructEvent* next();
  virtual bool            done();

  Int_t  Cut(Int_t entry);
  Int_t  GetEntry(Int_t entry);
  Int_t  LoadTree(Int_t entry);
  void   Init(TTree *tree);
  void   Loop();
  Bool_t Notify();
  void   Show(Int_t entry = -1);

  ClassDef(StEStructMCReader,1)
};

inline bool StEStructMCReader::hasTree() {
  return (fChain == 0);
}

inline bool StEStructMCReader::done() { return mAmDone; }

inline bool StEStructMCReader::hasEventCuts() {
  return (mECuts == 0);
}

inline bool StEStructMCReader::hasTrackCuts() {
  return (mTCuts == 0);
}

inline void StEStructMCReader::setEventCuts(StEStructEventCuts *cuts) {
  mECuts = cuts;
}

inline void StEStructMCReader::setTrackCuts(StEStructTrackCuts *cuts) {
  mTCuts = cuts;
}

#endif
