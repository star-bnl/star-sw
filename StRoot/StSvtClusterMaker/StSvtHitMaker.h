// $Id: StSvtHitMaker.h,v 1.21 2014/08/06 11:43:45 jeromel Exp $
// $Log: StSvtHitMaker.h,v $
// Revision 1.21  2014/08/06 11:43:45  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.20  2009/11/23 16:44:55  fisyak
// Remove references to tables
//
// Revision 1.19  2004/07/29 01:36:59  caines
// Changes for using the drift curves
//
// Revision 1.18  2004/07/07 18:09:24  caines
// Save out fraction drfi5 velocity scaler to StEvent
//
// Revision 1.17  2004/06/14 21:27:46  caines
// Fine tuning of drift velocity using laser spots from Jana Bielcikova
//
// Revision 1.16  2004/03/18 04:02:56  caines
// Remove from global scope variables used in debug mode as they shouldnt be there and caused erratic behaviour
//
// Revision 1.15  2004/01/27 02:34:55  perev
// LeakOff
//
// Revision 1.14  2003/09/10 19:47:35  perev
// ansi corrs
//
// Revision 1.13  2003/04/14 18:33:54  munhoz
// reading t0 from DB
//
// Revision 1.12  2003/01/28 20:29:02  munhoz
// new filters for clusters
//
// Revision 1.10  2002/02/22 18:43:43  caines
// Add SetFileNames function
//
// Revision 1.9  2002/02/16 22:05:06  jeromel
// Marcelo's recen changes to StSvtClusterMaker (needed to be in sync with
// StDbUtilities changes)
//
// Revision 1.8  2002/01/28 23:42:14  caines
// Move to SVT database with StSvtDbMaker
//
// Revision 1.7  2001/09/22 01:07:09  caines
// Fixes now that AddData() is cleared everyevent
//
// Revision 1.6  2001/08/07 20:52:16  caines
// Implement better packing of svt hardware and charge values
//
// Revision 1.5  2001/03/22 20:46:54  caines
// Comment out some of the QA histograms
//
// Revision 1.4  2000/11/30 20:42:08  caines
// Some more evaluation and use dataase
//
// Revision 1.3  2000/08/26 20:36:28  caines
// Adjustments for StSvtCoordinateTransform calls
//
// Revision 1.2  2000/08/24 04:26:56  caines
// Printout for debugging
//
// Revision 1.1  2000/08/21 13:03:40  caines
// First version of cluster->STAR hit maker
//
//
#ifndef STAR_StSvtHit
#define STAR_StSvtHit
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSvtHit base class                                                  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TH2.h"

//class ofstream;

class TFile;
class TNtuple;
class StSvtGeometry;
class StSvtHybridCollection;
class StSvtAnalysedHybridClusters;
class StSvtData;
class StSvtGeantHits;
class StSvtT0;
 
class StSvtHitMaker : public StMaker
{
 public: 
  StSvtHitMaker(const char *name = "SvtHit");
  StSvtHitMaker(StSvtHitMaker& svthit);
  ~StSvtHitMaker();

  virtual Int_t Init();
  virtual Int_t  InitRun(int runumber);
  virtual Int_t Make();
  virtual Int_t Finish();
  Int_t FillHistograms();
  Int_t GetSvtRawData();
  Int_t GetSvtClusterData();
  Int_t GetSvtGeometry();
  Int_t GetSvtDriftVelocity();
  Int_t GetSvtDriftCurve();
  Int_t GetSvtT0();
  void TransformIntoSpacePoint();
  void SaveIntoNtuple(int numOfCluster, int index);
  void SetWriteNtuple(int iwrite){iWrite = iwrite;};
  void SetFileNames(char* name1="/dev/null", char* name2="/dev/null");
  Int_t Eval();
  double LaserTemperatureCorrection();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StSvtHitMaker.h,v 1.21 2014/08/06 11:43:45 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}


 protected:

  int iWrite;
  //ofstream cluInfo; //!

  StSvtGeometry *m_geom; //!
  StSvtHybridCollection *mSvtCluColl; //!
  StSvtHybridCollection *mSvtGeantHitColl; //!
  StSvtHybridCollection *m_driftVeloc; //!
  StSvtHybridCollection *m_driftCurve; //!
  StSvtT0 *m_t0; //!

  StSvtAnalysedHybridClusters  *mSvtBigHit;  //!
  
  StSvtData *mSvtData; //!
  StSvtGeantHits *mSvtGeantHit;  //!
  TH2F     *m_x_vs_y;  //! x vs y of Si points
  int        mNwaf_no;  //! size of following array
  TH2F     **m_waf_no;  //! ladder no vs z of Si hit

  TNtuple *m_ClusTuple;                       //!
  TFile   *m_hfile;                           //!

  // Evaluation histos
  TH1F *mTimeHitResolution;  //!
  TH1F *mAnodeHitResolution;  //!
  TH1F *mXHitResolution;  //!
  TH1F *mYHitResolution;  //!
  TH1F *mZHitResolution;  //!
  TH2F *mHitResolution;   //!

  char* filenameN;
  char* filenameC;

  ClassDef(StSvtHitMaker,0)   //virtual base class for Makers

};


#endif
