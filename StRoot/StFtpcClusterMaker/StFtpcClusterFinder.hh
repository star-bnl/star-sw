// $Id: StFtpcClusterFinder.hh,v 1.23 2009/11/25 19:50:15 jcs Exp $
//
// $Log: StFtpcClusterFinder.hh,v $
// Revision 1.23  2009/11/25 19:50:15  jcs
// remove all references to StFtpcSoftwareMonitor
//
// Revision 1.22  2007/01/15 07:49:22  jcs
// replace printf, cout and gMesMgr with Logger
//
// Revision 1.21  2006/03/19 19:29:45  jcs
// Move cluster struct definitions to StFtpcClustersStructures.hh
// Create DEBUGFILE with bfc option 'fdbg'
//
// Revision 1.20  2004/06/18 12:04:57  jcs
// replace #ifdef...#elif...#endif conditional compiler directives with #ifdef...#endif #ifdef...#endif
//
// Revision 1.19  2004/06/18 09:04:40  jcs
// replace obsolete DEBUGFILE code with code to write out a root file for cluster/laser analysis
//
// Revision 1.18  2004/05/24 13:37:57  jcs
// save number of clusters found in StFtpcSoftwareMonitor
//
// Revision 1.17  2003/05/07 15:09:49  putschke
// improvements for cathode offset corretions
//
// Revision 1.16  2003/04/15 11:35:51  putschke
// Include corrections for inner cathode offset and move some parameter to database
//
// Revision 1.15  2002/08/02 11:26:41  oldi
// Chargestep corrected (it was looping over the sequences twice).
//
// Revision 1.14  2002/07/15 13:31:01  jcs
// incorporate charge step histos into cluster finder and remove StFtpcChargeStep
//
// Revision 1.13  2002/06/04 12:33:13  putschke
// new 2-dimenisional hitfinding algorithm
// correct error in padposition numbering
//
// Revision 1.12  2002/03/01 14:22:20  jcs
// add additional histograms to monitor cluster finding
//
// Revision 1.10  2002/02/10 21:12:32  jcs
// add deltaAirPressure to calcpadtrans call
//
// Revision 1.9  2001/04/02 12:10:16  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters
// from StarDb/ftpc
//
// Revision 1.8  2001/03/06 23:33:46  jcs
// use database instead of params
//
// Revision 1.7  2001/01/25 15:25:30  oldi
// Fix of several bugs which caused memory leaks:
//  - Some arrays were not allocated and/or deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way in StFtpcTrackMaker form where Holm cut and pasted it).
//    I changed all occurences to TObjArray which makes the program slightly
//    slower but much more save (in terms of memory usage).
//
// Revision 1.6  2000/08/03 14:39:00  hummler
// Create param reader to keep parameter tables away from cluster finder and
// fast simulator. StFtpcClusterFinder now knows nothing about tables anymore!
//
// Revision 1.4  2000/01/27 09:47:18  hummler
// implement raw data reader, remove type ambiguities that bothered kcc
//
// Revision 1.3  2000/01/03 12:48:52  jcs
// Add CVS Id strings
//

#ifndef STAR_StFtpcClusterFinder
#define STAR_StFtpcClusterFinder
// #define DEBUG 1


#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include "TObjArray.h"
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StFTPCReader.h"
#include "StFtpcParamReader.hh"
#include "StFtpcDbReader.hh"
#include "TH1.h"
#include "TH2.h"

#include "StFtpcClusterDebug.hh"

#define TRUE 1
#define FALSE 0
#define sqr(x) ((x)*(x))


class StFtpcClusterFinder
{

 private:
  TPCSequence test;
  TObjArray *mPoint;
  StFTPCReader *mReader; 
  StFtpcParamReader *mParam;
  StFtpcDbReader *mDb;
  TH1F *mHistoW;
  TH1F *mHistoE;
  TH2F *mHisto;
  TH2F *mhpad, *mhtime;

  StFtpcClusterDebug *mcldebug;

  int MAXSEQPEAKS;
  int MAXPEAKS;
  int MAXLOOPS;
  int MAXFASTLOOPS;
  float UNFOLDLIMIT;
  float UNFOLDFAILEDLIMIT;
  int MAXPADLENGTH;
 
  int mMinTimeBin;
  int mMinTimeBinMed;
  int mMinTimeBinOut;

  int mMaxPadlength;
  int mMaxTimelength;
  int mMaxPadlengthMed;
  int mMaxTimelengthMed;
  int mMaxPadlengthOut;
  int mMaxTimelengthOut;

  int DeltaTime;
  int DeltaPad;
 
  int iHardRow;
  int iHardSec;

  float mOffsetCathodeWest;
  float mOffsetCathodeEast;
  
  float mAngleOffsetWest;
  float mAngleOffsetEast;
  
  float mMinChargeWindow;

 public:
  StFtpcClusterFinder(StFTPCReader *reader, 
		      StFtpcParamReader *paramReader, 
                      StFtpcDbReader    *dbReader,
		      TObjArray *pointarray,
		      TH2F *hpad,
		      TH2F *htime,
                      TH2F *histo,
                      TH1F *histoW,
                      TH1F *histoE);
// For cluster and laser run analysis
  StFtpcClusterFinder(StFTPCReader *reader, 
		      StFtpcParamReader *paramReader, 
                      StFtpcDbReader    *dbReader,
		      TObjArray *pointarray,
		      TH2F *hpad,
		      TH2F *htime,
                      TH2F *histo,
                      TH1F *histoW,
                      TH1F *histoE,
		      StFtpcClusterDebug *cldebug);

  ~StFtpcClusterFinder();
  Bool_t DebugOn;
  int search();
  int findHits(TClusterUC *Cluster, int, int, double*, double*, float *);//!
  int fitPoints(TClusterUC*, int, int, double*, double*, TPeak*, int, float*);//!
  int padtrans(TPeak*, int, int, double*, double*);//!
  float gauss_2d(int, int, float, float, float, float, float);//!
  float sigmax(float);//!
  float sigmat(float);//!
  int calcpadtrans(double*, double*, double);//!
  int cucInit(TClusterUC*, int*, int*);//!
  TClusterUC *cucAlloc(TClusterUC*, int*, int*);//!
  int cucFree(TClusterUC*, int*, int*, TClusterUC*);//!
  bool geometryCut(TClusterUC*);
};

#endif
