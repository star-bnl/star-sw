// $Id: StFtpcClusterFinder.hh,v 1.4 2000/01/27 09:47:18 hummler Exp $
//
// $Log: StFtpcClusterFinder.hh,v $
// Revision 1.4  2000/01/27 09:47:18  hummler
// implement raw data reader, remove type ambiguities that bothered kcc
//
// Revision 1.3  2000/01/03 12:48:52  jcs
// Add CVS Id strings
//

#ifndef STAR_StFtpcClusterFinder
#define STAR_StFtpcClusterFinder
// #define DEBUG 1
/*#define DEBUGFILE 1*/



#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include "fcl_det.h"
#include "fcl_zrow.h"
#include "fcl_padtrans.h"
#include "fcl_timeoff.h"
#include "fcl_ampoff.h"
#include "fcl_ampslope.h"
#include "fcl_fppoint.h"
#include "tables/St_fcl_fppoint_Table.h"
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StFTPCReader.h"

#include "StFtpcCluster.hh"

#define TRUE 1
#define FALSE 0
#define sqr(x) ((x)*(x))
#define MAXNUMSEQUENCES 160
#define MAXSEQPEAKS 160
#define MAXPEAKS 160
#define MAXNUMCUC 128
#define MAXLOOPS 100
#define MAXFASTLOOPS 30
#define UNFOLDLIMIT 0.01
#define UNFOLDFAILEDLIMIT 0.50


typedef struct tagClusterUC
{
  int                  StartPad;
  int                  EndPad;
  int                  NumSequences;
  int                  CutOff;
  TPCSequence            Sequence[MAXNUMSEQUENCES];
  int                  SequencePad[MAXNUMSEQUENCES];
  struct tagClusterUC* NextClusterUC;
  int                  MemoryPtr;
} TClusterUC;

typedef struct
{
  int Timebin;
  TPCSequence Sequence;
  int height;
  int slope;
  int width;
} TPadPeak;

typedef struct
{
  int pad;
  int Timebin;
  int pad_saved;
  int Timebin_saved;
  TPCSequence Sequence;
  float TimePosition;
  float PadPosition;
  float PeakHeight;
  float OldTimePosition;
  float OldPadPosition;
  float OldPeakHeight;
  float TimeSigma;
  float PadSigma;
  float Rad;
  float Phi;
  float x;
  float y;
  float z;
} TPeak;


class StFtpcClusterFinder
{
 private:
  TPCSequence test;
 public:
  StFtpcClusterFinder();
  ~StFtpcClusterFinder();
  StFtpcCluster *search(StFTPCReader *reader,
			fcl_det_st *det,
			fcl_padtrans_st *padtrans,
			fcl_zrow_st *zrow,
			fcl_ampoff_st *ampoff,
			fcl_ampslope_st *ampslope,
			fcl_timeoff_st *timeoff,
			St_fcl_fppoint *fcl_fppoint,
			int padtransRows,
			int ampslopeRows,
			int ampoffRows,
			int timeoffRows);
  int findHits(TClusterUC *Cluster, int, int, double*, double*, FCL_DET_ST*, FCL_ZROW_ST*, float *, int *, int *, FCL_FPPOINT_ST*, FCL_AMPSLOPE_ST*, FCL_AMPOFF_ST*, FCL_TIMEOFF_ST*);//!
  int getSeqPeaksAndCalibAmp(TPCSequence*, int, int, int, TPadPeak*, int*, FCL_AMPSLOPE_ST*, FCL_AMPOFF_ST*);//!
  int fitPoints(TClusterUC*, int, int, double*, double*, TPeak*, int, FCL_DET_ST*, FCL_ZROW_ST*, float*, int*, int*, FCL_FPPOINT_ST*, FCL_TIMEOFF_ST*);//!
  int padtrans(TPeak*, int, int, FCL_DET_ST*, FCL_ZROW_ST*, double*, double*);//!
  float gauss_2d(int, int, float, float, float, float, float);//!
  float sigmax(float);//!
  float sigmat(float);//!
  int calcpadtrans(FCL_DET_ST*, FCL_PADTRANS_ST*, double*, double*);//!
  int cucInit(TClusterUC*, int*, int*);//!
  TClusterUC *cucAlloc(TClusterUC*, int*, int*);//!
  int cucFree(TClusterUC*, int*, int*, TClusterUC*);//!
};

#endif
