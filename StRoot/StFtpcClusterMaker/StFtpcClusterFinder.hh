#ifndef STAR_StFtpcClusterFinder
#define STAR_StFtpcClusterFinder
/*#define DEBUG 1*/
/*#define DEBUGFILE 1*/



#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <cmath>
#include "fcl_det.h"
#include "fcl_zrow.h"
#include "fcl_padtrans.h"
#include "fcl_ftpcadc.h"
#include "fcl_timeoff.h"
#include "fcl_ampoff.h"
#include "fcl_ampslope.h"
#include "fcl_fppoint.h"
#include "fcl_ftpcsqndx.h"
#include "fcl_ftpcadc.h"
#include "tables/St_fcl_fppoint_Table.h"

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

typedef struct 
{
  int Length;
  int StartTimebin;
  int StartADCEntry;
} TSequence;

typedef struct tagClusterUC
{
  int                  StartPad;
  int                  EndPad;
  int                  NumSequences;
  int                  CutOff;
  TSequence            Sequence[MAXNUMSEQUENCES];
  int                  SequencePad[MAXNUMSEQUENCES];
  struct tagClusterUC* NextClusterUC;
  int                  MemoryPtr;
} TClusterUC;

typedef struct
{
  int Timebin;
  TSequence Sequence;
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
  TSequence Sequence;
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
  TSequence test;
 public:
  StFtpcClusterFinder();
  ~StFtpcClusterFinder();
  StFtpcCluster *search(fcl_det_st *det,
			fcl_padtrans_st *padtrans,
			fcl_zrow_st *zrow,
			fcl_ampoff_st *ampoff,
			fcl_ampslope_st *ampslope,
			fcl_timeoff_st *timeoff,
			fcl_ftpcsqndx_st *ftpcsqndx,
			fcl_ftpcadc_st *ftpcadc,
			St_fcl_fppoint *fcl_fppoint,
			int padtransRows,
			int ampslopeRows,
			int ampoffRows,
			int timeoffRows,
			int ftpcsqndxRows);
  int findHits(TClusterUC *Cluster, int, int, double*, double*, FCL_FTPCADC_ST*, FCL_DET_ST*, FCL_ZROW_ST*, float *, int *, int *, FCL_FPPOINT_ST*, FCL_AMPSLOPE_ST*, FCL_AMPOFF_ST*, FCL_TIMEOFF_ST*);//!
  int getSeqPeaksAndCalibAmp(TSequence*, int, int, int, TPadPeak*, int*, FCL_FTPCADC_ST*, FCL_AMPSLOPE_ST*, FCL_AMPOFF_ST*);//!
  int fitPoints(TClusterUC*, int, int, double*, double*, TPeak*, int, FCL_FTPCADC_ST*, FCL_DET_ST*, FCL_ZROW_ST*, float*, int*, int*, FCL_FPPOINT_ST*, FCL_TIMEOFF_ST*);//!
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
