// $Id: StFtpcClusterFinder.hh,v 1.6 2000/08/03 14:39:00 hummler Exp $
//
// $Log: StFtpcClusterFinder.hh,v $
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
#include "TClonesArray.h"
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDAQMaker/StDAQReader.h"
#include "StDAQMaker/StFTPCReader.h"
#include "StFtpcParamReader.hh"

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
  TClonesArray *mPoint;
  StFTPCReader *mReader; 
  StFtpcParamReader *mParam;

 public:
  StFtpcClusterFinder(StFTPCReader *reader, 
		      StFtpcParamReader *paramReader, 
		      TClonesArray *pointarray);
  ~StFtpcClusterFinder();
  int search();
  int findHits(TClusterUC *Cluster, int, int, double*, double*, float *);//!
  int getSeqPeaksAndCalibAmp(TPCSequence*, int, int, int, TPadPeak*, int*);//!
  int fitPoints(TClusterUC*, int, int, double*, double*, TPeak*, int, float*);//!
  int padtrans(TPeak*, int, int, double*, double*);//!
  float gauss_2d(int, int, float, float, float, float, float);//!
  float sigmax(float);//!
  float sigmat(float);//!
  int calcpadtrans(double*, double*);//!
  int cucInit(TClusterUC*, int*, int*);//!
  TClusterUC *cucAlloc(TClusterUC*, int*, int*);//!
  int cucFree(TClusterUC*, int*, int*, TClusterUC*);//!
};

#endif
