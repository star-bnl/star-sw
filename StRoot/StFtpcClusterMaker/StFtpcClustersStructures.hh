// $Id: StFtpcClustersStructures.hh,v 1.2 2006/03/19 19:29:45 jcs Exp $
//
// $Log: StFtpcClustersStructures.hh,v $
// Revision 1.2  2006/03/19 19:29:45  jcs
// Move cluster struct definitions to StFtpcClustersStructures.hh
// Create DEBUGFILE with bfc option 'fdbg'
//

#define MAXNUMSEQUENCES 160
#define MAXNUMCUC 128

typedef struct tagClusterUC
{
  int                  StartPad;
  int                  EndPad;
  int                  NumSequences;
    int                  CutOff;
  TPCSequence          Sequence[MAXNUMSEQUENCES];
  int                  SequencePad[MAXNUMSEQUENCES];
  struct tagClusterUC* NextClusterUC;
  int                  MemoryPtr;
} TClusterUC;

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
