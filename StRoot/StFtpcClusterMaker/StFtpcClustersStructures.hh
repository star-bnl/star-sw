#define MAXNUMSEQUENCES 160
//#define MAXSEQPEAKS 160
//#define MAXPEAKS 160
#define MAXNUMCUC 128
//#define MAXLOOPS 100
//#define MAXFASTLOOPS 30
//#define UNFOLDLIMIT 0.01
//#define UNFOLDFAILEDLIMIT 0.5


struct TClusterUC
{
  int                  StartPad;
  int                  EndPad;
  int                  NumSequences;
  int                  CutOff;
  TPCSequence            Sequence[MAXNUMSEQUENCES];
  int                  SequencePad[MAXNUMSEQUENCES];
  struct TClusterUC* NextClusterUC;
  int                  MemoryPtr;
};

struct TPadPeak
{
  int Timebin;
  //int pad;
  TPCSequence Sequence;
  //int height;
  float height;
  int slope;
  int width;
};

struct TClusterMaxima 
{
  int nTimeMaxima;
  int SeqFirst;
  int SeqLength;
  int pad;
  TPadPeak TimeMax[MAXNUMSEQUENCES]; // genauer wie gross usw.
};

struct TPeak
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
};
