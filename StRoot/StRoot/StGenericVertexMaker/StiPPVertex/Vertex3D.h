#ifndef Vertex3D_h
#define Vertex3D_h

#include "StGenericVertexMaker/StiPPVertex/TrackData.h"
using namespace std;  // for vector

class TH1;
class TObjArray;

class Vertex3D {
 private:
  enum {mxHA=16, mxHE=10};
  TH1 *hA[mxHA];
  TH1 *hYX[mxHE], *hYZ[mxHE]; // event histogram
  int nHE;// counter of used histos

  vector<TrackData*> track;
  float cut_pT1,cut_pT2, cut_sigY;
  unsigned int  cut_numTrack;
  int   isFound;

 public:
  Vertex3D();
  virtual  ~Vertex3D();
  void clearEvent();
  void clearTracks();
  void initRun();
  void addTrack(TrackData*);
  void study(TVector3 r, int eveID);
  void doExtrapolation(); // in both directions
  void initHisto(TObjArray* );
  void setCuts(float pT1 ,float pT2 , float sigY, int nTr){cut_pT1 = pT1;cut_pT2 = pT2; cut_sigY=sigY; cut_numTrack=nTr;}
  // get-methods
  bool isValid(){ return isFound;}
  void dumpPrimTracks4beamLine(float z0, int eveID);
  void trackChi2QA(float z0);
};


#endif
