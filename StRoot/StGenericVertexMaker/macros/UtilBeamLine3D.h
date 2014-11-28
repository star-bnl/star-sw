#ifndef UtilBeamLine3D_h
#define UtilBeamLine3D_h

#include <TVector3.h>
#include <vector>
using namespace std;  // for vector
class TObjArray;
class TH1;
enum{mxPar=4}; // # of beam line params


class TrackStump{
 public:
  TVector3 r,p ; // track parameters, direction versor |p|=1
  float ery2,eryz,erz2; // squared error of track (only some)
  int nFitP,eveId;
  float chi2,z0;
  float P,Pt; // momentum, not sure if relevant
  int bad;// flag to discard bad tracks
  void print() {
    printf("r=%.2f %.2f %.2f  pu=%.3f %.3f %.3f   ery2=%.3f eryz=%.3f erz2=%.3f   nfp=%d chi=%.2f  zV=%.1f eveId=%d P=%.1f Pt=%.1f bad=%d\n",r.x(),r.y(),r.z(),p.x(),p.y(),p.z(),ery2,eryz,erz2,nFitP,chi2,z0,eveId,P,Pt,bad);
 }
};


class UtilBeamLine3D {
  enum {mxH=32};

  // QA params
  float cut_maxRxy, cut_maxZ; // (cm) track QA
  float cut_minChi2,cut_maxChi2; // track QA
  float cut_minP; // (GeV/c)  track momentum QA
  int par_filter; // selecting subset of track, see .cxx for definition

 public:
  int fcnMon1; // flag controling monitoring of fnc calculation
  int fcnCount; // total calls of likelihood function
  float cut_Dmax; // (cm) DCA cut-off for truncated likelihood, squeared
  TH1 *hA[mxH];
  vector<TrackStump> track;
  void readTracks(const TString fnameT);
  void initHisto( TObjArray * HList);
  int qaTracks();
  void scanChi2(double *par, int mode);
  void evalSolution(double *par);
  UtilBeamLine3D ();
  void print(int k=5);
};

extern UtilBeamLine3D util; // caries data & contrl to likelihood function


void beamLineLike3D(int &npar,double *grad,double &fcnval, double *inpPar,int iflag);

#endif
