#ifndef ScintHitList_h
#define ScintHitList_h

#include <TString.h>
class TH1F;
class TObjArray;

class ScintHitList {
 protected:
  int *active, *fired, *track; 
  int nActive, nFired,nTrack,nMatch;
  float  phi0, dPhi; // in radians
  float  eta0, dEta;
  int nEta,nPhi,nBin;
  int iPhiEta2bin(int iPhi,int iEta);
  void iBin2iPhiEta(int iBin,int &iPhi,int &iEta) ;
  void setActive(int iBin);
  void setFired(int iBin);
  TH1F *h[8];
  TString myName;
  float Wmatch, Wveto;

 public:
  ScintHitList(float Xphi0,float XdPhi,int mxPhi,float Xeta0, float XdEta,int mxEta,
	       const char *name, float wm, float wv);
  virtual  ~ScintHitList();
  void clear();
  void initRun();
  int addTrack(float eta, float phi);
  void print(int k=0);
  int getActive(int iBin);
  int getFired(int iBin);
  int getTrack(int iBin);
  bool isMatched(int ibin);
  bool isVetoed(int ibin);
  float getWeight(int ibin);
  void doHisto();
  void initHisto(TObjArray* );
  int getnFired(){ return nFired; }
  int phiBin(float phi); //phi is [0,2Pi]
  virtual int etaBin(float eta)=0; // varies with detector
  virtual float bin2EtaLeft(int iEta)=0; // varies with detector

};


#endif
