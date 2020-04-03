#ifndef _StEpdTrivialEventGenerator
#define _StEpdTrivialEventGenerator

/*
  This is a pretty trival class.  It samples some histograms (probability distributions)
   and generates a list of momenta.  And that's about it...
*/

class TH1D;
class TClonesArray;
class TRandom3;

class StEpdTrivialEventGenerator{
 private:
  TH1D* mDnDeta;
  TH1D* mV1versusEta;
  TH1D* mV2versusEta;
  TRandom3* mRan;
  TClonesArray* mTracks;
 public:
  // these are the three histograms that will be sampled.  Do whatever you want, but you have to use the same x-axis binning
  void SetDnDeta(TH1D* h);
  void SetV1versusEta(TH1D* h);
  void SetV2versusEta(TH1D* h);
  
  TClonesArray* Momenta();  // just a TClonesArray of TVector3 objects

  //  StEpdTrivialEventGenerator();
  StEpdTrivialEventGenerator(TH1D* DnDeta=0, TH1D* V1versusEta=0, TH1D* V2versusEta=0);
  ~StEpdTrivialEventGenerator();

};

inline void StEpdTrivialEventGenerator::SetDnDeta(TH1D* h){mDnDeta=h;}
inline void StEpdTrivialEventGenerator::SetV1versusEta(TH1D* h){mV1versusEta=h;}
inline void StEpdTrivialEventGenerator::SetV2versusEta(TH1D* h){mV2versusEta=h;}

#endif
