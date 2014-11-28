#ifndef ROOT_TpcCluster
#define ROOT_TpcCluster

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// TpcCluster                                                                //
//                                                                      //
// Description of the event and track parameters                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "Riostream.h"
#include "TObject.h"
#include "TClonesArray.h"
#include "StTpcPixel.h"
class StTpcHit;
class StMcTpcHit;

class TpcTrack : public TObject {
 public:
  TpcTrack(Int_t sector = 0, Int_t row = 0, Float_t pad = 0, Float_t timebin = 0, 
	   Int_t Npoints = 0, Int_t Nfitpoints = 0, Int_t ifPrim=0, 
	   Float_t px = 0,  Float_t py = 0, Float_t pz = 0, Float_t dEdx =0, Float_t TrackLength70 =0 ) :
    fSector(sector), fRow(row), mMcl_x((Short_t) (64*pad)), mMcl_t((Short_t) (64*timebin)), 
    fNpoints(Npoints), fNfitpoints(Nfitpoints), fifPrim(ifPrim),
    fpx(px), fpy(py), fpz(pz), fdEdx(dEdx), fTrackLength70(TrackLength70) {}
  virtual ~TpcTrack() {}
 private:
  Int_t   fSector;
  Int_t   fRow;
  Short_t mMcl_x;
  Short_t mMcl_t;
  Int_t   fNpoints;
  Int_t   fNfitpoints;
  Int_t   fifPrim;
  Float_t fpx;
  Float_t fpy;
  Float_t fpz;
  Float_t fdEdx;
  Float_t fTrackLength70;
  ClassDef(TpcTrack,2)
};
class TpcCluster : public TObject {
 public:
  TpcCluster();
  virtual ~TpcCluster() { Clear(); }
  void          Clear(Option_t *option ="");
  static void   Reset(Option_t *option ="");
  void          SetAdcSum(Int_t sum) {fAdcSum = sum;}
  void AddPixel(const StTpcPixel *Signal);
  void AddMcHit(const StMcTpcHit *mcHit);
  void AddRcHit(const StTpcHit   *rcHit);
  void AddRcTrack(const TpcTrack   *track);
  void AddRcTHit(const StTpcHit   *rcHit);
  void SetEventNo(Int_t ev) {fEvent = ev;}
  void SetDriftVelocities(Double_t dvWest = 0, Double_t dvEast = 0) {DVelWest=dvWest; DVelEast=dvEast;}
  void SetFrequency(Double_t f = 0) {Frequency = f;}
  void SetNofPV(Int_t m = -1) {fNofPV = m;}
  void SetNoTracksAtBestPV(Int_t m) {fNoTracksAtBestPV = m;}
  void SetXyzPV(Float_t x = 0, Float_t y = 0, Float_t z = 0) { fxV =x;  fyV =y;  fzV =z;}
  void SetSecRow(Int_t sec, Int_t row) {fSector = sec; fRow = row;}
  TClonesArray *Pixels() {return fPixels;}
  TClonesArray *McHit()  {return fMcHit;}
  TClonesArray *RcHit()  {return fRcHit;}
  TClonesArray *RcTrack(){return fRcTrack;}
  TClonesArray *RcTHit()  {return fRcHit;}
private:
  Int_t fEvent;
  Int_t fNoPixels;
  Int_t fNoMcHit;
  Int_t fNoRcHit;
  Int_t fNoRcTrack;
  Int_t fNoRcTHit;
  Int_t fAdcSum;
  Float_t DVelWest;
  Float_t DVelEast;
  Float_t Frequency;
  Int_t   fNofPV;
  Int_t   fNoTracksAtBestPV;
  Float_t fxV;
  Float_t fyV;
  Float_t fzV;
  Int_t   fSector;
  Int_t   fRow;
  TClonesArray *fPixels;
  TClonesArray *fMcHit;
  TClonesArray *fRcHit;
  TClonesArray *fRcTrack;
  TClonesArray *fRcTHit;
  static TClonesArray *fgPixels;
  static TClonesArray *fgMcHit;
  static TClonesArray *fgRcHit;
  static TClonesArray *fgRcTrack;
  static TClonesArray *fgRcTHit;
  
  ClassDef(TpcCluster,4)  //TpcCluster structure
};

#endif

