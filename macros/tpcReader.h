#ifndef tpcReader_h
#define tpcReader_h
#include "TROOT.h"
#include "TFile.h"
#include "TTree.h"
#include "TBranch.h"
#include "TClonesArray.h"
#include "tpcReader.h"
#define Pixels
//______________________________________________________________________________
class Pixel : public TObject {

private:
  Float_t  fTime;
  Float_t  fAdc;
public:
  Pixel(Float_t Time=0,Float_t Adc=0) : fTime(Time), fAdc(Adc) {}
  Float_t GetTime()   const {return fTime;}
  Float_t GetAdc()    const {return fAdc;}
  virtual ~Pixel() {}
  ClassDef(Pixel,2)  //A pixel segment
};
//______________________________________________________________________________
class EventHeader {

private:
  Int_t   fEvtNum;
  Int_t   fRun;
  Int_t   fDate;
  Int_t   fMag;
public:
  EventHeader(Int_t EvtNum=0,Int_t  Run=0,Int_t  Date=0, Int_t Mag=0): 
    fEvtNum(EvtNum), fRun(Run), fDate(Date), fMag(Mag) {}
  Int_t  GetEvtNum() const { return fEvtNum; }
  Int_t  GetRun()    const { return fRun; }
  Int_t  GetDate()   const { return fDate; }
  void   Set(Int_t EvtNum=0,Int_t  Run=0,Int_t  Date=0, Int_t Mag=0) {
    fEvtNum = EvtNum; fRun = Run; fDate = Date; fMag=Mag;}
  virtual ~EventHeader() { }
  
  ClassDef(EventHeader,1)  //Event Header
};

//______________________________________________________________________________
class Event : public TObject {

private:
  Int_t          fNpixel;            //Number of pixels
  EventHeader    fEvtHdr;
  Int_t   fSector;
  Int_t   fRow;
  Int_t   fPad;
  Int_t   fSeq;
  Float_t fSum;
  Float_t fMaximum;
  Int_t   fStartSeq;
  Float_t   fTimeAv;
#ifdef Pixels
  TClonesArray  *fPixels;           //->array with all pixels
  static TClonesArray *fgPixels;
#endif
public:
  Event() : fNpixel(0) {
#ifdef Pixels
    if (!fgPixels) fgPixels = new TClonesArray("Pixel", 1000); fPixels = fgPixels;
#endif
  }
  virtual      ~Event() {Clear();}
  void          Clear(Option_t *option ="") { 
    if (option);
    fNpixel = 0; 
#ifdef Pixels
    fPixels->Clear("C");
#endif
  }
  void          Reset(Option_t *option ="") {
    if (option);
#ifdef Pixels
    delete fgPixels; fgPixels = 0;
#endif
  }
  void          SetNpixel(Int_t n) { fNpixel = n; }
  void          SetHeader(Int_t EvtNum=0,Int_t  Run=0,Int_t  Date=0, Int_t Mag =0) {fEvtHdr.Set(EvtNum,Run,Date,Mag);}
  void          SetSecRowPad(Int_t Sector=0, Int_t Row=0, Int_t Pad=0, Int_t Seq=0) {
    fSector = Sector; fRow = Row; fPad = Pad; fSeq = Seq;}
  void          SetSumMax(Float_t Sum=0, Float_t Max=0, Int_t StartSeq=0, Float_t timeAverage = 0) {
    fSum = Sum; fMaximum = Max; fStartSeq = StartSeq; fTimeAv = timeAverage;}
  Pixel        *AddPixel(Float_t Time,Float_t Adc) {
#ifdef Pixels
    TClonesArray &pixels = *fPixels;
    Pixel *pixel = new(pixels[fNpixel++]) Pixel(Time,Adc);
    return pixel;
#else
    fNpixel++;
    return 0;
#endif
  }
  Int_t         GetNpixel() const { return fNpixel; }
  Int_t  GetSector() const {return fSector;}
  Int_t  GetRow()    const {return fRow;}
  Int_t  GetPad()    const {return fPad;}
  Int_t  GetSeq()    const {return fSeq;}
  ClassDef(Event,1)  //Event structure
};
//______________________________________________________________________________
#endif
