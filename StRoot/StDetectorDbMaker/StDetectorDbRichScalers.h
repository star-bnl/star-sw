#ifndef StDetectorDbRichScalers_h
#define StDetectorDbRichScalers_h
#include "St_trigDetSumsC.h"
#include "St_richvoltagesC.h"
#include "St_y1MultC.h"

class StDetectorDbRichScalers{
public:
  static StDetectorDbRichScalers* instance() {if (! fgInstance) fgInstance = new StDetectorDbRichScalers(); return fgInstance;}
  virtual ~StDetectorDbRichScalers() {fgInstance = 0;}
  Double_t getCTBWest() {return St_trigDetSumsC::instance()->ctbWest();}
  Double_t getCTBEast() {return St_trigDetSumsC::instance()->ctbEast();}
  Double_t getCTBOrTOFp() {return St_trigDetSumsC::instance()->ctbTOFp();}
  Double_t getTOFp() {return St_trigDetSumsC::instance()->tofp();}
  Double_t getZDCWest() {return St_trigDetSumsC::instance()->zdcWest();}
  Double_t getZDCEast() {return St_trigDetSumsC::instance()->zdcEast();}
  Double_t getZDCX() {return St_trigDetSumsC::instance()->zdcX();}
  Double_t getMult() {return St_trigDetSumsC::instance()->mult();}
  Double_t getL0() {return St_trigDetSumsC::instance()->L0();}
  Double_t getBBCX() {return St_trigDetSumsC::instance()->bbcX();}
  Double_t getBBCXCTB() {return St_trigDetSumsC::instance()->bbcXctbTOFp();}
  Double_t getBBCWest() {return St_trigDetSumsC::instance()->bbcWest();}
  Double_t getBBCEast() {return St_trigDetSumsC::instance()->bbcEast();}
  Double_t getBBCYellowBkg() {return St_trigDetSumsC::instance()->bbcYellowBkg();}
  Double_t getBBCBlueBkg() {return St_trigDetSumsC::instance()->bbcBlueBkg();}
  Double_t getPVPDWest() {return St_trigDetSumsC::instance()->pvpdWest();}
  Double_t getPVPDEast() {return St_trigDetSumsC::instance()->pvpdEast();}
  UInt_t   getRichHVStatus() {return St_richvoltagesC::instance()->status();}
  void     setValidityMargin(Double_t margin=0) {St_trigDetSumsC::instance()->validityMargin(margin);}
protected:
    StDetectorDbRichScalers() {}
private:
    static StDetectorDbRichScalers* fgInstance;
};

#endif
