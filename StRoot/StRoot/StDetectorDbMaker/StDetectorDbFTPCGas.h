#ifndef StDetectorDbFTPCGas_h
#define StDetectorDbFTPCGas_h
#include "St_ftpcGasSystemC.h"
#include "St_ftpcGasOutC.h"
#include "St_tpcGasC.h"

class StDetectorDbFTPCGas { 
 public: 
  static StDetectorDbFTPCGas* instance() {if (! fgInstance) fgInstance = new StDetectorDbFTPCGas(); return fgInstance;}
  ~StDetectorDbFTPCGas() {fgInstance = 0;}
  Double_t      getWestO2ppm() {return St_ftpcGasSystemC::instance()->westO2ppm();}
  Double_t      getWestO2mv()  {return St_ftpcGasSystemC::instance()->westO2mv();}
  Double_t      getEastO2ppm() {return St_ftpcGasSystemC::instance()->eastO2ppm();}
  Double_t      getEastO2mv()  {return St_ftpcGasSystemC::instance()->eastO2mv();}
  Double_t      getExtO2ppm()  {return St_ftpcGasSystemC::instance()->extO2ppm();}
  Double_t      getExtO2mv()   {return St_ftpcGasSystemC::instance()->extO2mv();}
  Double_t      getWestH2Odp() {return St_ftpcGasSystemC::instance()->westH2Odp();}
  Double_t      getEastH2Odp() {return St_ftpcGasSystemC::instance()->eastH2Odp();}
  Double_t      getFlowAr()    {return St_ftpcGasSystemC::instance()->flowAr();}
  Double_t      getFlowCO2()   {return St_ftpcGasSystemC::instance()->flowCO2();}
  Double_t      getGasOutEast(){return St_ftpcGasOutC::instance()->gasOutEast();}
  Double_t      getBody1East() {return St_ftpcGasOutC::instance()->body1East();}
  Double_t      getBody2East() {return St_ftpcGasOutC::instance()->body2East();}
  Double_t      getBody3East() {return St_ftpcGasOutC::instance()->body3East();}
  Double_t      getBody4East() {return St_ftpcGasOutC::instance()->body4East();}
  Double_t      getBody5East() {return St_ftpcGasOutC::instance()->body5East();}
  Double_t      getBody6East() {return St_ftpcGasOutC::instance()->body6East();}
  Double_t      getGasOutWest(){return St_ftpcGasOutC::instance()->gasOutWest();}
  Double_t      getBody1West() {return St_ftpcGasOutC::instance()->body1West();}
  Double_t      getBody2West() {return St_ftpcGasOutC::instance()->body2West();}
  Double_t      getBody3West() {return St_ftpcGasOutC::instance()->body3West();}
  Double_t      getBody4West() {return St_ftpcGasOutC::instance()->body4West();}
  Double_t      getBody5West() {return St_ftpcGasOutC::instance()->body5West();}
  Double_t      getBody6West() {return St_ftpcGasOutC::instance()->body6West();}
  Double_t      getBarometricPressure() {return St_tpcGasC::instance()->barometricPressure();}
private:
  static StDetectorDbFTPCGas* fgInstance;
  StDetectorDbFTPCGas() {}
}; 
#endif
