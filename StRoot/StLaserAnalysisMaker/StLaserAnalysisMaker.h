// $Id: StLaserAnalysisMaker.h,v 1.3 2014/08/06 11:43:25 jeromel Exp $
#ifndef STAR_StLaserAnalysisMaker
#define STAR_StLaserAnalysisMaker
#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "TTree.h"
class StLaserAnalysisMaker : public StMaker {
 private:
  TTree                *m_laser; //! Laser track-hit event Tree
  
 public: 
  StLaserAnalysisMaker(const char *name="Laser") : StMaker(name), m_laser(0){}
  virtual      ~StLaserAnalysisMaker() {}
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t InitRun  (int runumber); // Overload empty StMaker::InitRun 
  virtual void  Clear(const Option_t *option="");
  //virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 
  virtual Int_t        Finish();
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StLaserAnalysisMaker.h,v 1.3 2014/08/06 11:43:25 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  ClassDef(StLaserAnalysisMaker,0)   //StAF chain virtual base class for Makers
};
#endif
// $Log: StLaserAnalysisMaker.h,v $
// Revision 1.3  2014/08/06 11:43:25  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.2  2007/02/05 15:30:06  fisyak
// Freeze a version for laser drift velocity calations
//
