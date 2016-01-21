// $Id: $
#ifndef STAR_StMuD0AnalysisMaker
#define STAR_StMuD0AnalysisMaker
/*!
 *                                                                     
 * \class  StMuD0AnalysisMaker
 * \author fisyak
 * \date   2016/01/17
 * \brief  An example of D0 analysis based on MuDst
 */                                                                      
#include "StMaker.h"
#include "TTree.h"
#include "D0Event.h"
class StMuD0AnalysisMaker : public StMaker {
 private:
  TTree   *fTree;
  D0Event *fEvent;
 public: 
  StMuD0AnalysisMaker(const char *name="D0Ana") : StMaker(name), fTree(0), fEvent(0) {}
  virtual       ~StMuD0AnalysisMaker() {}
  virtual Int_t Init();
  virtual Int_t Make();
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }
  ClassDef(StMuD0AnalysisMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: $
