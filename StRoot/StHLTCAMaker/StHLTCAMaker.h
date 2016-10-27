// $Id: StHLTCAMaker.h,v 1.2 2013/09/16 19:54:04 fisyak Exp $

#ifndef STAR_StHTLCAMaker
#define STAR_StHTLCAMaker

/*!
 *                                                                     
 * \class  StHLTCAMaker
 * \author 
 * \date   2013/08/12
 * \brief  
 *
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
class StiKalmanTrack;
class StHLTCAMaker : public StMaker {
 private:
  // Private method declaration if any
 
 protected:
  // Protected method if any
  Bool_t badCAParameters(StiKalmanTrack* track, Bool_t outer);
 public: 
  StHLTCAMaker(const char *name="HLTCA") : StMaker(name){}
    virtual       ~StHLTCAMaker() {}
  virtual Int_t Init();
  virtual Int_t  Make();

  // virtual Int_t InitRun  (int runumber){return 0;}; // Overload empty StMaker::InitRun 
  // virtual Int_t FinishRun(int runumber){return 0;}; // Overload empty StMaker::FinishRun 

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StHLTCAMaker.h,v 1.2 2013/09/16 19:54:04 fisyak Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StHLTCAMaker,0)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StHLTCAMaker.h,v $
// Revision 1.2  2013/09/16 19:54:04  fisyak
// Add check for CA parameters
//
// Revision 1.1.1.1  2013/08/14 12:58:34  fisyak
// First step
//
// Revision 1.15  2003/09/10 19:47:43  perev
