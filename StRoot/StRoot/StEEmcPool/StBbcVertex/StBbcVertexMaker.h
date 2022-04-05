// $Id: StBbcVertexMaker.h,v 1.2 2014/08/06 11:42:58 jeromel Exp $

#ifndef STAR_StBbcVertexMaker
#define STAR_StBbcVertexMaker

/*!
 *                          
 * \class  StBbcVertexMaker
 * \author Balewski
 * \date   
 * \brief  
 *
 Axample to access muDst and pass it to ezTree analyzis class
 */                                                                      

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "BbcVertex.h"
class TObjArray  ;
class StMuDstMaker;

class StBbcVertexMaker : public StMaker, public  BbcVertex {
 private:
  StMuDstMaker* mMuDstMaker;
  bool unpackMuTrig(); // BBC, CTB

 public: 
  StBbcVertexMaker(const char *self="stEEsoloPi0", const char* muDstMakerName="muDstMaker");
  virtual       ~StBbcVertexMaker();
  virtual Int_t Init();
  virtual Int_t InitRun(int runNo);
  virtual Int_t Finish();

  virtual Int_t  Make();
  void SetHList(TObjArray * x){HList=x;}

  /// Displayed on session exit, leave it as-is please ...
  virtual const char *GetCVS() const {
    static const char cvs[]="Tag $Name:  $ $Id: StBbcVertexMaker.h,v 1.2 2014/08/06 11:42:58 jeromel Exp $ built " __DATE__ " " __TIME__ ; 
    return cvs;
  }

  ClassDef(StBbcVertexMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


// $Log: StBbcVertexMaker.h,v $
// Revision 1.2  2014/08/06 11:42:58  jeromel
// Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
//
// Revision 1.1  2004/08/31 03:44:13  balewski
// first
//
// Revision 1.4  2004/08/26 04:39:40  balewski
// towards pi0
//
// Revision 1.3  2004/08/09 20:28:31  balewski
// add trig selection
//
// Revision 1.2  2004/04/14 19:34:01  balewski
// access to trigger data
//
// Revision 1.1  2004/04/14 17:09:09  balewski
// new copy of pi0finder with towers only, should work on ezTree as well (after small cleanup)
//
