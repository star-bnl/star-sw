/*!
  \class StShadowMaker

  StShadowMaker blinds information from StEvent and encrypts/decrypts
  run numbers, events numbers, and file sequences.

*/

#ifndef STAR_StShadowMaker
#define STAR_StShadowMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StShadowMaker : public StMaker {
 public:
  StShadowMaker(const char *name="Shadow") : StMaker(name) {}
  virtual       ~StShadowMaker() {}
  virtual Int_t  InitRun(Int_t runNumber);
  virtual Int_t  Make();

  static int getRunNumber(int trueRunNumber=-1);
  static int getEventNumber(int trueEventNumber);
  static int getFileSeq(int trueFileSeq=-1);

  static int shadeRunNumber(int trueRunNumber);
  static int shadeEventNumber(int trueEventNumber);
  static int shadeFileSeq(int trueFileSeq);

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StShadowMaker.h,v 1.1 2019/04/23 15:29:01 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

 private:
  static int unshadeEventNumber(int eventNumber, int shade=-1);
  static int unshadeRunNumber(int runNumber, int shade=-1);
  static int unshadeFileSeq(int fileSeq, int shade=-1);

  static int shadeHelper(int a_0, int direction, int key, const int nbits, const int maxrounds);

  static int shadedRunNumber;
  static int shadedFileSeq;

  ClassDef(StShadowMaker,0)
};

#endif

//_____________________________________________________________________________
// $Id: StShadowMaker.h,v 1.1 2019/04/23 15:29:01 jeromel Exp $
// $Log: StShadowMaker.h,v $
// Revision 1.1  2019/04/23 15:29:01  jeromel
// Header is safe
//
//
