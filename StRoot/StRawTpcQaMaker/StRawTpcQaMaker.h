#ifndef STAR_StRawTpcQaMaker
#define STAR_StRawTpcQaMaker

//////////////////////////////////////////////////////////////////////////
//                                                                      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef StMaker_H
#include "StMaker.h"
#endif

// floats
class TH1F;
class TH2F;


class StRawTpcQaMaker : public StMaker {
private:

protected:
  TH2F     *myH1;    //! max #pads in each row
  TH1F     *myH2;    //! sector #

public: 
  StRawTpcQaMaker(const char *name="RawTpcQa");
  virtual       ~StRawTpcQaMaker();
  virtual Int_t  Init();
  virtual Int_t  Make();
  virtual Int_t  Finish();
  virtual Int_t  Clear();
  virtual void   PrintInfo();


 ClassDef(StRawTpcQaMaker, 1)   //StAF chain virtual base class for Makers
};

#endif


