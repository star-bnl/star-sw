// $Id: StHFillObject.h,v 1.2 1999/08/03 02:29:36 genevb Exp $
// $Log: StHFillObject.h,v $
// Revision 1.2  1999/08/03 02:29:36  genevb
// Re-implemented using TMethodCall's
//
// Revision 1.1  1999/07/29 23:27:34  genevb
// Introduction of new class
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StHFillObject allows member functions to be histogrammed             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StHFillObject
#define STAR_StHFillObject

#include "TObject.h"
class TMethodCall;
class TH1;

static const size_t dimMax = 8;
static const size_t nsMax = 50;
static const size_t bufSize = 1024;

class StHFillVars {
 public:
  StHFillVars();
  ~StHFillVars();
  void ClearBuffer();
  Char_t buffer[bufSize];
  Char_t buffer1[bufSize];
  Int_t indices[nsMax][dimMax];
  Float_t values[nsMax][dimMax];
  TMethodCall* methods[nsMax][dimMax];
  Char_t* opt[nsMax];
  Int_t dims[nsMax];
  Int_t nsets;
  TH1* histo[nsMax];
};


class StHFillObject : public TObject {
 private:
 protected:
 public:
  StHFillObject();
  virtual ~StHFillObject();
   static void Reset();
          void Setup(Option_t* option, Int_t hists=1);
          void GetValues(Int_t printIt=0);
  virtual void Draw(Option_t* option);
  virtual void Print(Option_t* option);
  virtual void ls(Option_t* option);
  virtual void Update();
  ClassDef(StHFillObject,1)   //virtual base class for Makers
};

#endif
