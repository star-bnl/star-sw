#ifndef STRAREEVENTCUT_HH
#define STRAREEVENTCUT_HH
#include <TObject.h>
class StEvent;
class StRareEventCut : public TObject {

 public:
  virtual int  Accept(StEvent* event) = 0; //!
  virtual void Report() = 0;  //!

  ClassDef(StRareEventCut,0)
    };
#endif
