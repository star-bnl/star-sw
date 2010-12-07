#ifndef _EXAMPLEMAIN_H_
#define _EXAMPLEMAIN_H_

#include "TObject.h"

class ExampleMain : public TObject 
{
 private:
  ExampleMain(){}  
 public:
  virtual ~ExampleMain(){}
  static int main();
  ClassDef(ExampleMain,0);
};
#endif
