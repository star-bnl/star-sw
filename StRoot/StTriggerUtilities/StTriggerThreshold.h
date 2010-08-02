// -*- mode:c++ -*-

#ifndef StTriggerThreshold_h
#define StTriggerThreshold_h

#include <cstdio>
#include "TObject.h"
#include "TString.h"

struct StTriggerThreshold : public TObject {
  int     object;		// crate
  int     index;		// board
  int     reg;			// register number
  TString label;		// register label
  int     value;		// register value
  int     defaultvalue;		// register default value

  void print();

  ClassDef(StTriggerThreshold,1)
};

void StTriggerThreshold::print()
{
  printf("object=%d index=%d reg=%d label=%s value=%d defaultvalue=%d\n",
	 object,index,reg,label.Data(),value,defaultvalue);
}

#endif // StTriggerThreshold_h
