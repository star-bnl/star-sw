// -*- mode:c++ -*-

#ifndef StTriggerThreshold_h
#define StTriggerThreshold_h

#include <cstdio>
#include "TObject.h"
#include "TString.h"

struct StTriggerThreshold : public TObject {
  Int_t   object;		// crate
  Int_t   index;		// board
  Int_t   reg;			// register number
  TString label;		// register label
  Int_t   value;		// register value
  Int_t   defaultvalue;		// register default value

  void print();

  ClassDef(StTriggerThreshold,1)
};

inline void StTriggerThreshold::print()
{
  printf("object=%d index=%d reg=%d label=%s value=%d defaultvalue=%d\n",
	 object,index,reg,label.Data(),value,defaultvalue);
}

#endif // StTriggerThreshold_h
