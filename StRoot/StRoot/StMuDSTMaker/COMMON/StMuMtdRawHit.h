#ifndef __StMuMtdRawHit_hh__
#define __StMuMtdRawHit_hh__

#include "TObject.h"

using namespace std;

class StMtdRawHit;

class StMuMtdRawHit : public TObject {

public:

  StMuMtdRawHit();
  StMuMtdRawHit(const StMtdRawHit* hit);
  ~StMuMtdRawHit() {; }

  bool      leadingEdge() const;
  bool      trailingEdge() const;
  int       fiberId() const;
  int       flag() const;
  int       backleg() const;
  int       channel() const;
  unsigned int   tdc() const;
	
private:

  Char_t   mFlag;
  UChar_t  mBackLeg;
  UChar_t  mChannel;
  UInt_t   mTdc;

ClassDef(StMuMtdRawHit,1)

};

#endif
