/***********************************************************************
 *
 * $Id: StStrangeMuDst.hh,v 2.0 2000/06/05 05:19:43 genevb Exp $
 * $Log: StStrangeMuDst.hh,v $
 * Revision 2.0  2000/06/05 05:19:43  genevb
 * New version of Strangeness micro DST package
 *
 *
 ***********************************************************************
 *
 * Description: Strangeness micro dst base classes
 *
 ***********************************************************************/
#ifndef StStrangeMuDst_hh
#define StStrangeMuDst_hh
#include "TObject.h"

class StStrangeMuDst : public TObject {
public:
  StStrangeMuDst() {}
  virtual ~StStrangeMuDst() {}
  ClassDef(StStrangeMuDst, 1)
};

//_____________________________________________________________________________

class StStrangeAssoc : public TObject {
public:
  StStrangeAssoc() {}
  StStrangeAssoc(Int_t indexRecoArray, Int_t indexMcArray);
  virtual ~StStrangeAssoc() {}

  Int_t indexRecoArray() const;
  Int_t indexMcArray() const;

private:
  Int_t mIndexRecoArray;
  Int_t mIndexMcArray;
  ClassDef(StStrangeAssoc,1)
};

inline Int_t StStrangeAssoc::indexRecoArray() const
             { return mIndexRecoArray; }
inline Int_t StStrangeAssoc::indexMcArray() const
             { return mIndexMcArray; }

#endif
