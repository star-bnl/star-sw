/***********************************************************************
 *
 * $Id: StStrangeMuDst.hh,v 3.1 2003/05/30 21:20:19 genevb Exp $
 * $Log: StStrangeMuDst.hh,v $
 * Revision 3.1  2003/05/30 21:20:19  genevb
 * doxygen savvy, encoding of FTPC mults, change virtual funcs
 *
 * Revision 3.0  2000/07/14 12:56:49  genevb
 * Revision 3 has event multiplicities and dedx information for vertex tracks
 *
 * Revision 2.1  2000/06/09 22:17:10  genevb
 * Allow MC data to be copied between DSTs, other small improvements
 *
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

/*!
 * \class StStrangeMuDst
 * \author Gene Van Buren
 */

class StStrangeMuDst : public TObject {
public:
  StStrangeMuDst() {}
  virtual ~StStrangeMuDst() {}
  ClassDef(StStrangeMuDst,3)
};

//_____________________________________________________________________________

/*!
 * \class StStrangeAssoc
 * \author Gene Van Buren
 */

class StStrangeAssoc : public TObject {
public:
  StStrangeAssoc() {}
  StStrangeAssoc(Int_t indexRecoArray, Int_t indexMcArray);
  virtual ~StStrangeAssoc() {}

  Int_t indexRecoArray() const;
  Int_t indexMcArray() const;

  void setIndexRecoArray(Int_t);
  void setIndexMcArray(Int_t);

private:
  Int_t mIndexRecoArray;
  Int_t mIndexMcArray;
  ClassDef(StStrangeAssoc,3)
};

/// @name Accessor functions
//@{
inline Int_t StStrangeAssoc::indexRecoArray() const
             { return mIndexRecoArray; }
inline Int_t StStrangeAssoc::indexMcArray() const
             { return mIndexMcArray; }
//@}

/// @name Set functions
//@{
inline void StStrangeAssoc::setIndexRecoArray(Int_t i)
             { mIndexRecoArray = i; }
inline void StStrangeAssoc::setIndexMcArray(Int_t i)
             { mIndexMcArray = i; }
//@}

#endif
