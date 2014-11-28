/*!
 * \class StDedxPidTraits 
 * \author Thomas Ullrich, Sep 1999
 */
/***************************************************************************
 *
 * $Id: StDedxPidTraits.h,v 2.15 2012/04/29 22:51:18 fisyak Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedxPidTraits.h,v $
 * Revision 2.15  2012/04/29 22:51:18  fisyak
 * Add field for Log2(<dX>)
 *
 * Revision 2.14  2010/08/31 19:51:56  fisyak
 * Clean up
 *
 * Revision 2.13  2004/09/16 02:24:18  perev
 * Small optimization of alignement
 *
 * Revision 2.12  2004/07/15 16:36:23  ullrich
 * Removed all clone() declerations and definitions. Use StObject::clone() only.
 *
 * Revision 2.11  2004/03/01 17:44:38  fisyak
 * Add Print method
 *
 * Revision 2.10  2002/11/15 20:38:36  fisyak
 * Set class version 2 for new calibration scheme
 *
 * Revision 2.9  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.8  2001/04/05 04:00:34  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.7  2001/03/24 03:34:43  perev
 * clone() -> clone() const
 *
 * Revision 2.6  2000/12/18 17:25:14  fisyak
 * Add track length used in dE/dx calculations
 *
 * Revision 2.5  2000/01/05 16:04:14  ullrich
 * Changed method name sigma() to errorOnMean().
 *
 * Revision 2.4  1999/11/29 17:07:27  ullrich
 * Moved method() from StTrackPidTraits to StDedxPidTraits.cxx
 *
 * Revision 2.3  1999/11/23 15:56:25  ullrich
 * Added clone() const method. Was pure virtual.
 *
 * Revision 2.2  1999/11/16 14:11:41  ullrich
 * Changed variance to sigma.
 *
 * Revision 2.1  1999/10/13 19:42:58  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StDedxPidTraits_hh
#define StDedxPidTraits_hh
#include "StTrackPidTraits.h"

class StDedxPidTraits : public StTrackPidTraits {
 public:
  StDedxPidTraits(StDetectorId det=kUnknownId, Short_t meth=kUndefinedMethodId,
		  UShort_t n=0, Float_t dedx=0, Float_t sig=0, Float_t log2dx=1) :
    StTrackPidTraits(det),
    mNumberOfPoints(n), mMethod(meth), mDedx(dedx), mSigma(sig), mLog2dX(log2dx) { /* noop */ }

  StDedxPidTraits(const dst_dedx_st& t) :
    StTrackPidTraits(t),
    mNumberOfPoints(t.ndedx), mMethod(t.method), mDedx(t.dedx[0]),
    mSigma(t.dedx[1]), mLog2dX(t.dedx[2]) { /* noop */ }

  ~StDedxPidTraits() { /* noop */ }

  UShort_t     numberOfPoints() const { return mNumberOfPoints%100; }
  Float_t      length()         const { return (mNumberOfPoints/100); }
  StDedxMethod method() const;
  Short_t      encodedMethod() const;
  Float_t      mean() const;
  Float_t      errorOnMean() const;
  void         Print(Option_t *opt = "") const;
  Float_t      log2dX() const {return mLog2dX;}
protected:
  UShort_t   mNumberOfPoints;
  Short_t    mMethod;
  Float_t    mDedx;
  Float_t    mSigma;
  Float_t    mLog2dX; // Log2 from average dX
  ClassDef(StDedxPidTraits,4)
};
#endif
