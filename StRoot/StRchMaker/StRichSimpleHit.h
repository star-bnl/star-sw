/***************************************************************
 * $Id: StRichSimpleHit.h,v 1.2 2000/05/23 16:55:58 lasiuk Exp $
 *
 * Description:
 *   Definition of the Hit object as reconstructed by
 *   the cluster/hit finder
 *
 ***************************************************************
 * $Log: StRichSimpleHit.h,v $
 * Revision 1.2  2000/05/23 16:55:58  lasiuk
 * Incorporate new MC info
 * add clone() where necessary
 * accomodate name changes
 *
 * Revision 1.3  2000/05/31 19:26:15  dunlop
 * Filling non-ctor entries in persistent hits + support for this
 *
 * Revision 1.2  2000/05/23 16:55:58  lasiuk
 * Incorporate new MC info
 * add clone() where necessary
 * accomodate name changes
 *
 * Revision 1.1  2000/05/18 11:34:18  lasiuk
 * iRename revision
 *
 * Revision 1.1  2000/04/05 16:39:44  lasiuk
 * Initial Revision
 *
 ***************************************************************/
#ifndef ST_RICH_SIMPLE_HIT
#define ST_RICH_SIMPLE_HIT

#include <iostream.h>
#include <vector>

#include "StRichHit.h"
using std::vector;
#endif

#include "StThreeVector.hh"

#include "StEvent/StRichHit.h"
#endif

enum StRichSimpleHitFlag {eDeconvoluted=1, eMip=2, eSaturatedPad=4};

class StRichSimpleHit  {
public:
    StRichSimpleHit();
    StRichSimpleHit(const StThreeVector<double>& xl, const StThreeVector<double>& dx);
#ifdef __ROOT__
    StRichSimpleHit(const StRichHit*);
#endif
    virtual ~StRichSimpleHit();
    const StThreeVector<double>& internal() const;
    //StRichSimpleHit& operator=(const StRichSimpleHit&){}
    const StThreeVector<double>& global()     const;
    const StThreeVector<double>& local()      const;
    const StThreeVector<double>& internal()   const;
    const StThreeVector<double>& localError() const;
    const StThreeVector<double>& sigma()      const;

    double maxAmplitude()  const;
    int    clusterNumber() const;
    StThreeVector<double>&  global();
    StThreeVector<double>&  local();
    StThreeVector<double>&  internal();
    StThreeVector<double>&  localError();
    bool isSet(StRichSimpleHitFlag f)      const;
    void setBit(StRichSimpleHitFlag f);
    void unSetBit(StRichSimpleHitFlag f);
    unsigned long flags()                  const;
    void printBits()                       const;
    
protected:
    StThreeVector<double> mGlobal;
    StThreeVector<double> mLocal;  // local coordinates
    StThreeVector<double> mLError;  // local error
    double                mCharge;
    double                mMaxAmplitude;
    unsigned int          mClusterNumber;
    unsigned long         mFlags;
    unsigned short        mNumberOfPads;
inline const StThreeVector<double>& StRichSimpleHit::global() const {return mGlobal;}
inline StThreeVector<double>& StRichSimpleHit::global() {return mGlobal;}
inline const StThreeVector<double>& StRichSimpleHit::local() const {return mLocal;}
inline StThreeVector<double>& StRichSimpleHit::internal() {return mInternal;}
inline const StThreeVector<double>& StRichSimpleHit::localError() const {return mLError;}
inline StThreeVector<double>& StRichSimpleHit::localError() {return mLError;}
inline const StThreeVector<double>& StRichSimpleHit::sigma() const {return mSigma;}
inline StThreeVector<double>& StRichSimpleHit::sigma() {return mSigma;}
inline void StRichSimpleHit::setCharge(double q) {mCharge = q;}
inline void StRichSimpleHit::setClusterNumber(int no) {mClusterNumber = no;}
// Flags
inline void StRichSimpleHit::setBit(StRichSimpleHitFlag b) { mFlags |= b; }
inline void StRichSimpleHit::unSetBit(StRichSimpleHitFlag b) { mFlags &= ~(b);}
inline bool StRichSimpleHit::isSet(StRichSimpleHitFlag b) const { return (mFlags & b); }
inline unsigned long StRichSimpleHit::flags() const { return (mFlags); }

// non-members
ostream& operator<<(ostream& os, const StRichSimpleHit& hit);

// typedef
#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StRichSimpleHit*>        HitVector;
#else
typedef vector<StRichSimpleHit*, allocator<StRichSimpleHit*> >                HitVector;
#endif
#endif
