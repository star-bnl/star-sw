/***************************************************************
 * $Id: StRichSimpleHit.h,v 2.2 2000/11/01 16:52:43 lasiuk Exp $
 *
 * Description:
 *   Definition of the Hit object as reconstructed by
 *   the cluster/hit finder
 *
 ***************************************************************
 * $Log: StRichSimpleHit.h,v $
 * Revision 2.2  2000/11/01 16:52:43  lasiuk
 * Use the enumerated types from StEvent.  correct the NAMESPACE macro
 * and print more bits in the printBit member
 *
 * Revision 2.1  2000/09/29 19:01:26  lasiuk
 * enumerated types added for flags
 * c'tor includes cp from persistent hit
 * number of pads added as a member
 *
 * Revision 2.0  2000/08/09 16:22:12  gans
 * Cosmetic Changes. Naming convention for TDrawable objects
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

#ifndef ST_NO_NAMESPACES
using std::vector;
#endif

#include "StThreeVector.hh"

// From StEvent
// if not running with STEVENT, set this and the enumerations
// will be defined here...otherwise use the StEvent definitions.
#include "StEnumerations.h"
#include "StRichHit.h"

class StRichSimpleHit  {
public:
    StRichSimpleHit();
    StRichSimpleHit(const StThreeVector<double>& xl, const StThreeVector<double>& dx);
#ifdef __ROOT__
    StRichSimpleHit(const StRichHit*);
#endif
    virtual ~StRichSimpleHit();
    virtual StRichSimpleHit* clone();
    
    //StRichSimpleHit(const StRichSimpleHit&){}
    //StRichSimpleHit& operator=(const StRichSimpleHit&){}
    
    const StThreeVector<double>& global()     const;
    const StThreeVector<double>& local()      const;
    const StThreeVector<double>& internal()   const;
    const StThreeVector<double>& localError() const;
    const StThreeVector<double>& sigma()      const;
    
    double charge()        const;
    double maxAmplitude()  const;
    int    clusterNumber() const;
    
    StThreeVector<double>&  global();
    StThreeVector<double>&  local();
    StThreeVector<double>&  internal();
    StThreeVector<double>&  localError();
    StThreeVector<double>&  sigma();
    
    void setCharge(double q);
    void setMaxAmplitude(double m);
    void setClusterNumber(int no);

    unsigned short numberOfPads() const;
    void setNumberOfPads(unsigned short);

    //
    // Flag Operation
    bool isSet(StRichHitFlag f)      const;
    void setBit(StRichHitFlag f);
    void unSetBit(StRichHitFlag f);
    unsigned long flags()                  const;
    void printBits()                       const;
    
protected:
    StThreeVector<double> mGlobal;
    StThreeVector<double> mLocal;  // local coordinates
    StThreeVector<double> mLError;  // local error
    StThreeVector<double> mInternal;  // fraction pad/row
    StThreeVector<double> mSigma;
    double                mCharge;
    double                mMaxAmplitude;
    unsigned int          mClusterNumber;
    unsigned long         mFlags;
    unsigned short        mNumberOfPads;
};

inline const StThreeVector<double>& StRichSimpleHit::global() const {return mGlobal;}
inline StThreeVector<double>& StRichSimpleHit::global() {return mGlobal;}
inline const StThreeVector<double>& StRichSimpleHit::local() const {return mLocal;}
inline StThreeVector<double>& StRichSimpleHit::local() {return mLocal;}
inline const StThreeVector<double>& StRichSimpleHit::internal() const {return mInternal;}
inline StThreeVector<double>& StRichSimpleHit::internal() {return mInternal;}
inline const StThreeVector<double>& StRichSimpleHit::localError() const {return mLError;}
inline StThreeVector<double>& StRichSimpleHit::localError() {return mLError;}
inline const StThreeVector<double>& StRichSimpleHit::sigma() const {return mSigma;}
inline StThreeVector<double>& StRichSimpleHit::sigma() {return mSigma;}
inline void StRichSimpleHit::setCharge(double q) {mCharge = q;}
inline double StRichSimpleHit::charge() const {return mCharge;}
inline void StRichSimpleHit::setMaxAmplitude(double m) {mMaxAmplitude = m;}
inline double StRichSimpleHit::maxAmplitude() const {return mMaxAmplitude;}
inline void StRichSimpleHit::setClusterNumber(int no) {mClusterNumber = no;}
inline int StRichSimpleHit::clusterNumber() const {return mClusterNumber;}
inline unsigned short StRichSimpleHit::numberOfPads() const {return mNumberOfPads;}
inline void StRichSimpleHit::setNumberOfPads(unsigned short n) {mNumberOfPads=n;}

inline StRichSimpleHit* StRichSimpleHit::clone() {return new StRichSimpleHit(*this);}
// Flags
inline void StRichSimpleHit::setBit(StRichHitFlag b) { mFlags |= b; }
inline void StRichSimpleHit::unSetBit(StRichHitFlag b) { mFlags &= ~(b);}
inline bool StRichSimpleHit::isSet(StRichHitFlag b) const { return (mFlags & b); }
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
