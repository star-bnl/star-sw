/***************************************************************
 * $Id: StRichHit.h,v 1.1 2000/04/05 16:39:44 lasiuk Exp $
 *
 * Description:
 *   Definition of the Hit object as reconstructed by
 *   the cluster/hit finder
 *
 ***************************************************************
 * $Log: StRichHit.h,v $
 * Revision 1.1  2000/04/05 16:39:44  lasiuk
 * Initial Revision
 *
 * Revision 1.1  2000/04/05 16:39:44  lasiuk
 * Initial Revision
 *
 ***************************************************************/
#ifndef ST_RICH_HIT
#define ST_RICH_HIT

#include <iostream.h>
#include <vector>

#ifndef ST_NO_TEMPLATE_DEF_ARGS
using std::vector;
#endif

#include "StThreeVector.hh"

class StRichHit {
public:
    StRichHit();
    StRichHit(double x, double y);
    ~StRichHit();
    //StRichHit(const StRichHit&){}
    //StRichHit& operator=(const StRichHit&){}
    
    const StThreeVector<double>& position() const;
    const StThreeVector<double>& internal() const;
    double charge()                  const;
    double maxAmplitude()            const;
    int    clusterNumber()           const;
    
    StThreeVector<double>&  position();
    StThreeVector<double>&  internal();

    void setCharge(double q);
    void setMaxAmplitude(double m);
    void setClusterNumber(int no);
    
private:
    StThreeVector<double> mPosition;  // local coordinates
    StThreeVector<double> mInternal;  // fraction pad/row
    StThreeVector<double> mSigma;
    double                mCharge;
    double                mMaxAmplitude;
    int                   mClusterNumber;
};

inline const StThreeVector<double>& StRichHit::position() const {return mPosition;}
inline StThreeVector<double>& StRichHit::position() {return mPosition;}
inline const StThreeVector<double>& StRichHit::internal() const {return mInternal;}
inline StThreeVector<double>& StRichHit::internal() {return mInternal;}
inline void StRichHit::setCharge(double q) {mCharge = q;}
inline double StRichHit::charge() const {return mCharge;}
inline void StRichHit::setMaxAmplitude(double m) {mMaxAmplitude = m;}
inline double StRichHit::maxAmplitude() const {return mMaxAmplitude;}
inline void StRichHit::setClusterNumber(int no) {mClusterNumber = no;}
inline int StRichHit::clusterNumber() const {return mClusterNumber;}
// non-members
ostream& operator<<(ostream& os, const StRichHit& hit);

// typedef
#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StRichHit*>        HitVector;
#else
typedef vector<StRichHit*, allocator<StRichHit*> >                HitVector;
#endif
#endif
