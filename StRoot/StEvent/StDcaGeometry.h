/*!
 * \class StDcaGeometry
 * \author Victor Perevoztchikov, Thomas Ullrich, May 2006
 */
/***************************************************************************
 *
 * $Id: StDcaGeometry.h,v 2.3 2008/03/04 01:03:14 perev Exp $
 *
 * Author: Victor Perevoztchikov, Thomas Ullrich, May 2006
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDcaGeometry.h,v $
 * Revision 2.3  2008/03/04 01:03:14  perev
 * remove redundant mHz
 *
 * Revision 2.2  2006/08/04 19:08:43  perev
 * CleanUpOnly
 *
 * Revision 2.1  2006/05/24 17:27:43  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StDcaGeometry_hh
#define StDcaGeometry_hh
#include "StObject.h"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
#include "THelixTrack.h"

class StDcaGeometry : public StObject {
public:
    StDcaGeometry();
    virtual ~StDcaGeometry();

    int              charge()    const;
    double           impact()    const;
    double           curvature() const;
    double           psi()       const;
    double           dipAngle()  const;
    double           tanDip()    const;
    double           pt()        const;
    double           z()         const;
    double           hz()        const;
    StThreeVectorF   origin()    const;
    StThreeVectorF   momentum()  const;
    StPhysicalHelixD helix()     const;
    THelixTrack      thelix()    const;
    const float*     errMatrix() const;

    //
    // Experts only set function
    //
    void set(const float pars[6], const float errs[15]);

private:
    char mBeg[1];//!
    /// signed impact parameter; Signed in such a way that:
    ///     x =  -impact*sin(Psi)
    ///     y =   impact*cos(Psi)
    float  mImp;
    ///  Z-coordinate of this track (reference plane)
    float  mZ;
    ///  Psi angle of the track
    float  mPsi;
    /// signed invert pt [sign = sign(-qB)]
    float  mPti;
    /// tangent of the track momentum dip angle
    float  mTan;
    /// signed curvature
    float  mCurv;
    
    /// pars errors
    float  mImpImp;
    float  mZImp, mZZ;
    float  mPsiImp, mPsiZ, mPsiPsi;
    float  mPtiImp, mPtiZ, mPtiPsi, mPtiPti;
    float  mTanImp, mTanZ, mTanPsi, mTanPti, mTanTan;
    char   mEnd[1];//!
    
    ClassDef(StDcaGeometry,2)
};
inline int     StDcaGeometry::charge() const 		{return (mPti<0)? -1:1;}
inline double  StDcaGeometry::impact() const 		{return mImp;}
inline double  StDcaGeometry::curvature() const 	{return mCurv;}
inline double  StDcaGeometry::psi()       const 	{return mPsi ;}
inline double  StDcaGeometry::dipAngle()  const 	{return atan(mTan);}
inline double  StDcaGeometry::tanDip() const 		{return mTan ;}
inline double  StDcaGeometry::pt()     const 		{return 1./fabs(mPti);}
inline double  StDcaGeometry::z()      const 		{return mZ   ;}
inline double  StDcaGeometry::hz()     const 		{return mCurv/mPti;}
inline const float* StDcaGeometry::errMatrix() const 	{return &mImpImp;}

#endif
