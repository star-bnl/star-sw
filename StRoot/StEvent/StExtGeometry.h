/*!
 * \class StExtGeometry
 * \author Victor Perevoztchikov,  November 2016
 */
/***************************************************************************
 *
 * $Id: StExtGeometry.h,v 2.3 2017/05/04 00:56:18 perev Exp $
 *
 * Author: Victor Perevoztchikov,  November  2016
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Id: StExtGeometry.h,v 2.3 2017/05/04 00:56:18 perev Exp $
 **************************************************************************/
#ifndef StExtGeometry_hh
#define StExtGeometry_hh

#include "StObject.h"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
#include "THelixTrack.h"

class StExtGeometry : public StObject {
public:
    friend class StTrack;

    StExtGeometry(const char *name="");
    virtual ~StExtGeometry();
    
    const char *name() const;
    int        charge() const;  // synchro with StiTrackNode charge definition
    double     rxy()       const;
    double     phi()       const;
    double     z()         const;
    double     curvature() const;
    double     psi()       const;
    double     tanDip()    const;
    double     pt()        const;
    double     hz()        const;
    double     length()    const;
    double     curve()     const;
    
    StThreeVectorF   origin()    const;
    StThreeVectorF   momentum()  const;
    
    StPhysicalHelixD helix()     const;
    THelixTrack      thelix()    const;
    const float*     params()    const;
    const float*     errMatrix() const;
    //
    // Experts only set function
    //
    void setName(const char *name);
    void set(double rXY,const double pars[6], const double errs[15]);
    void setLength(double len);
    void setCurve (double cur);
    
    void add(StExtGeometry **top);
    const StExtGeometry* next() const;
    
    /// pars
    enum {kPhi,kZ,kPsi,kPti,kTan,kCurv,kLen};
    /// pars errors
    enum {kPhiPhi
        ,kPhiZ,   kZZ
        ,kPhiPsi, kZPsi, kPsiPsi
        ,kPhiPti, kZPti, kPsiPti, kPtiPti
        ,kPhiTan, kZTan, kPsiTan, kPtiTan, kTanTan};
    
protected:
    char     mName[8];	//Name of external detector
    StExtGeometry *mNext;
    
    Float_t  mRxy;	// XY radius
    Float_t  mPhi;	// Azimutal angle of point
    Float_t  mZ;    	// Z coord of point
    Float_t  mPsi;	// Azimutal angle of track
    Float_t  mPti;  	// signed inverse pt [sign = sign(-qB)
    Float_t  mTan;  	// tangent of lambda(lambda ange bte track & xy plane
    Float_t  mCurv; 	// signed curvature [sign = sign(-qB)]
    Float_t  mLen;  	// total length of track to this point
    
    Float_t  mG[15];	// Error matrix
    char     mEnd[1];	//!
    
    ClassDef(StExtGeometry,2)
};

inline const char* StExtGeometry::name() const {return mName;}
inline int StExtGeometry::charge() const {return (mPti>0)? -1:1;}
inline double StExtGeometry::rxy() const {return mRxy;}
inline double StExtGeometry::phi() const {return mPhi ;}
inline double StExtGeometry::z() const {return mZ   ;}
inline double StExtGeometry::curvature() const {return mCurv;}
inline double StExtGeometry::psi() const {return mPsi ;}
inline double StExtGeometry::tanDip() const {return mTan ;}
inline double StExtGeometry::pt() const {return 1./fabs(mPti);}
inline double StExtGeometry::hz() const {return mCurv/mPti;}
inline double StExtGeometry::length() const {return mLen;}
inline double StExtGeometry::curve() const {return mCurv;}
inline const float* StExtGeometry::params() const {return &mPhi;}
inline const float*  StExtGeometry::errMatrix() const {return mG;}
inline void StExtGeometry::setLength(double len){mLen  = len;}
inline void StExtGeometry::setCurve (double cur){mCurv = cur;}
inline const StExtGeometry* StExtGeometry::next() const { return mNext;}

#endif
