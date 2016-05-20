/*!
 * \class StExtGeometry
 * \author Victor Perevoztchikov,  Apr 2016
 */
/***************************************************************************
 *
 * $Id: StExtGeometry.h,v 1.1 2016/05/20 18:40:41 perev Exp $
 *
 * Author: Victor Perevoztchikov,  Apr  2016
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef StExtGeometry_hh
#define StExtGeometry_hh
#include "StObject.h"
#include "StThreeVectorF.hh"
#include "StPhysicalHelixD.hh"
#include "THelixTrack.h"

class StExtGeometry : public StObject {
public:
    StExtGeometry(const char *name="");
    virtual ~StExtGeometry();

const char *name() const {return mName;}
    int            charge()    const {return (mPti>0)? -1:1;}  // synchro with StiTrackNode charge definition
    double         rxy()       const {return mRxy;}		  
    double         phi()       const {return mPhi ;}		  
    double         z()         const {return mZ   ;}		  
    double         curvature() const {return mCurv;}		  
    double         psi()       const {return mPsi ;}		  
    double         tanDip()    const {return mTan ;}		  
    double         pt()        const {return 1./fabs(mPti);}	  
    double         hz()        const {return mCurv/mPti;}	  
    double         length()    const {return mLen;}	  
    double         curve()     const {return mCurv;}	  
    StThreeVectorF   origin()    const;         
    StThreeVectorF   momentum()  const;        
    
    StPhysicalHelixD helix()     const;
    THelixTrack      thelix()    const;
    const Float_t*   params()    const {return &mPhi;}	  
    const Float_t*   errMatrix() const {return mG;} 
    //
    // Experts only set function
    //
    void set(const char *name);
    void set(const double pars[6], const double errs[15]);
    void set(const float  pars[6], const float  errs[15]);
    void setLength(double len){mLen  = len;}
    void setCurve (double cur){mCurv = cur;}

    void add(StExtGeometry **top);
    const StExtGeometry* next() const { return mNext;}

private:
    char     mName[4];	//Name of external detector
    StExtGeometry *mNext;
    Float_t  mRxy;	//XY radius 
    Float_t  mPhi;	//Azimutal angle of track
    Float_t  mZ;        //Z coord of track
    Float_t  mPsi;	//Azimutal angle of point
    Float_t  mPti;     	//signed inverse pt [sign = sign(-qB)
    Float_t  mTan;      //tangent of lambda(lambda ange bte track & xy plane
    Float_t  mCurv;     // signed curvature [sign = sign(-qB)]
    Float_t  mLen;     	// total length of track to this point 
    
    /// pars errors
enum {kPhiPhi
     ,kPhiZ,   kZZ
     ,kPhiPsi, kZPsi, kPsiPsi
     ,kPhiPti, kZPti, kPsiPti, kPtiPti
     ,kPhiTan, kZTan, kPsiTan, kPtiTan, kTanTan};

    Float_t  mG[15];	//Error matrix
    char     mEnd[1];	//!
    
    ClassDef(StExtGeometry,1)
};

#endif
