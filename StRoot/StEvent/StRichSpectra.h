/***************************************************************************
 *
 * $Id: StRichSpectra.h,v 2.1 2002/02/19 04:24:03 lasiuk Exp $
 *
 * Author: bl
 *         Dec 14, 2002
 ***************************************************************************
 *
 * Description: Output from StRichSpectraMaker for uDST storage
 *
 ***************************************************************************
 * $Log: StRichSpectra.h,v $
 * Revision 2.1  2002/02/19 04:24:03  lasiuk
 * addition of StRichSpectra information for uDST purposes
 *
 **************************************************************************/
#ifndef StRichSpectra_hh
#define StRichSpectra_hh

#include <iostream.h>
#include <float.h>
#include "StObject.h"

class StRichSpectra : public StObject {
public: 
    StRichSpectra(Int_t v=-999);
    ~StRichSpectra();
    StRichSpectra(Float_t,Float_t,Float_t,Float_t,Float_t,Float_t,
		  Float_t,Float_t,Int_t,Float_t,Int_t,Int_t,
		  Float_t,Float_t,Float_t,Float_t,Int_t,Float_t,
		  Float_t, Float_t, Float_t,
		  Int_t, Int_t, Int_t,
		  Int_t=-999);
    
    //StRichSpectra(const StRichSpectra&){/* nopt */}    
    //operator =(StRichSpectra(const StRichSpectra&) {/* nopt */}
    
    void setExtrapolatedPosition(Float_t, Float_t);
    void setExtrapolatedResidual(Float_t, Float_t);
    void setCorrectedExtrapolatedResidual(Float_t, Float_t);
    void setCherenkovAngle(Float_t);
    void setCherenkovSigma(Float_t);
    void setCherenkovPhotons(Int_t);
    void setPeakAngle(Float_t);
    void setPeakPhotons(Int_t);
    void setTotalPhotons(Int_t);
    void setMassSquared(Float_t);
    void setLineIntegralRatio(Float_t);
    void setLineIntegral(Float_t);
    void setAlpha(Float_t);
    void setFlag(Int_t);
    void setReserved(Float_t);

    void setMeanD(Float_t pi=FLT_MAX,  Float_t k=FLT_MAX, Float_t p=FLT_MAX);
    void setNumberOfD(Int_t pi=-100,  Int_t k=-100, Int_t p=-100);
    void setVersion(Int_t);


    
    Float_t getExtrapolatedX() const;
    Float_t getExtrapolatedY() const;
    Float_t getExtrapolatedXResidual() const;
    Float_t getExtrapolatedYResidual() const;
    Float_t getCorrectedExtrapolatedXResidual() const;
    Float_t getCorrectedExtrapolatedYResidual() const;
    Float_t getCherenkovAngle() const;
    Float_t getCherenkovSigma() const;
    Int_t   getCherenkovPhotons() const;
    Float_t getPeakAngle() const;
    Int_t   getPeakPhotons() const;
    Int_t   getTotalPhotons() const;
    Float_t getMassSquared() const;
    Float_t getLineIntegralRatio() const;
    Float_t getLineIntegral() const;
    Float_t getAlpha() const;
    Int_t   getFlag() const;
    Float_t getMeanDpi() const;
    Float_t getMeanDk()  const;
    Float_t getMeanDp()  const;
    Int_t   getMeanDnpi()  const;
    Int_t   getMeanDnk()  const;
    Int_t   getMeanDnp()  const;

    Float_t getReserved() const;
    Int_t   getVersion() const;

protected:
    Float_t mExtrapolatedX;
    Float_t mExtrapolatedY;    
    Float_t mDx;
    Float_t mDy;
    Float_t mCdx;
    Float_t mCdy;
    Float_t mCherenkovAngle;
    Float_t mCherenkovAngleSigma;
    Int_t mNumberOfPhotons;
    Float_t mPeakAngle;
    Int_t mPeakPhotons;
    Int_t mTotalPhotons;
    Float_t mMassSquared;
    Float_t mLineIntegralRatio;
    Float_t mLineIntegral;
    Float_t mAlpha;
    Int_t mFlag;
    Float_t mReserved;

    Float_t mDpi,  mDk,  mDp;
    Int_t   mNDpi, mNDk, mNDp;

    Int_t mVersion;
    //
    // Required Track Parameters:
    // innerTrackHelix
    // outerTrackHelix
    // charge
    // firstPointonTrack(x,y,z)
    // lastPointonTrack(x,y,z)
    // chi2
    // numberOfFitPoints
    
virtual const char *GetCVS() const	{
    static const char cvs[]=
	"Tag $Name:  $ $Id $ built "__DATE__" "__TIME__ ;
    return cvs;
}
private:
    // the following is a ROOT macro  that is needed in all ROOT code
    ClassDef(StRichSpectra, 1)   //StAF chain virtual base class for Makers
	};

inline void StRichSpectra::setExtrapolatedPosition(Float_t x, Float_t y) {mExtrapolatedX = x;mExtrapolatedY=y;}
inline void StRichSpectra::setExtrapolatedResidual(Float_t dx, Float_t dy) {mDx=dx;mDy=dy;}
inline void StRichSpectra::setCorrectedExtrapolatedResidual(Float_t dx, Float_t dy) {mCdx=dx;mCdy=dy;}
inline void StRichSpectra::setCherenkovAngle(Float_t theta) {mCherenkovAngle=theta;}
inline void StRichSpectra::setCherenkovSigma(Float_t s) {mCherenkovAngleSigma=s;}
inline void StRichSpectra::setCherenkovPhotons(Int_t n) {mNumberOfPhotons=n;}
inline void StRichSpectra::setPeakAngle(Float_t pa) {mPeakAngle=pa;}
inline void StRichSpectra::setPeakPhotons(Int_t np) {mPeakPhotons=np;}
inline void StRichSpectra::setTotalPhotons(Int_t tp) {mTotalPhotons=tp;}
inline void StRichSpectra::setMassSquared(Float_t m2) {mMassSquared=m2;}
inline void StRichSpectra::setLineIntegralRatio(Float_t lir) {mLineIntegralRatio=lir;}
inline void StRichSpectra::setLineIntegral(Float_t li) {mLineIntegral = li;}
inline void StRichSpectra::setAlpha(Float_t alpha) {mAlpha=alpha;}
inline void StRichSpectra::setFlag(Int_t f) {mFlag = f;}
inline void StRichSpectra::setMeanD(Float_t pi,  Float_t k, Float_t p) {mDpi=pi; mDk=k, mDp=p;}
inline void StRichSpectra::setNumberOfD(Int_t pi,  Int_t k, Int_t p) {mNDpi=pi; mNDk=k; mNDp=p;}
inline void StRichSpectra::setReserved(Float_t r) {mReserved = r;}
inline void StRichSpectra::setVersion(Int_t v) {mVersion = v;}

inline Float_t StRichSpectra::getExtrapolatedX() const {return  mExtrapolatedX;}
inline Float_t StRichSpectra::getExtrapolatedY() const {return  mExtrapolatedY;}
inline Float_t StRichSpectra::getExtrapolatedXResidual() const {return  mDx;}
inline Float_t StRichSpectra::getExtrapolatedYResidual() const {return  mDy;}
inline Float_t StRichSpectra::getCorrectedExtrapolatedXResidual() const {return  mCdx;}
inline Float_t StRichSpectra::getCorrectedExtrapolatedYResidual() const {return  mCdy;}
inline Float_t StRichSpectra::getCherenkovAngle() const {return  mCherenkovAngle;}
inline Float_t StRichSpectra::getCherenkovSigma() const {return  mCherenkovAngleSigma;}
inline Int_t   StRichSpectra::getCherenkovPhotons() const {return  mNumberOfPhotons;}
inline Float_t StRichSpectra::getPeakAngle() const {return  mPeakAngle;}
inline Int_t   StRichSpectra::getPeakPhotons() const {return  mPeakPhotons;}
inline Int_t   StRichSpectra::getTotalPhotons() const {return  mTotalPhotons;}
inline Float_t StRichSpectra::getMassSquared() const {return  mMassSquared;}
inline Float_t StRichSpectra::getLineIntegralRatio() const {return  mLineIntegralRatio;}
inline Float_t StRichSpectra::getLineIntegral() const {return  mLineIntegral;}
inline Float_t StRichSpectra::getAlpha() const {return  mAlpha;}
inline Int_t   StRichSpectra::getFlag() const {return  mFlag;}
inline Float_t StRichSpectra::getMeanDpi() const { return mDpi; }
inline Float_t StRichSpectra::getMeanDk()  const { return mDk; }
inline Float_t StRichSpectra::getMeanDp()  const { return mDp; }
inline Int_t   StRichSpectra::getMeanDnpi()  const { return mNDpi; }
inline Int_t   StRichSpectra::getMeanDnk()  const { return mNDk; }
inline Int_t   StRichSpectra::getMeanDnp()  const { return mNDp; }

inline Float_t StRichSpectra::getReserved() const {return  mReserved;}
inline Int_t   StRichSpectra::getVersion() const {return  mVersion;}

//non-members
ostream& operator<<(ostream& os, const StRichSpectra& t);
#endif
