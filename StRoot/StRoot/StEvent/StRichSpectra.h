/*!
 * \class StRichSpectra 
 * \author Brian Lasiuk, Dec 14, 2002
 */
/***************************************************************************
 *
 * $Id: StRichSpectra.h,v 2.6 2014/08/06 11:43:08 jeromel Exp $
 *
 * Author: Brian Lasiuk, Dec 14, 2002
 ***************************************************************************
 *
 * Description: Output from StRichSpectraMaker for uDST storage
 *
 ***************************************************************************
 * $Log: StRichSpectra.h,v $
 * Revision 2.6  2014/08/06 11:43:08  jeromel
 * Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
 *
 * Revision 2.5  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.4  2002/02/22 22:56:50  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.3  2002/02/19 23:22:30  ullrich
 * Minor modifications.
 *
 * Revision 2.2  2002/02/19 16:54:33  ullrich
 * Minor changes - code not altered.
 *
 * Revision 2.1  2002/02/19 04:24:03  lasiuk
 * addition of StRichSpectra information for uDST purposes
 *
 **************************************************************************/
#ifndef StRichSpectra_hh
#define StRichSpectra_hh

#include <Stiostream.h>
#include <float.h>
#include "StObject.h"

class StRichSpectra : public StObject {
public: 
    StRichSpectra(int v=-999);
    ~StRichSpectra();
    StRichSpectra(float,float,float,float,float,float,
		  float,float,int,float,int,int,
		  float,float,float,float,int,float,
		  float, float, float,
		  int, int, int,
		  int=-999);
    
    //StRichSpectra(const StRichSpectra&) use default
    //StRichSpectra& operator =(const StRichSpectra&) use default
    
    void setExtrapolatedPosition(float, float);
    void setExtrapolatedResidual(float, float);
    void setCorrectedExtrapolatedResidual(float, float);
    void setCherenkovAngle(float);
    void setCherenkovSigma(float);
    void setCherenkovPhotons(int);
    void setPeakAngle(float);
    void setPeakPhotons(int);
    void setTotalPhotons(int);
    void setMassSquared(float);
    void setLineIntegralRatio(float);
    void setLineIntegral(float);
    void setAlpha(float);
    void setFlag(int);
    void setReserved(float);

    void setMeanD(float pi=FLT_MAX,  float k=FLT_MAX, float p=FLT_MAX);
    void setNumberOfD(int pi=-100,  int k=-100, int p=-100);
    void setVersion(int);
    
    float getExtrapolatedX() const;
    float getExtrapolatedY() const;
    float getExtrapolatedXResidual() const;
    float getExtrapolatedYResidual() const;
    float getCorrectedExtrapolatedXResidual() const;
    float getCorrectedExtrapolatedYResidual() const;
    float getCherenkovAngle() const;
    float getCherenkovSigma() const;
    int   getCherenkovPhotons() const;
    float getPeakAngle() const;
    int   getPeakPhotons() const;
    int   getTotalPhotons() const;
    float getMassSquared() const;
    float getLineIntegralRatio() const;
    float getLineIntegral() const;
    float getAlpha() const;
    int   getFlag() const;
    float getMeanDpi() const;
    float getMeanDk()  const;
    float getMeanDp()  const;
    int   getMeanDnpi()  const;
    int   getMeanDnk()  const;
    int   getMeanDnp()  const;

    float getReserved() const;
    int   getVersion() const;

protected:
    Float_t mExtrapolatedX;
    Float_t mExtrapolatedY;    
    Float_t mDx;
    Float_t mDy;
    Float_t mCdx;
    Float_t mCdy;
    Float_t mCherenkovAngle;
    Float_t mCherenkovAngleSigma;
    Int_t   mNumberOfPhotons;
    Float_t mPeakAngle;
    Int_t   mPeakPhotons;
    Int_t   mTotalPhotons;
    Float_t mMassSquared;
    Float_t mLineIntegralRatio;
    Float_t mLineIntegral;
    Float_t mAlpha;
    Int_t   mFlag;
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
	    "Tag $Name:  $ $Id $ built " __DATE__ " " __TIME__ ;
	return cvs;
    }
private:
    ClassDef(StRichSpectra, 1) 
};

inline void StRichSpectra::setExtrapolatedPosition(float x, float y) {mExtrapolatedX = x;mExtrapolatedY=y;}
inline void StRichSpectra::setExtrapolatedResidual(float dx, float dy) {mDx=dx;mDy=dy;}
inline void StRichSpectra::setCorrectedExtrapolatedResidual(float dx, float dy) {mCdx=dx;mCdy=dy;}
inline void StRichSpectra::setCherenkovAngle(float theta) {mCherenkovAngle=theta;}
inline void StRichSpectra::setCherenkovSigma(float s) {mCherenkovAngleSigma=s;}
inline void StRichSpectra::setCherenkovPhotons(int n) {mNumberOfPhotons=n;}
inline void StRichSpectra::setPeakAngle(float pa) {mPeakAngle=pa;}
inline void StRichSpectra::setPeakPhotons(int np) {mPeakPhotons=np;}
inline void StRichSpectra::setTotalPhotons(int tp) {mTotalPhotons=tp;}
inline void StRichSpectra::setMassSquared(float m2) {mMassSquared=m2;}
inline void StRichSpectra::setLineIntegralRatio(float lir) {mLineIntegralRatio=lir;}
inline void StRichSpectra::setLineIntegral(float li) {mLineIntegral = li;}
inline void StRichSpectra::setAlpha(float alpha) {mAlpha=alpha;}
inline void StRichSpectra::setFlag(int f) {mFlag = f;}
inline void StRichSpectra::setMeanD(float pi,  float k, float p) {mDpi=pi; mDk=k, mDp=p;}
inline void StRichSpectra::setNumberOfD(int pi,  int k, int p) {mNDpi=pi; mNDk=k; mNDp=p;}
inline void StRichSpectra::setReserved(float r) {mReserved = r;}
inline void StRichSpectra::setVersion(int v) {mVersion = v;}

inline float StRichSpectra::getExtrapolatedX() const {return  mExtrapolatedX;}
inline float StRichSpectra::getExtrapolatedY() const {return  mExtrapolatedY;}
inline float StRichSpectra::getExtrapolatedXResidual() const {return  mDx;}
inline float StRichSpectra::getExtrapolatedYResidual() const {return  mDy;}
inline float StRichSpectra::getCorrectedExtrapolatedXResidual() const {return  mCdx;}
inline float StRichSpectra::getCorrectedExtrapolatedYResidual() const {return  mCdy;}
inline float StRichSpectra::getCherenkovAngle() const {return  mCherenkovAngle;}
inline float StRichSpectra::getCherenkovSigma() const {return  mCherenkovAngleSigma;}
inline int   StRichSpectra::getCherenkovPhotons() const {return  mNumberOfPhotons;}
inline float StRichSpectra::getPeakAngle() const {return  mPeakAngle;}
inline int   StRichSpectra::getPeakPhotons() const {return  mPeakPhotons;}
inline int   StRichSpectra::getTotalPhotons() const {return  mTotalPhotons;}
inline float StRichSpectra::getMassSquared() const {return  mMassSquared;}
inline float StRichSpectra::getLineIntegralRatio() const {return  mLineIntegralRatio;}
inline float StRichSpectra::getLineIntegral() const {return  mLineIntegral;}
inline float StRichSpectra::getAlpha() const {return  mAlpha;}
inline int   StRichSpectra::getFlag() const {return  mFlag;}
inline float StRichSpectra::getMeanDpi() const { return mDpi; }
inline float StRichSpectra::getMeanDk()  const { return mDk; }
inline float StRichSpectra::getMeanDp()  const { return mDp; }
inline int   StRichSpectra::getMeanDnpi()  const { return mNDpi; }
inline int   StRichSpectra::getMeanDnk()  const { return mNDk; }
inline int   StRichSpectra::getMeanDnp()  const { return mNDp; }

inline float StRichSpectra::getReserved() const {return  mReserved;}
inline int   StRichSpectra::getVersion() const {return  mVersion;}

//non-members
ostream& operator<<(ostream&, const StRichSpectra&);
#endif
