/***************************************************************************
 *
 * $Id: StRichSpectra.cxx,v 2.1 2002/02/19 04:24:02 lasiuk Exp $
 *
 * Author: bl
 *         Dec 14, 2002
 ***************************************************************************
 *
 * Description: Output from StRichSpectraMaker for uDST storage
 *
 ***************************************************************************
 * $Log: StRichSpectra.cxx,v $
 * Revision 2.1  2002/02/19 04:24:02  lasiuk
 * addition of StRichSpectra information for uDST purposes
 *
 **************************************************************************/
#include "StRichSpectra.h"

static const char rcsid[] = "$Id: StRichSpectra.cxx,v 2.1 2002/02/19 04:24:02 lasiuk Exp $";

ClassImp(StRichSpectra)
    
StRichSpectra::StRichSpectra(Int_t v) {/*nopt*/}

StRichSpectra::~StRichSpectra() {/*nopt*/}

StRichSpectra::StRichSpectra(Float_t x,   Float_t y,   Float_t dx,    Float_t dy,
			     Float_t cdx, Float_t cdy, Float_t theta, Float_t sigma,
			     Int_t nopho, Float_t pan, Int_t ppho,    Int_t totphotons,
			     Float_t mas, Float_t lir, Float_t li,    Float_t alpha,
			     Int_t flag,  Float_t reserved,
			     Float_t dpi, Float_t dk, Float_t dp,
			     Int_t ndpi, Int_t ndk, Int_t ndp,
			     Int_t version)
    : mExtrapolatedX(x) ,mExtrapolatedY(y), mDx(dx), mDy(dy), mCdx(cdx), mCdy(cdy),
      mCherenkovAngle(theta), mCherenkovAngleSigma(sigma), mNumberOfPhotons(nopho),
      mPeakAngle(pan), mPeakPhotons(ppho), mTotalPhotons(totphotons),
      mMassSquared(mMassSquared), mLineIntegralRatio(mLineIntegralRatio),
      mLineIntegral(mLineIntegral), mAlpha(mAlpha), mFlag(mFlag), mReserved(mReserved),
      mDpi(dpi), mDk(dk), mDp(dp), mNDpi(ndpi), mNDk(ndk), mNDp(ndp),
      mVersion(mVersion)
{/*nopt*/}
    
ostream&
operator<<(ostream& os, const StRichSpectra& t)
{
    return (os << "StRichSpectra::>"
            << "\n\tExtrapolatedX:  " << t.getExtrapolatedX() << ", " << t.getExtrapolatedY()
            << "\n\tResidualX:      " << t.getExtrapolatedXResidual() << ", " << t.getExtrapolatedYResidual()
            << "\n\tCorr Residual:  " << t.getCorrectedExtrapolatedXResidual() << ", " << t.getCorrectedExtrapolatedYResidual()
            << "\n\tCherenkovAngle: " << t.getCherenkovAngle() << "+/-" << t.getCherenkovSigma() << " (" << t.getCherenkovPhotons() << ")"
            << "\n\tPeakAngle       " << t.getPeakAngle() << " (" << t.getTotalPhotons() << ")"
            << "\n\tMass2           " << t.getMassSquared()
	    << "\n\tLineIntegral    " << t.getLineIntegralRatio() << "-->" << t.getLineIntegral()
	    << "\n\tAlpha           " << t.getAlpha()
	    << "\n\tFlag            " << t.getFlag()
	    << "\n\treserved        " << t.getReserved()
	    << "\n\tversion         " << t.getVersion());
}
