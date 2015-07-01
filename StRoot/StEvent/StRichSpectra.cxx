/***************************************************************************
 *
 * $Id: StRichSpectra.cxx,v 2.3 2002/02/22 03:02:12 ullrich Exp $
 *
 * Author: Brian Lasiuk, Dec 14, 2002
 ***************************************************************************
 *
 * Description: Output from StRichSpectraMaker for uDST storage
 *
 ***************************************************************************
 * $Log: StRichSpectra.cxx,v $
 * Revision 2.3  2002/02/22 03:02:12  ullrich
 * Fixed bug in constructor.
 *
 * Revision 2.2  2002/02/19 16:54:33  ullrich
 * Minor changes - code not altered.
 *
 * Revision 2.1  2002/02/19 04:24:02  lasiuk
 * addition of StRichSpectra information for uDST purposes
 *
 **************************************************************************/
#include "StRichSpectra.h"

static const char rcsid[] = "$Id: StRichSpectra.cxx,v 2.3 2002/02/22 03:02:12 ullrich Exp $";

ClassImp(StRichSpectra)
    
StRichSpectra::StRichSpectra(int v) : mVersion(v) {/*nopt*/}

StRichSpectra::~StRichSpectra() {/*nopt*/}

StRichSpectra::StRichSpectra(float x,   float y,   float dx,    float dy,
			     float cdx, float cdy, float theta, float sigma,
			     int nopho, float pan, int ppho,    int totphotons,
			     float mas, float lir, float li,    float alpha,
			     int flag,  float reserved,
			     float dpi, float dk, float dp,
			     int ndpi, int ndk, int ndp,
			     int version)
    : mExtrapolatedX(x) ,mExtrapolatedY(y), mDx(dx), mDy(dy), mCdx(cdx), mCdy(cdy),
      mCherenkovAngle(theta), mCherenkovAngleSigma(sigma), mNumberOfPhotons(nopho),
      mPeakAngle(pan), mPeakPhotons(ppho), mTotalPhotons(totphotons),
      mMassSquared(mas), mLineIntegralRatio(lir), mLineIntegral(li), mAlpha(alpha),
      mFlag(flag), mReserved(reserved),
      mDpi(dpi), mDk(dk), mDp(dp),
      mNDpi(ndpi), mNDk(ndk), mNDp(ndp),
      mVersion(version)
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
