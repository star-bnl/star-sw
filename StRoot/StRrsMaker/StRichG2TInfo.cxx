/****************************************************************
 * $Id: StRichG2TInfo.cxx,v 2.0 2000/08/09 16:17:00 gans Exp $
 *
 * Description:
 *   G2T which is drawn in the pad monitor
 *
 ****************************************************************
 *
 * $Log: StRichG2TInfo.cxx,v $
 * Revision 2.0  2000/08/09 16:17:00  gans
 * Readded Files That were not added in last CVS. Cosmetic Changes, naming convention
 * for StRichDrawableT(foo)
 *
 * Revision 1.1  2000/04/05 15:55:17  lasiuk
 * Initial Revision
 *
 ****************************************************************/
#include "StRichG2TInfo.h"

StRichG2TInfo::StRichG2TInfo(){ /* nopt */ }

StRichG2TInfo::StRichG2TInfo(double x, double y, int trackp, char* type)
    : mX(x), mY(y), mTrackp(trackp), mType(type) { /* nopt */ }

StRichG2TInfo::~StRichG2TInfo() {/* nopt */}

