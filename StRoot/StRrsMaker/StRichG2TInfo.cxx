/****************************************************************
 * $Id: StRichG2TInfo.cxx,v 1.1 2000/04/05 15:55:17 lasiuk Exp $
 *
 * Description:
 *   G2T which is drawn in the pad monitor
 *
 ****************************************************************
 *
 * $Log: StRichG2TInfo.cxx,v $
 * Revision 1.1  2000/04/05 15:55:17  lasiuk
 * Initial Revision
 *
 ****************************************************************/
#include "StRichG2TInfo.h"

StRichG2TInfo::StRichG2TInfo(){ /* nopt */ }

StRichG2TInfo::StRichG2TInfo(double x, double y, int trackp, char* type)
    : mX(x), mY(y), mTrackp(trackp), mType(type) { /* nopt */ }

StRichG2TInfo::~StRichG2TInfo() {/* nopt */}

