/***************************************************************************
 *
 * $Id: StFcsPoint.cxx,v 2.1 2021/01/11 20:25:37 ullrich Exp $
 *
 * Author: akio ogawa 2018
 ***************************************************************************
 *
 * Description: Implementation of StFcsPoint, the StEvent FCS
 *              photon structure
 *
 ***************************************************************************
 *
 * $Log: StFcsPoint.cxx,v $
 * Revision 2.1  2021/01/11 20:25:37  ullrich
 * Initial Revision
 *
 ***************************************************************************/
#include "StFcsPoint.h"
#include "St_base/StMessMgr.h"
#include "TMath.h"

static const char rcsid[] = "$Id: StFcsPoint.cxx,v 2.1 2021/01/11 20:25:37 ullrich Exp $";

StFcsPoint::StFcsPoint() :  StObject(), mFourMomentum(0.,0.,0.,0.) { /* no op */ }

StFcsPoint::~StFcsPoint() { /* no op */ }

void StFcsPoint::print(int opt) {
    cout << Form("StFcsPoint: Det=%2d ClusterId=%2d local=%7.2f %7.2lf xyz=%7.2lf %7.2lf %7.2lf E=%7.2lf ET=%6.2lf",
                 detectorId(), parentClusterId(),
                 x(), y(), 
		 xyz().x(), xyz().y(), xyz().z(), 
		 energy(), fourMomentum().perp())
	 << endl;
}
