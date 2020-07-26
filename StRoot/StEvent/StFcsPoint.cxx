/***************************************************************************
 *
 * $Id: StFcsPoint.cxx,v 1.3 2020/05/29 18:57:04 akio Exp $
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
 * Revision 1.3  2020/05/29 18:57:04  akio
 * adding some enum for EPD as PRES and 4x4 trigger patch
 *
 * Revision 1.2  2019/10/23 13:27:07  akio
 * including StFcsPoint for StFcsPointMaker
 *
 * Revision 1.1  2018/11/14 16:49:00  akio
 * FCS codes in offline/upgrade/akio
 *
 *
 ***************************************************************************/
#include "StFcsPoint.h"
#include "St_base/StMessMgr.h"
#include "TMath.h"

static const char rcsid[] = "$Id: StFcsPoint.cxx,v 1.3 2020/05/29 18:57:04 akio Exp $";

StFcsPoint::StFcsPoint() :  StObject(), mFourMomentum(0.,0.,0.,0.) { /* no op */ }

StFcsPoint::~StFcsPoint() { /* no op */ }

void StFcsPoint::print(int opt) {
    cout << Form("StFcsPoint: Det=%2d ClusterId=%2d local=%7.2f %7.2f xyz=%7.2f %7.2f %7.2f E=%7.2f ET=%6.2f",
                 detectorId(), parentClusterId(),
                 x(), y(), 
		 XYZ().x(), XYZ().y(), XYZ().z(), 
		 energy(), fourMomentum().perp())
	 << endl;
}
