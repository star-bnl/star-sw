/*****************************************************************
 * $Id: StMuPmdHit.cxx,v 1.1 2004/10/19 01:40:21 mvl Exp $
 *
 * Class : StMuPmdHit
 * Author: Supriya Das
 * ****************************************************************
 *
 * Description: This is the Hit class for PMD in MuDst
 * ****************************************************************
 * $Log: StMuPmdHit.cxx,v $
 * Revision 1.1  2004/10/19 01:40:21  mvl
 * New class to hold Pmd hits (raw adc information)
 *
 *
 * ****************************************************************/

#include "StMuPmdHit.h"

ClassImp(StMuPmdHit)

StMuPmdHit::StMuPmdHit()
{
}

StMuPmdHit::~StMuPmdHit()
{
}

StMuPmdHit::StMuPmdHit(StMuPmdHit* hit)
{
  mEnergy =hit ->energy();
  mADC = hit->adc();
  mSuperModule = hit->superModule();
  mSubDetector = hit->subDetector();
  mRow = hit->row();
  mCol = hit->column();
}
