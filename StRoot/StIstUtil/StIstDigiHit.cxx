/***************************************************************************
*
* $Id: StIstDigiHit.cxx,v 1.4 2014/04/30 12:11:23 smirnovd Exp $
*
* Author: Yaping Wang (Thank Dmitri Smirnov's updates)
****************************************************************************
* Description:
* Data structure for additional function of StIstHit..
****************************************************************************
*
* $Log: StIstDigiHit.cxx,v $
* Revision 1.4  2014/04/30 12:11:23  smirnovd
* Hide static member inherited from StIstHit base class and assign a new value relevant for StIstDigiHit. Redefine static operators new and delete "inherited" from the base StIstHit class
*
* Revision 1.3  2014/03/27 22:46:47  smirnovd
* Updated broken style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.2  2014/03/01 00:19:37  ypwang
* correct return value of getMeanRow() and Log added
*
*
*
****************************************************************************
* StIstDigiHit.cxx,v 1.0
* Revision 1.0 2014/02/25 21:00:00 Yaping
* Initial version
****************************************************************************/

#include "StIstDigiHit.h"


StIstDigiHit::StIstDigiHit() : StIstHit(), mApv(0), mMeanColumn(-1),
   mMeanRow(-1), mClusterSizeFlag(false)
{ }


StIstDigiHit::StIstDigiHit(const StIstHit &istHit) : StIstHit(istHit)
{
   mApv        = ((unsigned char)((0.5 * kIstSensorActiveSizeZ + istHit.localPosition(2)) / kIstPadPitchColumn)) / 2 + 1;
   mMeanColumn = 0.5 + (0.5 * kIstSensorActiveSizeZ + istHit.localPosition(2)) / kIstPadPitchColumn;
   mMeanRow    = 0.5 + (0.5 * kIstSensorActiveSizeRPhi - istHit.localPosition(0)) / kIstPadPitchRow;
   mClusterSizeFlag = istHit.getNRawHitsRPhi() < 2 ? 1 : 0;
}


void StIstDigiHit::setApv(unsigned char apvId) { mApv = apvId; }
void StIstDigiHit::setMeanColumn(float meanColumn) { mMeanColumn = meanColumn; }
void StIstDigiHit::setClusterSizeFlag(bool flag)   { mClusterSizeFlag = flag; }
void StIstDigiHit::setMeanRow(float meanRow) { mMeanRow = meanRow;}

unsigned char StIstDigiHit::getApv() const { return mApv; }
float StIstDigiHit::getMeanColumn() const { return mMeanColumn; }
float StIstDigiHit::getMeanRow() const { return mMeanRow; }
bool  StIstDigiHit::getClusterSizeFlag() const { return mClusterSizeFlag; }
float StIstDigiHit::localPositionErr(unsigned int i) const
{
   if (i == 0) {
      if (mClusterSizeFlag)
         return kIstPosResolutionRPhi;
      else
         return kIstPosResolutionRPhi2;
   }
   else if (i == 2)
      return kIstPosResolutionZ;
   else
      return 0;
}
