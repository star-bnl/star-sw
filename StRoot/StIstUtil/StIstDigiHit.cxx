#include "StIstDigiHit.h"


StMemoryPool StIstDigiHit::mPool(sizeof(StIstDigiHit));


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
