#include <algorithm>

#include "StPxlDigiHit.h"


const double StPxlDigiHit::mFirstPixelX =  (StPxlConsts::kPxlNumRowsPerSensor - 1) * StPxlConsts::kPixelSize / 2;
const double StPxlDigiHit::mFirstPixelZ = -(StPxlConsts::kPxlNumColumnsPerSensor - 1) * StPxlConsts::kPixelSize / 2;


StPxlDigiHit::StPxlDigiHit() : StPxlHit()
{
}


/** Suitable for constructing a PXL hit in a fast simulation. */
StPxlDigiHit::StPxlDigiHit(const double (&localPos)[3], unsigned sector, unsigned ladder, unsigned sensor,
   unsigned short idTruth) :
   StPxlHit(localPos, sector, ladder, sensor, idTruth)
{
   // Update parents mMeanRow/mMeanColumn using the provided hit coordinates
   mMeanRow    = -(mLocalPosition[0] - mFirstPixelX) / StPxlConsts::kPixelSize;
   mMeanColumn =  (mLocalPosition[2] - mFirstPixelZ) / StPxlConsts::kPixelSize;
}


/** Suitable for constructing a PXL hit from a cluster. */
StPxlDigiHit::StPxlDigiHit(const StPxlCluster &cluster, unsigned sector, unsigned ladder, unsigned sensor) :
   StPxlHit(cluster.rowCenter(), cluster.columnCenter(), sector, ladder, sensor)
{
   mIdTruth  = cluster.idTruth();
   mNRawHits = cluster.nRawHits();

   mLocalPosition[0] = mFirstPixelX - StPxlConsts::kPixelSize * mMeanRow;
   mLocalPosition[1] = 0;
   mLocalPosition[2] = mFirstPixelZ + StPxlConsts::kPixelSize * mMeanColumn;
}


StPxlDigiHit::StPxlDigiHit(const double (&localPos)[3], unsigned sector, unsigned ladder, unsigned sensor,
   const StThreeVectorF& position, const StThreeVectorF& error, unsigned int hwPosition,
   float charge, unsigned char trackRefCount, unsigned short idTruth, unsigned short quality, unsigned short id) :
   StPxlHit(localPos, sector, ladder, sensor, position, error,
      hwPosition, charge, trackRefCount, idTruth, quality, id)
{
   // Update parents mMeanRow/mMeanColumn using the provided hit coordinates
   mMeanRow    = -(mLocalPosition[0] - mFirstPixelX) / StPxlConsts::kPixelSize;
   mMeanColumn =  (mLocalPosition[2] - mFirstPixelZ) / StPxlConsts::kPixelSize;
}


/** Update local X hit coordinate using pixel unit representation. */
void StPxlDigiHit::setMeanRow(double val)
{
   mMeanRow = val;
   mLocalPosition[0] = mFirstPixelX - StPxlConsts::kPixelSize * mMeanRow;
}


/** Update local Z hit coordinate using pixel unit representation. */
void StPxlDigiHit::setMeanColumn(double val)
{
   mMeanColumn = val;
   mLocalPosition[2] = mFirstPixelZ + StPxlConsts::kPixelSize * mMeanColumn;
}


/** Update hit coordinates by providing values in cm. */
void StPxlDigiHit::setLocalPosition(const double (&coords)[3])
{
   std::copy(coords, coords+3, mLocalPosition);

   mMeanRow    = -(mLocalPosition[0] - mFirstPixelX) / StPxlConsts::kPixelSize;
   mMeanColumn =  (mLocalPosition[2] - mFirstPixelZ) / StPxlConsts::kPixelSize;
}
