#ifndef StPxlDigiHit_h
#define StPxlDigiHit_h

#include "StPxlUtil/StPxlConstants.h"
#include "StPxlClusterMaker/StPxlCluster.h"
#include "StEvent/StPxlHit.h"


/** */
class StPxlDigiHit: public StPxlHit
{
public:

   StPxlDigiHit();
   StPxlDigiHit(const double (&localPos)[3], unsigned sector, unsigned ladder, unsigned sensor, unsigned short idTruth);
   StPxlDigiHit(const StPxlCluster &cluster, unsigned sector, unsigned ladder, unsigned sensor);
   StPxlDigiHit(const double (&localPos)[3], unsigned sector, unsigned ladder, unsigned sensor,
      const StThreeVectorF& position, const StThreeVectorF& error, unsigned int hwPosition,
      float charge, unsigned char trachRefCount, unsigned short idTruth, unsigned short quality, unsigned short id);

   void setMeanRow(double val);
   void setMeanColumn(double val);
   void setLocalPosition(const double (&coords)[3]);

private:

    //@{
    /**
     * Local coordinates of the left lower corner of PXL sensor. These static
     * values are used to convert local hit position from cm to "pixel" units
     * and back.
     */
    static const double mFirstPixelX;
    static const double mFirstPixelZ;
    //@}
};

#endif
