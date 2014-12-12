#ifndef StIstCollection_hh
#define StIstCollection_hh

#include "StObject.h"
#include "StEvent/StEnumerations.h"
#include "StIstRawHitCollection.h"
#include "StIstClusterCollection.h"
#include "StIstConsts.h"

using namespace StIstConsts;


/**
 * A data collection for StIstRawHitCollection and StIstClusterCollection
 * classes, and not written into StEvent.
 *
 * \author Yaping Wang
 * \date March 2013
 */
class StIstCollection : public StObject
{
public:
   StIstCollection();
   ~StIstCollection();

   size_t getNumRawHits() const;                   // overall
   size_t getNumRawHits( int ladder) const;      // per ladder
   size_t getNumClusters() const;                  // overall
   size_t getNumClusters( int ladder ) const;    // per ladder
   size_t getNumTimeBins() const;
   void setNumTimeBins(size_t nTimebin);

   StIstRawHitCollection *getRawHitCollection( int ladder );
   const StIstRawHitCollection *getRawHitCollection( int ladder ) const;

   StIstClusterCollection *getClusterCollection( int ladder );
   const StIstClusterCollection *getClusterCollection( int ladder ) const;

protected:
   StIstRawHitCollection mRawHitCollection[kIstNumLadders];
   StIstClusterCollection mClusterCollection[kIstNumLadders];
   size_t mNumTimeBins;

   ClassDef(StIstCollection, 1);
};

#endif


/***************************************************************************
*
* $Log: StIstCollection.h,v $
* Revision 1.8.2.1  2014/12/12 22:33:29  smirnovd
* Squashed commit of the following:
*
*     StIstDb: Modified getter for sensors transormation matrix to accept ladder and sensor id-s using human friendly numbering starting with 1. The input values outside of possible ranges will return a null pointer
*
*     Use flags to indicate DbMaker readiness
*
*     Return fatal if database tables are not found
*
*     StIstDb: Added method to access transformation matrix for a given IST ladder/sensor pair
*
*     Set class version to 0 in order to avoid IO dictionary generation by ROOT's CINT. STAR makers are not persistent
*
*     [Style] Changes in comments and user feedback only
*
*     [Minor] Coding style clean-up. Removed unconstructive comments
*
*     Do not destruct StIstDb object as the ownership is passed to the framework
*
*     Renamed printGeoHMatrices to customary Print as that what users of ROOT framework normaly expect
*
*     Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.10  2014/11/18 23:11:36  smirnovd
* [Minor] Coding style clean-up. Removed unconstructive comments
*
* Revision 1.9  2014/11/18 23:08:38  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.8  2014/09/09 08:23:46  ypwang
* all unsgined char was updated to int type as Victor P. suggested
*
* Revision 1.7  2014/03/27 22:47:02  smirnovd
* Remove unnecessary Clear() method. Use destructor instead
*
* Revision 1.6  2014/03/13 22:10:12  smirnovd
* Move some constants from StIstUtil/StIstConsts.h to StEvent/StEnumerations.h to avoid external dependance of StEvent on StIstUtil
*
* Revision 1.5  2014/02/14 14:37:57  ypwang
* remove StMuDstMaker and getNumLadders() member function from StIstCollection
*
* Revision 1.4  2014/02/13 02:35:49  smirnovd
* Moved CVS log to the bottom of the file
*
* Revision 1.3  2014/02/03 16:12:20  ypwang
* updating scripts
*
*
****************************************************************************
* StIstCollection.h,v 1.0
* Revision 1.0 2013/11/04 15:15:30 Yaping
* Initial version
****************************************************************************/
