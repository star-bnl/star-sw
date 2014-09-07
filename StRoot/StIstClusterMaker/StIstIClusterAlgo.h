/***************************************************************************
* $Id: StIstIClusterAlgo.h,v 1.7 2014/09/07 11:41:36 ypwang Exp $
* Author: Yaping Wang, March 2013
***************************************************************************/

#ifndef StIstIClusterAlgo_hh
#define StIstIClusterAlgo_hh

#include "Stypes.h"
class StIstRawHitCollection;
class StIstClusterCollection;
class StIstCollection;


/**
* Description: Virtual class for clustering algorithm implementation.
*/
class StIstIClusterAlgo
{
public:
   virtual Int_t doClustering(const StIstCollection &istCollection, StIstRawHitCollection &, StIstClusterCollection & ) = 0;

   virtual ~StIstIClusterAlgo() = 0;

   virtual void setUsedTimeBin(unsigned char tb) = 0;
   virtual void setSplitFlag( bool splitFlag) = 0;

protected:

private:

   ClassDef( StIstIClusterAlgo, 0 );
};
#endif


/***************************************************************************
* $Log: StIstIClusterAlgo.h,v $
* Revision 1.7  2014/09/07 11:41:36  ypwang
* ClassDef version updated from 1 to 0, and remove Init() function
*
* Revision 1.6  2014/08/22 21:32:45  smirnovd
* Moved doxygen comment to where it belongs
*
* Revision 1.5  2014/08/22 15:55:15  smirnovd
* Fixed style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.4  2014/08/22 15:50:00  smirnovd
* Moved CVS history to the end of file
*
* Revision 1.3  2014/02/08 03:34:16  ypwang
* updating scripts
*
* Revision 1.0 2013/11/04 Yaping
* Initial version
****************************************************************************/
