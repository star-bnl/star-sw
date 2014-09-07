/***************************************************************************
* $Id: StIstIClusterAlgo.h,v 1.8 2014/09/07 13:54:45 ypwang Exp $
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

   void setUsedTimeBin(unsigned char tb = -1);
   void setSplitFlag( bool splitFlag = 1);

protected:
   Bool_t mSplitCluster;
   UChar_t mTimeBin;

private:

   ClassDef( StIstIClusterAlgo, 0 );
};

inline void StIstIClusterAlgo::setSplitFlag( bool splitFlag )      { mSplitCluster = splitFlag; };
inline void StIstIClusterAlgo::setUsedTimeBin(unsigned char tb)    { mTimeBin = tb; };
#endif


/***************************************************************************
* $Log: StIstIClusterAlgo.h,v $
* Revision 1.8  2014/09/07 13:54:45  ypwang
* move setUsedTimeBin() and setSplitFlag() setters from inherited classes to their base class StIstIClusterAlgo.h
*
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
