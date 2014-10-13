/* $Id: StIstHitMaker.h,v 1.9 2014/10/13 22:28:03 smirnovd Exp $ */

#ifndef StIstHitMaker_hh
#define StIstHitMaker_hh

#include "StMaker.h"

class StIstDb;
class THashList;


/**
 * Calculates hit global position, and writes IST hits to StIstHitCollection.
 *
 * \author: Yaping Wang
 * \date March 2013
 */
class StIstHitMaker : public StMaker
{
public:
   StIstHitMaker( const char *name = "ist_hit" );
   Int_t Init();
   Int_t InitRun(Int_t runnumber);
   Int_t Make();

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StIstHitMaker.h,v 1.9 2014/10/13 22:28:03 smirnovd Exp $ built "__DATE__" "__TIME__ ; return cvs;}

protected:
   THashList *listGeoMSensorOnGlobal;
   StIstDb *mIstDb;

private:
   ClassDef(StIstHitMaker, 1);
};
#endif


/***************************************************************************
*
* $Log: StIstHitMaker.h,v $
* Revision 1.9  2014/10/13 22:28:03  smirnovd
* StIstHitMaker: Corrected style with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.8  2014/10/13 22:21:56  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.7  2014/08/12 23:08:09  ypwang
* remove the cluster number cut per ladder, due to chip occupancy cut was added in raw hit maker which can do the bad column rejection
*
* Revision 1.6  2014/07/29 20:13:31  ypwang
* update the IST DB obtain method
*
* Revision 1.5  2014/06/27 21:31:40  ypwang
* remove data member istHitCollection and related Clear() function
*
* Revision 1.4  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstHitMaker.h,v 1.0
* Revision 1.0 2013/11/04 16:05:30 Yaping
* Initial version
****************************************************************************/
