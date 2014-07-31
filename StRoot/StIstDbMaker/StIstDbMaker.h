/***************************************************************************
*
* $Id: StIstDbMaker.h,v 1.14 2014/07/31 21:01:29 smirnovd Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* IST calibration/geometry DBs access maker.
****************************************************************************
*
* $Log: StIstDbMaker.h,v $
* Revision 1.14  2014/07/31 21:01:29  smirnovd
* Set class version to 1 as version 0 has a special meaning in root cint world
*
* Revision 1.13  2014/07/31 21:00:36  ypwang
* c++ format style improvements; virtual keyword added for destructor
*
* Revision 1.12  2014/07/31 18:24:03  ypwang
* add destructor and deallocate the mIstDb; c++ formatting style improvements and formatted with astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f
*
* Revision 1.11  2014/07/29 19:50:25  ypwang
* IST DB dataset in order to separate from IST Db maker
*
* Revision 1.10  2014/07/15 23:17:52  smirnovd
* Improved doxygen documentation
*
* Revision 1.9  2014/03/27 22:46:55  smirnovd
* Remove unnecessary protection
*
* Revision 1.8  2014/03/27 22:46:38  smirnovd
* Renamed static data member according to mixed star/root convention
*
* Revision 1.7  2014/03/25 03:01:57  ypwang
* get rid of GetIstPedNoise(), GetIstGain(), GetIstMapping() and GetIstControl() functions; use TDataSet instead of Db table structure
*
* Revision 1.6  2014/03/24 15:49:48  ypwang
* checks added and const pointers returned for GetIstPedNoise, GetIstGain, GetIstMapping and GetIstControl functions
*
* Revision 1.5  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstDbMaker.h,v 1.0
* Revision 1.0 2013/11/04 16:15:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstDbMaker_hh
#define StIstDbMaker_hh

#include "StMaker.h"

class StIstDb;

class StIstDbMaker : public StMaker
{

public:
   StIstDbMaker(const char *name = "istDb");
   virtual ~StIstDbMaker();
   Int_t  InitRun(Int_t runNumber);

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstDbMaker.h,v 1.14 2014/07/31 21:01:29 smirnovd Exp $ built "__DATE__" "__TIME__ ; return cvs;}

private:
   StIstDb *mIstDb;

   ClassDef(StIstDbMaker, 1)
};

#endif
