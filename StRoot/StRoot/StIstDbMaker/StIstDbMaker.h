/* $Id: StIstDbMaker.h,v 1.23 2015/02/04 07:56:20 smirnovd Exp $ */

#ifndef StIstDbMaker_hh
#define StIstDbMaker_hh

#include "StMaker.h"

class StIstDb;


/**
 * IST calibration/geometry DBs access maker.
 *
 * \author Yaping Wang
 * \date June 2013
 */
class StIstDbMaker : public StMaker
{
public:
   StIstDbMaker(const char *name = "istDb");
   virtual Int_t Init();
   Int_t  InitRun(Int_t runNumber);
   Int_t  Make();

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstDbMaker.h,v 1.23 2015/02/04 07:56:20 smirnovd Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

private:
   /// See StIstDb for details on created data structure. The ownership is passed to the STAR
   /// framework via ToWhiteBoard()
   StIstDb *mIstDb;
   Int_t mReady;

   ClassDef(StIstDbMaker, 0)
};

#endif


/***************************************************************************
*
* $Log: StIstDbMaker.h,v $
* Revision 1.23  2015/02/04 07:56:20  smirnovd
* Create StIstDb object in constructor and pass it to the framework in Init()
*
* It makes perfect sense to do it this way because the StIstDb obect is created
* once by the maker and later reused/updated only at every new run.
*
* Revision 1.22  2014/11/19 18:29:47  genevb
* Use flags to indicate DbMaker readiness
*
* Revision 1.21  2014/11/19 04:17:34  genevb
* Return fatal if database tables are not found
*
* Revision 1.20  2014/11/18 23:11:50  smirnovd
* Set class version to 0 in order to avoid IO dictionary generation by ROOT's CINT. STAR makers are not persistent
*
* Revision 1.19  2014/11/18 23:11:35  smirnovd
* [Minor] Coding style clean-up. Removed unconstructive comments
*
* Revision 1.18  2014/11/18 23:10:27  smirnovd
* Do not destruct StIstDb object as the ownership is passed to the framework
*
* Revision 1.17  2014/11/18 23:08:37  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.16  2014/08/06 11:43:22  jeromel
* Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
*
* Revision 1.15  2014/08/01 22:15:04  ypwang
* mIstDb geometry matrices print out when Debug2 enabled
*
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
