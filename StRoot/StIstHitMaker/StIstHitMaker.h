/***************************************************************************
*
* $Id: StIstHitMaker.h,v 1.4 2014/02/08 03:34:16 ypwang Exp $
*
* Author: Yaping Wang, March 2013
****************************************************************************
* Description: 
* Calculates hit global position, and writes IST hits to StIstHitCollection.
****************************************************************************
*
* $Log: StIstHitMaker.h,v $
* Revision 1.4  2014/02/08 03:34:16  ypwang
* updating scripts
*
*
****************************************************************************
* StIstHitMaker.h,v 1.0
* Revision 1.0 2013/11/04 16:05:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstHitMaker_hh
#define StIstHitMaker_hh

#include "StMaker.h"

class StIstDbMaker;
class THashList;
class St_istControl;
class StIstHitCollection;

class StIstHitMaker : public StMaker
{
 public:
  StIstHitMaker( const char* name="ist_hit" );
  Int_t Init();
  Int_t InitRun(Int_t runnumber);
  Int_t Make();
  void Clear( Option_t *opts = "" );

  virtual const char *GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StIstHitMaker.h,v 1.4 2014/02/08 03:34:16 ypwang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

 protected:
  THashList *listGeoMSensorOnGlobal;
  StIstDbMaker *mIstDbMaker;

  UShort_t mMinNumOfRawHits, mMaxNumOfRawHits;

  StIstHitCollection *istHitCollection;

 private:
  ClassDef(StIstHitMaker,1);
};
#endif
