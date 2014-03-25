/***************************************************************************
*
* $Id: StIstDbMaker.h,v 1.7 2014/03/25 03:01:57 ypwang Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* IST calibration/geometry DBs access maker.
****************************************************************************
*
* $Log: StIstDbMaker.h,v $
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

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include "THashList.h"
class TDataSet;

class StIstDbMaker : public StMaker
{

public:
   StIstDbMaker(const char *name = "istDb");
   Int_t  InitRun(Int_t runNumber);
   THashList *GetRotations() 	{return fRotList; }
   const TDataSet *GetPedNoise() {return mPedNoise;}
   const TDataSet *GetGain()     {return mGain;    }
   const TDataSet *GetMapping()  {return mMapping; }
   const TDataSet *GetControl()  {return mControl; }

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstDbMaker.h,v 1.7 2014/03/25 03:01:57 ypwang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

private:
   Int_t CalculateSensorsPosition();

   static THashList *fRotList;
   const TDataSet *mPedNoise;
   const TDataSet *mGain;
   const TDataSet *mMapping;
   const TDataSet *mControl;

   ClassDef(StIstDbMaker, 0)
};
#endif
