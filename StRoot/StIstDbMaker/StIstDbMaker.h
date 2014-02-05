/***************************************************************************
*
* $Id: StIstDbMaker.h,v 1.4 2014/02/05 17:32:46 ypwang Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* IST calibration/geometry DBs access maker.
****************************************************************************
*
* $Log: StIstDbMaker.h,v $
* Revision 1.4  2014/02/05 17:32:46  ypwang
* updating script
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
class St_istPedNoise;
class St_istGain;
class St_istMapping;

class StIstDbMaker : public StMaker
{

public:
   StIstDbMaker(const char *name = "istDb");
   Int_t  InitRun(Int_t runNumber);
   THashList *GetRotations() 	{return fRotList; }
   St_istPedNoise *GetPedNoise() {return mPedNoise;}
   St_istGain	 *GetGain()	{return mGain;    }
   St_istMapping  *GetMapping()  {return mMapping; }

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstDbMaker.h,v 1.4 2014/02/05 17:32:46 ypwang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

private:
   Int_t CalculateSensorsPosition();
   void   GetIstPedNoise();
   void   GetIstGain();
   void    GetIstMapping();

   static THashList *fRotList;
   St_istPedNoise   *mPedNoise;
   St_istGain       *mGain;
   St_istMapping	   *mMapping;

   ClassDef(StIstDbMaker, 0)
};
#endif
