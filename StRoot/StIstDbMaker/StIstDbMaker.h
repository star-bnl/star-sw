/***************************************************************************
*
* $Id: StIstDbMaker.h,v 1.3 2014/02/03 16:12:19 ypwang Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* IST calibration/geometry DBs access maker.
****************************************************************************
*
* $Log: StIstDbMaker.h,v $
* Revision 1.3  2014/02/03 16:12:19  ypwang
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
class St_istPedNoise;
class St_istGain;
class St_istMapping;

class StIstDbMaker : public StMaker
{

public:
   StIstDbMaker(const char *name = "istDb");
   virtual Int_t  Init();
   virtual Int_t  InitRun(Int_t runNumber);
   virtual Int_t  Make();
   virtual Int_t  Finish();
   virtual void   Clear(const char *opt);
   virtual THashList *GetRotations() 	{return fRotList; }
   virtual Int_t CalculateSensorsPosition();
   virtual St_istPedNoise *GetPedNoise() {return mPedNoise;}
   virtual St_istGain	 *GetGain()	{return mGain;    }
   virtual St_istMapping  *GetMapping()  {return mMapping; }
   void	  GetIstPedNoise();
   void	  GetIstGain();
   void    GetIstMapping();

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstDbMaker.h,v 1.3 2014/02/03 16:12:19 ypwang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

private:
   static THashList *fRotList;
   St_istPedNoise   *mPedNoise;
   St_istGain       *mGain;
   St_istMapping	   *mMapping;

   ClassDef(StIstDbMaker, 0)
};
#endif
