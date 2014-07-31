/***************************************************************************
*
* $Id: StIstDb.h,v 1.2 2014/07/31 21:01:21 smirnovd Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* IST calibration/geometry DBs access maker.
****************************************************************************
*
* $Log: StIstDb.h,v $
* Revision 1.2  2014/07/31 21:01:21  smirnovd
* Made class getters const because we change nothing in the object
*
* Revision 1.1  2014/07/29 19:50:25  ypwang
* IST DB dataset in order to separate from IST Db maker
*
*
****************************************************************************
* StIstDb.h,v 1.0
* Revision 1.0 2014/7/28 16:15:30 Yaping
* Initial version
****************************************************************************/

#ifndef StIstDb_hh
#define StIstDb_hh

#include "StObject.h"
#include "THashList.h"

class Survey_st;
class istPedNoise_st;
class istControl_st;
class istGain_st;
class istMapping_st;
class istChipConfig_st;

class StIstDb : public StObject
{

public:
   StIstDb();
   THashList *GetRotations() const      	{return mgRotList; }
   const istPedNoise_st *GetPedNoise() const 		{return mIstPedNoise;}
   const istGain_st *GetGain() const     		{return mIstGain;    }
   const istMapping_st *GetMapping() const  		{return mIstMapping; }
   const istControl_st *GetControl() const  		{return mIstControl; }
   const istChipConfig_st *GetChipStatus() const 	{return mIstChipStatus; }

   Int_t SetGeoHMatrices(Survey_st **tables);
   void SetPedNoise(istPedNoise_st *pedNoise) 	{mIstPedNoise = pedNoise;}
   void SetGain(istGain_st *gain)		{mIstGain     = gain;}
   void SetMapping(istMapping_st *mapping)    	{mIstMapping  = mapping;}
   void SetControl(istControl_st *control)    	{mIstControl  = control;}
   void SetChipStatus(istChipConfig_st *chipStatus) {mIstChipStatus = chipStatus;}

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstDb.h,v 1.2 2014/07/31 21:01:21 smirnovd Exp $ built "__DATE__" "__TIME__ ; return cvs;}

private:
   static THashList 	*mgRotList;
   istPedNoise_st 	*mIstPedNoise;
   istGain_st 		*mIstGain;
   istMapping_st 	*mIstMapping;
   istControl_st 	*mIstControl;
   istChipConfig_st 	*mIstChipStatus;

   ClassDef(StIstDb, 0)
};

#endif
