/***************************************************************************
*
* $Id: StIstDb.h,v 1.5 2014/08/05 17:48:58 ypwang Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* IST calibration/geometry DBs access maker.
****************************************************************************
*
* $Log: StIstDb.h,v $
* Revision 1.5  2014/08/05 17:48:58  ypwang
* update Print() function to PrintGeoHMatrices()
*
* Revision 1.4  2014/08/01 22:25:48  ypwang
* Add several simple getters and data members for sub-level geometry matrices obtain; Add Print() function which print out all IST geometry matrices
*
* Revision 1.3  2014/07/31 21:01:29  smirnovd
* Set class version to 1 as version 0 has a special meaning in root cint world
*
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
#include "TGeoMatrix.h"
#include "StEvent/StEnumerations.h"
using namespace StIstConsts;

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
   const TGeoHMatrix *GetGeoHMatrixTpcOnGlobal() const	{return mGeoHMatrixTpcOnGlobal; }
   const TGeoHMatrix *GetGeoHMatrixIdsOnTpc() const     	{return &mGeoHMatrixIdsOnTpc; }
   const TGeoHMatrix *GetGeoHMatrixPstOnIds() const     	{return &mGeoHMatrixPstOnIds; }
   const TGeoHMatrix *GetGeoHMatrixIstOnPst() const     	{return &mGeoHMatrixIstOnPst; }
   const TGeoHMatrix *GetGeoHMatrixLadderOnIst(Int_t ladder) const     {return &mGeoHMatrixLadderOnIst[ladder - 1]; }
   const TGeoHMatrix *GetGeoHMatrixSensorOnLadder(Int_t ladder, Int_t sensor) const	{return &mGeoHMatrixSensorOnLadder[ladder - 1][sensor - 1]; }

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

   void PrintGeoHMatrices() const;

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstDb.h,v 1.5 2014/08/05 17:48:58 ypwang Exp $ built "__DATE__" "__TIME__ ; return cvs;}

private:
   static THashList 	*mgRotList;
   TGeoHMatrix *mGeoHMatrixTpcOnGlobal;
   TGeoHMatrix mGeoHMatrixIdsOnTpc;
   TGeoHMatrix mGeoHMatrixPstOnIds;
   TGeoHMatrix mGeoHMatrixIstOnPst;
   TGeoHMatrix mGeoHMatrixLadderOnIst[kIstNumLadders];
   TGeoHMatrix mGeoHMatrixSensorOnLadder[kIstNumLadders][kIstNumSensorsPerLadder];

   istPedNoise_st 	*mIstPedNoise;
   istGain_st 		*mIstGain;
   istMapping_st 	*mIstMapping;
   istControl_st 	*mIstControl;
   istChipConfig_st 	*mIstChipStatus;

   ClassDef(StIstDb, 1)
};

#endif
