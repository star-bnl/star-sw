#ifndef STSSTDBMAKER_H
#define STSSTDBMAKER_H

#ifndef StMaker_H
#include "StMaker.h"
#endif
class St_sstWafersPosition;
class sstSlsCtrl_st;
class sstDimensions_st;
class sstConfiguration_st;
class sstMaskChip_st;
#include "StSstUtil/StSstBarrel.hh"
#include "THashList.h"

class StSstDbMaker : public StMaker
{
private:
   StSstBarrel           *mySst;
   sstDimensions_st      *dimensions;//!
   sstConfiguration_st   *config;//!
   sstSlsCtrl_st         *ctrl;//!
   Int_t                  mode;//!
   EReturnCodes           mReady; ///< Status code returned by Make(). It is !kStOk if failed to access DB tables
   static THashList      *fRotList;
   map<unsigned int,short> mMapMaskChips; //!

public:
   StSstDbMaker(const char *name = "SstDb");
   virtual     ~StSstDbMaker();
   virtual Int_t  InitRun(Int_t runNumber);
   virtual Int_t  Make();
   virtual THashList            *getRotations(){return fRotList;}
   virtual St_sstWafersPosition *calculateWafersPosition();
   virtual StSstBarrel          *getSst(){return mySst;}
   virtual sstSlsCtrl_st        *getSlsCtrl(){return ctrl;}
   virtual Int_t                 getMode(){return mode;}
   virtual St_sstWafersPosition *getSstWafersPos(){return m_positions;}
   virtual sstDimensions_st     *getSstDimensions(){return dimensions;}

   static const TGeoHMatrix     *getHMatrixSensorOnGlobal(int ladder, int sensor);

   Int_t maskChip(Int_t side, Int_t ladder, Int_t wafer, Int_t chip) const; 
   void  setMaskChips(sstMaskChip_st *maskChipTable);
   St_sstWafersPosition  *m_positions;//!
   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StSstDbMaker.h,v 1.11 2016/05/31 21:51:59 bouchet Exp $ built " __DATE__ " " __TIME__ ; return cvs;}
   ClassDef(StSstDbMaker, 0)  //StAF chain virtual base class for Makers
};
// Global pointers:
R__EXTERN StSstDbMaker *gStSstDbMaker;
#endif


