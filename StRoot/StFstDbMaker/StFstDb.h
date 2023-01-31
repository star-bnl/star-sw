/* $Id: StFstDb.h */

#ifndef StFstDb_hh
#define StFstDb_hh

#include "StObject.h"
#include "THashList.h"
#include "TGeoMatrix.h"
#include "StEvent/StFstConsts.h"

class Survey_st;
class fstPedNoise_st;
class fstControl_st;
class fstGain_st;
class fstMapping_st;
class fstChipConfig_st;


/**
 * FST calibration/geometry database access proxy.
 *
 * relation within STAR frame
 * FstOnGlobal = Tpc2Magnet * Fst2Tpc *    Hss2Fst     * Wedge2Hss * Sensor2Wedge * PS

 * Naming of roatation matrices in this maker :
 * positionGlobal  = tpc2Global * fst2Tpc * hss2Fst * wedge2Hss * sensor2Wedge * positionOnSensor

 * numbering
 * Id  = 1000 + (wedge-1)*3 + sensor
 * 1<= wedge <= 36
 * 0<= sensor <= 2
 *
 *
 * \author Shenghui Zhang
 * \date Oct 2021
 */
class StFstDb : public StObject
{

public:
   StFstDb();
   THashList *getRotations() const                                                {return mgRotList; }
   const TGeoHMatrix *getGeoHMatrixTpcOnGlobal() const                            {return mGeoHMatrixTpcOnGlobal; }
   const TGeoHMatrix *getGeoHMatrixFstOnTpc() const                               {return &mGeoHMatrixFstOnTpc; }
   const TGeoHMatrix *getGeoHMatrixHssOnFst() const                               {return &mGeoHMatrixHssOnFst; }
   const TGeoHMatrix *getGeoHMatrixWedgeOnHss() const                             {return &mGeoHMatrixWedgeOnHss; }
   const TGeoHMatrix *getGeoHMatrixSensorOnWedge(Int_t wedge, Int_t sensor) const {return &mGeoHMatrixSensorOnWedge[wedge - 1][sensor]; }
   static const TGeoHMatrix *getHMatrixSensorOnGlobal(int wedge, int sensor);

   const fstPedNoise_st *getPedNoise() const 		{return mFstPedNoise;}
   const fstGain_st *getGain() const     		{return mFstGain;    }
   const fstMapping_st *getMapping() const  		{return mFstMapping; }
   const fstControl_st *getControl() const  		{return mFstControl; }
   const fstChipConfig_st *getChipStatus() const 	{return mFstChipStatus; }
      
   Int_t setGeoHMatrices(Survey_st **tables);
   void setPedNoise(fstPedNoise_st *pedNoise) 	    { mFstPedNoise   = pedNoise;   }
   void setGain(fstGain_st *gain)		    { mFstGain       = gain;       }
   void setMapping(fstMapping_st *mapping)    	    { mFstMapping    = mapping;    }
   void setControl(fstControl_st *control)    	    { mFstControl    = control;    }
   void setChipStatus(fstChipConfig_st *chipStatus) { mFstChipStatus = chipStatus; }
   
   virtual void Print(Option_t *opt = "") const;

private:
   static THashList 	*mgRotList; ///< A list of TGeoHMatrix transormations for each FST sensor
   TGeoHMatrix *mGeoHMatrixTpcOnGlobal;
   TGeoHMatrix mGeoHMatrixFstOnTpc;
   TGeoHMatrix mGeoHMatrixHssOnFst;
   TGeoHMatrix mGeoHMatrixWedgeOnHss;
   TGeoHMatrix mGeoHMatrixSensorOnWedge[kFstNumWedges][kFstNumSensorsPerWedge];

   fstPedNoise_st 	*mFstPedNoise;
   fstGain_st 		*mFstGain;
   fstMapping_st 	*mFstMapping;
   fstControl_st 	*mFstControl;
   fstChipConfig_st 	*mFstChipStatus;
   
   ClassDef(StFstDb, 1)
};

#endif
