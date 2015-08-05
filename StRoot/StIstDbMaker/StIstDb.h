/* $Id: StIstDb.h,v 1.12 2015/08/03 14:26:03 smirnovd Exp $ */

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


/**
 * IST calibration/geometry database access proxy.
 *
 * relation within STAR frame
 * IstOnGlobal = Tpc2Magnet * Ids2Tpc *    Ist2Ids     * Ladder2Ist * Sensor2Ladder * PS
 * with
 * Ids2Tpc = IstIdsOnTpc
 * Ist2Ids = IstIstOnPst * IstPstOnIds

 * Naming of roatation matrices in this maker :
 * positionGlobal  = tpc2Global * ids2Tpc * pst2Ids * ist2Pst * ladder2Ist * sensor2Ladder * positionOnSensor

 * numbering
 * Id  = 1000 + (ladder-1)*6 + sensor
 * 1<= ladder <= 24
 * 1<= sensor <= 6
 *
 *
 * \author Yaping Wang
 * \date June 2013
 */
class StIstDb : public StObject
{

public:
   StIstDb();
   virtual ~StIstDb() {fgInstance = 0;}
   static StIstDb* 	instance() {return fgInstance;}
   THashList *getRotations() const      	{return mgRotList; }
   const TGeoHMatrix *getGeoHMatrixTpcOnGlobal() const	{return mGeoHMatrixTpcOnGlobal; }
   const TGeoHMatrix *getGeoHMatrixIdsOnTpc() const     	{return &mGeoHMatrixIdsOnTpc; }
   const TGeoHMatrix *getGeoHMatrixPstOnIds() const     	{return &mGeoHMatrixPstOnIds; }
   const TGeoHMatrix *getGeoHMatrixIstOnPst() const     	{return &mGeoHMatrixIstOnPst; }
   const TGeoHMatrix *getGeoHMatrixLadderOnIst(Int_t ladder) const     {return &mGeoHMatrixLadderOnIst[ladder - 1]; }
   const TGeoHMatrix *getGeoHMatrixSensorOnLadder(Int_t ladder, Int_t sensor) const	{return &mGeoHMatrixSensorOnLadder[ladder - 1][sensor - 1]; }
   static const TGeoHMatrix *getHMatrixSensorOnGlobal(int ladder, int sensor);

   const istPedNoise_st *getPedNoise() const 		{return mIstPedNoise;}
   const istGain_st *getGain() const     		{return mIstGain;    }
   const istMapping_st *getMapping() const  		{return mIstMapping; }
   const istControl_st *getControl() const  		{return mIstControl; }
   const istChipConfig_st *getChipStatus() const 	{return mIstChipStatus; }

   Int_t setGeoHMatrices(Survey_st **tables);
   void setPedNoise(istPedNoise_st *pedNoise) 	{mIstPedNoise = pedNoise;}
   void setGain(istGain_st *gain)		{mIstGain     = gain;}
   void setMapping(istMapping_st *mapping)    	{mIstMapping  = mapping;}
   void setControl(istControl_st *control)    	{mIstControl  = control;}
   void setChipStatus(istChipConfig_st *chipStatus) {mIstChipStatus = chipStatus;}

   virtual void Print(Option_t *opt = "") const;

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstDb.h,v 1.12 2015/08/03 14:26:03 smirnovd Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

private:
   static THashList 	*mgRotList; ///< A list of TGeoHMatrix transormations for each IST sensor
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
   static StIstDb       *fgInstance;

   ClassDef(StIstDb, 1)
};

#endif


/***************************************************************************
*
* $Log: StIstDb.h,v $
* Revision 1.12  2015/08/03 14:26:03  smirnovd
* Corrected style with 'astyle -s3 -p -H -A3 -k3 -O -o -y -Y -f'
*
* Revision 1.11  2014/11/18 23:11:57  smirnovd
* StIstDb: Added method to access transformation matrix for a given IST ladder/sensor pair
*
* Revision 1.10  2014/11/18 23:10:20  smirnovd
* Renamed printGeoHMatrices to customary Print as that what users of ROOT framework normaly expect
*
* Revision 1.9  2014/11/18 23:08:37  smirnovd
* Moved CVS log to the end of file and updated doxygen-style comments
*
* Revision 1.8  2014/09/05 17:36:54  smirnovd
* Slightly cleaned up (doxygen) comments
*
* Revision 1.7  2014/08/06 18:44:21  ypwang
* replace assert statement for gStTpcDb with normal variable check and LOG_WARN printout; non-ROOT methods formatted with STAR coding style
*
* Revision 1.6  2014/08/06 11:43:22  jeromel
* Suffix on literals need to be space (later gcc compiler makes it an error) - first wave of fixes
*
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
