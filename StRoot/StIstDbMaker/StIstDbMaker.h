/***************************************************************************
*
* $Id: StIstDbMaker.h,v 1.10 2014/07/15 23:17:52 smirnovd Exp $
*
* Author: Yaping Wang, June 2013
****************************************************************************
* Description:
* IST calibration/geometry DBs access maker.
****************************************************************************
*
* $Log: StIstDbMaker.h,v $
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

#ifndef StIstDbMaker_hh
#define StIstDbMaker_hh

#include "StMaker.h"
#include "THashList.h"

class TDataSet;


/*!
 * This maker retrieves data from the IST detector survey position measurements,
 * channel status, and other calibration and run time information via the
 * standard STAR database interface.
 *
 * With the survey data the transformation to the global STAR coordinate system
 * are represented as:
 *
 * <pre>
 * IstOnGlobal = Tpc2Magnet * Ids2Tpc *    Ist2Ids     * Ladder2Ist * Sensor2Ladder * PS
 *
 * with
 *
 * Ids2Tpc = IstIdsOnTpc
 * Ist2Ids = IstIstOnPst * IstPstOnIds
 * </pre>
 *
 * Naming and number convention of rotation matrices used in this maker:
 *
 * <pre>
 * positionGlobal  = tpc2Global * ids2Tpc * pst2Ids * ist2Pst * ladder2Ist * sensor2Ladder * positionOnSensor
 *
 * Id  = 1000 + (ladder-1)*6 + sensor
 * 1<= ladder <= 24
 * 1<= sensor <= 6
 * </pre>
 */
class StIstDbMaker : public StMaker
{

public:
   StIstDbMaker(const char *name = "istDb");
   Int_t  InitRun(Int_t runNumber);
   THashList *GetRotations() 	{return mgRotList; }
   const TDataSet *GetPedNoise() {return mPedNoise;}
   const TDataSet *GetGain()     {return mGain;    }
   const TDataSet *GetMapping()  {return mMapping; }
   const TDataSet *GetControl()  {return mControl; }

   virtual const char *GetCVS() const
   {static const char cvs[] = "Tag $Name:  $ $Id: StIstDbMaker.h,v 1.10 2014/07/15 23:17:52 smirnovd Exp $ built "__DATE__" "__TIME__ ; return cvs;}

private:
   Int_t CalculateSensorsPosition();

   static THashList *mgRotList; ///< A list of TGeoHMatrix transormations for each IST sensor
   const TDataSet *mPedNoise;
   const TDataSet *mGain;
   const TDataSet *mMapping;
   const TDataSet *mControl;

   ClassDef(StIstDbMaker, 0)
};

#endif
