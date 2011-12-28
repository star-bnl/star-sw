/***************************************************************************
 *
 * $Id: StTpcDb.h,v 1.35 2010/05/27 19:14:26 fisyak Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: This is the interface between the database and the offline
 *              TPC software.  This classes takes care of the annoying
 *              calls to the root infrastucture, packages and manipulates
 *              the data, and returns the data to the user via simple
 *              interface classes.    
 *
 ***************************************************************************
 *
 * $Log: StTpcDb.h,v $
 * Revision 1.35  2010/05/27 19:14:26  fisyak
 * Take out flavoring by 'sim' for tpcGlobalPosition,tpcSectorPosition and starClockOnl tables. remove usage tpcISTimeOffsets and tpcOSTimeOffsets tables
 *
 * Revision 1.34  2009/12/07 23:44:58  fisyak
 * Drop coordinate transformation for fortran, remove TpcHitErr
 *
 * Revision 1.33  2009/11/02 17:31:41  fisyak
 * use directly field from StarMagField, replace St_tpcGainC and St_tpcT0C by St_tpcPadGainT0C, add remove defaults in coordinate transformations
 *
 * Revision 1.32  2009/03/16 14:13:31  fisyak
 * Use StDetectorDb chairs for TpcGlobalPosition and TpcSectorPosition
 *
 * Revision 1.31  2008/09/10 15:46:36  fisyak
 * Recalculate Tpc drift velocity once per event, avoid expensive conversion to unix time
 *
 * Revision 1.30  2008/08/01 14:28:25  fisyak
 * Add new getT0, clean up
 *
 * Revision 1.29  2007/08/12 15:06:30  fisyak
 * Use separated East/West drift velocities only >= 2007, for back compartibility
 *
 * Revision 1.28  2007/07/12 20:21:09  fisyak
 * Drift velocity depends on TPC half, use online RHIC clock
 *
 * Revision 1.27  2007/03/21 17:27:01  fisyak
 * use TGeoHMatrix, change mode for switching drift velocities
 *
 * Revision 1.26  2004/10/27 21:44:28  fisyak
 * Add debug print for tables Validities, add access to ExB correction
 *
 * Revision 1.25  2004/02/23 00:35:00  fisyak
 * Add access to tpcPadResponse
 *
 * Revision 1.24  2004/01/14 22:54:30  fisyak
 * Add hooks for Pedestal and tpcGain
 *
 * Revision 1.23  2002/04/02 00:16:31  hardtke
 * New class that gets hit errors from database
 *
 * Revision 1.22  2002/02/06 18:39:13  hardtke
 * Add tpc Field Cage structure
 *
 * Revision 1.21  2001/08/14 18:18:03  hardtke
 * Add sector position structures
 *
 * Revision 1.20  2001/05/21 23:25:34  hardtke
 * Add tpcGlobalPosition to StTpcDb.  This includes the global position offset and the rotation w.r.t. the magnet
 *
 * Revision 1.19  2000/08/10 18:41:34  hardtke
 * only look for L0_trigger table once per event -- improves timing
 *
 * Revision 1.18  2000/08/09 14:54:54  hardtke
 * Add Clear option, set trigger table pointer to 0 after each event
 *
 * Revision 1.17  2000/08/08 19:15:23  hardtke
 * use correct trigger time offset in case of laser
 *
 * Revision 1.16  2000/05/11 17:17:27  hardtke
 * make trigger time offset available -- currently NOT different for beam and laser events
 *
 * Revision 1.15  2000/03/27 21:21:22  fine
 * Adjusted to ROOT 2.24
 *
 * Revision 1.14  2000/02/10 00:29:09  hardtke
 * Add tpg functions to StTpcDbMaker, fix a few bugs
 *
 * Revision 1.13  2000/01/25 16:01:10  fisyak
 * Devorce with StAF
 *
 * Revision 1.12  2000/01/11 15:49:52  hardtke
 * get Electronics table from Calibrations database, Fix error messages
 *
 * Revision 1.11  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef ClassStTpcDb
#define ClassStTpcDb

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StTpcDb                                                              //
//                                                                      //
// This class implements to offline interface to the STAR database      //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#include "StMessMgr.h"
#include "StRTpcPadPlane.h"
#include "StRTpcWirePlane.h"
#include "StRTpcDimensions.h"
#include "StRTpcElectronics.h"
#include "StRTpcSlowControlSim.h"
#include "StDetectorDbMaker/St_tpcGlobalPositionC.h"
#include "StDetectorDbMaker/St_tpcSectorPositionC.h"
#include "StRTpcFieldCage.h"
#include "TTable.h"
#include "StDetectorDbMaker/St_tpcPedestalC.h"
#include "StDetectorDbMaker/St_tpcPadResponseC.h"
#include "StDetectorDbMaker/St_tpcPadGainT0C.h"
#include "StDbUtilities/StMagUtilities.h"
#include "TGeoMatrix.h"
class StMaker;
class St_tpcDriftVelocity;
class St_trgTimeOffset;
class St_dst_L0_Trigger;
//class StTpcCoordinateTransform;

class StTpcDb {
 private:
 enum { kCalibration,kGeometry,kConditions } ;
 StMaker* mk;
 StTpcPadPlaneI*       PadPlane;      //!
 StTpcWirePlaneI*      WirePlane;     //!
 StTpcDimensionsI*     dimensions;    //! 
 StTpcSlowControlSimI* slowControlSim;//! 
 StTpcElectronicsI*    electronics;   //!
 StTpcFieldCageI*      FC;
 TDataSet*           tpctrg[3];     //!
 St_tpcDriftVelocity*  dvel;          //!
 St_trgTimeOffset*     toff;          //!
 St_dst_L0_Trigger*    trigtype;      //!
 // StTpcCoordinateTransform* transform; //!
 StMagUtilities*       mExB;           //!
 Int_t                 m_Debug;        //!
 TGeoHMatrix          *mTpc2GlobalMatrix;//!
#if 0
 TGeoHMatrix          *mTpcSectorAlignment[24][2];
#endif
 Float_t               mDriftVel[2];   //!
 UInt_t                mUc;            //! time for which above mDriftVel have been calculateed
 protected:
   StTpcDb() {}
   void GetDataBase(StMaker* maker);
 public:
   StTpcDb(TDataSet* input);
   StTpcDb(StMaker* makerDb);
   virtual ~StTpcDb();
   void Clear();
   StTpcPadPlaneI* PadPlaneGeometry();
   StTpcWirePlaneI* WirePlaneGeometry();
   StTpcDimensionsI* Dimensions();
   StTpcSlowControlSimI* SlowControlSim();
   StTpcElectronicsI* Electronics();
   St_tpcGlobalPositionC* GlobalPosition() {return St_tpcGlobalPositionC::instance();}
   StTpcFieldCageI* FieldCage();
   St_tpcSectorPositionC *SectorPosition() {return St_tpcSectorPositionC::instance();}
   TTable *getTpcTable(int i);
   St_tpcPedestalC *Pedestal();
   St_tpcPadGainT0C    *tpcGain() {return St_tpcPadGainT0C::instance();}
   St_tpcPadGainT0C    *tpcT0()   {return St_tpcPadGainT0C::instance();}
   St_tpcPadResponseC *PadResponse();
   TTable          *FindTable(const Char_t *name, Int_t dbIndex=kCalibration);
   //small pieces of data:
   void  SetDriftVelocity();
   float DriftVelocity(Int_t sector=24);
   float triggerTimeOffset();
   int dvelcounter;
   StMagUtilities* ExB() {return mExB;}
   void SetExB(StMagUtilities *m) {mExB = m;}
   void SetTpc2GlobalMatrix(TGeoHMatrix *m);
   void SetDebug(Int_t m) {m_Debug = m;}
   Int_t Debug() {return m_Debug;}
   const TGeoHMatrix &Tpc2GlobalMatrix() const {return *mTpc2GlobalMatrix;}
#if 0
   const TGeoHMatrix &TpcSectorAlignment(Int_t io = 0; Int_t sector = 1) const {return *mTpcSectorAlignment[sector-1][io];}
#endif
#ifdef __ROOT__
   ClassDef(StTpcDb,0)
#endif
};

// Global pointers:
R__EXTERN StTpcDb* gStTpcDb;

#endif
