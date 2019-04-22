/***************************************************************************
 *
 * $Id: StTpcDbMaker.cxx,v 1.77 2019/04/22 20:47:15 genevb Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  
 *This make initializes and controls the TPC interface to the database
 *
 ***************************************************************************
 *
 * $Log: StTpcDbMaker.cxx,v $
 * Revision 1.77  2019/04/22 20:47:15  genevb
 * Introducing codes for AbortGapCleaning distortion corrections
 *
 * Revision 1.76  2018/12/06 19:30:59  genevb
 * Check on StMagUtilities instance from StTpcDb instead of global
 *
 * Revision 1.75  2018/12/05 18:12:09  genevb
 * Use NoReset attribute to prevent reinstantiation of StMagUtilities
 *
 * Revision 1.74  2018/07/06 22:13:16  smirnovd
 * [Cosmetic] Remove unused variables and commented code
 *
 * Revision 1.73  2018/06/29 21:46:22  smirnovd
 * Revert iTPC-related changes committed on 2018-06-20 through 2018-06-28
 *
 * Revert "NoDead option added"
 * Revert "Fill mag field more carefully"
 * Revert "Assert commented out"
 * Revert "Merging with TPC group code"
 * Revert "Remove too strong assert"
 * Revert "Restore removed by mistake line"
 * Revert "Remove not used anymore file"
 * Revert "iTPCheckIn"
 *
 * Revision 1.70  2018/06/08 18:18:37  genevb
 * Introduce padrow 40 correction for iTPC GridLeak Wall, reduce includes dependencies
 *
 * Revision 1.69  2018/04/30 23:18:11  smirnovd
 * [Cosmetic] Minor changes in various files
 *
 * - Renamed data member s/m/mMass/ in StikalmanTrack
 * - Changes in white space
 * - Return STAR code
 *
 * Revision 1.68  2018/04/11 02:39:49  genevb
 * Distortion smearing by calibration resolutions
 *
 * Revision 1.67  2017/11/13 21:14:27  fisyak
 * Enable Mag.Field depending flavor
 *
 * Revision 1.66  2017/01/30 17:59:13  fisyak
 * Undo commit
 *
 * Revision 1.64  2017/01/06 22:30:45  genevb
 * Introduce FullGridLeak distortion correction
 *
 * Revision 1.63  2015/05/21 21:48:22  fisyak
 * Fix array out of bound, comment out tpcGlobalPosition field dependence
 *
 * Revision 1.62  2014/06/26 21:32:57  fisyak
 * New Tpc Alignment, v632
 *
 * Revision 1.61  2012/09/17 19:39:44  fisyak
 * Add rotation for Half Tpc's
 *
 * Revision 1.60  2011/08/23 22:14:24  genevb
 * Introduce sector alignment distortion corrections
 *
 * Revision 1.59  2011/01/18 14:39:43  fisyak
 * Clean up TpcDb interfaces and Tpc coordinate transformation
 *
 * Revision 1.58  2010/10/28 19:08:43  genevb
 * Introduce GG Voltage Error switch
 *
 * Revision 1.57  2010/09/01 21:11:32  fisyak
 * Use Mag.field flavor besides simu flag
 *
 * Revision 1.56  2010/05/27 20:46:25  genevb
 * Allow discontinued use of FullMagF geometry flavors, now just use ofl with appropriate timestamp
 *
 * Revision 1.55  2010/05/27 19:14:26  fisyak
 * Take out flavoring by 'sim' for tpcGlobalPosition,tpcSectorPosition and starClockOnl tables. remove usage tpcISTimeOffsets and tpcOSTimeOffsets tables
 *
 * Revision 1.54  2010/03/18 14:40:47  fisyak
 * back to 3 parameter constration of StMagUtilities
 *
 * Revision 1.53  2010/03/17 15:53:15  fisyak
 * Move StTpcdEdxCorrection to StdEdxY2Maker to avoid dependence of StTpcDb on StDetectorDbMaker
 *
 * Revision 1.52  2010/03/15 23:29:47  fisyak
 * switch from St_tpcAnodeHVC => St_tpcAnodeHVavgC
 *
 * Revision 1.51  2010/01/26 21:04:42  fisyak
 * Add new dE/dx calibration tables: TpcRowQ, tpcMethaneIn, tpcWaterOut, TpcZDC
 *
 * Revision 1.50  2009/12/07 23:44:58  fisyak
 * Drop coordinate transformation for fortran, remove TpcHitErr
 *
 * Revision 1.49  2009/11/23 16:50:27  fisyak
 * St_tpcAnodeHVavgC => St_tpcAnodeHVC, comment out exported for fortran coordinate transformations
 *
 * Revision 1.48  2009/11/06 13:41:31  fisyak
 * Revert the change done 11/03/09
 *
 * Revision 1.47  2009/11/02 17:31:41  fisyak
 * use directly field from StarMagField, replace St_tpcGainC and St_tpcT0C by St_tpcPadGainT0C, add remove defaults in coordinate transformations
 *
 * Revision 1.46  2009/08/11 20:38:04  genevb
 * slightly more detailed message
 *
 * Revision 1.45  2009/05/01 19:09:23  fisyak
 * StTpcDbMaker::Make is aware about TPC trips and generagte EoF when this happenss
 *
 * Revision 1.44  2008/09/10 15:46:37  fisyak
 * Recalculate Tpc drift velocity once per event, avoid expensive conversion to unix time
 *
 * Revision 1.43  2008/08/01 14:28:34  fisyak
 * Add new getT0, clean up
 *
 * Revision 1.42  2008/07/10 20:25:31  fisyak
 * Warn of
 *
 * Revision 1.41  2007/12/28 00:30:06  fine
 * Add a function to calculate the tpc coord transfoirmation in one step
 *
 * Revision 1.40  2007/08/04 00:38:04  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.39  2007/07/12 20:21:09  fisyak
 * Drift velocity depends on TPC half, use online RHIC clock
 *
 * Revision 1.38  2007/04/28 17:57:19  perev
 * Redundant StChain.h removed
 *
 * Revision 1.37  2007/03/21 17:27:02  fisyak
 * use TGeoHMatrix, change mode for switching drift velocities
 *
 * Revision 1.36  2006/02/27 19:20:53  fisyak
 * Set simu flag for tpcISTimeOffsets and tpcOSTimeOffsets tables
 *
 * Revision 1.35  2005/03/30 17:56:59  fisyak
 * Fix a bug with flavor handling, StTpcDb has to be instantiated after setting flavor
 *
 * Revision 1.34  2004/10/27 21:45:13  fisyak
 * Add debug print for tables Validities, add access to ExB correction
 *
 * Revision 1.33  2004/06/05 23:38:22  fisyak
 * Add more chairs for TPC Db parameters
 *
 * Revision 1.32  2004/05/03 23:29:28  perev
 * WarnOff
 *
 * Revision 1.31  2003/04/10 21:30:59  hardtke
 * Allow multiple InitRun calls
 *
 * Revision 1.30  2003/01/12 20:38:23  jeromel
 * fabs() not abs() for doube
 *
 * Revision 1.29  2002/04/02 00:16:31  hardtke
 * New class that gets hit errors from database
 *
 * Revision 1.28  2002/02/12 22:50:35  hardtke
 * separate geometrical tpc rotation from field twist
 *
 * Revision 1.27  2002/02/05 22:21:08  hardtke
 * Move Init code to InitRun
 *
 * Revision 1.26  2002/01/03 00:01:09  hardtke
 * Add switches for type of drift velocity data (i.e. laser vs. t0 analysis).  Default to use either.
 *
 * Revision 1.25  2001/10/25 22:59:36  hardtke
 * Add function tpc_localsector_to_local
 *
 * Revision 1.24  2001/10/24 21:36:20  hardtke
 * Add sim flavor option
 *
 * Revision 1.23  2001/07/27 23:52:33  hardtke
 * use Global (magnet) coordinates
 *
 * Revision 1.22  2001/06/21 16:27:52  perev
 * two error matrix transformation methods added
 *
 * Revision 1.21  2001/06/19 23:07:13  hardtke
 * Restore to old functionality using Tpc Local Coordinates
 *
 * Revision 1.20  2001/04/19 19:52:48  hardtke
 * add tpc_pad_time_offset function and add ifdef for static arrays
 *
 * Revision 1.19  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
 * Revision 1.18  2000/08/09 14:54:54  hardtke
 * Add Clear option, set trigger table pointer to 0 after each event
 *
 * Revision 1.17  2000/08/08 19:15:23  hardtke
 * use correct trigger time offset in case of laser
 *
 * Revision 1.16  2000/07/06 21:37:34  hardtke
 * speed up tpc_pad_to_x function
 *
 * Revision 1.15  2000/05/31 19:50:16  hardtke
 * speed up tpc_time_to_z and tpc_z_to_time by factor of 5
 *
 * Revision 1.14  2000/04/11 16:06:26  hardtke
 * improve speed of tpc_row_par and tpc_global_to_sector
 *
 * Revision 1.13  2000/02/24 18:21:51  hardtke
 * re-define drift distance as central membrane to gating grid
 *
 * Revision 1.12  2000/02/23 22:21:09  hardtke
 * add tpc_global_to_local_p
 *
 * Revision 1.11  2000/02/23 21:03:17  hardtke
 * fix tpc_row_par -- causing tpt problems
 *
 * Revision 1.10  2000/02/23 15:09:57  hardtke
 * move tpg_detector and tpg_pad_plane from .const to .data
 *
 * Revision 1.9  2000/02/22 19:40:30  hardtke
 * fix tpc_row_par to give expected results
 *
 * Revision 1.8  2000/02/17 19:43:20  hardtke
 * fixes to tpc functions
 *
 * Revision 1.7  2000/02/10 00:29:09  hardtke
 * Add tpg functions to StTpcDbMaker, fix a few bugs
 *
 * Revision 1.6  2000/01/11 15:49:53  hardtke
 * get Electronics table from Calibrations database, Fix error messages
 *
 * Revision 1.5  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/

#include <assert.h>
#include "StTpcDbMaker.h"
#include "StDbUtilities/StCoordinates.hh"
#include "StDbUtilities/StTpcPadCoordinate.hh"
#include "StDbUtilities/StMagUtilities.h"
#include "StarMagField.h"
#include "math_constants.h"
#include "StDetectorDbMaker/StDetectorDbTpcRDOMasks.h"
#include "StDetectorDbMaker/StDetectorDbMagnet.h"
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/St_tpcChargeEventC.h"
#include "StEventTypes.h"
#if ROOT_VERSION_CODE < 331013
#include "TCL.h"
#else
#include "TCernLib.h"
#endif
ClassImp(StTpcDbMaker)
//_____________________________________________________________________________
Int_t StTpcDbMaker::InitRun(int runnumber){
  // Create Needed Tables:    
  //Float_t gFactor = StarMagField::Instance()->GetFactor();
  // Set Table Flavors
  if (! IAttr("Simu")) {
    Float_t gFactor = StarMagField::Instance()->GetFactor();
    if (gFactor<-0.8) {
      gMessMgr->Info() << "StTpcDbMaker::Full Reverse Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
      SetFlavor("ofl+FullMagFNegative","tpcGlobalPosition");
    }
    else if (gFactor<-0.2) {
      gMessMgr->Info() << "StTpcDbMaker::Half Reverse Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
      SetFlavor("ofl+HalfMagFNegative","tpcGlobalPosition");
    }
    else if (gFactor<0.2) {
      gMessMgr->Info() << "StTpcDbMaker::Zero Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
      SetFlavor("ofl+ZeroMagF","tpcGlobalPosition");
    }
    else if (gFactor<0.8) {
      gMessMgr->Info() << "StTpcDbMaker::Half Forward Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
      SetFlavor("ofl+HalfMagFPositive","tpcGlobalPosition");
    }
    else if (gFactor<1.2) {
      gMessMgr->Info() << "StTpcDbMaker::Full Forward Field Twist Parameters.  If this is an embedding run, you should not use it." << endm;
      SetFlavor("ofl+FullMagFPositive","tpcGlobalPosition");
    }
  }
  if         (IAttr("useLDV")) {
    SetFlavor("laserDV","tpcDriftVelocity");
    gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from laser analysis" << endm;
  } else if (IAttr("useNewLDV")) {
    SetFlavor("NewlaserDV","tpcDriftVelocity");
    gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from New laser analysis" << endm;
  } else if (IAttr("useCDV")) {
    SetFlavor("ofl","tpcDriftVelocity");
    gMessMgr->Info() << "StTpcDbMaker::Using drift velocity from T0 analysis" << endm;
  } else {
    SetFlavor("ofl+laserDV","tpcDriftVelocity");
    gMessMgr->Info() << "StTpcDbMaker::Using any drift velocity" << endm;
  }
  StTpcDb::instance()->SetDriftVelocity();
  
  if (IAttr("ExB") && !((StTpcDb::instance()->ExB()) && IAttr("NoReset"))) {
    // Backward compatibility preserved.
    Int_t mask=1;                                    // Al Saulys request
    if        ( IAttr("EB1") ){      // Do nothing (i.e. bit 1 at 0)
    } else if ( IAttr("EB2") ){      // Force bit 1 at 1 regardless
      mask = mask | 2;
    } else {
      if(IAttr("OldRuns")) mask = mask | 2 ;  // Jim Thomas request
    }
    // Other options introduced in October 2001 for distortion corrections
    // studies and year1 re-production. Those are OR additive to the mask.
    //(void) printf("StBFChain:: Options list : %d %d %d %d %d %d %d %d\n",
    //		  kPadrow13,kTwist,kClock,kMembrane,kEndcap,
    //            kIFCShift,kSpaceCharge,kSpaceChargeR2);
    if( IAttr("OBmap")      ) mask |= ( kBMap         << 1);
    if( IAttr("OPr13")      ) mask |= ( kPadrow13     << 1);
    if( IAttr("OPr40")      ) mask |= ( kPadrow40     << 1);
    if( IAttr("OTwist")     ) mask |= ( kTwist        << 1);
    if( IAttr("OClock")     ) mask |= ( kClock        << 1);
    if( IAttr("OCentm")     ) mask |= ( kMembrane     << 1);
    if( IAttr("OECap")      ) mask |= ( kEndcap       << 1);
    if( IAttr("OIFC")       ) mask |= ( kIFCShift     << 1);
    if( IAttr("OSpaceZ")    ) mask |= ( kSpaceCharge  << 1);
    if( IAttr("OSpaceZ2")   ) mask |= ( kSpaceChargeR2<< 1);
    if( IAttr("OShortR")    ) mask |= ( kShortedRing  << 1);
    if( IAttr("OBMap2d")    ) mask |= ( kFast2DBMap   << 1);
    if( IAttr("OGridLeak")  ) mask |= ( kGridLeak     << 1);
    if( IAttr("OGridLeak3D")) mask |= ( k3DGridLeak   << 1);
    if( IAttr("OGridLeakFull")) mask |= ( kFullGridLeak   << 1);
    if( IAttr("OGGVoltErr") ) mask |= ( kGGVoltError  << 1);
    if( IAttr("OSectorAlign"))mask |= ( kSectorAlign  << 1);
    if( IAttr("ODistoSmear")) mask |= ( kDistoSmearing<< 1);
    if( IAttr("OAbortGap"))   mask |= ( kAbortGap     << 1);
    LOG_QA << "Instantiate ExB The option passed will be " << Form("%d 0x%X\n",mask,mask) << endm;
    // option handling needs some clean up, but right now we stay compatible
    Int_t option = (mask & 0x7FFFFFFE) >> 1;
#ifndef __NEW_MagUtilities__
    StMagUtilities *magU = new StMagUtilities(gStTpcDb, GetDataBase("RunLog"), option);
#else
    StMagUtilities *magU = new StMagUtilities(gStTpcDb, option);
#endif
    StTpcDb::instance()->SetExB(magU);
  }
  StTpcDb::instance()->SetTpcRotations();
  return kStOK;
}
//_____________________________________________________________________________
Int_t StTpcDbMaker::Make(){
  // check that TPC is tripped 
  if (St_tpcAnodeHVavgC::instance()->tripped()) {
    gMessMgr->Info() << "StTpcDbMaker::TPC has tripped - declaring EOF to avoid possibly bad data" << endm;
    return kStEOF;
  }
  StTpcDb::instance()->SetDriftVelocity();
  St_trgTimeOffsetC::instance()->SetLaser(kFALSE);
  if (IAttr("laserIT")) {
    St_trgTimeOffsetC::instance()->SetLaser(kTRUE);
  } else {
    StEvent* pEvent = dynamic_cast<StEvent*> (GetInputDS("StEvent"));
    if (pEvent) {
      const StTriggerIdCollection* trig = pEvent->triggerIdCollection();
      if (trig) {
	const StTriggerId *nominal = trig->nominal();
	if (nominal) {
	  Int_t TriggerId = 0;
	  StTpcDb::instance()->SetTriggerId(TriggerId);
	  static Int_t goodIds[2] = {9200,9201}; // Laser trigger IDs
	  for (Int_t i = 0; i < 2; i++) {
	    if (nominal->isTrigger(goodIds[i])) {TriggerId = goodIds[i]; break;}
	  }
	  if (TriggerId) {
	    St_trgTimeOffsetC::instance()->SetLaser(kTRUE);
	    StTpcDb::instance()->SetTriggerId(TriggerId);
	  }
	}
      }
      if (IAttr("OAbortGap")) {
        StTriggerData* trg = pEvent->triggerData();
        if (trg) St_tpcChargeEventC::instance()->findChargeTimes(trg->bunchCounter());
      }
    }
  }
  //  SetTpcRotations();
  return kStOK;
}

