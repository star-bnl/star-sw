/***********************************************************************
 *
 * $Id: StMagUtilities.cxx,v 1.120 2019/08/30 20:16:54 genevb Exp $
 *
 * Author: Jim Thomas   11/1/2000
 *
 ***********************************************************************
 *
 * Description: Utilities for the Magnetic Field
 *
 ***********************************************************************
 *
 * $Log: StMagUtilities.cxx,v $
 * Revision 1.120  2019/08/30 20:16:54  genevb
 * Be sure to restore SpaceCharge settings in PredictSpaceCharge()
 *
 * Revision 1.119  2019/05/18 04:39:24  genevb
 * Properly include ion pile-up of Abort Gap Cleaning, plus a little clean-up
 *
 * Revision 1.118  2019/04/22 20:47:11  genevb
 * Introducing codes for AbortGapCleaning distortion corrections
 *
 * Revision 1.117  2018/12/06 19:36:45  genevb
 * Move Instance() definition to resolve undefined symbol
 *
 * Revision 1.116  2018/10/19 20:41:28  genevb
 * Clean up after Y. Fisyak modifications (which were for iTPC, not dE/dx), and add new PredictSpaceCharge() using real hit radii
 *
 * Revision 1.115  2018/10/17 20:45:24  fisyak
 * Restore update for Run XVIII dE/dx calibration removed by Gene on 08/07/2018
 *
 * Revision 1.114  2018/06/21 04:23:45  genevb
 * Simpler way to address calculation of data arrays for wrong sign of the voltages
 *
 * Revision 1.113  2018/06/08 18:18:36  genevb
 * Introduce padrow 40 correction for iTPC GridLeak Wall, reduce includes dependencies
 *
 * Revision 1.112  2018/06/01 18:21:17  jhthomas
 * Update to UndoPadRow13Distortion() in order to move #define's inside the function as const Int_t's
 *
 * Revision 1.111  2018/04/11 02:35:57  genevb
 * Distortion smearing by calibration resolutions
 *
 * Revision 1.110  2017/10/26 02:47:41  genevb
 * Allow FullGridLeak to work on specific sheets via sheet widths
 *
 * Revision 1.109  2017/04/12 19:47:02  genevb
 * Generic SpaceCharge and GridLeak functions independent of specific modes
 *
 * Revision 1.108  2017/01/06 22:31:24  genevb
 * Introduce FullGridLeak distortion correction, speed tweek to Poisson3DRelaxation
 *
 * Revision 1.107  2016/05/13 06:02:51  genevb
 * Compile and Coverity warnings, one typo in Radius calc, very minor optimizations
 *
 * Revision 1.106  2015/06/30 21:43:40  genevb
 * Allow for a (dummy) initialization call of UndoDistortion()
 *
 * Revision 1.105  2014/07/24 06:58:32  fisyak
 * Add exact cast for CXX11
 *
 * Revision 1.104  2014/07/08 10:07:38  fisyak
 * Add print out for new schema
 *
 * Revision 1.103  2014/07/08 09:50:43  fisyak
 * Fix old correction with 2D and 3D mag.field
 *
 * Revision 1.102  2014/07/01 20:29:02  fisyak
 * Clean up
 *
 * Revision 1.101  2014/06/28 16:23:18  fisyak
 * Use switch to chose between New and Old schema
 *
 * Revision 1.100  2014/06/27 14:18:13  fisyak
 * Add switch between old and new schema
 *
 * Revision 1.99  2014/06/26 21:29:26  fisyak
 * New Tpc Alignment, v632
 *
 * Revision 1.98  2014/01/17 16:33:04  genevb
 * Remove accidental change to B3DField for coordinates
 *
 * Revision 1.97  2014/01/17 03:52:41  genevb
 * More careful check on updating SpaceCharge
 *
 * Revision 1.96  2014/01/16 17:55:13  genevb
 * Two speed improvements: less calls to DB for SpaceCharge, avoid unnecessary cartesian/cylindrical coordinate conversions
 *
 * Revision 1.95  2013/12/11 18:27:56  genevb
 * Account for GG voltage errorsi + shifts in UndoGGVoltErrorDistortion(), other minor optimizations
 *
 * Revision 1.94  2013/09/25 20:38:56  genevb
 * Meaningful return codes for Predict...()
 *
 * Revision 1.93  2013/03/18 17:18:38  genevb
 * Fix for ticket 2529, and some array copying optimzation via memcopy
 *
 * Revision 1.92  2012/12/26 17:45:53  genevb
 * reinitialization fixed for PredictSpaceCharge functions
 *
 * Revision 1.91  2012/12/10 22:46:33  genevb
 * Handle multiple runs by reinitialization at reinstantiation, introduce corrections modes, enable iterative UndoDistortions
 *
 * Revision 1.90  2012/10/31 20:05:10  genevb
 * Row radii stored in arrays of doubles
 *
 * Revision 1.89  2012/10/25 22:44:37  genevb
 * Switch from hardcoded to DB for several values, and fix a bug with east-west-asymmetric 3DGridLeak since ver. 1.82
 *
 * Revision 1.88  2012/04/25 19:22:56  genevb
 * More use of GLWeights, more realistic geometry model in PredictSpaceCharge
 *
 * Revision 1.87  2012/02/02 18:19:08  genevb
 * Catch small/zero primary E field
 *
 * Revision 1.86  2011/09/16 21:52:30  genevb
 * Minor fixes for sector misalignment: output statement, and outermost radius
 *
 * Revision 1.85  2011/08/23 22:15:10  genevb
 * Introduce sector alignment distortion corrections and big speed improvements to Poisson relaxations
 *
 * Revision 1.84  2010/10/28 19:10:59  genevb
 * Provide for  usage of tpcHVPlanes and GG Voltage Error
 *
 * Revision 1.83  2010/05/30 21:12:44  genevb
 * For GridLeak studies: more knobs to adjust GL and SC in Predict() functions
 *
 * Revision 1.82  2010/02/25 21:49:05  genevb
 * Using sector number to better handle post-membrane hits, prep for sector-by-sector GL, and GGVoltage errors
 *
 * Revision 1.81  2009/12/11 04:53:57  genevb
 * Give the enum constants unique names
 *
 * Revision 1.80  2009/12/11 03:55:21  genevb
 * Singleton implementation + no defines in header
 *
 * Revision 1.79  2009/11/06 13:38:05  fisyak
 * Revert the change done 11/03/09
 *
 * Revision 1.77  2009/10/22 04:46:44  jhthomas
 * Update scheme for pad row numbering in Predict Space Charge
 *
 * Revision 1.76  2009/10/19 21:29:42  jhthomas
 * Improved execution speed of many algorithms: especially GridLeak.
 *
 * Revision 1.75  2009/10/01 22:41:02  jhthomas
 * Update grid spacing in UndoShort and prepare for other gridding upgrades to achieve higher resolution results.
 *
 * Revision 1.74  2009/08/25 00:44:17  jhthomas
 * Significant Updates to Undo3DGridLeakDistortion.  Faster and more reliable near pad row 13
 *
 * Revision 1.73  2008/05/27 14:26:40  fisyak
 * Use TChairs, absorb shift tau shift, introduce sector to sector time offset
 *
 * Revision 1.72  2008/03/27 00:11:23  jhthomas
 * Modify previous magfield changes and set 'zero' field to ~1 gauss in a more robust way.
 * Add SpaceChargeEWRatio and appropriate functions that allow us to calibrate d-au collisions.
 *
 * Revision 1.71  2007/07/12 19:20:41  fisyak
 * Account that StDetectorDbSpaceChargeR2 is not inherit from StDetectorDbSpaceCharge anymore
 *
 * Revision 1.70  2007/03/21 16:36:09  fisyak
 * zero field mean 1G
 *
 * Revision 1.69  2006/12/16 23:45:38  jhthomas
 * Add ShortedManualRing() for Gene, and protect against B=0 field ... instead set to 0.25 gauss as minimum
 *
 * Revision 1.68  2006/08/07 20:38:13  fisyak
 * TMatrix is typedef to TMatrixT<Float_t> now, with ROOT 5,12
 *
 * Revision 1.67  2006/07/28 04:59:04  jhthomas
 * Add code by GeneVB to update the ShortedRing tables every time the DB changes.
 *
 * Revision 1.66  2006/07/19 22:27:36  jhthomas
 * Update PredictSpaceCharge
 *
 * Revision 1.65  2006/06/27 18:18:24  jhthomas
 * ADD new PredictSpaceCharge() function so that it includes fit errors in the prediction
 * It is now capable of including the SSD and SVT hits in the predictor/corrector loop
 *
 * Revision 1.64  2005/06/01 20:52:53  perev
 * Workaround for new version TMatrix
 *
 * Revision 1.63  2005/05/24 18:54:03  jhthomas
 * Add 3DGridLeak distortion correction and utilities to support it.
 *
 * Revision 1.62  2005/02/18 09:23:36  jhthomas
 * *Compile* before you submits to CVS :-)
 *
 * Revision 1.61  2005/02/18 08:39:02  jhthomas
 * Change InnerOuterPadRatio from 0.7 to 0.6 at the request of GVB
 *
 * Revision 1.60  2005/02/17 01:59:41  jhthomas
 * Make GetSpaceCharge a public member function.
 * Change InnerOuterPadRatio to 0.7 for GridLeak.  Note that it is 1.3 if you do not use
 * the GridLeak.  This is a kludge and InnerOuterPadRatio should come out of the DB, someday.
 * Fix typo in UndoShortedRing.
 *
 * Revision 1.59  2005/02/09 23:50:35  jeromel
 * Changes by JHT for SpaceCharge / Leak corrections
 *
 * Revision 1.56  2004/10/20 17:52:36  jhthomas
 * Add GetSpaceChargeMode() function
 *
 * Revision 1.55  2004/08/29 21:48:33  jhthomas
 * Put Manual space charge back to 0.0 in order to enable DB.  Previous CVS was a mistake.
 *
 * Revision 1.54  2004/08/29 19:59:53  jhthomas
 * *** empty log message ***
 *
 * Revision 1.53  2004/07/01 17:48:12  jhthomas
 * Add Event by Event SpaceCharge capabilities from GVB.  Start adding incomplete/unfinished work on Endcaps from JT.
 *
 * Revision 1.52  2004/04/03 00:44:10  jhthomas
 * Blew it again.  I sure wish this wasn\'t an archive!
 *
 * Revision 1.51  2004/04/03 00:34:42  jhthomas
 * Accidently deleted a line on the previous committ
 *
 * Revision 1.50  2004/04/03 00:22:21  jhthomas
 * Update Spacecharge R2 to use new DB call built by Gene VB
 *
 * Revision 1.49  2004/04/01 22:19:18  jhthomas
 * Update Omega Tau parameters to Run IV values.
 * Increase speed of space charge calculation with new Relaxation Algorithm.
 * Start to build 3D space charge capabilities.  This is a work in progress.
 *
 * Revision 1.48  2004/03/16 20:44:00  jhthomas
 * Various minor bug fixes.  Add new (faster) 2D Bfield distortion routines.
 * Improve spacecharge calculation so it is faster.
 *
 * Revision 1.47  2004/03/01 17:22:39  jhthomas
 * Change Shorted Ring Algorithm over to Wieman\'s Bessel Function solution.  It is faster.
 * Also Fix Jerome\'s famous non-ascii typo.
 *
 * Revision 1.46  2004/02/14 23:57:40  jeromel
 * File still had binary characters in CVS. Corrected and adjusted doc
 *
 * Revision 1.45  2004/02/11 22:26:55  perev
 * More prints for NO TPC DB
 *
 * Revision 1.44  2004/01/22 16:20:43  jhthomas
 * Add Hardwired code for Shorted Ring with External Resistor.
 * Change Omega Tau factors.  May need update, later.
 *
 * Revision 1.43  2004/01/20 02:52:18  jhthomas
 * Add code for extra resistor outside TPC to help remedy short.  !!This code currently commented out!!
 *
 * Revision 1.42  2004/01/16 23:48:12  jhthomas
 * Fix integer math as suggested by Gene Van Buren
 *
 * Revision 1.41  2004/01/06 20:04:41  jhthomas
 * Add new routine to handle a shorted ring on the East end of the TPC.
 * Also new routine to help redo the space charge calculations.
 *
 * Revision 1.40  2003/10/28 02:09:45  perev
 *  r<IFCRadius skipped
 *
 * Revision 1.39  2003/10/25 00:57:02  perev
 * Redundand debug print removed
 *
 * Revision 1.38  2003/10/25 00:36:49  perev
 * Defence against divergency added (????)
 *
 * Revision 1.37  2003/09/30 04:05:12  jhthomas
 * Explicity initialize "static ilow = 0" parameters that are used in Search(blah,blah,ilow)
 *
 * Revision 1.36  2003/09/02 17:57:51  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.35  2003/06/27 18:41:14  jhthomas
 * Add new function called FixSpaceChargeDistortion( ,,,,, )
 * It can be used to convert the old (Uniform) space charge corrections to
 * the new (1/R**2) space charge corrections.  This correction can be
 * applied to individual track momenta on the microDSTs and it does not
 * require a re-production of the data to get the 1/R**2 spacecharge corrections.
 *
 * Revision 1.34  2002/09/18 22:50:33  jhthomas
 * Set default space charge density to zero.  Time dependent values come from DB.
 *
 * Revision 1.33  2002/09/18 22:21:35  jhthomas
 * Add new option for 1/R**2 space charge density distribution.  Flag = 0x800
 *
 * Revision 1.32  2002/02/23 02:47:50  jhthomas
 * Technical Bug Fix - minus one twice
 *
 * Revision 1.31  2002/02/22 17:44:18  jhthomas
 * Get CathodeV and GG from DB. Change Defaults.  Change Instantiation argument
 * order. Update D\'Oxygen documentation.  Remove 2000/2001 E field switch.
 *
 * Revision 1.30  2002/02/12 22:50:57  hardtke
 * separate geometrical tpc rotation from field twist
 *
 * Revision 1.29  2002/02/06 18:39:45  hardtke
 * Use Database for tpc Field cage parameters
 *
 * Revision 1.28  2002/02/03 21:17:11  dunlop
 * Fix the spacecharge instance, so that it gets called to
 * reset ONLY if the instance is non-zero, e.g. wanted.
 * (Previous log: call SpaceCharge::instance() every hit
 * to reset the DetectorDbMaker array.  Only works on 2nd+ event if you do this.)
 *
 * Revision 1.27  2002/02/03 20:59:47  dunlop
 * *** empty log message ***
 *
 * Revision 1.26  2002/02/02 02:05:30  jhthomas
 * Included gFactor explicitly in SpaceCharge call
 *
 * Revision 1.25  2002/02/02 01:01:09  jeromel
 * Jim\'s modif for FC & SpaceCharge corrections.
 *
 * Revision 1.23  2001/10/25 23:00:24  hardtke
 * Use database to get a few parameters in StMagUtilities (including twist)
 *
 * Revision 1.22  2001/10/06 06:14:06  jeromel
 * Sorry for multiple commits but ... added one more comment line.
 *
 * Revision 1.21  2001/10/05 20:19:38  dunlop
 * Made default BMap + Padrow13 + Twist + Clock.
 * Made selection logic symmetric
 *
 * Revision 1.20  2001/10/05 03:44:25  jeromel
 * Modifications by Jamie so we can turn on/off every corrections.
 *
 * Revision 1.18  2001/09/06 18:27:39  jeromel
 * Modifications for larger number of ExB options, forcing different configuration 9EB1 EB2 ...). Added loading of StTableUtilities when \'display\' option is required.
 *
 * Revision 1.17  2001/08/08 20:11:42  jeromel
 * Added debugging lines for ExB correction option. WAS NEVER ON ==> Corrected & -> | (i.e. mea culpa)
 *
 * Revision 1.16  2001/08/01 18:34:39  jhthomas
 * Add temporary mode flag for year 2 running (different cathode potentials)
 *
 * Revision 1.15  2001/07/24 00:20:20  jhthomas
 * Protect Divide by Zero in UndoBDistortion
 *
 * Revision 1.14  2001/06/15 00:52:15  jhthomas
 * Protect discontinuity in distortions at CM
 *
 * Revision 1.13  2001/06/14 22:12:11  jhthomas
 * Speedup UndoBDistorion by adding table lookups
 *
 * Revision 1.12  2001/06/13 16:36:43  jhthomas
 * Improve the speed and timing of the PadRow13 correction.
 * Add 3D magnetic field functions so now both 2D and 3D are availble.
 *
 * Revision 1.3  2000/12/15 16:10:45  jhthomas
 * Add PadRow13, Clock, and Twist corrections to UndoDistortion
 *
 * Revision 1.2  2000/11/03 02:41:58  jhthomas
 * Added CVS comment structure to .h and .cxx files
 *
 ***********************************************************************/

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StMagUtilities Class                                                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

/*!

\class StMagUtilities 

\author Jim Thomas 10 October 2000

A package of Bfield routines and distortion corrections for the STAR 
TPC.  Methods included to read the correct Bfield map and scale it 
according to a scale factor provided during instantiation.
All corrections automatically adjust themselves for different
B field settings and E field settings.  Even reversed fields. 

<p>

An enumerated argument provided at the time of instantiation selects
a constant magnetic field (value=1) or the measured magnetic field (value=2)
at a field setting that you select manually.  Alternatively, you can use the
database to determine the magnetic field setting but you must then provide a
a time stamp and use a different instantiation (this is usually done in the chain).

The enumerations for the manual settings are:
enum   EBField  { kUndefined = 0, kConstant = 1, kMapped = 2, kChain = 3 } ;
"kConstant = 1" means you wish to work with a constant, uniform, field.
"kMapped = 2"   means you want to read values from the measured magnet maps. 
The other enumerations are undefined and reserved for future expansion.

<p>

This code works in kGauss, cm - but note that the Bfield maps on disk 
are in gauss, cm.

<p>

A mode switch can be used to select the distortions that will be 
applied to the data.  A choice of mode = 0 will give the default
set of distortions.  Other modes can be selected by turning on the 
appropriate bit field, shown below.  

<br>

Bit counting starts at 1 for the mode switch (...,3,2,1) <br>

<br>

enum   DistortSelect                                                  <br>
{                                                                     <br>
  kBMap              = 0x08,     // Bit 4                             <br>
  kPadrow13          = 0x10,     // Bit 5                             <br>
  kTwist             = 0x20,     // Bit 6                             <br>
  kClock             = 0x40,     // Bit 7                             <br>
  kMembrane          = 0x80,     // Bit 8                             <br>
  kEndcap            = 0x100,    // Bit 9                             <br>
  kIFCShift          = 0x200,    // Bit 10                            <br>
  kSpaceCharge       = 0x400,    // Bit 11                            <br>
  kSpaceChargeR2     = 0x800,    // Bit 12                            <br>
  kShortedRing       = 0x1000,   // Bit 13                            <br>
  kFast2DBMap        = 0x2000,   // Bit 14                            <br>
  kGridLeak          = 0x4000,   // Bit 15                            <br>
  k3DGridLeak        = 0x8000,   // Bit 16                            <br>
  kGGVoltError       = 0x10000,  // Bit 17                            <br>
  kSectorAlign       = 0x20000,  // Bit 18                            <br>
  kDisableTwistClock = 0x40000,  // Bit 19                            <br>
  kFullGridLeak      = 0x80000,  // Bit 20                            <br>
  kDistoSmearing     = 0x100000, // Bit 21                            <br>
  kPadrow40          = 0x200000, // Bit 22                            <br>
  kAbortGap          = 0x400000  // Bit 23                            <br>
} ;                                                                   <br>

Note that the option flag used in the chain is 2x larger 
than shown here in order to allow the first bit to be used 
as an on/off flag and then it is shifted away before entering 
StMagUtilities.  This can be summarized by saying:

<br> 

Bit counting starts at 0 for the chain option flag (...,3,2,1,0) <br>

<p>

To do:  <br>
- Add a routine to distort the track if we are given a Geant Vector full of points == a track
- Add simulated B field map in the regions where the field is not mapped.
- Tilted CM and endcap parameters from DB
- Spacecharge blob at negative X parameters from DB

*/
#include <assert.h>
#include "StMagUtilities.h"
#include "TFile.h"
#include "TNtuple.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TF1.h"
#include "TH2.h"
#include "TRandom.h"
#include "StTpcDb/StTpcDb.h"
#include "tables/St_MagFactor_Table.h"
#include "StDetectorDbMaker/St_tpcHVPlanesC.h"
#include "StDetectorDbMaker/St_tpcAnodeHVavgC.h"
#include "StDetectorDbMaker/St_tpcHighVoltagesC.h"
#include "StDetectorDbMaker/St_tpcFieldCageShortC.h"
#include "StDetectorDbMaker/St_tpcOmegaTauC.h"
#include "StDetectorDbMaker/St_tpcGridLeakC.h"
#include "StDetectorDbMaker/St_spaceChargeCorC.h"
#include "StDetectorDbMaker/StTpcSurveyC.h"
#include "StDetectorDbMaker/St_trigDetSumsC.h"
#include "StDetectorDbMaker/St_tpcPadConfigC.h"
#include "StDetectorDbMaker/St_tpcCalibResolutionsC.h"
#include "StDetectorDbMaker/St_tpcChargeEventC.h"
#include "StDetectorDbMaker/St_tpcSCGLC.h"
#include "StDbUtilities/StTpcCoordinateTransform.hh"
  //#include "StDetectorDbMaker/StDetectorDbMagnet.h"
static Float_t  gFactor  = 1.0 ;        // Multiplicative factor (allows scaling and sign reversal)
StMagUtilities *StMagUtilities::fgInstance = 0 ;
static const Float_t  PiOver12 = TMath::Pi()/12. ;  // Commonly used constant
static const Float_t  PiOver6 = TMath::Pi()/6. ;  // Commonly used constant
TNtuple *StMagUtilities::fgDoDistortion = 0;
TNtuple *StMagUtilities::fgUnDoDistortion = 0;
static const size_t threeFloats = 3 * sizeof(Float_t);


// Parameters derived from GARFIELD simulations of the GridLeaks performed by Irakli Chakaberia:
//   https://drupal.star.bnl.gov/STAR/node/36657
// Standard operation has been 1100 V and 1390 V for inner and outer respectively in recent Runs.
// -23.38+exp(0.004751*1390) = 714.6 +/- 18.8 outer-outer
// -16.68+exp(0.004731*1390) = 701.0 +/- 18.8 inner of outer sector
// -21.76+exp(0.005123*1100) = 258.4 +/-  8.4 outer of inner sector
// -42.15+exp(0.005345*1100) = 315.5 +/-  8.4 inner-inner
// The GL_q functions deliver a total charge (not charge density) out of each charge sheet over a
// Cartesian area (i.e. a fixed extent in distance along the pad rows that does not grow with radius)
// The GL_rho functions convert the total charge to a charge density
const Float_t GL_charge_y_lo[4] {52.04,121.80-0.85,121.80     ,191.49} ;
const Float_t GL_charge_y_hi[4] {52.85,121.80     ,121.80+0.99,192.53} ;
Float_t GL_q_inner_of_innerSec(Float_t voltage=1100.0) { return -42.15 + TMath::Exp(0.005345 * voltage); }
Float_t GL_q_outer_of_innerSec(Float_t voltage=1100.0) { return -21.76 + TMath::Exp(0.005123 * voltage); }
Float_t GL_q_inner_of_outerSec(Float_t voltage=1390.0) { return -16.68 + TMath::Exp(0.004731 * voltage); }
Float_t GL_q_outer_of_outerSec(Float_t voltage=1390.0) { return -23.38 + TMath::Exp(0.004751 * voltage); }
Float_t GL_rho_inner_of_innerSec(Float_t voltage=1100.0)
  { return GL_q_inner_of_innerSec(voltage) / (GL_charge_y_hi[0]- GL_charge_y_lo[0]) ; }
Float_t GL_rho_outer_of_innerSec(Float_t voltage=1100.0)
  { return GL_q_outer_of_innerSec(voltage) / (GL_charge_y_hi[1]- GL_charge_y_lo[1]) ; }
Float_t GL_rho_inner_of_outerSec(Float_t voltage=1390.0)
  { return GL_q_inner_of_outerSec(voltage) / (GL_charge_y_hi[2]- GL_charge_y_lo[2]) ; }
Float_t GL_rho_outer_of_outerSec(Float_t voltage=1390.0)
  { return GL_q_outer_of_outerSec(voltage) / (GL_charge_y_hi[3]- GL_charge_y_lo[3]) ; }

double SpaceChargeRadialDependence(double Radius) {
  return ( 3191./(Radius*Radius) + 122.5/Radius - 0.395 ) / 15823. ;
}

//________________________________________


ClassImp(StMagUtilities);
struct Distortion_t {
  Float_t sector, xL, yL, zL, xLC, yLC, zLC;
};
static Distortion_t D;
static const Char_t *Dnames = {"sector:xL:yL:zL:xLC:yLC:zLC"};
//________________________________________________________________________________
StMagUtilities* StMagUtilities::Instance()  { return fgInstance; }
//________________________________________________________________________________
void    StMagUtilities::SetDoDistortionT  (TFile *f) {
  if (! f) return;
  f->cd();
  fgDoDistortion = new TNtuple("DoDist","Result of DoDistrotion in TPC CS",Dnames);
}
//________________________________________________________________________________
void    StMagUtilities::SetUnDoDistortionT(TFile *f) {
  if (! f) return;
  f->cd();
  fgUnDoDistortion = new TNtuple("UnDoDist","Result of UnDoDistrotion in TPC CS",Dnames);
}
//________________________________________________________________________________
/// StMagUtilities constructor using the DataBase
StMagUtilities::StMagUtilities (StTpcDb* /* dbin */, Int_t mode )
{ 
  if (fgInstance) {
    cout << "ReInstate StMagUtilities. Be sure that this is want you want !" << endl;
    SafeDelete(fgInstance);
  }
  fgInstance = this;
  GetDistoSmearing(mode);    // Get distortion smearing from the DB
  GetMagFactor()        ;    // Get the magnetic field scale factor from the DB
  GetTPCParams()        ;    // Get the TPC parameters from the DB
  GetTPCVoltages( mode );    // Get the TPC Voltages from the DB! (after GetTPCParams!)
  GetHVPlanes()         ;    // Get the parameters that describe the HV plane errors (after GetTPCVoltages!)
  GetOmegaTau ()        ;    // Get Omega Tau parameters
  GetSpaceCharge()      ;    // Get the spacecharge variable from the DB
  GetSpaceChargeR2()    ;    // Get the spacecharge variable R2 from the DB and EWRatio
  GetShortedRing()      ;    // Get the parameters that describe the shorted ring on the field cage
  GetGridLeak( mode )   ;    // Get the parameters that describe the gating grid leaks
  GetAbortGapCharge()   ;    // Get the parameters that describe the Abort Gap charge events
  CommonStart( mode )   ;    // Read the Magnetic and Electric Field Data Files, set constants
  UseManualSCForPredict(kFALSE) ; // Initialize use of Predict() functions;
}


/// StMagUtilities constructor not using the DataBase
StMagUtilities::StMagUtilities ( const StarMagField::EBField map, const Float_t factor, Int_t mode )       
{ 
  if (fgInstance) {
    cout << "ReInstate StMagUtilities. Be sure that this is want you want !" << endl;
    SafeDelete(fgInstance);
    }
  fgInstance = this;
  GetDistoSmearing(0)   ;        // Do not get distortion smearing out of the DB
  GetMagFactor()        ;        // Get the magnetic field scale factor from the StarMagField
  fTpcVolts      =  0   ;        // Do not get TpcVoltages out of the DB   - use defaults in CommonStart
  fOmegaTau      =  0   ;        // Do not get OmegaTau out of the DB      - use defaults in CommonStart
  fAbortGapCharge=  0   ;        // Do not get AbortGap out of the DB      - use defaults in CommonStart
  ManualSpaceCharge(0)  ;        // Do not get SpaceCharge out of the DB   - use defaults inserted here.
  ManualSpaceChargeR2(0,1) ;     // Do not get SpaceChargeR2 out of the DB - use defaults inserted here, SpcChg and EWRatio
  ManualShortedRing(0,0,0,0,0) ; // No shorted rings
  fGridLeak      =  0   ;        // Do not get Grid Leak data from the DB  - use defaults in CommonStart
  fHVPlanes      =  0   ;        // Do not get GGVoltErr data from the DB  - use defaults in CommonStart
  CommonStart( mode )   ;        // Read the Magnetic and Electric Field Data Files, set constants
  UseManualSCForPredict(kFALSE) ; // Initialize use of Predict() functions;
}


//________________________________________

void StMagUtilities::GetDistoSmearing (Int_t mode)
{
  fCalibResolutions = ((mode & kDistoSmearing) > 0 ?St_tpcCalibResolutionsC::instance() : 0);
  mRandom = (fCalibResolutions ? new TRandom(time(NULL)) : 0);
}


//________________________________________

void StMagUtilities::GetMagFactor () 
{ 
  gFactor = StarMagField::Instance()->GetFactor();
}
void StMagUtilities::GetTPCParams ()  
{ 
  St_tpcWirePlanesC*    wires = StTpcDb::instance()->WirePlaneGeometry();
  St_tpcPadConfigC*      pads = St_tpcPadConfigC::instance();
  St_tpcFieldCageC*     cages = StTpcDb::instance()->FieldCage();
  St_tpcDimensionsC*     dims = StTpcDb::instance()->Dimensions();
  if (! StTpcDb::IsOldScheme()) { // new schema
    XTWIST = 0; 
    YTWIST = 0;
    EASTCLOCKERROR = 0;
    WESTCLOCKERROR = 0;
    mDistortionMode = kDisableTwistClock;
  } else { // old schema
    St_tpcGlobalPositionC* glob = StTpcDb::instance()->GlobalPosition();
    XTWIST         =   1e3*glob->TpcEFieldRotationY() ; 
    YTWIST         =  -1e3*glob->TpcEFieldRotationX() ;            
    EASTCLOCKERROR =   1e3*cages->EastClockError();
    WESTCLOCKERROR =   1e3*cages->WestClockError();
    mDistortionMode= 0;
  }
  StarDriftV     =  1e-6*StTpcDb::instance()->DriftVelocity() ;        
  TPC_Z0         =  dims->gatingGridZ() ;
  IFCShift       =  cages->InnerFieldCageShift();
  for (Int_t sec = 1; sec <= 24; sec++) {
    INNER[sec-1]          =  pads->innerPadRows(sec);
    TPCROWS[sec-1]        =  pads->padRows(sec);
    for ( Int_t row = 1 ; row <= TPCROWS[sec-1] ; row++ )
      TPCROWR[sec-1][row-1] = pads->radialDistanceAtRow(sec,row);
  }
  IFCRadius      =    47.90 ;  // Radius of the Inner Field Cage (GVB: not sure where in DB?)
  OFCRadius      =  dims->senseGasOuterRadius();
  INNERGGFirst   =  wires->firstInnerSectorGatingGridWire();
  OUTERGGFirst   =  wires->firstOuterSectorGatingGridWire();
  INNERGGLast    =  INNERGGFirst + 
                    wires->gatingGridWirePitch() * (wires->numInnerSectorGatingGridWires() - 1);
  OUTERGGLast    =  OUTERGGFirst + 
                    wires->gatingGridWirePitch() * (wires->numOuterSectorGatingGridWires() - 1);
  GAPRADIUS      =  0.5 * (INNERGGLast + OUTERGGFirst);
  // Note (2012-10-25): currently GAPRADIUS from the DB (121.7975) differs very slightly
  //                    (by 25 microns) from non-DB value (121.8000)
  WIREGAP        =  OUTERGGFirst - INNERGGLast;
}

void StMagUtilities::GetE()
{
  RPitch         =  1.150 ;            // Field Cage Ring to Ring pitch (cm)
  Float_t R_0    =  2.130 ;            // First resistor (R0) between CM and ring number one (Mohm)
  Float_t RStep  =  2.000 ;            // Resistor chain value (except the first one) (Mohm)
  Float_t R_182  =  0.310 ;            // Last resistor in the IFC chain
  Rtot           =  R_0 + 181*RStep + R_182 ;    // Total resistance of the (normal) resistor chain
  Rfrac          =  TPC_Z0*RStep/(Rtot*RPitch) ; // Fraction of full resistor chain inside TPC drift volume (~1.0)

  // Effectivenesses determined from voltage tuning tool at:
  // http://www.star.bnl.gov/public/tpc/hard/tpcrings/page6.html
  GGeffectiveness      =  0.981;       // Effectiveness of GG voltage to be the average at its plane
  deltaGGeffectiveness =  0.964;       // Effectiveness of GG voltage changes to be expressed in average
  GGideal              =  CathodeV * (1.0 - Rfrac) / GGeffectiveness ;
  StarMagE             =  TMath::Abs((CathodeV-GG)/TPC_Z0) ;         // STAR Electric Field (V/cm) Magnitude
  if (TMath::Abs(StarMagE) < 1e-6) {
    cout << "StMagUtilities ERROR **** Calculations fail with extremely small or zero primary E field:" << endl;
    cout << "StMagUtilities     StarMagE = (CathodeV - GG) / TPC_Z0 = (" << CathodeV
      << " - " << GG << ") / " << TPC_Z0 << " = " << StarMagE << " V/cm" << endl;
    exit(1);
  } 
}

void StMagUtilities::GetTPCVoltages (Int_t mode)  
{
  // GetTPCParams() must be called first!
  fTpcVolts      =  St_tpcHighVoltagesC::instance() ;  // Initialize the DB for TpcVoltages
  CathodeV       =  fTpcVolts->getCathodeVoltage() * 1000 ; 
  GG             =  fTpcVolts->getGGVoltage() ; 
  
  if (mode & kPadrow40) {
    for (Int_t i = 0 ; i < 24; i++ ) {
      Inner_GLW_Voltage[i] = fTpcVolts->getGridLeakWallTip(i+1) ; // faces inner & outer sector near wall tip
      Outer_GLW_Voltage[i] = fTpcVolts->getGridLeakWallSide(i+1) ; // face outer sector only on wall side
    }
  } else if (mode & kPadrow13) {
    for (Int_t i = 0 ; i < 24; i++ ) {
      Inner_GLW_Voltage[i] = 222 ;
      Outer_GLW_Voltage[i] = 222 ;
    }
  } // else don't touch GLW voltages
  GetE() ;
  St_tpcAnodeHVavgC* anodeVolts = St_tpcAnodeHVavgC::instance() ;
  if (mode & k3DGridLeak) {
    // For now, a bit complicated, but assign 1 to those with most common
    //   voltages, and -1 ("unknown") otherwise
    double maxInner = 1170;
    double maxOuter = 1390;
    double stepsInner = 35;
    double stepsOuter = 45;
    TH1I innerVs("innerVs","innerVs",5,maxInner-3.5*stepsInner,maxInner+1.5*stepsInner);
    TH1I outerVs("outerVs","outerVs",5,maxOuter-3.5*stepsOuter,maxOuter+1.5*stepsOuter);
    for (Int_t i = 1 ; i < 25; i++ ) {
      int inner = St_tpcPadConfigC::instance()->innerPadRows(i);
      innerVs.Fill(anodeVolts->voltagePadrow(i,inner));
      outerVs.Fill(anodeVolts->voltagePadrow(i,inner+1));
    }
    double cmnInner = innerVs.GetBinCenter(innerVs.GetMaximumBin());
    double cmnOuter = outerVs.GetBinCenter(outerVs.GetMaximumBin());
    cout << "StMagUtilities assigning common anode voltages as " << cmnInner << " , " << cmnOuter << endl;
    for (Int_t i = 1 ; i < 25; i++ ) {
      GLWeights[i] = ( ( TMath::Abs(anodeVolts->voltagePadrow(i,INNER[i-1]  ) - cmnInner) < stepsInner/2. ) &&
                       ( TMath::Abs(anodeVolts->voltagePadrow(i,INNER[i-1]+1) - cmnOuter) < stepsOuter/2. ) ? 1 : -1 );
    }
  } else if (mode & kFullGridLeak) {

    // Scale charge densities so that total charge, in cylindrical units, of middle
    // sheet is the same as it used to be: rho * area ~ rho * Delta(r^2)
    float norm = ( 122.595*122.595 - 121.0*121.0 ) /
                 ( GL_rho_outer_of_innerSec() *
                     (GL_charge_y_hi[1]*GL_charge_y_hi[1] - GL_charge_y_lo[1]*GL_charge_y_lo[1]) +
                   GL_rho_inner_of_outerSec() *
                     (GL_charge_y_hi[2]*GL_charge_y_hi[2] - GL_charge_y_lo[2]*GL_charge_y_lo[2]) ) ;

    for (Int_t i = 0 ; i < 24; i++ ) {
      GLWeights[i   ] = GL_rho_inner_of_innerSec(anodeVolts->voltagePadrow(i+1,         1)) * norm ;
      GLWeights[i+24] = GL_rho_outer_of_innerSec(anodeVolts->voltagePadrow(i+1,INNER[i]  )) * norm ;
      GLWeights[i+48] = GL_rho_inner_of_outerSec(anodeVolts->voltagePadrow(i+1,INNER[i]+1)) * norm ;
      GLWeights[i+72] = GL_rho_outer_of_outerSec(anodeVolts->voltagePadrow(i+1,TPCROWS[i])) * norm ;
    }
  }
  
}

Bool_t StMagUtilities::UpdateTPCHighVoltages ()
{
  static tpcHighVoltages_st* voltagesTable = 0;

  St_tpcHighVoltagesC* voltagesChair = St_tpcHighVoltagesC::instance();
  tpcHighVoltages_st* new_voltagesTable = voltagesChair->Struct();
  Bool_t update = (new_voltagesTable != voltagesTable) || DoOnce;
  // using mode = kPadrow40 means only update high voltages, not anode voltages
  if (update) { GetTPCVoltages(kPadrow40); voltagesTable = new_voltagesTable; }
  return update;
}

void StMagUtilities::GetSpaceCharge ()  
{ 
  static spaceChargeCor_st* spaceTable = 0;
  static St_trigDetSumsC* scalers = 0;

  St_spaceChargeCorC* spaceChair = dynamic_cast<St_spaceChargeCorC*>(St_spaceChargeCorR1C::instance());
  spaceChargeCor_st* new_spaceTable = spaceChair->Struct();
  St_trigDetSumsC* new_scalers = St_trigDetSumsC::instance();
  if (new_spaceTable == spaceTable && new_scalers == scalers) return;
  fSpaceCharge =  spaceChair;
  spaceTable = new_spaceTable;
  scalers = new_scalers;

  SpaceCharge    =  fSpaceCharge->getSpaceChargeCoulombs((double)gFactor) ; 
}

void StMagUtilities::GetSpaceChargeR2 ()  
{ 
  static spaceChargeCor_st* spaceTable = 0;
  static St_trigDetSumsC* scalers = 0;

  St_spaceChargeCorC* spaceChair = dynamic_cast<St_spaceChargeCorC*>(St_spaceChargeCorR2C::instance());
  spaceChargeCor_st* new_spaceTable = spaceChair->Struct();
  St_trigDetSumsC* new_scalers = St_trigDetSumsC::instance();
  if (new_spaceTable == spaceTable && new_scalers == scalers) return;
  fSpaceChargeR2 =  spaceChair;
  spaceTable = new_spaceTable;
  scalers = new_scalers;

  SpaceChargeR2      = fSpaceChargeR2->getSpaceChargeCoulombs((double)gFactor) ;
  SmearCoefSC        = (fCalibResolutions ? mRandom->Gaus(1,fCalibResolutions->SpaceCharge()) : 1.0);
  SpaceChargeEWRatio = fSpaceChargeR2->getEWRatio() ;
}

void StMagUtilities::GetShortedRing ()
{
  St_tpcFieldCageShortC* shortedRingsChair = St_tpcFieldCageShortC::instance();
  ShortTableRows = (Int_t) shortedRingsChair->GetNRows() ;
  for ( Int_t i = 0 ; i < ShortTableRows ; i++)
    {
      if ( i >= 10 ) break ;
      Side[i]              =  (Int_t)   shortedRingsChair->side(i)              ;   // Location of Short E=0 / W=1
      Cage[i]              =  (Int_t)   shortedRingsChair->cage(i)              ;   // Location of Short IFC=0 / OFC=1
      Ring[i]              =  (Float_t) shortedRingsChair->ring(i)              ;   // Location of Short counting out from the CM.  CM==0 
      MissingResistance[i] =  (Float_t) shortedRingsChair->MissingResistance(i) ;   // Amount of Missing Resistance due to this short (MOhm)
      Resistor[i]          =  (Float_t) shortedRingsChair->resistor(i)          ;   // Amount of compensating resistance added for this short
    }
}

Bool_t StMagUtilities::UpdateShortedRing ()
{
  static tpcFieldCageShort_st* shortsTable = 0;

  St_tpcFieldCageShortC* shortedRingsChair = St_tpcFieldCageShortC::instance();
  tpcFieldCageShort_st* new_shortsTable = shortedRingsChair->Struct();
  Bool_t update = (new_shortsTable != shortsTable) || DoOnce;
  if (update) { GetShortedRing(); shortsTable = new_shortsTable; }
  return update;
}

/// Manually setup a shorted ring in the TPC
/*! Insert one shorted ring of your choice
    Side(E=0/W=1)  Cage(IFC=0/OFC=1)  Ring(# from CM)  MissingResistance(MOhm)  ExtraResistance(MOhm)
    For addtional information, see the comments associated with UndoShortedRing().
 */
void StMagUtilities::ManualShortedRing ( Int_t EastWest, Int_t InnerOuter, 
					 Float_t RingNumber, Float_t MissingRValue, Float_t ExtraRValue )
{
  // Insert one shorted ring of your choice.  Manually testing more than one shorted ring requires editing the code (below).
  ShortTableRows       =  1             ;              // Number of rows in the shorted ring table
  Side[0]              =  EastWest      ;              // Side of the TPC (E=0/W=1) 
  Cage[0]              =  InnerOuter    ;              // Field Cage (IFC=0/OFC=1 )
  Ring[0]              =  RingNumber    ;              // Ring Number (# from CM)
  MissingResistance[0] =  MissingRValue ;              // Missing Resistance due to short (MOhm)
  Resistor[0]          =  ExtraRValue   ;              // Extra Resistance added as compensation for the short (MOhm)
  if ( ( Side[0] + Cage[0] + Ring[0] + TMath::Abs(MissingResistance[0]) + TMath::Abs(Resistor[0]) ) == 0.0 ) ShortTableRows = 0 ;
  // JT Test of shorted ring repair ... uncomment and expand this section if you need to test more than one shorted ring 
  // ShortTableRows = 1 ;
  // Ring[0] = 182.5 ; Ring[1] = 0.0 ; Ring[2] = 0.0 ;
  // MissingResistance[0] = 2.0 ; MissingResistance[1] = 0.0 ; MissingResistance[2] = 0.0 ;
  // Resistor[0] = 0.0 ;  Resistor[1] = 0.0 ; Resistor[2] = 0.0 ;
  // End JT Test 
}

void StMagUtilities::GetOmegaTau ()
{
  fOmegaTau  =  St_tpcOmegaTauC::instance();
  TensorV1   =  fOmegaTau->getOmegaTauTensorV1();
  TensorV2   =  fOmegaTau->getOmegaTauTensorV2();
  mCorrectionsMode = fOmegaTau->distortionCorrectionsMode(); // default is 0 (important for old calibs)
}

/// Space Charge Correction Mode
/*!
     The spacecharge correction is performed using one of a variety
     of modes.  See the UndoSpaceChargeDistortion*() functions for
     more information on the different shapes used to correct the
     distortion.  Additionally, the magnitude of the correction may
     be set either manually, or from the database.  This routine
     provides a method to determine which mode is in use at any
     given time.  Return values are as follows:

      0 : no correction
     10 : uniform, from DB
     11 : uniform, manually set
     20 : R2, from DB
     21 : R2, manually set
*/
Int_t StMagUtilities::GetSpaceChargeMode()
{
   if (mDistortionMode & kSpaceCharge) {
     if (fSpaceCharge) return 10;
     else return 11;
   }
   if (mDistortionMode & kSpaceChargeR2) {
     if (fSpaceChargeR2) return 20;
     else return 21;
   }
   return 0;
}

void StMagUtilities::GetGridLeak ( Int_t mode )
{
   fGridLeak   =  St_tpcGridLeakC::instance()  ;
   InnerGridLeakStrength  =  fGridLeak -> getGridLeakStrength ( kGLinner )  ;  // Relative strength of the Inner grid leak
   InnerGridLeakRadius    =  fGridLeak -> getGridLeakRadius   ( kGLinner )  ;  // Location (in local Y coordinates) of Inner grid leak 
   InnerGridLeakWidth     =  fGridLeak -> getGridLeakWidth    ( kGLinner )  ;  // Half-width of the Inner grid leak.  
   MiddlGridLeakStrength  =  fGridLeak -> getGridLeakStrength ( kGLmiddl )  ;  // Relative strength of the Middle grid leak
   MiddlGridLeakRadius    =  fGridLeak -> getGridLeakRadius   ( kGLmiddl )  ;  // Location (in local Y coordinates) of Middl grid leak
   MiddlGridLeakWidth     =  fGridLeak -> getGridLeakWidth    ( kGLmiddl )  ;  // Half-width of the Middle grid leak.  
   OuterGridLeakStrength  =  fGridLeak -> getGridLeakStrength ( kGLouter )  ;  // Relative strength of the Outer grid leak
   OuterGridLeakRadius    =  fGridLeak -> getGridLeakRadius   ( kGLouter )  ;  // Location (in local Y coordinates) of Outer grid leak 
   OuterGridLeakWidth     =  fGridLeak -> getGridLeakWidth    ( kGLouter )  ;  // Half-width of the Outer grid leak.  
   if (mode & kFullGridLeak) {
     if (InnerGridLeakWidth <= 0) memset(  GLWeights     ,0,24*sizeof(Float_t));
     if (MiddlGridLeakWidth <= 0) memset(&(GLWeights[24]),0,48*sizeof(Float_t));
     if (OuterGridLeakWidth <= 0) memset(&(GLWeights[72]),0,24*sizeof(Float_t));
   }
  SmearCoefGL = (fCalibResolutions && fCalibResolutions->GridLeak() > 0 ?
    mRandom->Gaus(1,fCalibResolutions->GridLeak()) : 1.0);
}

void StMagUtilities::ManualGridLeakStrength (Double_t inner, Double_t middle, Double_t outer)
{
  InnerGridLeakStrength  =  inner  ;  // Relative strength of the Inner grid leak
  MiddlGridLeakStrength  =  middle ;  // Relative strength of the Middle grid leak
  OuterGridLeakStrength  =  outer  ;  // Relative strength of the Outer grid leak
  // InnerGridLeakStrength  =  1.0   ;  // JT test (Note that GainRatio is taken into account, below.)
  // OuterGridLeakStrength  =  1.0   ;  // JT test (keep these the same unless you really know what you are doing.) 
}

void StMagUtilities::ManualGridLeakRadius (Double_t inner, Double_t middle, Double_t outer)
{
  InnerGridLeakRadius    =  inner  ;  // Location (in local Y coordinates) of the Inner grid leak 
  MiddlGridLeakRadius    =  middle ;  // Location (in local Y coordinates) of the Middle grid leak 
  OuterGridLeakRadius    =  outer  ;  // Location (in local Y coordinates) of the Outer grid leak 
}

void StMagUtilities::ManualGridLeakWidth (Double_t inner, Double_t middle, Double_t outer)
{
  InnerGridLeakWidth     =  inner  ;  // Half-width of the Inner grid leak.  
  MiddlGridLeakWidth     =  middle ;  // Half-width of the Middle grid leak.  Must oversized for numerical reasons.
  OuterGridLeakWidth     =  outer  ;  // Half-width of the Outer grid leak.  
}

void StMagUtilities::GetHVPlanes ()
{
   // GetTPCVoltages() must be called first!
   fHVPlanes = St_tpcHVPlanesC::instance() ;
   tpcHVPlanes_st* HVplanes = fHVPlanes -> Struct();
   Float_t deltaVGGCathode = GG - GGideal;
   deltaVGGEast = (  HVplanes -> GGE_shift_z * StarMagE) + deltaVGGCathode;
   deltaVGGWest = (- HVplanes -> GGW_shift_z * StarMagE) + deltaVGGCathode;
}

void StMagUtilities::ManualGGVoltError (Double_t east, Double_t west)
{
  deltaVGGEast = east;
  deltaVGGWest = west;
}

void StMagUtilities::GetAbortGapCharge() {
  fAbortGapCharge = St_tpcChargeEventC::instance() ;
  AbortGapCharges = fAbortGapCharge->getCharges() ;
  AbortGapTimes   = fAbortGapCharge->getTimes() ;
  AbortGapChargeCoef = (St_tpcSCGLC::instance()->SC())[0] ; // temporary location for these?
  IonDriftVel        = (St_tpcSCGLC::instance()->SC())[1] ; // temporary location for these?
}

//________________________________________

//  Standard maps for E and B Field Distortions ... Note: These are no longer read from a file but are listed here (JT, 2009).
//  These maps have enough resolution for all fields except the Grid Leak Calculations.  So note that the Grid Leak calculations
//  (and PadRow13 calculations) don't use these tables but have custom lists of eRList[] built into each function.
//
Float_t StMagUtilities::eRList[EMap_nR] = {   48.0,   49.0,
                                              50.0,   52.0,   54.0,   56.0,   58.0,   60.0,   62.0,   64.0,   66.0,   68.0, 
					      70.0,   72.0,   74.0,   76.0,   78.0,   80.0,   82.0,   84.0,   86.0,   88.0, 
					      90.0,   92.0,   94.0,   96.0,   98.0,  100.0,  102.0,  104.0,  106.0,  108.0, 
					     110.0,  112.0,  114.0,  116.0,  118.0,  120.0,  122.0,  124.0,  126.0,  128.0, 
					     130.0,  132.0,  134.0,  136.0,  138.0,  140.0,  142.0,  144.0,  146.0,  148.0, 
					     150.0,  152.0,  154.0,  156.0,  158.0,  160.0,  162.0,  164.0,  166.0,  168.0, 
					     170.0,  172.0,  174.0,  176.0,  178.0,  180.0,  182.0,  184.0,  186.0,  188.0, 
					     190.0,  192.0,  193.0,  194.0,  195.0,  196.0,  197.0,  198.0,  199.0,  199.5  } ;

Float_t StMagUtilities::ePhiList[EMap_nPhi] = {  0.0000, 0.5236, 1.0472, 1.5708, 2.0944, 2.6180, 3.1416,
					         3.6652, 4.1888, 4.7124, 5.2360, 5.7596, 6.2832  } ;  // 13 planes of phi - so can wrap around

Float_t StMagUtilities::eZList[EMap_nZ] = { -208.5, -208.0, -207.0, -206.0, -205.0, -204.0, -202.0,
					    -200.0, -198.0, -196.0, -194.0, -192.0, -190.0, -188.0, -186.0, -184.0, -182.0,
					    -180.0, -178.0, -176.0, -174.0, -172.0, -170.0, -168.0, -166.0, -164.0, -162.0,
					    -160.0, -158.0, -156.0, -154.0, -152.0, -150.0, -148.0, -146.0, -144.0, -142.0,
					    -140.0, -138.0, -136.0, -134.0, -132.0, -130.0, -128.0, -126.0, -124.0, -122.0,
					    -120.0, -118.0, -116.0, -114.0, -112.0, -110.0, -108.0, -106.0, -104.0, -102.0,
					    -100.0,  -98.0,  -96.0,  -94.0,  -92.0,  -90.0,  -88.0,  -86.0,  -84.0,  -82.0,
					     -80.0,  -78.0,  -76.0,  -74.0,  -72.0,  -70.0,  -68.0,  -66.0,  -64.0,  -62.0,
					     -60.0,  -58.0,  -56.0,  -54.0,  -52.0,  -50.0,  -48.0,  -46.0,  -44.0,  -42.0,
					     -40.0,  -38.0,  -36.0,  -34.0,  -32.0,  -30.0,  -28.0,  -26.0,  -24.0,  -22.0,
					     -20.0,  -18.0,  -16.0,  -14.0,  -12.0,  -10.0,   -8.0,   -6.0,   -4.0,   -2.0,
					      -1.0,   -0.5,   -0.2,   -0.1,  -0.05,   0.05,    0.1,    0.2,    0.5,    1.0,   
  					       2.0,    4.0,    6.0,    8.0,   10.0,   12.0,   14.0,   16.0,   18.0,   20.0, 
  					      22.0,   24.0,   26.0,   28.0,   30.0,   32.0,   34.0,   36.0,   38.0,   40.0, 
  					      42.0,   44.0,   46.0,   48.0,   50.0,   52.0,   54.0,   56.0,   58.0,   60.0, 
  					      62.0,   64.0,   66.0,   68.0,   70.0,   72.0,   74.0,   76.0,   78.0,   80.0, 
  					      82.0,   84.0,   86.0,   88.0,   90.0,   92.0,   94.0,   96.0,   98.0,  100.0, 
  					     102.0,  104.0,  106.0,  108.0,  110.0,  112.0,  114.0,  116.0,  118.0,  120.0, 
  					     122.0,  124.0,  126.0,  128.0,  130.0,  132.0,  134.0,  136.0,  138.0,  140.0, 
  					     142.0,  144.0,  146.0,  148.0,  150.0,  152.0,  154.0,  156.0,  158.0,  160.0, 
  					     162.0,  164.0,  166.0,  168.0,  170.0,  172.0,  174.0,  176.0,  178.0,  180.0, 
  					     182.0,  184.0,  186.0,  188.0,  190.0,  192.0,  194.0,  196.0,  198.0,  200.0, 
					     202.0,  204.0,  205.0,  206.0,  207.0,  208.0,  208.5   } ;
//
//  Note the careful steps in Z around the Central Membrane due to discontinuity at CM.  Needed for interpolation tools on grid.
//  Also note that whenever we interpolate this grid, we explicitly do not allow Z to get closer to CM than 0.2 cm.  This gives
//  three points for quadratic interpolation in all cases.
//  End of standard map parameter lists

/// Initialization method.  This will sort and apply the options received by the tpt Maker
void StMagUtilities::CommonStart ( Int_t mode )
{
  cout << "StMagUtilities::CommonSta  Magnetic Field scale factor is " << gFactor << endl ;
  if ( StTpcDb::instance() == 0 )
    {
      cout << "StMagUtilities::CommonSta  ***NO TPC DB, Using default TPC parameters. You sure it is OK??? ***" << endl ; 
      cout << "StMagUtilities::CommonSta  ***NO TPC DB, Using default TPC parameters. You sure it is OK??? ***" << endl ; 
      cout << "StMagUtilities::CommonSta  ***NO TPC DB, Using default TPC parameters. You sure it is OK??? ***" << endl ; 
      cout << "StMagUtilities::CommonSta  ***NO TPC DB, Using default TPC parameters. You sure it is OK??? ***" << endl ; 
      StarDriftV  =     5.54 ;      // Drift Velocity (cm/microSec) Magnitude
      TPC_Z0      =    208.7 ;      // Z location of STAR TPC Gated Grid (cm)
#ifndef __NO_TWIST__
      XTWIST      =   -0.165 ;      // X Displacement of West end of TPC wrt magnet (mRad)
      YTWIST      =    0.219 ;      // Y Displacement of West end of TPC wrt magnet (mRad)
#endif /*! __NO_TWIST__ */
      IFCShift    =   0.0080 ;      // Shift of the IFC towards the West Endcap (cm) (2/1/2002)
#ifndef __NO_CLOCK__
      EASTCLOCKERROR =   0.0 ;      // Phi rotation of East end of TPC in milli-radians
      WESTCLOCKERROR = -0.43 ;      // Phi rotation of West end of TPC in milli-radians
#endif /* ! __NO_CLOCK__ */
      IFCRadius   =    47.90 ;      // Radius of the Inner Field Cage
      OFCRadius   =    200.0 ;      // Radius of the Outer Field Cage
      INNERGGFirst =  53.0   ;      // Radius of the first Inner Gating Grid Wire
      INNERGGLast  = 121.0   ;      // Radius of the last Inner Gating Grid Wire
      OUTERGGFirst = 122.595 ;      // Radius of the first Outer Gating Grid Wire
      OUTERGGLast  = 191.395 ;      // Radius of the last Outer Gating Grid Wire
      GAPRADIUS   =    121.8 ;      // Radius of the gap between the inner and outer grids (cm) at sector centerline
      WIREGAP     =    1.595 ;      // Width of the gap between the inner and outer grids (cm)
      for ( Int_t sec = 0; sec < 24; sec++)
        {
          INNER[sec]          =  13   ;      // Number of TPC rows in the inner sectors
          TPCROWS[sec]        =  45   ;      // Total number of TPC rows per sector (Inner + Outer)
          for ( Int_t row = 0 ; row < TPCROWS[sec] ; row++ )
            {
              if ( row < 8 )
	        TPCROWR[sec][row] = 60.0 + row*4.8 ;
              else if ( row < INNER[sec] )
	        TPCROWR[sec][row] = 93.6 + (row-8+1)*5.2 ;
              else
	        TPCROWR[sec][row] = 127.195 + (row-INNER[sec])*2.0 ;
            }
        }
      mCorrectionsMode = 0;

      cout << "StMagUtilities::CommonSta  WARNING -- Using hard-wired TPC parameters. " << endl ; 
    }
  else  cout << "StMagUtilities::CommonSta  Using TPC parameters from DataBase. " << endl ; 
  
  if ( fTpcVolts == 0 ) 
    { 
      CathodeV    = -27950.0 ;      // Cathode Voltage (volts)
      GG          =   -115.0 ;      // Gating Grid voltage (volts)
      GetE() ;
      for (Int_t i = 0 ; i < 25; i++ ) GLWeights[i] = 1; // Initialize as uniform
      for (Int_t i = 0 ; i < 24; i++ ) { Inner_GLW_Voltage[i] = 999; Outer_GLW_Voltage[i] = 999; }
      cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected TpcVoltages setting. " << endl ;
    } 
  else  cout << "StMagUtilities::CommonSta  Using TPC voltages from the DB."   << endl ; 
  
  if ( fOmegaTau == 0 ) 
    {
      TensorV1    =  1.35 ;  // Drift velocity tensor term: in the ExB direction
      TensorV2    =  1.10 ;  // Drift velocity tensor term: direction perpendicular to Z and ExB
      cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected OmegaTau parameters. " << endl ; 
    }
  else  cout << "StMagUtilities::CommonSta  Using OmegaTau parameters from the DB."   << endl ; 

  if (fSpaceCharge) cout << "StMagUtilities::CommonSta  Using SpaceCharge values from the DB." << endl ; 
  else              cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected SpaceCharge settings. " << endl ; 
  
  if (fSpaceChargeR2) cout << "StMagUtilities::CommonSta  Using SpaceChargeR2 values from the DB." << endl ;
  else                cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected SpaceChargeR2 settings. " << endl ; 
  
  if ( fGridLeak == 0 ) 
    {
      ManualGridLeakStrength(0.0,15.0,0.0);
      ManualGridLeakRadius(53.0,GAPRADIUS,195.0);
      ManualGridLeakWidth(0.0,3.0,0.0);
      cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected GridLeak parameters. " << endl ; 
    }
  else  cout << "StMagUtilities::CommonSta  Using GridLeak parameters from the DB."   << endl ; 
  
  if ( fHVPlanes == 0 ) 
    {
      ManualGGVoltError(0.0,0.0);
      cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected HV planes parameters. " << endl ; 
    }
  else  cout << "StMagUtilities::CommonSta  Using HV planes parameters from the DB."   << endl ; 

  if ( fAbortGapCharge == 0 )
    {
      IonDriftVel = 181.67; // http://nuclear.ucdavis.edu/~bkimelman/protected/TPC_Meeting_Apr_10.pdf
      cout << "StMagUtilities::CommonSta  WARNING -- Using default Ion Drift Velocity. " << endl ;
    }

  // Parse the mode switch which was received from the Tpt maker
  // To turn on and off individual distortions, set these higher bits
  // Default behavior: no bits set gives you the following defaults

  mDistortionMode |= mode;
  if (mDistortionMode & kDisableTwistClock) {
    mDistortionMode &= ~(mDistortionMode & kTwist);
    mDistortionMode &= ~(mDistortionMode & kClock);
    mDistortionMode &= ~(mDistortionMode & kFast2DBMap);
  }
  if ( !( mode & ( kBMap | kPadrow13 | kPadrow40 | kTwist | kClock | kMembrane | kEndcap | kIFCShift | kSpaceCharge | kSpaceChargeR2 |
                   kAbortGap | kShortedRing | kFast2DBMap | kGridLeak | k3DGridLeak | kGGVoltError | kSectorAlign | kFullGridLeak))) 
    {
       mDistortionMode |= kPadrow40 ;
       mDistortionMode |= kAbortGap ;
       if (! (mDistortionMode & kDisableTwistClock)) {
	 mDistortionMode |= kFast2DBMap ;
	 mDistortionMode |= kTwist ;
	 mDistortionMode |= kClock ;
       }
       mDistortionMode |= kIFCShift ;
       printf("StMagUtilities::CommonSta  Default mode selection\n");
    } 
  else printf("StMagUtilities::CommonSta  Using mode option 0x%X\n",mode);
 
  printf("StMagUtilities::CommonSta  Using correction mode 0x%X\n",mCorrectionsMode);
  iterateDistortion = mCorrectionsMode & kIterateUndo;
  iterationFailCounter = -1;
  printf("StMagUtilities::CommonSta  Version  ");
  if ( mDistortionMode & kBMap )          printf ("3D Mag Field Distortions") ;
  if ( mDistortionMode & kFast2DBMap )    printf ("2D Mag Field Distortions") ;
  if ( mDistortionMode & kPadrow13 )      printf (" + Padrow 13") ;
  if ( mDistortionMode & kPadrow40 )      printf (" + Padrow 40") ;
  if ( mDistortionMode & kTwist )         printf (" + Twist") ;
  if ( mDistortionMode & kClock )         printf (" + Clock") ;
  if ( mDistortionMode & kIFCShift )      printf (" + IFCShift") ;
  if ( mDistortionMode & kSpaceCharge )   printf (" + SpaceCharge") ;
  if ( mDistortionMode & kSpaceChargeR2 ) printf (" + SpaceChargeR2") ;
  if ( mDistortionMode & kAbortGap )      printf (" + AbortGapSpaceR2") ;
  if ( mDistortionMode & kMembrane )      printf (" + Central Membrane") ;
  if ( mDistortionMode & kEndcap )        printf (" + Endcap") ;
  if ( mDistortionMode & kShortedRing )   printf (" + ShortedRing") ;
  if ( mDistortionMode & kGridLeak )      printf (" + GridLeak") ;
  if ( mDistortionMode & k3DGridLeak )    printf (" + 3DGridLeak") ;
  if ( mDistortionMode & kSectorAlign )   printf (" + SectorAlign") ;
  if ( mDistortionMode & kFullGridLeak )  printf (" + FullGridLeak") ;
  if ( mDistortionMode & kDistoSmearing ) printf (" + DistoSmearing") ;
  if ( ! StTpcDb::IsOldScheme())          printf (" + New TPC Alignment schema") ;
  usingCartesian = kTRUE; // default

  printf("\n");
 
  Float_t  B[3], X[3] = { 0, 0, 0 } ;
  Float_t  OmegaTau ;                       // For an electron, OmegaTau carries the sign opposite of B 
  BField(X,B) ;                             // Work in kGauss, cm and assume Bz dominates

  // Theoretically, OmegaTau is defined as shown in the next line.  
  // OmegaTau   =  -10. * B[2] * StarDriftV / StarMagE ;  // cm/microsec, Volts/cm
  // Instead, we will use scaled values from Amendolia et al NIM A235 (1986) 296 and include their
  // characterization of the electron drift velocity tensor with different omega-tau's in different directions.
  // Float_t TensorV1    =  1.34 ;  // Drift velocity tensor term: in the ExB direction
  // Float_t TensorV2    =  1.11 ;  // Drift velocity tensor term: direction perpendicular to Z and ExB
  // Gene Van Buren's work with the shorted ring and/or shifted GG values has determined the following numbers in STAR
  // Float_t TensorV1    =  1.36 ;  // Drift velocity tensor term: in the ExB direction
  // Float_t TensorV2    =  1.11 ;  // Drift velocity tensor term: direction perpendicular to Z and ExB
  // Gene's error bars are +- 0.03 on the term in the ExB diretion and +-0.06 in the perpendicular direction
  // To reinforce the fact that these numbers are only good to 2 or 3 percent I am going to round off Gene's numbers
  // Float_t TensorV1    =  1.35 ;  // Drift velocity tensor term: in the ExB direction
  // Float_t TensorV2    =  1.10 ;  // Drift velocity tensor term: direction perpendicular to Z and ExB

  OmegaTau   =  -10.0 * B[2] * StarDriftV / StarMagE ;     // B in kGauss, note the sign of B is important 

  Const_0    =  1. / ( 1. +  TensorV2*TensorV2*OmegaTau*OmegaTau ) ;
  Const_1    =  TensorV1*OmegaTau / ( 1. + TensorV1*TensorV1*OmegaTau*OmegaTau ) ;
  Const_2    =  TensorV2*TensorV2*OmegaTau*OmegaTau / ( 1. + TensorV2*TensorV2*OmegaTau*OmegaTau ) ;

  cout << "StMagUtilities::BField        =  " << B[2] << " kGauss at (0,0,0)" <<  endl ; 
  cout << "StMagUtilities::DriftVel      =  " << StarDriftV << " cm/microsec" <<  endl ; 
  cout << "StMagUtilities::TPC_Z0        =  " << TPC_Z0 << " cm" << endl ; 
  cout << "StMagUtilities::TensorV1+V2   =  " << TensorV1 << " " << TensorV2 << endl ; 
  cout << "StMagUtilities::OmegaTau1+2   =  " << OmegaTau * TensorV1 << " " << OmegaTau * TensorV2 << endl ; 
  cout << "StMagUtilities::XTWIST        =  " << XTWIST << " mrad" << endl ;
  cout << "StMagUtilities::YTWIST        =  " << YTWIST << " mrad" << endl ;
  cout << "StMagUtilities::SpaceCharge   =  " << SpaceCharge << " Coulombs/epsilon-nought" << endl ;
  cout << "StMagUtilities::SpaceChargeR2 =  " << SpaceChargeR2 << " Coulombs/epsilon-nought" << "  EWRatio = " 
                                              << SpaceChargeEWRatio << endl ;
  if (mDistortionMode & kDistoSmearing) {
  cout << "StMagUtilities::SmearCoefSC   =  " << SmearCoefSC << endl;
  cout << "StMagUtilities::SmearCoefGL   =  " << SmearCoefGL << endl;
  }
  if (mDistortionMode & kAbortGap) {
  cout << "StMagUtilities::AbortGapCoef  =  " << AbortGapChargeCoef << endl;
  }
  cout << "StMagUtilities::IFCShift      =  " << IFCShift << " cm" << endl ;
  cout << "StMagUtilities::CathodeV      =  " << CathodeV << " volts" << endl ;
  cout << "StMagUtilities::GG            =  " << GG << " volts" << endl ;
  if (mDistortionMode & kPadrow40) {
  cout << "StMagUtilities::Inner_GLW_V   =  " ;
    for ( Int_t i = 0 ; i < 24 ; i++ ) cout << Inner_GLW_Voltage[i] << " " ; cout << "volts" << endl ;
  cout << "StMagUtilities::Outer_GLW_V   =  " ;
    for ( Int_t i = 0 ; i < 24 ; i++ ) cout << Outer_GLW_Voltage[i] << " " ; cout << "volts" << endl ;
  }
  cout << "StMagUtilities::EastClock     =  " << EASTCLOCKERROR << " mrad" << endl;
  cout << "StMagUtilities::WestClock     =  " << WESTCLOCKERROR << " mrad" << endl;
  cout << "StMagUtilities::Side          =  " ;  
  for ( Int_t i = 0 ; i < ShortTableRows ; i++ ) cout << Side[i] << " " ; cout << "Location of Short E=0 / W=1 " << endl;
  cout << "StMagUtilities::Cage          =  " ;  
  for ( Int_t i = 0 ; i < ShortTableRows ; i++ ) cout << Cage[i] << " " ; cout << "Location of Short IFC = 0 / OFC = 1" << endl;
  cout << "StMagUtilities::Ring          =  " ;  
  for ( Int_t i = 0 ; i < ShortTableRows ; i++ ) cout << Ring[i] << " " ; cout << "Rings - Location of Short counting from the CM" << endl;
  cout << "StMagUtilities::MissingOhms   =  " ;  
  for ( Int_t i = 0 ; i < ShortTableRows ; i++ ) cout << MissingResistance[i] << " " ; cout << "MOhms Missing Resistance" << endl;
  cout << "StMagUtilities::CompResistor  =  " ;  
  for ( Int_t i = 0 ; i < ShortTableRows ; i++ ) cout << Resistor[i] << " " ; cout << "MOhm Compensating Resistor Value" << endl;
  cout << "StMagUtilities::InnerGridLeak =  " << InnerGridLeakStrength << " " << InnerGridLeakRadius << " " << InnerGridLeakWidth << endl;
  cout << "StMagUtilities::MiddlGridLeak =  " << MiddlGridLeakStrength << " " << MiddlGridLeakRadius << " " << MiddlGridLeakWidth << endl;
  cout << "StMagUtilities::OuterGridLeak =  " << OuterGridLeakStrength << " " << OuterGridLeakRadius << " " << OuterGridLeakWidth << endl;
  cout << "StMagUtilities::deltaVGG      =  " << deltaVGGEast << " V (east) : " << deltaVGGWest << " V (west)" << endl;
  cout << "StMagUtilities::GLWeights     =  " ;
  if (mDistortionMode & k3DGridLeak) {
    for ( Int_t i = 1 ; i < 25 ; i++ ) cout << GLWeights[i] << " " ;
    cout << endl;
  } else if (mDistortionMode & kFullGridLeak) {
    cout << endl;
    for ( Int_t i = 0 ; i < 24 ; i++ ) {
      for ( Int_t j = 0 ; j < 96 ; j+=24 ) cout << std::fixed << "    " << std::setprecision(2) << GLWeights[i+j];
      cout << endl;
    }
  } else cout << "N/A" << endl;

  doingDistortion = kFALSE;
  DoOnce = kTRUE;

}

//________________________________________


//________________________________________



void StMagUtilities::UndoDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{
  // Control by flags JCD Oct 4, 2001
  //
  // NOTE: x[],Xprime[] must be Cartesian for this function!

  if (!x) {
    // dummy call, e.g. UndoDistortion(0,0,0)
    // makes sure everything is initialized for current timestamp
    const Float_t Xtemp1[3] = {100.,0.,100.};
    Float_t Xtemp2[3];
    Bool_t tempIterDist = iterateDistortion;
    iterateDistortion = kFALSE; // Do not iterate for dummy call
    UndoDistortion(Xtemp1,Xtemp2,3);
    iterateDistortion = tempIterDist;
    return;
  }

  Float_t Xprime1[3], Xprime2[3] ;

  SectorNumber( Sector, x ) ;

  if (iterateDistortion) {
    // iteration to determine UndoDistortion()
    iterateDistortion = kFALSE;

    memcpy(Xprime1,x,threeFloats);
    memcpy(Xprime,x,threeFloats);
    const Float_t MINDIST_SQ = 1e-8; // ~1 micron accuracy
    Float_t dist_sq = 1e10;
    int iter = 0;
    while (dist_sq > MINDIST_SQ) { // iterate to a precision of 10 microns
      UndoDistortion ( Xprime1, Xprime2, Sector ) ;
      Xprime1[0] = x[0] - (Xprime1[0] - Xprime2[0]) ;
      Xprime1[1] = x[1] - (Xprime1[1] - Xprime2[1]) ;
      Xprime1[2] = x[2] - (Xprime1[2] - Xprime2[2]) ;
      dist_sq = (Xprime1[0]-Xprime[0])*(Xprime1[0]-Xprime[0])
              + (Xprime1[1]-Xprime[1])*(Xprime1[1]-Xprime[1])
              + (Xprime1[2]-Xprime[2])*(Xprime1[2]-Xprime[2]) ;
      if (++iter > 20 && dist_sq > MINDIST_SQ) { // Not converging
        // Take average of last two solutions
        Xprime[0] = 0.5*(Xprime[0] + Xprime1[0]);
        Xprime[1] = 0.5*(Xprime[1] + Xprime1[1]);
        Xprime[2] = 0.5*(Xprime[2] + Xprime1[2]);
        dist_sq = 0;
        if (iterationFailCounter >=0) iterationFailCounter++;
      } else {
        memcpy(Xprime,Xprime1,threeFloats);
      }
    }

    iterateDistortion = kTRUE;
    return;
  } // end of iteration


  // Set it up
/*
  memcpy(Xprime1,x,threeFloats);
 

  Float_t r2 = x[0]*x[0] + x[1]*x[1] ;   // Point must be inside TPC to be suffer distortions, check this.
  if ( r2 >= OFCRadius*OFCRadius || r2 <= IFCRadius*IFCRadius || x[2] >= TPC_Z0 || x[2] <= -1*TPC_Z0 )
    {
      memcpy(Xprime,x,threeFloats);
      return ;
    }
*/
  if ( TMath::Abs(x[2]) >= TPC_Z0 )
    { memcpy(Xprime,x,threeFloats); return ; }
  Cart2Polar(x,Xprime1[0],Xprime1[1]);
  if  (Xprime1[0] > OFCRadius || Xprime1[0] < IFCRadius )
    { memcpy(Xprime,x,threeFloats); return ; }
  Xprime1[2] = x[2];
  usingCartesian = kFALSE;
      
  if (mDistortionMode & kBMap) {
      FastUndoBDistortion    ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & kFast2DBMap) {
      FastUndo2DBDistortion    ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if ((mDistortionMode & kBMap) && (mDistortionMode & kFast2DBMap)) {
      cout << "StMagUtilities ERROR **** Do not use kBMap and kFast2DBMap at the same time" << endl ;
      cout << "StMagUtilities ERROR **** These routines have duplicate functionality so don't do both." << endl ;
      exit(1) ;
  }

  if ((mDistortionMode & kPadrow13) && (mDistortionMode & kPadrow40)) {
      cout << "StMagUtilities ERROR **** Do not use kPadrow13 and kPadrow40 at the same time" << endl ;
      cout << "StMagUtilities ERROR **** These routines have duplicate functionality so don't do both." << endl ;
      exit(1) ;
  }

  if (mDistortionMode & kPadrow13) {
      UndoPad13Distortion    ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }
  
  if (mDistortionMode & kPadrow40) {
      UndoPad40Distortion    ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }
  
  if (mDistortionMode & kTwist) {
      UndoTwistDistortion    ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & kClock) {
      UndoClockDistortion    ( Xprime1, Xprime2, Sector ) ; 
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & kMembrane) {
      UndoMembraneDistortion ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & kEndcap) { 
      UndoEndcapDistortion ( Xprime1, Xprime2, Sector ) ; 
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & kIFCShift) { 
      UndoIFCShiftDistortion ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & (kSpaceCharge | kSpaceChargeR2)) { 
      UndoSpaceChargeDistortion ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & kAbortGap) {
      UndoAbortGapDistortion ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & kShortedRing) { 
      UndoShortedRingDistortion ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & (kGridLeak | k3DGridLeak | kFullGridLeak)) { 
      UndoGridLeakDistortion ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & kGGVoltError) {
      UndoGGVoltErrorDistortion ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }

  if (mDistortionMode & kSectorAlign) {
      UndoSectorAlignDistortion ( Xprime1, Xprime2, Sector ) ;
      memcpy(Xprime1,Xprime2,threeFloats);
  }


  // Return it

  //memcpy(Xprime,Xprime1,threeFloats);
  Polar2Cart(Xprime1[0],Xprime1[1],Xprime);
  Xprime[2] = Xprime1[2];
  usingCartesian = kTRUE;

  DoOnce = kFALSE;
  if (! iterateDistortion && fgUnDoDistortion) {
    D.sector = Sector;
    D.xL = x[0];
    D.yL = x[1];
    D.zL = x[2];
    D.xLC = Xprime[0];
    D.yLC = Xprime[1];
    D.zLC = Xprime[2];
    fgUnDoDistortion->Fill(&D.sector);
  }
}

//________________________________________


/// Main Entry Point for requests to DO the E and B field distortions (for simulations)
void StMagUtilities::DoDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  // NOTE: x[],Xprime[] must be Cartesian for this function!

  Bool_t tempIterDist = iterateDistortion;
  Bool_t tempDoingDist = doingDistortion;
  iterateDistortion = kFALSE; // Do not iterate for DoDistortion()
  doingDistortion = kTRUE;

  UndoDistortion ( x, Xprime, Sector ) ;

  Xprime[0] = 2*x[0] - Xprime[0] ;
  Xprime[1] = 2*x[1] - Xprime[1] ;
  Xprime[2] = 2*x[2] - Xprime[2] ;

  iterateDistortion = tempIterDist;

  if (fgDoDistortion) {
    D.sector = Sector;
    D.xL = x[0];
    D.yL = x[1];
    D.zL = x[2];
    D.xLC = Xprime[0];
    D.yLC = Xprime[1];
    D.zLC = Xprime[2];
    fgDoDistortion->Fill(&D.sector);
  }
  doingDistortion = tempDoingDist;
}


//________________________________________


/// B field distortions in 3D ( no Table ) - calculate the distortions due to the shape of the B field
/*! 
    Distortions are calculated point by point and integrated in real time.
    This avoids the time required to set up a table of distorted values but
    is slow for a very large number of points ( > 10,000 ).
*/
void StMagUtilities::UndoBDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  // NOTE: x[],Xprime[] must be Cartesian for this function!

  Double_t ah ;                                        // ah carries the sign opposite of E (for forward integration)
  Float_t  B[3] ; 
  Int_t    sign, index = 1 , NSTEPS ;              
  
  sign = SectorSide( Sector, x ) ;                     // 1 = TPC West, -1 = TPC East

  Xprime[0]  =  x[0] ;                                 // Integrate backwards from TPC plane to 
  Xprime[1]  =  x[1] ;                                 // the point the electron cluster was born. 
  Xprime[2]  =  sign * TPC_Z0 ;                        // Prepare for different readout planes

  for ( NSTEPS = 5 ; NSTEPS < 1000 ; NSTEPS += 2 )     // Choose ah to be about 1.0 cm, NSTEPS must be odd
    {
      ah = ( x[2] - sign * TPC_Z0 ) / ( NSTEPS - 1 ) ; // Going Backwards! See note above.
      if ( TMath::Abs(ah) < 1.0 ) break ;
    }

  for ( Int_t i = 1; i <= NSTEPS; ++i )                // Simpson's Integration Loop
    {
      if ( i == NSTEPS ) index = 1 ;
      Xprime[2] +=  index*(ah/3) ;
      B3DFieldTpc( Xprime, B , Sector) ;               // Work in kGauss, cm (uses Cartesian coordinates)
      if ( TMath::Abs(B[2]) > 0.001 )                  // Protect From Divide by Zero Faults
	{
	  Xprime[0] +=  index*(ah/3)*( Const_2*B[0] - Const_1*B[1] ) / B[2] ;
	  Xprime[1] +=  index*(ah/3)*( Const_2*B[1] + Const_1*B[0] ) / B[2] ;
	}
      if ( index != 4 ) index = 4; else index = 2 ;
    }    

}

/// 2D - faster - B field distortions ( no Table ) - calculate the distortions due to the shape of the B field
/*! 
    Distortions are calculated point by point and integrated in real time.
    This avoids the time required to set up a table of distorted values but
    is slow for a very large number of points ( > 10,000 ).
*/
void StMagUtilities::Undo2DBDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  // NOTE: x[],Xprime[] must be Cartesian for this function!

  Double_t ah ;                             // ah carries the sign opposite of E (for forward integration)
  Float_t  B[3] ; 
  Int_t    sign, index = 1 , NSTEPS ;              
  
  sign = SectorSide( Sector, x ) ;                     // 1 = TPC West, -1 = TPC East

  Xprime[0]  =  x[0] ;                                 // Integrate backwards from TPC plane to 
  Xprime[1]  =  x[1] ;                                 // the point the electron cluster was born. 
  Xprime[2]  =  sign * TPC_Z0 ;                        // Prepare for different readout planes

  for ( NSTEPS = 5 ; NSTEPS < 1000 ; NSTEPS += 2 )     // Choose ah to be about 1.0 cm, NSTEPS must be odd
    {
      ah = ( x[2] - sign * TPC_Z0 ) / ( NSTEPS - 1 ) ; // Going Backwards! See note above.
      if ( TMath::Abs(ah) < 1.0 ) break ;
    }

  for ( Int_t i = 1; i <= NSTEPS; ++i )                // Simpson's Integration Loop
    {
      if ( i == NSTEPS ) index = 1 ;
      Xprime[2] +=  index*(ah/3) ;
      BFieldTpc( Xprime, B, Sector);                   // Work in kGauss, cm (uses Cartesian coordinates)
      if ( TMath::Abs(B[2]) > 0.001 )                  // Protect From Divide by Zero Faults
	{
	  Xprime[0] +=  index*(ah/3)*( Const_2*B[0] - Const_1*B[1] ) / B[2] ;
	  Xprime[1] +=  index*(ah/3)*( Const_2*B[1] + Const_1*B[0] ) / B[2] ;
	}
      if ( index != 4 ) index = 4; else index = 2 ;
    }    



}

/// 3D - B field distortions (Table) - calculate the distortions due to the shape of the B field
/*! 
    Distortions are calculated in 3D and then stored in a table.  This method requires
    about 1 minute of CPU time to generate the table but it is very fast after the
    table has been created.  Use it when you have a large number of points ( > 10,000 ).
*/
void StMagUtilities::FastUndoBDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  static  Float_t dx3D[EMap_nPhi][EMap_nR][EMap_nZ], dy3D[EMap_nPhi][EMap_nR][EMap_nZ] ;
  static  Int_t   ilow = 0, jlow = 0, klow = 0 ;
  const   Int_t   PHIORDER = 1 ;                    // Linear interpolation = 1, Quadratic = 2 ... PHI Table is crude so use linear interp
  const   Int_t   ORDER    = 1 ;                    // Linear interpolation = 1, Quadratic = 2         

  Int_t   i, j, k ;
  Float_t xx[3]   ;
  Float_t save_x[ORDER+1], saved_x[ORDER+1] ;
  Float_t save_y[ORDER+1], saved_y[ORDER+1] ;

  Float_t r, phi ;
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;            // Table uses phi from 0 to 2*Pi
  Float_t z = LimitZ( Sector, x ) ;                 // Protect against discontinuity at CM

  if ( DoOnce )
    {
      cout << "StMagUtilities::FastUndoD  Please wait for the tables to fill ... ~90 seconds" << endl ;
      for ( k = 0 ; k < EMap_nPhi ; k++ )
	{
	  for ( i = 0 ; i < EMap_nR ; i++ )
	    {
	      xx[0] = eRList[i] * TMath::Cos(ePhiList[k]) ;
	      xx[1] = eRList[i] * TMath::Sin(ePhiList[k]) ;
	      for ( j = 0 ; j < EMap_nZ ; j++ )
		{
		  xx[2] = eZList[j] ;
		  UndoBDistortion(xx,Xprime) ;  // uses Cartesian coordinates
		  dx3D[k][i][j]   = Xprime[0] - xx[0] ;
		  dy3D[k][i][j]   = Xprime[1] - xx[1] ;
		}
	    }
	}
    }

  Search( EMap_nPhi, ePhiList, phi, klow ) ;
  Search( EMap_nR,   eRList,   r,   ilow ) ;
  Search( EMap_nZ,   eZList,   z,   jlow ) ;
  if ( klow < 0 ) klow = 0 ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow + ORDER  >=  EMap_nPhi-1 ) klow =  EMap_nPhi - 1 - ORDER ;
  if ( ilow + ORDER  >=  EMap_nR-1   ) ilow =  EMap_nR   - 1 - ORDER ;
  if ( jlow + ORDER  >=  EMap_nZ-1   ) jlow =  EMap_nZ   - 1 - ORDER ;
  
  for ( k = klow ; k < klow + ORDER + 1 ; k++ )
    {
      for ( i = ilow ; i < ilow + ORDER + 1 ; i++ )
	{
	  save_x[i-ilow]  = Interpolate( &eZList[jlow], &dx3D[k][i][jlow], ORDER, z ) ;
	  save_y[i-ilow]  = Interpolate( &eZList[jlow], &dy3D[k][i][jlow], ORDER, z ) ;
	}
      saved_x[k-klow]  = Interpolate( &eRList[ilow], save_x, ORDER, r )   ; 
      saved_y[k-klow]  = Interpolate( &eRList[ilow], save_y, ORDER, r )   ; 
    }
  
  if (usingCartesian) {
    Xprime[0] = Interpolate( &ePhiList[klow], saved_x, PHIORDER, phi ) + x[0] ;
    Xprime[1] = Interpolate( &ePhiList[klow], saved_y, PHIORDER, phi ) + x[1];
  } else {
    Polar2Cart(x[0],x[1],xx);
    xx[0] += Interpolate( &ePhiList[klow], saved_x, PHIORDER, phi ) ;
    xx[1] += Interpolate( &ePhiList[klow], saved_y, PHIORDER, phi ) ;
    Cart2Polar(xx,Xprime[0],Xprime[1]);
  }
  Xprime[2] = x[2] ;

}


/// 2D - faster - B field distortions (Table) - calculate the distortions due to the shape of the B field
/*! 
    Distortions are calculated and then stored in a table.  This calculation uses a 2D
    magnetic field which is phi symmetric.  The real 3D field has a slight twist that
    may be important for high precision work.  I recommend using this faster version (JT) 
    if you don't care about distortions smaller than 200 microns. 
    This method requires about 10 seconds of CPU time to generate the table but it is 
    very fast after the table has been created. Use it when you have a large number 
    of points ( > 10,000 ).
*/
void StMagUtilities::FastUndo2DBDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  static  Float_t dR[EMap_nR][EMap_nZ], dRPhi[EMap_nR][EMap_nZ] ;
  static  Int_t   ilow = 0, jlow = 0 ;
  const   Int_t   ORDER  = 1 ;                      // Linear interpolation = 1, Quadratic = 2         

  Int_t   i, j ;
  Float_t xx[3] ;
  Float_t save_dR[ORDER+1], saved_dR ;
  Float_t save_dRPhi[ORDER+1], saved_dRPhi ;

  Float_t r,phi;
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;            // Table uses phi from 0 to 2*Pi
  Float_t z = LimitZ( Sector, x ) ;                 // Protect against discontinuity at CM

  if ( DoOnce )
    {
      cout << "StMagUtilities::FastUndo2  Please wait for the tables to fill ...  ~5 seconds" << endl ;
      for ( i = 0 ; i < EMap_nR ; i++ )
	{
	  xx[0] = eRList[i] ;
	  xx[1] = 0 ;
	  for ( j = 0 ; j < EMap_nZ ; j++ )
	    {
	      xx[2] = eZList[j] ;
	      Undo2DBDistortion(xx,Xprime) ; // uses Cartesian coords
	      dR[i][j] = Xprime[0] ;
	      dRPhi[i][j] = Xprime[1] ;
	    }
	}
    }

  Search( EMap_nR, eRList, r, ilow ) ;
  Search( EMap_nZ, eZList, z, jlow ) ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( ilow + ORDER  >=  EMap_nR-1 ) ilow =  EMap_nR - 1 - ORDER ;
  if ( jlow + ORDER  >=  EMap_nZ-1 ) jlow =  EMap_nZ - 1 - ORDER ;
  
  for ( i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
	  save_dR[i-ilow]    = Interpolate( &eZList[jlow], &dR[i][jlow], ORDER, z )   ;
	  save_dRPhi[i-ilow] = Interpolate( &eZList[jlow], &dRPhi[i][jlow], ORDER, z )   ;
    }

  saved_dR    = Interpolate( &eRList[ilow], save_dR,    ORDER, r )   ; 
  saved_dRPhi = Interpolate( &eRList[ilow], save_dRPhi, ORDER, r )   ; 


  if ( r > 0.0 ) 
    {
      r   =  saved_dR ;  // Note that we calculate these quantities as if on the X axis, so phi == 0 while calculating.  
      phi =  phi + saved_dRPhi / r ;      
      if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
    }

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;
  
}


//________________________________________


/// Twist distortion
/*!
    Remove the effects of a simple "twist" of the TPC in the magnet.  If there is
    an angle between the E and B fields, there will be a distortion in the recorded
    tracks.  This routine takes out that distortion.
 */
void StMagUtilities::UndoTwistDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  Double_t        Zdrift ;
  Int_t           sign ;

  // Work in TPC coordinates but note that XTWIST and YTWIST reported in Magnet coord system 
  // so they have been negated (below)  
  
  Float_t z = LimitZ( Sector, x ) ;                 // Protect against discontinuity at CM
  sign = SectorSide( Sector, x ) ;                  // 1 = TPC West, -1 = TPC East

  Zdrift = sign * ( TPC_Z0 - TMath::Abs(z) ) ;
  if (usingCartesian) {
    Xprime[0] = x[0] - (     Const_1 * YTWIST - Const_2 * XTWIST ) * Zdrift/1000 ;
    Xprime[1] = x[1] - ( -1* Const_1 * XTWIST - Const_2 * YTWIST ) * Zdrift/1000 ;
  } else {
    Float_t xx[2];
    Polar2Cart(x[0],x[1],xx);
    xx[0] -= (     Const_1 * YTWIST - Const_2 * XTWIST ) * Zdrift/1000 ;
    xx[1] -= ( -1* Const_1 * XTWIST - Const_2 * YTWIST ) * Zdrift/1000 ;
    Cart2Polar(xx,Xprime[0],Xprime[1]);
  }
  Xprime[2] = x[2] ;                                   // Subtract to undo the distortion 

}


//________________________________________


/// Pad row 13 distortion
/*!
    Remove the effect of the mechanical imperfections between the inner sectors
    and the outer sectors.  There is a gap between the sectors that allow E field
    lines to leak out of the anode and gated grid region.  HHWieman has modelled this
    effect and his solution is used to remove the distortions.
 */
void StMagUtilities::UndoPad13Distortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  const Int_t   ORDER    =  2           ;         // ORDER = 1 is linear, ORDER = 2 is quadratice interpolation (Leave at 2 for legacy reasons)
  const Int_t   NZDRIFT  =  19          ;         // Dimension of the vector to contain ZDriftArray
  const Int_t   NYARRAY  =  37          ;         // Dimension of the vector to contain the YArray
  const Int_t   TERMS    =  400         ;         // Number of terms in the sum
  const Float_t SCALE    =  0.192       ;         // Set the scale for the correction
  const Float_t BOX      =  200.0 - GAPRADIUS ;   // Size of the box in which to work
  const Float_t PI       =  TMath::Pi() ;

  // Note custom grids in R and Z
  // PadRow13 corrections highly focussed near pad row 13 with weak Z dependence.  Radial points on YARRAY
  // lie over the pads for the first few pad rows on either side of the gap.

  static Float_t ZDriftArray[NZDRIFT] = {0,1,2,3,4,5,7.5,10,12.5,15,17.5,20,25,30,50,75,100,210,220} ;

  static Float_t YArray[NYARRAY] = { 50.0, 75.0,  100.0,
				     103.5, 104.0, 104.5, 
				     108.7, 109.2, 109.7,
				     113.9, 114.4, 114.9,
				     118.9, 119.6, 119.8, 
				     120.0, 120.25, 120.5, 120.75, 
				     121.0, 121.5, 122.1, 122.6, 
				     124.2, 125.2, 
				     126.2, 127.195, 
				     128.2, 129.195,
				     130.2, 131.195,
				     132.2, 133.195, 
				     137.195, 150., 
				     198., 200. } ;  

  static Double_t C[TERMS] ;                     // Coefficients for series
  static Float_t  SumArray[NZDRIFT][NYARRAY] ;
  static Int_t    ilow = 0, jlow = 0 ;
  
  Float_t  y, z, Zdrift, save_sum[3] ;
  Double_t r, phi, phi0, sum = 0.0 ;

  if ( DoOnce ) 
    {                          // Put these coefficients in a table to save time
      cout << "StMagUtilities::PadRow13   Please wait for the tables to fill ...  ~5 seconds" << endl ;
      C[0] = WIREGAP * GG * SCALE / ( 2 * BOX ) ;   
      for ( Int_t i = 1 ; i < TERMS ; i++ )
	  C[i] = 2 * GG * SCALE * TMath::Sin( WIREGAP*i*PI/( 2*BOX ) ) / ( i * PI ) ;
      for ( Int_t i = 0; i < NZDRIFT ; i++ )
	{
	  Zdrift = ZDriftArray[i] ;
	  for ( Int_t j = 0; j < NYARRAY ; j++ )
	    {
	      sum = 0.0 ;
	      y = YArray[j] ;
	      for ( Int_t k = 1 ; k < TERMS ; k++ )
		{
		  sum += ( C[k] / StarMagE ) * ( 1. - TMath::Exp(-1*k*PI*Zdrift/BOX) )
		         * TMath::Sin(k*PI*(y-GAPRADIUS)/BOX) ;
		}
	      SumArray[i][j] = sum ;
	    }
	}
    }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }               // Phi ranges from pi to -pi
  phi0   =  ( (Int_t)((TMath::Abs(phi)+PiOver12)/PiOver6 + 6.0 ) - 6.0 ) * PiOver6 ;
  if ( phi < 0 ) phi0 *= -1. ;
  y      =  r * TMath::Cos( phi0 - phi ) ;
  z = LimitZ( Sector, x ) ;                         // Protect against discontinuity at CM
  Zdrift =  TPC_Z0 - TMath::Abs(z) ;

  Search ( NZDRIFT, ZDriftArray,  Zdrift, ilow ) ;
  Search ( NYARRAY, YArray, y, jlow ) ;

  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( ilow + ORDER  >=    NZDRIFT - 1 ) ilow =   NZDRIFT - 1 - ORDER ;
  if ( jlow + ORDER  >=    NYARRAY - 1 ) jlow =   NYARRAY - 1 - ORDER ;

  for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
      save_sum[i-ilow]   = Interpolate( &YArray[jlow], &SumArray[i][jlow], ORDER, y )   ;
    }

  sum  = Interpolate( &ZDriftArray[ilow], save_sum, ORDER, Zdrift )   ; 

  if ( r > 0.0 )
    {
      phi =  phi - ( Const_1*(-1*sum)*TMath::Cos(phi0-phi) + Const_0*sum*TMath::Sin(phi0-phi) ) / r ;      
      r   =  r   - ( Const_0*sum*TMath::Cos(phi0-phi) - Const_1*(-1*sum)*TMath::Sin(phi0-phi) ) ;  
    }                                               // Subtract to Undo the distortions

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;
  
}


//__________________________________________


/// PadRow 40 and/or PadRow 13 distortion correction
/*! 
    There is a gap between the inner and outer sectors that allow E field lines to 
    leak out of the anode and gated grid region.  This routine removes the effect of 
    this (static) mechanical imperfection between the inner and the outer sectors. 

    The distortions are modelled within a "box" around the gap between the sectors.
    By definition, the distortions are zero outside the box ... but the distortion
    can still be significant if a secondary electron drifts through the box on its
    way to the padplane.

    The (circa)1995 TPC had 13 pad rows on the inner sector.  The iTPC has 40 rows on 
    the inner sector.   This routine will calculate the distortion for either the old or
    new iTPC sectors.  The iTPC has a GridLeak wall between the inner and outer sectors.
    The wall can prevent some or all ions from dynamically leaking out into the TPC.
    This routine calculate the STATIC distortion caused by the wall voltages (or lack 
    thereof).  The DYNAMIC distortions are treated elsewhere (SpaceCharge).

    The data were calculated on a grid with the following parameters.  The center of the gap is at point 5000.
    
    COLUMNS = 10001     // Radial direction in TPC
    ROWS    =  5401     // Z direction in TPC
    GPPMM   =    20     // Grid Points per millimeter

    Select_Maps      
  
    0   No Wall  Old TPC     (circa 1995)   Valid for all data taken prior to Run 19 (and part of 18)  
    1   100 Volt Difference  Outer wall. Difference between 500_113 and 400_113 volt setting.
    2   100 Volt Difference  Inner wall. Difference between 113_600 and 113_500 volt setting. 
    3   113 Volt 113 Volt    Minimum Static Distortion (but large dynamic Grid Leak)

    Note that only 1001 points (centered around 5000) are saved in the maps.  The remaining 9000 points are zero and so are suppressed.
    Sum these maps with the following weights:
    Total = output_113_113[] - ((Outer_GLW_Voltage+113.0)/100.0)*output_100_diff[] + ((Inner_GLW_Voltage+113.0)/100.0)*output_diff_100[]

    The new iTPC sectors require two voltages to be applied to the GridLeak Walls.  Typical values are Inner_GLW_Voltage = -113 volts 
    and Outer_GLW_Voltage = -700 volts. Note negative magnitude.  If the HV trips off, then both voltages = 0.0
    The old TPC sectors (year < 2019) require a different map without the wall in place. Inner_GLW_Voltage >= (positive) 100 selects 
    the old "no_wall" configuration.  Inner_GLW_Voltage <= 0.0 selects the iTPC configuration.  

    Note that Voltage >= (positive) 100 is the flag for the old sectors.
*/
void StMagUtilities::UndoPad40Distortion( const Float_t x[], Float_t Xprime[], Int_t Sector )
{

  const Int_t   ROWS           =   5401              ;   // Number of ROWS in the relaxation grid that created DataInTheGap. (must be ODD)
  const Int_t   COLUMNS        =  10001              ;   // Number of COLUMNS in the relaxation grid that created DataInTheGap. (must be ODD)
  const Int_t   SAVEDCOLUMNS   =   1001              ;   // Number of saved COLUMNS from DataInTheGap[].  The remainder are zeros. (must be ODD)
  const Int_t   GPPMM          =     20              ;   // Grid Points Per Millimeter for the grid that created DataInTheGap
  const Int_t   nMAPS          =      4              ;   // Number of Maps to read from GetGLWallData()
  const Int_t   nTERMS         =    800              ;   // Number of terms in the Fourier sum (unique to a set of calculated maps)
  const Int_t   nZDRIFT        =     20              ;   // Dimension of the vector that contains ZDriftArray
  const Int_t   nYARRAY        =     61              ;   // Dimension of the vector that contains YArray
  const Int_t   ORDER          =      2              ;   // ORDER = 1 linear, ORDER = 2 quadratic interpolation (Leave at 2 for legacy reasons)
  const Float_t BOX = (COLUMNS-1)/(GPPMM*10.0)       ;   // Width of the relaxation grid (in mm) that created DataInTheGap
  const Float_t PI             =  TMath::Pi()        ;

  // Note custom grids in R and Z to ensure that pads in both the old and new TPC achieve accurate results.
  // PadRow40 (PadRow13) corrections are strongly focussed near pad row 40 (13) with only a weak Z dependence.
  // Radial points in YARRAY lie over the pads for the first few pad rows on either side of the gap for both
  // the old and new TPC padplanes.

  static Int_t   ilow = 0, jlow = 0                  ;   // Remember location in interpolation table
  static Float_t Bn[nTERMS+1]                        ;   // Coefficients for series
  static Float_t SumArray[nMAPS][nZDRIFT][nYARRAY]   ;   // Array containing distortion integrals for interpolation
  static Float_t ZDriftArray[nZDRIFT] = {0,1,2,3,4,5,7.5,10,12.5,15,17.5,20,22.5,25,30,50,75,100,210,220} ;

  static Float_t YArray[nYARRAY] =  { 50.0,   75.0,  100.0,   102.0, 
				      103.5,  104.0,  104.5,   105.0,
				      105.8,  106.6,  107.4,   108.2, 
				      108.7,  109.2,  109.7,   110.15,
				      110.95, 111.75, 112.5,   113.3,
				      113.9,  114.4,  114.9,   115.75,
				      116.55, 117.35, 118.15,  118.9,
				      119.25, 119.6,  119.8,   120.0,
				      120.25, 120.5,  120.75,  121.0,
				      121.5,  122.1,  122.6,   124.2,
				      125.2,  126.2,  127.195, 128.2,
				      129.2,  130.2,  131.195, 132.2,
				      133.2,  134.2,  135.195, 136.2,
				      137.2,  139.2,  140.2,   142.2,
				      144.2,  146.2,  150.0,   198.,
				      200.0 } ;  

  Float_t r, phi, phi0, totalSum      ; 
  Float_t cosPhi0Phi, sinPhi0Phi      ;
  Float_t y, z, save_sum[nMAPS][3]    ;
  Float_t Zdrift, sum                 ;
  Float_t MapSum[nMAPS]               ;
  Float_t DataInTheGap[SAVEDCOLUMNS]  ;
  static  Bool_t DoOnceLocal = true ;

  if (fTpcVolts) {DoOnceLocal = UpdateTPCHighVoltages() ;}
  if ( DoOnceLocal ) 
    {                                                                
      cout << "StMagUtilities::PadRow40   Filling tables ..." << endl ;
      Int_t OFFSET = (COLUMNS-SAVEDCOLUMNS)/2 ;                                            // Explicitly plan for zero'd out data

      for ( Int_t MapID = 0 ; MapID < nMAPS ; MapID++ )                                    // Read maps and store locally
	{

	  GetGLWallData ( MapID, DataInTheGap ) ;
	  for ( Int_t n = 1 ; n <= nTERMS ; n++ )                                          // Calculate Bn[] coefficients 
	    {                                                                              // Integrate by Simpsons Rule 
	      Float_t COEFFICIENT = n*PI/(COLUMNS-1)                                   ;   // Reduce multiple calculations
	      sum = DataInTheGap[0] * TMath::Sin(COEFFICIENT*OFFSET)                   ;   // Addition of first point with wt=1
	      for ( Int_t i = 1 ; i < SAVEDCOLUMNS ; i+=2 ) sum += 4 * DataInTheGap[i] * TMath::Sin(COEFFICIENT*(i+OFFSET)) ;
	      for ( Int_t i = 2 ; i < SAVEDCOLUMNS ; i+=2 ) sum += 2 * DataInTheGap[i] * TMath::Sin(COEFFICIENT*(i+OFFSET)) ;
	      sum -= DataInTheGap[SAVEDCOLUMNS-1]*TMath::Sin(COEFFICIENT*(SAVEDCOLUMNS-1+OFFSET)) ; // Subtraction of last point 
	      sum *= 2.0/(3.0*(COLUMNS-1))                                             ;  
	      Bn[n] = sum ;
	    }
	  
	  for ( Int_t i = 0 ; i < nZDRIFT ; i++ )
	    {
	      z = ZDriftArray[i] ;
	      if ( z > (ROWS-1)/(10.0*GPPMM) ) z = (ROWS-1)/(10.0*GPPMM) ;
	      for ( Int_t j = 0; j < nYARRAY ; j++ )
		{
		  sum = 0.0 ;
		  y = YArray[j] ;
		  SumArray[MapID][i][j] = 0.0 ;
		  if ( y <= GAPRADIUS-BOX/2 || y >= GAPRADIUS+BOX/2 ) continue ;
		  for ( Int_t n = 1 ; n <= nTERMS ; n++ )
		    {
		      sum += ( Bn[n]/StarMagE ) * ( 1. - TMath::Exp((-1*n*PI*z)/BOX) ) * TMath::Cos((n*PI*(y-GAPRADIUS+BOX/2))/BOX) ; 
		    }
		  SumArray[MapID][i][j] = sum ;
		}
	    }  

	}
      DoOnceLocal = false ;
    }

  if (usingCartesian) Cart2Polar(x,r,phi) ;
  else { r = x[0] ; phi = x[1] ; }                      // Phi ranges from pi to -pi
  phi0   =  ( (Int_t)((TMath::Abs(phi)+PiOver12)/PiOver6 + 6.0 ) - 6.0 ) * PiOver6 ;
  if ( phi < 0 ) phi0 *= -1.              ;
  cosPhi0Phi = TMath::Cos( phi0 - phi )   ;
  sinPhi0Phi = TMath::Sin( phi0 - phi )   ;
  y      =  r * cosPhi0Phi                ;
  z = LimitZ( Sector, x )                 ;             // Protect against discontinuity at CM
  Zdrift =  TPC_Z0 - TMath::Abs(z)        ;

  Search ( nZDRIFT, ZDriftArray,  Zdrift, ilow ) ;
  Search ( nYARRAY, YArray, y, jlow )            ;

  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( ilow + ORDER  >=    nZDRIFT - 1 ) ilow =   nZDRIFT - 1 - ORDER ;
  if ( jlow + ORDER  >=    nYARRAY - 1 ) jlow =   nYARRAY - 1 - ORDER ;

  for ( Int_t MapID = 0 ; MapID < nMAPS ; MapID++ )                                    
    {
      for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
	{
	  save_sum[MapID][i-ilow]  = Interpolate( &YArray[jlow], &SumArray[MapID][i][jlow], ORDER, y ) ;
	}
      MapSum[MapID]  = Interpolate( &ZDriftArray[ilow], save_sum[MapID], ORDER, Zdrift ) ; 
    }

  if ( Inner_GLW_Voltage[Sector-1] >= 100.0 ) totalSum = MapSum[0] ;
  else totalSum = MapSum[3] - ((Outer_GLW_Voltage[Sector-1]+113.0)/100.0)*MapSum[1] + ((Inner_GLW_Voltage[Sector-1]+113.0)/100.0)*MapSum[2] ;
  totalSum = -totalSum; // arrays feeding MapSum were calculated for wrong sign voltage
  
  if ( r > 0.0 )
    {
      phi =  phi - ( Const_1*(-1*totalSum)*cosPhi0Phi + Const_0*totalSum*sinPhi0Phi ) / r  ;      
      r   =  r   - ( Const_0*totalSum*cosPhi0Phi - Const_1*(-1*totalSum)*sinPhi0Phi )      ;  
    }                                                                        // Subtract to Undo the distortions

  if (usingCartesian) Polar2Cart(r,phi,Xprime) ;
  else { Xprime[0] = r ; Xprime[1] = phi ; }
  Xprime[2] = x[2] ;
  
}

void StMagUtilities::GetGLWallData ( const Int_t select, Float_t DataInTheGap[] )
{

  //  The data were calculated on a grid with the following parameters.  The center of the gap is at point 5000.
  //
  //  COLUMNS = 10001     // Radial direction in TPC
  //  ROWS    =  5401     // Z direction in TPC
  //  GPPMM   =    20     // Grid Points per millimeter
  //
  //  The new iTPC sectors require two voltages to be applied to the GridLeak Walls.  Typical values are
  //  Inner_GLW_Voltage = -113 volts and Outer_Voltage = -700 volts.   If the HV trips off, then the voltage = 0 map is used.
  //  The old TPC sectors (year < 2019) require a different map without a wall. Inner_GLW_Voltage > 0 selects the old "no_wall"
  //  configuration.  Note that Inner_GLW_Voltage > 0 is the flag to use the old TPC sector map.
  //
  //  List of Maps        
  //
  //  0   No Wall  Old TPC     (circa 1995) Valid for all data taken prior to Run 19 (Run 18 had one iTPC sector with the new wall)  
  //  1   100 Volt Difference  Outer wall. Difference between 500_113 and 400_113 volt setting. Output_100_diff[]
  //  2   100 Volt Difference  Inner wall. Difference between 113_600 and 113_500 volt setting. Output_diff_100[]
  //  3   113 Volt 113 Volt    Minimum Static Distortion (but large dynamic Grid Leak)
  //
  //  Sum these maps with the following weights:
  //  Total = output_113_113[] - ((Outer_GLW_Voltage+113.0)/100.0)*output_100_diff[] + ((Inner_GLW_Voltage+113.0)/100.0)*output_diff_100[]
  //  Note that only 1001 points (centered around 5000) are saved in the maps.  The remaining 9000 points are zero and so are suppressed.
  
  

  Float_t output_nowall_oldTPC[1001] =
    { -0.0399262,  -0.0363446,  -0.0319311,   -0.026963,  -0.0217547,  -0.0166374,  -0.0119395, -0.00796491, -0.00497478, -0.00317029,
      -0.0026798, -0.00355035, -0.00574406, -0.00913993,  -0.0135409,  -0.0186854,  -0.0242637,  -0.0299365,  -0.0353564,  -0.0401892,
      -0.0441351,  -0.0469478,  -0.0484502,  -0.0485461,  -0.0472267,  -0.0445716,  -0.0407452,  -0.0359861,  -0.0305932,  -0.0249076,
      -0.0192911,  -0.0141041,  -0.0096827,  -0.0063177, -0.00423598, -0.00358583, -0.00442701, -0.00672641,  -0.0103594,  -0.0151168,
      -0.0207176,  -0.0268257,  -0.0330708,  -0.0390711,  -0.0444573,  -0.0488955,  -0.0521087,  -0.0538948,   -0.054139,  -0.0528226,
      -0.0500243,  -0.0459166,  -0.0407551,  -0.0348637,  -0.0286146,  -0.0224057,  -0.0166353,  -0.0116778, -0.00785986, -0.00543934,
      -0.00458888, -0.00538426, -0.00779888,  -0.0117046,  -0.0168789,  -0.0230179,   -0.029755,  -0.0366829,  -0.0433793,  -0.0494324,
      -0.0544676,  -0.0581708,  -0.0603086,  -0.0607437,  -0.0594445,  -0.0564874,  -0.0520536,  -0.0464185,  -0.0399351,  -0.0330126,
      -0.0260916,  -0.0196165,  -0.0140078, -0.00963553, -0.00679589, -0.00569172, -0.00641918, -0.00896067,  -0.0131849,  -0.0188543,
      -0.0256384,  -0.0331341,  -0.0408904,  -0.0484353,  -0.0553062,  -0.0610783,  -0.0653917,  -0.0679743,  -0.0686603,  -0.0674007,
      -0.0642684,  -0.0594539,  -0.0532552,  -0.0460599,  -0.0383218,  -0.0305332,  -0.0231947,  -0.0167834,  -0.0117234, -0.00835846,
      -0.0069298, -0.00755992,  -0.0102438,  -0.0148479,  -0.0211172,  -0.0286902,  -0.0371199,  -0.0459016,  -0.0545032,  -0.0623987,
      -0.0691008,  -0.0741922,   -0.077352,  -0.0783771,   -0.077196,  -0.0738746,  -0.0686137,  -0.0617378,  -0.0536762,  -0.0449369,
      -0.0360764,  -0.0276647,  -0.0202502,  -0.0143252,  -0.0102942,   -0.008448, -0.00894392,  -0.0117948,  -0.0168663,  -0.0238839,
      -0.0324485,  -0.0420595,  -0.0521457,  -0.0620996,  -0.0713153,  -0.0792263,  -0.0853413,   -0.089276,  -0.0907782,  -0.0897453,
      -0.0862323,  -0.0804503,  -0.0727554,  -0.0636277,  -0.0536432,  -0.0434389,  -0.0336736,  -0.0249868,  -0.0179592,  -0.0130748,
      -0.0106902,  -0.0110102,  -0.0140735,  -0.0197479,  -0.0277365,  -0.0375944,  -0.0487542,  -0.0605603,  -0.0723086,  -0.0832897,
      -0.0928335,   -0.100351,   -0.105374,   -0.107582,    -0.10683,   -0.103155,  -0.0967769,  -0.0880903,   -0.077639,  -0.0660858,
      -0.0541717,  -0.0426718,  -0.0323462,  -0.0238935,  -0.0179052,  -0.0148282,  -0.0149342,  -0.0183005,  -0.0248024,   -0.034118,
      -0.0457455,  -0.0590319,  -0.0732118,   -0.087453,   -0.100908,   -0.112767,   -0.122308,   -0.128944,   -0.132262,   -0.132051,
      -0.128318,   -0.121292,   -0.111413,  -0.0993066,  -0.0857491,  -0.0716216,  -0.0578565,  -0.0453796,   -0.035053,  -0.0276199,
      -0.0236565,   -0.023533,  -0.0273873,  -0.0351128,  -0.0463603,  -0.0605565,  -0.0769351,  -0.0945821,    -0.11249,   -0.129618,
      -0.14496,   -0.157603,   -0.166788,   -0.171963,   -0.172815,   -0.169302,   -0.161653,   -0.150362,   -0.136165,   -0.119994,
      -0.102924,  -0.0861121,  -0.0707229,  -0.0578581,  -0.0484864,  -0.0433804,  -0.0430651,  -0.0477797,  -0.0574575,  -0.0717239,
      -0.0899131,   -0.111104,   -0.134173,   -0.157857,   -0.180833,   -0.201798,   -0.219549,   -0.233061,   -0.241558,   -0.244559,
      -0.241926,    -0.23387,   -0.220952,   -0.204054,   -0.184333,   -0.163154,   -0.142012,   -0.122441,   -0.105921,  -0.0937841,
      -0.0871277,  -0.0867421,  -0.0930531,   -0.106087,   -0.125457,   -0.150382,   -0.179718,   -0.212025,   -0.245647,   -0.278808,
      -0.309722,     -0.3367,   -0.358262,   -0.373232,   -0.380818,   -0.380678,    -0.37295,    -0.35826,   -0.337697,   -0.312761,
      -0.285283,   -0.257322,   -0.231047,   -0.208605,   -0.191988,   -0.182909,   -0.182683,   -0.192137,   -0.211543,   -0.240579,
      -0.278333,   -0.323338,   -0.373643,   -0.426914,   -0.480568,   -0.531922,   -0.578351,   -0.617456,   -0.647223,   -0.666155,
      -0.673394,   -0.668795,   -0.652969,   -0.627279,   -0.593792,   -0.555182,   -0.514604,   -0.475519,   -0.441506,   -0.416053,
      -0.402343,    -0.40305,   -0.420159,   -0.454811,   -0.507198,     -0.5765,   -0.660889,   -0.757582,   -0.862956,   -0.972725,
      -1.08215,    -1.18631,    -1.28036,    -1.35986,    -1.42104,     -1.4611,    -1.47841,    -1.47276,    -1.44542,    -1.39926,
      -1.33871,    -1.26967,    -1.19934,    -1.13599,    -1.08863,    -1.06669,     -1.0796,    -1.13642,    -1.24539,    -1.41362,
      -1.64666,    -1.94827,     -2.3202,    -2.76204,    -3.27118,    -3.84283,    -4.47024,    -5.14482,    -5.85654,    -6.59422,
      -7.346,      -8.09977,     -8.8436,    -9.56622,    -10.2574,    -10.9083,    -11.5118,    -12.0628,     -12.558,    -12.9966,
      -13.3795,    -13.7095,    -13.9914,    -14.2311,    -14.4357,    -14.6129,    -14.7708,    -14.9172,    -15.0598,     -15.205,
      -15.3587,    -15.5251,    -15.7073,    -15.9066,    -16.1231,    -16.3554,    -16.6011,    -16.8566,    -17.1176,    -17.3795,
      -17.6373,    -17.8864,    -18.1224,    -18.3417,    -18.5414,    -18.7197,    -18.8758,    -19.0099,    -19.1233,    -19.2181,
      -19.2974,    -19.3646,    -19.4237,    -19.4785,    -19.5331,    -19.5909,     -19.655,    -19.7277,    -19.8107,    -19.9046,
      -20.0092,    -20.1237,    -20.2462,    -20.3744,    -20.5055,    -20.6364,    -20.7639,    -20.8848,    -20.9965,    -21.0966,
      -21.1834,    -21.2557,    -21.3133,    -21.3565,    -21.3864,    -21.4048,    -21.4139,    -21.4164,     -21.415,    -21.4125,
      -21.4118,    -21.4153,    -21.4249,    -21.4422,     -21.468,    -21.5025,    -21.5453,    -21.5953,     -21.651,    -21.7103,
      -21.7709,    -21.8303,    -21.8861,    -21.9359,    -21.9777,    -22.0098,    -22.0312,    -22.0413,    -22.0401,    -22.0284,
      -22.0072,    -21.9782,    -21.9434,    -21.9052,    -21.8658,    -21.8275,    -21.7926,    -21.7629,    -21.7397,    -21.7241,
      -21.7165,    -21.7167,     -21.724,    -21.7372,    -21.7548,    -21.7748,    -21.7951,    -21.8136,    -21.8281,    -21.8367,
      -21.8379,    -21.8305,    -21.8137,    -21.7873,    -21.7518,     -21.708,    -21.6571,    -21.6009,    -21.5413,    -21.4804,
      -21.4203,     -21.363,    -21.3105,    -21.2642,    -21.2251,    -21.1938,    -21.1706,    -21.1548,    -21.1457,    -21.1419,
      -21.1418,    -21.1434,    -21.1446,    -21.1435,    -21.1381,    -21.1267,    -21.1079,    -21.0807,    -21.0446,    -20.9997,
      -20.9462,    -20.8853,    -20.8182,    -20.7465,    -20.6722,    -20.5971,    -20.5233,    -20.4525,    -20.3865,    -20.3264,
      -20.2731,    -20.2272,    -20.1886,    -20.1568,    -20.1311,    -20.1101,    -20.0924,    -20.0762,    -20.0599,    -20.0417,
      -20.0201,    -19.9938,    -19.9617,    -19.9233,    -19.8783,    -19.8271,    -19.7703,    -19.7088,    -19.6441,    -19.5776,
      -19.5111,    -19.4462,    -19.3845,    -19.3273,    -19.2759,    -19.2309,    -19.1927,    -19.1613,     -19.136,    -19.1161,
      -19.1003,     -19.087,    -19.0746,    -19.0614,    -19.0455,    -19.0255,         -19,     -18.968,    -18.9288,    -18.8823,
      -18.8287,    -18.7688,    -18.7036,    -18.6345,    -18.5632,    -18.4915,    -18.4213,    -18.3541,    -18.2918,    -18.2354,
      -18.186,     -18.144,    -18.1094,    -18.0817,    -18.0601,    -18.0432,    -18.0294,    -18.0169,    -18.0038,    -17.9879,
      -17.9675,    -17.9411,    -17.9072,     -17.865,    -17.8143,     -17.755,    -17.6878,    -17.6137,    -17.5343,    -17.4513,
      -17.3669,    -17.2829,    -17.2016,    -17.1248,    -17.0541,    -16.9908,    -16.9356,    -16.8886,    -16.8497,    -16.8178,
      -16.7916,    -16.7693,    -16.7489,    -16.7279,     -16.704,    -16.6748,    -16.6383,    -16.5926,    -16.5366,    -16.4694,
      -16.3909,    -16.3015,    -16.2022,    -16.0948,    -15.9812,    -15.8638,    -15.7452,    -15.6281,     -15.515,    -15.4081,
      -15.3092,    -15.2196,    -15.1401,    -15.0704,    -15.0098,    -14.9568,    -14.9094,    -14.8649,    -14.8202,    -14.7721,
      -14.7173,    -14.6527,    -14.5755,    -14.4834,    -14.3746,    -14.2485,    -14.1049,    -13.9447,    -13.7696,     -13.582,
      -13.3851,    -13.1825,    -12.9781,    -12.7758,    -12.5793,    -12.3921,    -12.2168,    -12.0554,    -11.9086,    -11.7762,
      -11.6568,    -11.5477,    -11.4452,    -11.3446,    -11.2402,     -11.126,    -10.9953,    -10.8418,    -10.6592,     -10.442,
      -10.1853,    -9.88567,    -9.54092,    -9.15041,    -8.71517,    -8.23789,    -7.72289,    -7.17603,    -6.60446,    -6.01647,
      -5.42109,    -4.82787,    -4.24645,    -3.68621,    -3.15595,    -2.66358,    -2.21577,    -1.81777,    -1.47323,    -1.18411,
      -0.950602,   -0.771239,   -0.642961,    -0.56131,   -0.520656,   -0.514466,   -0.535612,   -0.576679,   -0.630282,   -0.689371,
      -0.747497,   -0.799048,   -0.839434,   -0.865214,   -0.874163,   -0.865283,    -0.83875,   -0.795814,   -0.738644,   -0.670148,
      -0.593756,   -0.513198,   -0.432273,   -0.354635,   -0.283593,   -0.221943,    -0.17184,   -0.134707,   -0.111193,   -0.101173,
      -0.103795,   -0.117558,   -0.140431,   -0.169992,   -0.203579,   -0.238455,   -0.271968,   -0.301693,   -0.325563,    -0.34197,
      -0.34984,   -0.348664,   -0.338509,   -0.319988,   -0.294198,   -0.262639,   -0.227107,    -0.18958,   -0.152089,   -0.116601,
      -0.0849043,  -0.0585064,  -0.0385575,  -0.0257941,  -0.0205093,  -0.0225507,   -0.031344,  -0.0459406,  -0.0650864,   -0.087305,
      -0.110993,   -0.134518,   -0.156316,   -0.174983,   -0.189353,    -0.19856,   -0.202077,   -0.199742,   -0.191754,    -0.17865,
      -0.161263,   -0.140664,   -0.118092,  -0.0948737,  -0.0723401,  -0.0517494,  -0.0342117,  -0.0206266,  -0.0116346, -0.00758542,
      -0.00852463,  -0.0141983,  -0.0240761,  -0.0373896,   -0.053185,  -0.0703843,  -0.0878536,   -0.104472,   -0.119201,    -0.13114,
      -0.139583,   -0.144052,   -0.144324,   -0.140436,   -0.132677,   -0.121565,   -0.107814,  -0.0922781,   -0.075906,  -0.0596757,
      -0.0445367,  -0.0313527,  -0.0208515,  -0.0135836, -0.00989249, -0.00989815,  -0.0134945,  -0.0203604,  -0.0299833,  -0.0416941,
      -0.0547112,  -0.0681894,  -0.0812731,  -0.0931478,   -0.103089,   -0.110504,   -0.114966,   -0.116236,   -0.114275,   -0.109241,
      -0.101476,  -0.0914837,  -0.0798936,  -0.0674217,   -0.054825,   -0.042855,  -0.0322117,  -0.0235022,  -0.0172056,  -0.0136455,
      -0.0129735,  -0.0151626,  -0.0200122,  -0.0271629,  -0.0361212,  -0.0462918,  -0.0570154,  -0.0676102,  -0.0774133,  -0.0858214,
      -0.0923259,   -0.096543,  -0.0982346,  -0.0973208,  -0.0938823,  -0.0881527,  -0.0805021,  -0.0714122,  -0.0614456,  -0.0512097,
      -0.0413194,  -0.0323593,  -0.0248484,  -0.0192095,  -0.0157447,  -0.0146188,  -0.0158514,  -0.0193175,  -0.0247573,  -0.0317941,
      -0.0399591,   -0.048721,  -0.0575196,  -0.0658003,   -0.073048,  -0.0788181,  -0.0827625,  -0.0846499,  -0.0843778,  -0.0819777,
      -0.077611,  -0.0715572,  -0.0641956,  -0.0559808,  -0.0474136,  -0.0390097,  -0.0312686,  -0.0246423,  -0.0195091,  -0.0161507,
      -0.0147367,  -0.0153153,  -0.0178121,  -0.0220358,  -0.0276921,  -0.0344022,  -0.0417275,   -0.049197,  -0.0563367,  -0.0626988,
      -0.0678883,  -0.0715871,  -0.0735725,  -0.0737295,  -0.0720566,  -0.0686649,  -0.0637696,  -0.0576758,  -0.0507586,  -0.0434396,
      -0.03616,  -0.0293538,  -0.0234211,  -0.0187039,  -0.0154656,  -0.0138762,  -0.0140023,  -0.0158046,  -0.0191413,  -0.0237779,
      -0.0294027,  -0.0356467,  -0.0421067,  -0.0483709,  -0.0540438,  -0.0587705,   -0.062258,  -0.0642926,  -0.0647525,  -0.0636136,
      -0.0609507,  -0.0569314,  -0.0518052,   -0.045887,  -0.0395375,  -0.0331401,  -0.0270776,  -0.0217081,  -0.0173439,  -0.0142322,
      -0.0125409,  -0.0123488,  -0.0136418,  -0.0163146,  -0.0201781,  -0.0249717,  -0.0303802,  -0.0360537,  -0.0416292,  -0.0467532,
      -0.0511037,   -0.054409,  -0.0564645,  -0.0571446,  -0.0564096,  -0.0543071,  -0.0509689,  -0.0466015,  -0.0414731,  -0.0358963,
      -0.0302087,  -0.0247517,  -0.0198494,  -0.0157891,  -0.0128035,  -0.0110573,  -0.0106373,   -0.011548,  -0.0137118,  -0.0169747,
      -0.0211162,  -0.0258637,  -0.0309095,  -0.0359305,  -0.0406075,  -0.0446455,  -0.0477914,  -0.0498492,  -0.0506919,  -0.0502681,
      -0.0486055,  -0.0458076,  -0.0420474,  -0.0375557,  -0.0326067,  -0.0275007,  -0.0225453,  -0.0180362,  -0.0142395,  -0.0113752,
      -0.00960402, -0.00901811, -0.00963575,  -0.0114009,   -0.014187,  -0.0178059,  -0.0220192,  -0.0265538,  -0.0311189,  -0.0354242,
      -0.0391975,  -0.0422017,   -0.044249,  -0.0452121,  -0.0450316,  -0.0437194,  -0.0413569,  -0.0380902,  -0.0341199,  -0.0296889,
      -0.0250665,  -0.0205323,  -0.0163581,  -0.0127917,  -0.0100418, -0.00826492, -0.00755652, -0.00794541, -0.00939243,  -0.0117934,
      -0.0149858,  -0.0187596,  -0.0228703,  -0.0270542,  -0.0310451,  -0.0345904,   -0.037467,  -0.0394947,  -0.0405469,  -0.0405584,
      -0.0395287,  -0.0375216,  -0.0346612,  -0.0311235,  -0.0271251,  -0.0229098,  -0.0187333,   -0.014847,  -0.0114832,  -0.0088399,
      -0.00706962, -0.00626981,  -0.0064773, -0.00766644, -0.00975103,  -0.0125901,  -0.0159969,  -0.0197507,   -0.023611,   -0.027332,
      -0.0306782 } ;

  Float_t output_100_diff[1001] =
    { 0.0194742,   0.0226635,   0.0255329,   0.0279047,   0.0296322,   0.0306094,   0.0307774,   0.0301287,   0.0287076,   0.0266079,
      0.0239667,   0.0209559,   0.0177718,   0.0146223,   0.0117138,  0.00923819,  0.00736053,  0.00620849,  0.00586409,  0.00635833,
      0.00766905,  0.00972214,   0.0123961,   0.0155295,   0.0189313,   0.0223927,   0.0257005,   0.0286504,   0.0310605,   0.0327824,
      0.0337111,   0.0337919,   0.0330238,   0.0314599,   0.0292042,   0.0264052,   0.0232465,   0.0199355,     0.01669,   0.0137244,
      0.0112363,  0.00939359,  0.00832371,  0.00810544,  0.00876369,   0.0102677,   0.0125327,   0.0154252,   0.0187715,   0.0223682,
      0.0259953,   0.0294301,   0.0324614,   0.0349031,   0.0366059,   0.0374674,   0.0374382,   0.0365259,   0.0347943,     0.03236,
      0.0293848,   0.0260659,   0.0226232,   0.0192853,   0.0162755,   0.0137967,   0.0120191,   0.0110687,   0.0110195,   0.0118882,
      0.0136331,   0.0161564,   0.0193104,   0.0229065,   0.0267274,   0.0305404,   0.0341126,   0.0372256,   0.0396897,   0.0413558,
      0.0421254,   0.0419571,   0.0408694,   0.0389397,   0.0362999,   0.0331279,   0.0296366,   0.0260602,   0.0226399,   0.0196076,
      0.0171719,   0.0155041,   0.0147272,   0.0149078,   0.0160518,   0.0181034,   0.0209486,   0.0244222,    0.028318,   0.0324023,
      0.0364281,   0.0401512,   0.0433461,     0.04582,   0.0474259,   0.0480718,    0.047727,   0.0464247,   0.0442595,   0.0413825,
      0.037991,   0.0343169,   0.0306111,   0.0271284,   0.0241106,    0.021771,   0.0202804,   0.0197566,   0.0202555,   0.0217682,
      0.0242204,   0.0274775,   0.0313525,   0.0356181,   0.0400206,   0.0442966,   0.0481899,   0.0514674,   0.0539351,   0.0554501,
      0.0559306,   0.0553613,   0.0537946,    0.051348,   0.0481966,   0.0445623,   0.0406998,   0.0368802,   0.0333737,   0.0304322,
      0.028273,   0.0270645,   0.0269149,   0.0278649,   0.0298851,   0.0328767,   0.0366782,   0.0410747,   0.0458122,   0.0506133,
      0.0551952,   0.0592878,   0.0626516,   0.0650928,   0.0664771,   0.0667377,   0.0658811,   0.0639866,   0.0612017,   0.0577335,
      0.0538351,   0.0497898,   0.0458932,   0.0424335,   0.0396734,   0.0378325,   0.0370727,   0.0374875,   0.0390953,   0.0418373,
      0.0455815,     0.05013,   0.0552316,   0.0605978,   0.0659214,    0.070896,   0.0752362,   0.0786965,   0.0810874,   0.0822884,
      0.0822565,   0.0810294,   0.0787243,   0.0755305,   0.0716978,   0.0675211,    0.063321,   0.0594237,   0.0561392,   0.0537413,
      0.0524494,   0.0524131,   0.0537023,   0.0563016,   0.0601105,   0.0649493,   0.0705697,   0.0766708,    0.082918,   0.0889646,
      0.094475,    0.099146,    0.102728,     0.10504,    0.105987,    0.105562,    0.103849,     0.10102,   0.0973253,   0.0930751,
      0.0886221,    0.084338,   0.0805894,    0.077714,   0.0759974,   0.0756539,   0.0768116,   0.0795024,    0.083659,   0.0891179,
      0.0956288,     0.10287,    0.110469,    0.118025,    0.125139,    0.131433,    0.136584,     0.14034,    0.142541,    0.143129,
      0.142156,     0.13978,    0.136258,     0.13193,    0.127198,    0.122499,    0.118282,    0.114969,    0.112937,    0.112485,
      0.113814,    0.117015,    0.122057,    0.128789,    0.136949,    0.146174,     0.15603,    0.166031,    0.175678,    0.184488,
      0.192026,    0.197939,    0.201976,    0.204014,    0.204063,     0.20227,    0.198916,    0.194397,    0.189201,    0.183878,
      0.179007,    0.175154,    0.172839,    0.172495,     0.17444,    0.178854,    0.185761,    0.195022,    0.206345,    0.219293,
      0.233317,    0.247785,    0.262021,    0.275354,    0.287156,    0.296894,    0.304162,    0.308715,    0.310491,    0.309623,
      0.306432,    0.301419,    0.295232,    0.288633,    0.282447,    0.277514,    0.274632,    0.274502,    0.277678,    0.284526,
      0.295193,    0.309586,    0.327372,    0.347989,    0.370676,    0.394512,    0.418475,    0.441504,    0.462566,     0.48073,
      0.495234,    0.505539,    0.511383,    0.512807,    0.510173,    0.504155,    0.495709,    0.486034,    0.476501,    0.468576,
      0.463731,    0.463352,    0.468643,    0.480538,    0.499632,    0.526115,    0.559745,    0.599836,     0.64527,    0.694553,
      0.745879,    0.797231,    0.846496,    0.891594,    0.930618,    0.961972,    0.984496,    0.997585,     1.00127,    0.996305,
      0.984152,    0.967007,    0.947739,    0.929802,    0.917114,    0.913906,    0.924547,     0.95335,     1.00438,     1.08124,
      1.18692,     1.32363,     1.49263,     1.69421,     1.92758,     2.19094,     2.48144,     2.79537,     3.12823,     3.47493,
      3.83,        4.1878,     4.54274,     4.88954,     5.22342,     5.54028,     5.83687,      6.1109,     6.36112,     6.58732,
      6.7903,      6.97181,     7.13439,     7.28127,     7.41612,     7.54291,     7.66564,     7.78817,     7.91402,     8.04619,
      8.18706,     8.33825,     8.50058,     8.67409,     8.85806,     9.05105,      9.2511,     9.45577,      9.6624,     9.86821,
      10.0705,     10.2668,     10.4551,     10.6338,     10.8019,     10.9591,     11.1058,     11.2427,     11.3713,     11.4936,
      11.6118,     11.7283,     11.8454,     11.9656,     12.0908,     12.2228,     12.3626,     12.5111,     12.6682,     12.8334,
      13.0059,     13.1839,     13.3659,     13.5496,     13.7329,     13.9136,     14.0898,     14.2598,     14.4222,     14.5763,
      14.7217,     14.8587,      14.988,     15.1109,     15.2288,     15.3437,     15.4577,     15.5729,     15.6911,     15.8143,
      15.9437,     16.0805,     16.2252,     16.3777,     16.5376,     16.7039,     16.8751,     17.0496,     17.2254,     17.4004,
      17.5726,     17.7402,     17.9016,     18.0556,     18.2015,     18.3389,     18.4683,     18.5902,     18.7061,     18.8175,
      18.9262,     19.0343,     19.1438,     19.2567,     19.3747,     19.4991,      19.631,     19.7707,     19.9181,     20.0728,
      20.2335,     20.3988,     20.5669,     20.7356,     20.9028,     21.0665,     21.2247,     21.3758,     21.5185,     21.6523,
      21.7768,     21.8924,          22,      22.101,     22.1972,     22.2905,     22.3832,     22.4775,     22.5755,      22.679,
      22.7893,     22.9076,     23.0341,     23.1686,     23.3105,     23.4584,     23.6105,     23.7647,     23.9186,     24.0698,
      24.2158,     24.3545,      24.484,      24.603,     24.7108,     24.8071,     24.8925,      24.968,     25.0352,     25.0963,
      25.1537,     25.2099,     25.2676,     25.3291,     25.3965,     25.4715,     25.5552,     25.6477,     25.7489,     25.8576,
      25.9721,       26.09,     26.2088,     26.3253,     26.4363,     26.5389,     26.6303,     26.7082,     26.7708,     26.8171,
      26.847,      26.861,     26.8606,     26.8479,     26.8257,     26.7969,     26.7651,     26.7334,     26.7051,     26.6829,
      26.6687,     26.6637,     26.6684,     26.6819,     26.7026,     26.7279,     26.7543,     26.7777,     26.7935,      26.797,
      26.7836,      26.749,     26.6896,     26.6027,     26.4865,     26.3405,     26.1654,     25.9632,      25.737,     25.4908,
      25.2293,     24.9577,     24.6813,     24.4048,     24.1326,     23.8679,     23.6124,     23.3666,     23.1287,     22.8955,
      22.6617,     22.4203,      22.163,       21.88,     21.5611,     21.1958,     20.7738,     20.2859,      19.724,     19.0824,
      18.3574,     17.5485,      16.658,     15.6916,     14.6582,     13.5698,     12.4411,     11.2892,     10.1332,     8.99309,
      7.88962,     6.84309,     5.87276,     4.99609,     4.22806,     3.58054,     3.06181,     2.67627,     2.42417,     2.30165,
      2.30085,     2.41025,     2.61505,     2.89783,     3.23917,     3.61844,     4.01456,     4.40684,     4.77568,     5.10333,
      5.3745,     5.57682,     5.70123,     5.74218,     5.69772,     5.56935,     5.36185,     5.08291,     4.74262,     4.35297,
      3.92722,     3.47929,     3.02313,     2.57212,     2.13855,     1.73318,     1.36484,     1.04025,    0.763831,    0.537724,
      0.361867,     0.23419,    0.150889,    0.106761,   0.0955836,    0.110522,    0.144525,    0.190704,    0.242677,    0.294848,
      0.342624,    0.382561,    0.412424,    0.431185,    0.438934,    0.436745,    0.426488,      0.4106,    0.391852,    0.373098,
      0.357048,    0.346059,    0.341968,    0.345968,    0.358534,    0.379407,    0.407624,    0.441596,    0.479235,    0.518097,
      0.555558,    0.588994,    0.615952,     0.63432,    0.642455,    0.639294,    0.624419,    0.598079,    0.561177,    0.515207,
      0.462166,    0.404426,    0.344597,    0.285364,    0.229338,    0.178898,    0.136065,     0.10239,   0.0788746,   0.0659282,
      0.0633603,    0.070408,    0.085797,    0.107832,    0.134511,    0.163648,    0.193018,    0.220482,    0.244117,    0.262323,
      0.273909,    0.278153,    0.274828,    0.264201,    0.247001,    0.224357,    0.197714,    0.168738,      0.1392,    0.110863,
      0.0853726,   0.0641537,   0.0483293,    0.038655,   0.0354816,   0.0387418,   0.0479645,   0.0623139,   0.0806502,    0.101608,
      0.123687,    0.145351,    0.165123,    0.181677,    0.193922,     0.20106,    0.202636,    0.198557,    0.189088,    0.174836,
      0.156695,    0.135794,    0.113415,   0.0909116,   0.0696226,   0.0507877,   0.0354711,   0.0244977,   0.0184055,   0.0174166,
      0.0214286,   0.0300267,   0.0425149,   0.0579648,   0.0752775,   0.0932556,     0.11068,    0.126388,    0.139344,    0.148702,
      0.15386,    0.154489,    0.150551,    0.142299,    0.130255,    0.115172,   0.0979861,   0.0797511,    0.061574,   0.0445429,
      0.029659,   0.0177749,  0.00954321,  0.00537846,  0.00543411,  0.00959667,   0.0174965,   0.0285347,   0.0419235,   0.0567387,
      0.0719795,   0.0866324,   0.0997348,    0.110435,    0.118045,     0.12208,    0.122288,    0.118661,    0.111433,    0.101062,
      0.0882014,   0.0736502,   0.0583066,   0.0431076,   0.0289707,   0.0167357,  0.00711347, 0.000642535, -0.00234263, -0.00173019,
      0.00236465,  0.00960958,   0.0194785,    0.031287,    0.044237,   0.0574682,    0.070113,   0.0813509,   0.0904597,   0.0968595,
      0.100148,    0.100122,   0.0967917,   0.0903737,   0.0812775,   0.0700766,   0.0574713,   0.0442432,   0.0312048,   0.0191487,
      0.00879776, 0.000760401,  -0.0045064,  -0.0067242, -0.00580874, -0.00187325,  0.00478068,    0.013684,   0.0242313,   0.0357204,
      0.047397,    0.058503,   0.0683241,    0.076234,   0.0817328,   0.0844778,   0.0843024,    0.081226,   0.0754499,    0.067344,
      0.0574208,   0.0463027,   0.0346815,   0.0232744,   0.0127786,  0.00382772, -0.00304721, -0.00745043, -0.00914488, -0.00806568,
      -0.00432264,  0.00180825,  0.00990365,   0.0194208,   0.0297326,    0.040168,   0.0500541,   0.0587587,   0.0657299,   0.0705293,
      0.0728594,   0.0725803,   0.0697176,   0.0644593,   0.0571422,   0.0482301,    0.038283,   0.0279213,   0.0177867,  0.00850097,
      0.000627825, -0.00536248, -0.00912172,  -0.0104446,  -0.0092804, -0.00573501,-6.28809e-05,  0.00735011,    0.016012,   0.0253563,
      0.0347787,   0.0436746,   0.0514775,   0.0576943,   0.0619356,   0.0639392,   0.0635858,    0.060905,   0.0560728,   0.0493988,
      0.0413064,   0.0323047,   0.0229562,   0.0138409,  0.00552003, -0.00149942, -0.00679589,  -0.0100591,  -0.0111089, -0.00990526,
      -0.00654997, -0.00127922,  0.00555238,   0.0134948,   0.0220318,   0.0306133,   0.0386909,   0.0457517,   0.0513506,   0.0551376,
      0.0568791,   0.0564717,   0.0539479,   0.0494731,   0.0433339,   0.0359198,   0.0276975,   0.0191815,    0.010901,  0.00336685,
      -0.00296033, -0.00769898,  -0.0105699,   -0.011413,  -0.0101966, -0.00701865, -0.00209951,  0.00423263,   0.0115631,   0.0194173,
      0.0272912,   0.0346826,   0.0411234,   0.0462082,   0.0496194,    0.051147,   0.0507005,   0.0483141,   0.0441439,   0.0384569,
      0.0316137,   0.0240453,   0.0162255,  0.00864104,  0.00176056,  -0.0039943, -0.00827528,  -0.0108289,  -0.0115116,  -0.0102984,
      -0.00728351, -0.00267348,  0.00322611,   0.0100307,   0.0173012,   0.0245722,    0.031381,    0.037297,   0.0419483,   0.0450446,
      0.0463954,   0.0459208,   0.0436561,   0.0397491,   0.0344497,   0.0280939,    0.021082,   0.0138532,  0.00685788, 0.000528868,
      -0.0047452, -0.00864425,  -0.0109365,  -0.0114922,  -0.0102918, -0.00742571, -0.00308868,  0.00243339,  0.00878187,   0.0155483,
      0.0223003,    0.028609,   0.0340759,   0.0383574,   0.0411868,   0.0423897,   0.0418952,   0.0397393,   0.0360623,   0.0310991,
      0.0251644,   0.0186319,    0.011911,  0.00542069,-0.000436952, -0.00530164, -0.00887746,   -0.010951,  -0.0114045,  -0.0102224,
      -0.00749195, -0.00339729,  0.00179289,  0.00774252,   0.0140697,   0.0203707,    0.026246,   0.0313246,   0.0352877,   0.0378883,
      0.0389664,   0.0384582,   0.0364002,    0.032926,   0.0282574,   0.0226901,    0.016575,   0.0102952,  0.00424248, -0.00120778,
      -0.00571991, -0.00901886,  -0.0109072,   -0.011277,  -0.0101164, -0.00750965, -0.00363127,  0.00126513,  0.00686331,   0.0128046,
      0.0187105,   0.0242068,   0.0289469,   0.0326332,   0.0350361,   0.0360076,   0.0354904,   0.0335211,   0.0302271,   0.0258188,
      0.0205752 } ;

  Float_t output_diff_100[1001] =
    { -0.0366669,  -0.0258292,   -0.016614,  -0.0096185, -0.00530268, -0.00395997, -0.00569845,  -0.0104336,  -0.0178929,  -0.0276325,
      -0.0390646,  -0.0514936,  -0.0641597,  -0.0762858,  -0.0871267,  -0.0960156,   -0.102406,   -0.105907,   -0.106306,   -0.103586,
      -0.0979244,   -0.089684,  -0.0793898,  -0.0676974,   -0.055352,  -0.0431418,  -0.0318492,  -0.0222018,  -0.0148261,  -0.0102086,
      -0.00866466,  -0.0103183,  -0.0150937,  -0.0227198,   -0.032746,  -0.0445704,  -0.0574769,  -0.0706795,  -0.0833714,  -0.0947753,
      -0.104192,   -0.111046,   -0.114919,   -0.115579,   -0.112993,   -0.107334,  -0.0989644,  -0.0884205,  -0.0763749,  -0.0635967,
      -0.0509033,  -0.0391096,  -0.0289766,  -0.0211641,  -0.0161889,   -0.014392,  -0.0159168,  -0.0206993,  -0.0284718,  -0.0387788,
      -0.0510049,  -0.0644129,  -0.0781895,  -0.0914956,   -0.103519,   -0.113526,   -0.120906,   -0.125211,   -0.126184,   -0.123776,
      -0.118149,   -0.109668,  -0.0988764,  -0.0864653,  -0.0732289,  -0.0600161,  -0.0476773,  -0.0370116,   -0.028716,  -0.0233424,
      -0.0212618,  -0.0226412,  -0.0274319,  -0.0353724,  -0.0460035,  -0.0586965,  -0.0726922,  -0.0871474,   -0.101188,   -0.113961,
      -0.124694,   -0.132735,     -0.1376,   -0.139004,   -0.136872,   -0.131356,   -0.122816,   -0.111802,  -0.0990245,  -0.0853026,
      -0.0715197,   -0.058566,  -0.0472832,  -0.0384122,  -0.0325462,  -0.0300932,  -0.0312502,  -0.0359897,  -0.0440611,  -0.0550052,
      -0.0681823,  -0.0828112,  -0.0980178,   -0.112889,    -0.12653,   -0.138118,   -0.146958,   -0.152523,   -0.154488,   -0.152751,
      -0.147443,   -0.138917,   -0.127727,   -0.114598,   -0.100377,  -0.0859826,  -0.0723494,  -0.0603674,  -0.0508275,  -0.0443721,
      -0.0414542,  -0.0423087,  -0.0469367,  -0.0551046,  -0.0663581,  -0.0800491,  -0.0953759,   -0.111432,   -0.127262,   -0.141922,
      -0.154537,   -0.164356,     -0.1708,   -0.173498,   -0.172309,   -0.167337,    -0.15892,   -0.147612,   -0.134151,    -0.11941,
      -0.104348,   -0.0899484,  -0.0771561,  -0.0668223,  -0.0596491,   -0.056146,   -0.056597,  -0.0610427,  -0.0692763,  -0.0808555,
      -0.0951292,   -0.111277,   -0.128359,   -0.145374,   -0.161321,   -0.175262,   -0.186381,   -0.194031,   -0.197782,    -0.19744,
      -0.193068,   -0.184979,   -0.173717,   -0.160028,   -0.144809,   -0.129061,    -0.11382,   -0.100097,  -0.0888135,  -0.0807473,
      -0.0764799,  -0.0763617,   -0.080489,   -0.088697,   -0.100569,   -0.115459,   -0.132536,   -0.150826,   -0.169281,   -0.186836,
      -0.202477,   -0.215307,   -0.224596,   -0.229833,   -0.230753,   -0.227357,   -0.219917,   -0.208951,   -0.195204,   -0.179594,
      -0.163159,   -0.146997,   -0.132194,   -0.119762,    -0.11057,   -0.105296,   -0.104381,   -0.108003,   -0.116063,   -0.128189,
      -0.143759,   -0.161938,   -0.181728,   -0.202028,   -0.221701,   -0.239642,    -0.25485,   -0.266486,   -0.273927,   -0.276805,
      -0.275031,   -0.268801,    -0.25859,   -0.245116,   -0.229308,   -0.212241,   -0.195079,   -0.178998,   -0.165121,   -0.154444,
      -0.147776,   -0.145691,   -0.148488,   -0.156176,   -0.168466,   -0.184793,   -0.204342,   -0.226101,    -0.24892,    -0.27158,
      -0.292864,   -0.311634,   -0.326898,   -0.337874,   -0.344034,   -0.345143,   -0.341272,   -0.332796,   -0.320373,   -0.304912,
      -0.287511,     -0.2694,   -0.251866,   -0.236173,   -0.223492,   -0.214827,   -0.210952,   -0.212366,   -0.219262,   -0.231509,
      -0.24866,    -0.269977,   -0.294471,   -0.320959,   -0.348132,   -0.374635,   -0.399143,    -0.42044,   -0.437497,   -0.449528,
      -0.456044,   -0.456881,   -0.452216,   -0.442559,   -0.428727,   -0.411799,   -0.393058,   -0.373914,    -0.35583,   -0.340234,
      -0.328436,    -0.32155,   -0.320433,   -0.325625,    -0.33732,   -0.355348,   -0.379181,   -0.407957,   -0.440529,   -0.475521,
      -0.51141,    -0.546603,   -0.579532,   -0.608742,   -0.632975,   -0.651245,   -0.662897,   -0.667653,   -0.665634,    -0.65736,
      -0.643734,      -0.626,   -0.605679,   -0.584499,   -0.564303,   -0.546954,   -0.534236,   -0.527756,   -0.528857,   -0.538539,
      -0.557397,   -0.585579,   -0.622767,   -0.668181,   -0.720604,   -0.778435,   -0.839755,    -0.90242,   -0.964162,     -1.0227,
      -1.07587,    -1.12171,    -1.15862,     -1.1854,    -1.20141,    -1.20658,    -1.20146,    -1.18726,    -1.16585,    -1.13967,
      -1.11175,    -1.08557,    -1.06499,     -1.0541,    -1.05711,     -1.0782,    -1.12137,    -1.19032,    -1.28827,    -1.41789,
      -1.58114,    -1.77925,    -2.01258,    -2.28066,    -2.58213,     -2.9148,     -3.2757,    -3.66113,    -4.06679,    -4.48791,
      -4.91936,    -5.35585,    -5.79208,    -6.22285,    -6.64332,    -7.04905,    -7.43624,    -7.80174,    -8.14321,    -8.45915,
      -8.74892,    -9.01278,    -9.25178,    -9.46778,    -9.66327,    -9.84132,    -10.0054,    -10.1593,    -10.3068,    -10.4516,
      -10.5975,    -10.7475,    -10.9044,    -11.0703,    -11.2468,    -11.4345,    -11.6336,    -11.8434,    -12.0626,    -12.2895,
      -12.5219,    -12.7571,    -12.9924,    -13.2252,    -13.4528,    -13.6728,    -13.8833,    -14.0829,    -14.2706,    -14.4461,
      -14.6098,    -14.7626,     -14.906,    -15.0418,    -15.1725,    -15.3005,    -15.4284,    -15.5588,    -15.6941,    -15.8364,
      -15.9872,    -16.1477,    -16.3185,    -16.4995,      -16.69,    -16.8888,    -17.0943,    -17.3043,    -17.5165,    -17.7286,
      -17.9379,    -18.1422,    -18.3395,    -18.5283,    -18.7073,    -18.8761,    -19.0347,    -19.1837,    -19.3245,    -19.4586,
      -19.5883,    -19.7159,     -19.844,     -19.975,    -20.1114,    -20.2552,    -20.4081,    -20.5713,    -20.7454,    -20.9303,
      -21.1253,    -21.3294,    -21.5407,    -21.7571,    -21.9762,    -22.1955,    -22.4123,    -22.6243,    -22.8294,    -23.0261,
      -23.2131,      -23.39,     -23.557,    -23.7148,    -23.8648,     -24.009,    -24.1496,    -24.2892,    -24.4306,    -24.5766,
      -24.7295,    -24.8917,    -25.0647,    -25.2498,    -25.4473,    -25.6572,    -25.8785,    -26.1098,     -26.349,    -26.5937,
      -26.8411,    -27.0885,     -27.333,    -27.5721,    -27.8037,     -28.026,    -28.2381,    -28.4397,    -28.6311,    -28.8136,
      -28.9888,    -29.1591,    -29.3273,    -29.4964,    -29.6696,    -29.8498,      -30.04,    -30.2425,    -30.4589,    -30.6904,
      -30.9372,     -31.199,    -31.4745,    -31.7618,    -32.0583,    -32.3611,     -32.667,    -32.9727,     -33.275,    -33.5712,
      -33.859,     -34.1365,    -34.4031,    -34.6586,    -34.9038,    -35.1404,    -35.3708,    -35.5981,    -35.8258,    -36.0576,
      -36.2974,    -36.5488,    -36.8152,    -37.0993,     -37.403,    -37.7275,     -38.073,    -38.4386,    -38.8227,    -39.2227,
      -39.6353,     -40.057,    -40.4835,     -40.911,    -41.3355,    -41.7537,    -42.1629,    -42.5612,    -42.9478,     -43.323,
      -43.6882,    -44.0459,    -44.3995,    -44.7534,    -45.1126,    -45.4822,    -45.8678,    -46.2745,     -46.707,    -47.1693,
      -47.6643,    -48.1938,    -48.7584,    -49.3572,    -49.9881,    -50.6478,    -51.3319,    -52.0352,    -52.7521,    -53.4768,
      -54.2036,    -54.9274,    -55.6442,    -56.3509,    -57.0462,    -57.7303,    -58.4053,    -59.0753,    -59.7461,    -60.4252,
      -61.1216,    -61.8452,     -62.607,    -63.4179,    -64.2888,    -65.2298,    -66.2496,    -67.3555,    -68.5522,    -69.8422,
      -71.2249,    -72.6966,    -74.2507,    -75.8771,    -77.5628,    -79.2922,    -81.0469,    -82.8069,    -84.5504,     -86.255,
      -87.8979,    -89.4568,    -90.9107,    -92.2403,    -93.4286,    -94.4617,    -95.3293,    -96.0246,    -96.5453,     -96.893,
      -97.074,     -97.0983,    -96.9802,    -96.7375,    -96.3909,    -95.9638,    -95.4813,    -94.9695,    -94.4549,    -93.9632,
      -93.5189,    -93.1441,    -92.8581,    -92.6764,    -92.6103,    -92.6664,    -92.8465,    -93.1468,    -93.5588,    -94.0687,
      -94.6583,     -95.305,     -95.983,    -96.6637,    -97.3166,    -97.9107,     -98.415,    -98.7999,    -99.0381,    -99.1056,
      -98.9825,    -98.6539,      -98.11,    -97.3472,    -96.3676,    -95.1793,    -93.7961,    -92.2368,    -90.5247,    -88.6867,
      -86.7522,     -84.752,     -82.717,    -80.6773,    -78.6605,    -76.6913,    -74.7897,     -72.971,    -71.2445,    -69.6137,
      -68.0761,    -66.6229,    -65.2402,     -63.909,    -62.6065,    -61.3069,    -59.9831,    -58.6072,    -57.1528,    -55.5954,
      -53.9144,    -52.0937,    -50.1227,    -47.9971,    -45.7193,    -43.2983,    -40.7499,    -38.0957,    -35.3632,    -32.5841,
      -29.7935,    -27.0286,    -24.3272,    -21.7264,    -19.2611,    -16.9628,    -14.8584,    -12.9693,    -11.3105,    -9.89043,
      -8.7105,     -7.76534,     -7.0432,    -6.52648,    -6.19271,    -6.01548,     -5.9657,    -6.01276,    -6.12587,    -6.27521,
      -6.4331,       -6.575,    -6.68028,    -6.73284,    -6.72148,    -6.64001,    -6.48715,    -6.26619,    -5.98445,     -5.6526,
      -5.28383,    -4.89298,    -4.49556,     -4.1069,    -3.74129,    -3.41119,    -3.12671,    -2.89507,    -2.72042,    -2.60368,
      -2.54267,    -2.53239,    -2.56539,    -2.63231,    -2.72249,    -2.82462,     -2.9274,    -3.02018,    -3.09355,    -3.13978,
      -3.15323,    -3.13054,    -3.07077,     -2.9753,    -2.84771,    -2.69345,    -2.51943,    -2.33361,    -2.14442,    -1.96032,
      -1.78924,    -1.63816,     -1.5127,    -1.41688,    -1.35287,    -1.32096,    -1.31959,    -1.34551,    -1.39399,    -1.45918,
      -1.53446,    -1.61289,    -1.68759,    -1.75218,    -1.80112,    -1.83003,    -1.83591,    -1.81729,    -1.77425,     -1.7084,
      -1.62273,    -1.52139,     -1.4094,    -1.29233,    -1.17599,    -1.06599,    -0.96748,   -0.884812,   -0.821306,   -0.779075,
      -0.758919,   -0.760313,   -0.781463,   -0.819449,    -0.87042,   -0.929852,   -0.992835,    -1.05438,    -1.10971,    -1.15458,
      -1.18547,    -1.19983,    -1.19619,    -1.17423,    -1.13477,    -1.07972,    -1.01191,   -0.934906,   -0.852802,   -0.769928,
      -0.690599,   -0.618846,   -0.558175,   -0.511364,   -0.480303,   -0.465901,   -0.468043,   -0.485616,   -0.516596,   -0.558189,
      -0.607011,   -0.659312,   -0.711203,   -0.758904,    -0.79897,   -0.828495,   -0.845283,   -0.847969,   -0.836087,   -0.810089,
      -0.771295,   -0.721801,   -0.664338,   -0.602092,   -0.538502,   -0.477045,   -0.421021,    -0.37335,   -0.336404,   -0.311866,
      -0.300634,   -0.302783,   -0.317571,   -0.343493,   -0.378389,   -0.419587,   -0.464069,   -0.508668,   -0.550262,   -0.585967,
      -0.61331,   -0.630374,   -0.635911,    -0.62941,   -0.611114,   -0.581998,   -0.543697,   -0.498398,   -0.448695,   -0.397424,
      -0.347486,   -0.301666,   -0.262463,   -0.231937,   -0.211591,   -0.202284,   -0.204184,   -0.216768,   -0.238865,   -0.268733,
      -0.304176,   -0.342684,   -0.381593,   -0.418251,   -0.450182,   -0.475236,    -0.49172,   -0.498496,   -0.495042,   -0.481482,
      -0.458568,   -0.427624,   -0.390461,   -0.349256,   -0.306417,   -0.264424,   -0.225684,   -0.192374,   -0.166315,   -0.148858,
      -0.14081,    -0.142389,   -0.153213,   -0.172336,    -0.19831,   -0.229279,     -0.2631,   -0.297481,    -0.33012,    -0.35885,
      -0.381774,   -0.397376,   -0.404612,   -0.402975,    -0.39251,   -0.373816,   -0.347997,   -0.316592,    -0.28147,   -0.244716,
      -0.208495,   -0.174921,   -0.145923,   -0.123128,   -0.107764,   -0.100584,   -0.101831,    -0.11122,   -0.127967,   -0.150839,
      -0.178235,    -0.20829,   -0.238996,   -0.268322,   -0.294346,   -0.315369,   -0.330022,   -0.337347,   -0.336849,    -0.32853,
      -0.312876,   -0.290829,   -0.263718,   -0.233179,   -0.201045,   -0.169236,   -0.139632,   -0.113963,  -0.0936982,  -0.0799579,
      -0.0734487,  -0.0744216,  -0.0826608,  -0.0975011,   -0.117873,   -0.142374,   -0.169357,   -0.197035,   -0.223596,   -0.247312,
      -0.266649,    -0.28036,   -0.287559,   -0.287774,   -0.280974,   -0.267564,    -0.24836,   -0.224532,   -0.197529,   -0.168988,
      -0.140626,    -0.11414,  -0.0910929,  -0.0728244,  -0.0603648,  -0.0543754,  -0.0551096,  -0.0624006,  -0.0756752,  -0.0939923,
      -0.116105,    -0.14054,   -0.165692,   -0.189924,    -0.21167,   -0.229531,   -0.242363,   -0.249344,   -0.250023,   -0.244347,
      -0.23266,    -0.21568,   -0.194446,    -0.17026,   -0.144595,   -0.119007,  -0.0950368,  -0.0741122,  -0.0574613,  -0.0460371,
      -0.0404601,   -0.040982,  -0.0474728,  -0.0594317,  -0.0760208,  -0.0961202,     -0.1184,   -0.141403,   -0.163639,   -0.183677,
      -0.200234,   -0.212253,   -0.218969,   -0.219952,   -0.215136,   -0.204816,   -0.189629,   -0.170512,   -0.148642,   -0.125358,
      -0.10208,   -0.0802181,  -0.0610843,  -0.0458119,  -0.0352857,  -0.0300887,  -0.0304677,  -0.0363199,  -0.0472022,  -0.0623604,
      -0.0807791,   -0.101246,   -0.122431,   -0.142965,   -0.161533,   -0.176949,   -0.188232,   -0.194667,   -0.195847,   -0.191694,
      -0.182467,   -0.168737,   -0.151355,   -0.131395,   -0.110084,  -0.0887271,  -0.0686232,  -0.0509856,   -0.036866,  -0.0270899,
      -0.0222065,  -0.0224563,   -0.027758,  -0.0377154,  -0.0516442,  -0.0686165,  -0.0875206,   -0.107131,   -0.126186,   -0.143466,
      -0.157872 } ;

  Float_t output_113_113[1001] =
    { -0.0252237,  -0.0236161,   -0.021104,  -0.0178422,  -0.0140333, -0.00991613, -0.00575021, -0.00179993,  0.00168219,  0.00447149,
      0.00638547,   0.0072956,  0.00713573,  0.00590671,  0.00367676, 0.000577596, -0.00320339, -0.00743462,  -0.0118546,   -0.016188,
      -0.0201635,  -0.0235301,  -0.0260735,  -0.0276301,  -0.0280971,  -0.0274397,  -0.0256936,  -0.0229637,  -0.0194171,   -0.015274,
      -0.0107934, -0.00625743, -0.00195376,  0.00184263,  0.00488678,  0.00697941,  0.00797976,  0.00781493,  0.00648491,  0.00406311,
      0.000692253, -0.00342408, -0.00803389,  -0.0128522,   -0.017579,  -0.0219175,  -0.0255936,  -0.0283726,  -0.0300749,   -0.030587,
      -0.02987,  -0.0279617,  -0.0249756,  -0.0210938,   -0.016556,  -0.0116453, -0.00667038, -0.00194609,  0.00222596,  0.00557674,
      0.00788691,  0.00900103,  0.00883786,  0.00739601,  0.00475468,  0.00106925, -0.00343786,  -0.0084907,  -0.0137768,  -0.0189664,
      -0.0237333,  -0.0277753,  -0.0308334,  -0.0327086,  -0.0332748,  -0.0324871,  -0.0303859,  -0.0270943,  -0.0228117,  -0.0178012,
      -0.0123742, -0.00687059, -0.00163808,  0.00298998,  0.00671557,  0.00929508,   0.0105551,   0.0104039,  0.00883804,  0.00594309,
      0.00188918, -0.00307905, -0.00865725,     -0.0145,  -0.0202421,  -0.0255217,  -0.0300027,  -0.0333965,  -0.0354803,   -0.036112,
      -0.0352395,  -0.0329051,  -0.0292435,  -0.0244742,  -0.0188883,   -0.012831, -0.00668025,-0.000823154,  0.00436811,  0.00856002,
      0.011479,    0.0129293,   0.0128058,   0.0111017,  0.00790989,  0.00341769, -0.00210369, -0.00831562,  -0.0148327,  -0.0212466,
      -0.0271516,  -0.0321698,   -0.035976,  -0.0383176,   -0.039032,  -0.0380577,  -0.0354384,  -0.0313222,  -0.0259527,  -0.0196549,
      -0.0128153, -0.00585844, 0.000779598,  0.00667855,   0.0114605,   0.0148142,   0.0165152,    0.016441,   0.0145797,    0.011032,
      0.00600597, -0.00019502, -0.00719025,  -0.0145449,  -0.0217968,  -0.0284853,  -0.0341802,  -0.0385092,  -0.0411819,  -0.0420092,
      -0.0409158,  -0.0379467,  -0.0332654,   -0.027145,  -0.0199522,  -0.0121251, -0.00414674,  0.00348496,   0.0102884,   0.0158289,
      0.0197463,   0.0217786,   0.0217797,   0.0197297,   0.0157381,   0.0100376,  0.00297157,  -0.0050264,  -0.0134589,  -0.0217951,
      -0.0295037,  -0.0360866,  -0.0411103,  -0.0442343,  -0.0452334,  -0.0440133,  -0.0406185,  -0.0352308,  -0.0281597,  -0.0198246,
      -0.0107299, -0.00143405,  0.00748456,    0.015464,   0.0219941,   0.0266494,    0.029117,   0.0292176,   0.0269186,   0.0223381,
      0.0157394,  0.00751661, -0.00182869,  -0.0117173,   -0.021528,  -0.0306357,  -0.0384511,  -0.0444579,  -0.0482465,  -0.0495413,
      -0.0482198,  -0.0443225,  -0.0380529,  -0.0297668,  -0.0199523, -0.00920106,  0.00182733,   0.0124459,    0.021983,   0.0298246,
      0.0354541,   0.0384858,   0.0386915,   0.0360172,   0.0305888,   0.0227077,   0.0128338,  0.00155976,  -0.0104245,  -0.0223736,
      -0.0335324,  -0.0431836,  -0.0506931,  -0.0555517,  -0.0574087,   -0.056097,  -0.0516465,  -0.0442855,  -0.0344301,  -0.0226614,
      -0.00969122,  0.00367928,    0.016609,   0.0282687,   0.0378932,   0.0448306,   0.0485852,   0.0488515,   0.0455369,    0.038771,
      0.0289025,   0.0164806,   0.0022248,   -0.013017,  -0.0283195,   -0.042736,  -0.0553582,   -0.065373,  -0.0721164,  -0.0751174,
      -0.0741324,  -0.0691642,  -0.0604682,  -0.0485408,  -0.0340943,  -0.0180165, -0.00131922,   0.0149222,   0.0296351,    0.041816,
      0.0505956,   0.0552956,   0.0554756,   0.0509666,    0.041887,   0.0286425,   0.0119078, -0.00740993,  -0.0282215,  -0.0493195,
      -0.0694512,  -0.0873951,   -0.102039,   -0.112451,   -0.117946,   -0.118129,   -0.112935,   -0.102637,  -0.0878392,  -0.0694524,
      -0.0486418,   -0.026764,  -0.0052882,   0.0142918,   0.0305497,   0.0422169,   0.0482661,   0.0479823,   0.0410166,   0.0274194,
      0.00764954,  -0.0174407,  -0.0466459,  -0.0784708,   -0.111213,   -0.143062,   -0.172207,   -0.196949,   -0.215814,   -0.227646,
      -0.231699,   -0.227692,   -0.215851,   -0.196912,   -0.172101,   -0.143076,   -0.111849,  -0.0806766,  -0.0519344,  -0.0279793,
      -0.0110062, -0.00290916, -0.00515341,  -0.0186681,  -0.0437655,  -0.0800935,   -0.126625,   -0.181686,   -0.243025,   -0.307916,
      -0.373302,   -0.435956,   -0.492669,   -0.540441,   -0.576682,   -0.599388,   -0.607307,   -0.600073,   -0.578292,   -0.543596,
      -0.498636,    -0.44703,   -0.393258,   -0.342507,    -0.30048,   -0.273167,   -0.266594,   -0.286556,   -0.338361,   -0.426573,
      -0.55479,   -0.725455,   -0.939712,    -1.19731,     -1.4966,    -1.83451,    -2.20668,    -2.60761,    -3.03082,    -3.46912,
      -3.91487,    -4.36025,     -4.7976,    -5.21966,    -5.61984,    -5.99248,    -6.33304,     -6.6382,    -6.90597,    -7.13574,
      -7.32817,    -7.48519,    -7.60977,    -7.70581,    -7.77785,    -7.83088,    -7.87008,    -7.90054,    -7.92705,    -7.95392,
      -7.98474,    -8.02231,    -8.06851,    -8.12429,    -8.18968,    -8.26385,    -8.34521,    -8.43156,    -8.52022,    -8.60825,
      -8.69263,    -8.77041,    -8.83893,    -8.89591,    -8.93963,    -8.96897,    -8.98345,    -8.98327,    -8.96926,     -8.9428,
      -8.90574,    -8.86029,    -8.80884,    -8.75387,    -8.69778,    -8.64274,    -8.59062,    -8.54285,    -8.50038,    -8.46363,
      -8.43249,    -8.40636,    -8.38417,     -8.3645,    -8.34566,    -8.32577,    -8.30295,    -8.27538,    -8.24143,    -8.19973,
      -8.1493,    -8.08954,    -8.02032,    -7.94193,    -7.85506,    -7.76079,    -7.66048,     -7.5557,    -7.44815,     -7.3395,
      -7.23137,    -7.12519,    -7.02213,    -6.92304,     -6.8284,    -6.73832,    -6.65253,    -6.57041,    -6.49103,    -6.41323,
      -6.33568,    -6.25698,    -6.17573,    -6.09064,    -6.00059,    -5.90472,    -5.80244,     -5.6935,    -5.57798,    -5.45631,
      -5.3292,    -5.19764,    -5.06279,    -4.92597,    -4.78851,    -4.65173,    -4.51684,    -4.38487,    -4.25663,    -4.13264,
      -4.01311,    -3.89796,    -3.78682,    -3.67903,    -3.57372,    -3.46986,    -3.36631,    -3.26192,    -3.15557,    -3.04627,
      -2.9332,    -2.81575,     -2.6936,    -2.56669,    -2.43525,    -2.29978,    -2.16102,    -2.01992,    -1.87754,    -1.73506,
      -1.59363,    -1.45438,    -1.31831,    -1.18624,    -1.05878,   -0.936292,   -0.818871,   -0.706345,    -0.59829,   -0.494057,
      -0.392817,   -0.293606,   -0.195392,  -0.0971273,  0.00218312,    0.103428,    0.207336,    0.314439,    0.425038,    0.539191,
      0.656711,    0.777178,    0.899964,     1.02427,     1.14917,     1.27366,     1.39673,     1.51738,     1.63471,     1.74796,
      1.85651,     1.95994,     2.05803,     2.15079,     2.23839,     2.32119,      2.3997,     2.47451,     2.54629,     2.61572,
      2.68344,     2.75004,     2.81596,     2.88155,     2.94696,      3.0122,     3.07709,     3.14132,     3.20443,     3.26586,
      3.32495,     3.38105,     3.43347,      3.4816,     3.52487,     3.56284,      3.5952,     3.62176,     3.64251,     3.65758,
      3.6672,     3.67176,     3.67171,     3.66757,     3.65988,     3.64915,     3.63587,     3.62043,     3.60314,     3.58416,
      3.56352,     3.54112,     3.51671,      3.4899,      3.4602,     3.42702,     3.38971,     3.34757,     3.29992,     3.24609,
      3.18547,     3.11753,     3.04186,     2.95816,     2.86628,     2.76622,     2.65813,     2.54231,     2.41921,     2.28942,
      2.15367,     2.01278,     1.86772,      1.7195,     1.56924,     1.41813,     1.26739,     1.11829,    0.972119,    0.830169,
      0.693732,    0.564068,    0.442392,     0.32985,    0.227495,    0.136263,   0.0569418, -0.00985115,  -0.0636982,   -0.104405,
      -0.13202,   -0.146851,   -0.149477,   -0.140743,    -0.12176,  -0.0938823,   -0.058685,  -0.0179251,   0.0265032,    0.072622,
      0.118429,    0.161963,    0.201365,     0.23495,    0.261261,    0.279126,    0.287705,    0.286519,    0.275471,    0.254854,
      0.225333,    0.187926,    0.143955,    0.094994,   0.0428023,  -0.0107534,  -0.0637787,   -0.114435,   -0.161022,   -0.202049,
      -0.236308,   -0.262921,   -0.281383,   -0.291582,   -0.293804,   -0.288716,   -0.277333,   -0.260968,   -0.241169,   -0.219639,
      -0.198158,   -0.178487,   -0.162283,   -0.151018,   -0.145895,   -0.147792,   -0.157206,   -0.174225,   -0.198513,   -0.229317,
      -0.265494,   -0.305552,   -0.347714,   -0.389991,   -0.430267,   -0.466387,   -0.496251,   -0.517908,   -0.529637,   -0.530024,
      -0.518021,   -0.492996,    -0.45476,   -0.403576,   -0.340148,   -0.265599,   -0.181418,  -0.0894111,  0.00837995,     0.10975,
      0.212417,     0.31411,    0.412648,     0.50602,    0.592452,    0.670462,    0.738906,       0.797,    0.844334,    0.880869,
      0.906916,    0.923104,    0.930337,     0.92974,    0.922601,     0.91031,    0.894292,    0.875954,     0.85662,    0.837492,
      0.819605,    0.803797,    0.790696,    0.780706,    0.774014,    0.770599,    0.770259,    0.772634,    0.777243,    0.783518,
      0.790843,    0.798592,    0.806164,    0.813011,    0.818666,    0.822764,     0.82505,    0.825387,    0.823759,    0.820256,
      0.815069,    0.808471,    0.800794,    0.792412,    0.783713,    0.775083,     0.76688,     0.75942,    0.752959,    0.747687,
      0.743717,    0.741088,    0.739763,    0.739637,    0.740546,     0.74228,    0.744596,    0.747231,    0.749921,    0.752411,
      0.75447,     0.755903,    0.756559,    0.756334,    0.755179,    0.753097,     0.75014,    0.746406,    0.742031,    0.737176,
      0.732026,    0.726771,    0.721598,    0.716683,    0.712181,    0.708218,    0.704886,    0.702237,    0.700288,    0.699014,
      0.698354,    0.698215,     0.69848,    0.699011,    0.699659,    0.700273,    0.700706,    0.700823,    0.700509,    0.699673,
      0.698255,    0.696222,    0.693577,    0.690353,     0.68661,    0.682435,    0.677935,    0.673227,     0.66844,    0.663699,
      0.659126,    0.654826,    0.650889,    0.647379,    0.644337,    0.641773,    0.639671,    0.637987,    0.636652,    0.635578,
      0.634661,    0.633787,     0.63284,    0.631705,    0.630279,     0.62847,     0.62621,    0.623452,    0.620174,    0.616383,
      0.612112,    0.607417,    0.602376,    0.597083,    0.591645,    0.586172,    0.580776,     0.57556,    0.570615,    0.566012,
      0.561804,    0.558015,    0.554647,    0.551672,     0.54904,    0.546678,    0.544497,    0.542394,    0.540262,    0.537992,
      0.535481,     0.53264,    0.529396,    0.525698,    0.521519,    0.516859,    0.511744,    0.506225,    0.500374,    0.494283,
      0.488054,    0.481798,    0.475623,    0.469634,    0.463922,    0.458561,    0.453603,    0.449077,    0.444982,    0.441293,
      0.437959,    0.434909,    0.432051,    0.429285,    0.426501,    0.423592,    0.420458,    0.417009,    0.413178,    0.408916,
      0.404203,    0.399043,    0.393469,    0.387537,    0.381326,    0.374932,     0.36846,    0.362022,    0.355729,    0.349683,
      0.34397,     0.338659,    0.333795,    0.329396,    0.325453,    0.321932,    0.318772,    0.315892,    0.313195,    0.310576,
      0.307923,    0.305129,    0.302098,    0.298748,    0.295017,     0.29087,    0.286297,    0.281317,    0.275972,    0.270333,
      0.264485,    0.258533,    0.252586,    0.246756,    0.241152,    0.235867,    0.230979,    0.226545,    0.222594,    0.219129,
      0.216125,    0.213531,    0.211273,    0.209259,    0.207385,    0.205538,     0.20361,    0.201495,    0.199106,    0.196372,
      0.193245,    0.189706,    0.185762,    0.181447,    0.176822,    0.171967,    0.166981,    0.161971,     0.15705,    0.152327,
      0.147902,    0.143858,    0.140259,    0.137141,    0.134516,    0.132366,    0.130647,    0.129292,    0.128212,    0.127306,
      0.126461,    0.125567,    0.124517,    0.123216,    0.121588,    0.119579,    0.117161,    0.114334,    0.111123,    0.107584,
      0.10379,     0.0998372,   0.0958296,   0.0918792,   0.0880958,   0.0845814,   0.0814234,   0.0786893,   0.0764226,   0.0746399,
      0.0733301,    0.072455,   0.0719515,   0.0717357,    0.071708,   0.0717592,   0.0717772,   0.0716541,   0.0712927,   0.0706126,
      0.0695551,   0.0680867,   0.0662014,   0.0639205,   0.0612921,   0.0583873,   0.0552961,   0.0521216,   0.0489739,   0.0459631,
      0.043192,    0.0407506,   0.0387097,   0.0371167,   0.0359931,   0.0353325,   0.0351014,   0.0352412,   0.0356718,   0.0362965,
      0.0370081,   0.0376956,   0.0382507,   0.0385752,   0.0385863,   0.0382221,   0.0374456,   0.0362465,    0.034642,    0.032676,
      0.0304156,   0.0279478,   0.0253731,      0.0228,    0.020338,   0.0180903,   0.0161482,   0.0145846,   0.0134502,   0.0127697,
      0.0125406,   0.0127334,   0.0132933,   0.0141437,   0.0151909,     0.01633,   0.0174514,   0.0184475,   0.0192196,   0.0196839,
      0.0197766,   0.0194581,   0.0187149,    0.017561,   0.0160368,   0.0142063,   0.0121533,    0.009976,  0.00778115,  0.00567701,
      0.00376676,  0.00214209, 0.000877411, 2.52803e-05,  -0.0003869,-0.000358562, 8.40117e-05, 0.000889335,  0.00198395,  0.00327702,
      0.00466598,  0.00604298,  0.00730153,  0.00834322,  0.00908375,   0.0094582,  0.00942507,  0.00896876,  0.00810059,  0.00685796,
      0.00530207 } ;
  const size_t thousandFloats = 1001*sizeof(Float_t);
  switch (select) {
    case 3 : memcpy(DataInTheGap,output_113_113 ,thousandFloats); break;
    case 2 : memcpy(DataInTheGap,output_diff_100,thousandFloats); break;
    case 1 : memcpy(DataInTheGap,output_100_diff,thousandFloats); break;
    case 0 :
    default : memcpy(DataInTheGap,output_nowall_oldTPC,thousandFloats);
  }

}


//________________________________________


/// Clock distortion
/*!
    The East endwheel of the TPC and the West endwheel of the TPC are not perfectly aligned.
    They were inserted separately into the TPC field cage tube.  They are aligned at the outer
    diameter (4 meters) to within about 1 mm.  This causes a slight misalingment of the relative
    coordinate systems.  By convention, we assume that one end is perfect and attribute all of 
    the error to a rotation of the other end ... however, the method (and the DB) allow you to
    input a rotation angle for each end, if you wish.  Note: this is a coordinate transformation
    and not a distortion correction.  It is here for historical reasons.
 */
void StMagUtilities::UndoClockDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  Double_t r, phi, z ;

  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }

  z = LimitZ( Sector, x ) ;                         // Protect against discontinuity at CM

  if ( z < 0 )  phi += EASTCLOCKERROR/1000. ;       // Phi rotation error in milli-radians
  if ( z > 0 )  phi += WESTCLOCKERROR/1000. ;       // Phi rotation error in milli-radians
  // Do nothing if z = 0 

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;

}


//________________________________________


/// Membrane distortion
/*!

 */
void StMagUtilities::UndoMembraneDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  cout << "StMagUtilities::UndoMembrane  This routine was made obosolete on 10/1/2009.  Do not use it." << endl ;
  exit(0) ;

  // Membrane Distortion correction is Obsolete.  Disabled by JT 2009
  /*
  Double_t r, phi, z ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  z      =  x[2] ;

  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM
 
  Float_t  Er_integral, Ephi_integral ;
  Interpolate3DEdistortion( r, phi, z, cmEr, cmEphi, Er_integral, Ephi_integral ) ;

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;
  */
  // End of Deletion

}


//________________________________________


/// Endcap distortion
/*!

 */
void StMagUtilities::UndoEndcapDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  cout << "StMagUtilities::UndoEndcap  This routine was made obosolete on 10/1/2009.  Do not use it." << endl ;
  exit(0) ;

  // EndCap Distortion correction is Obsolete.  Disabled by JT 2009
  /*
  Double_t r, phi, z ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  z      =  x[2] ;

  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM

  Float_t  Er_integral, Ephi_integral ;
  Interpolate3DEdistortion( r, phi, z, endEr, endEphi, Er_integral, Ephi_integral ) ;

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;
  */
  // End of Deletion

}


//________________________________________


/// IFC Shift Distortion
/*! 
    The Inner field cage of the TPC is not perfectly aligned with the outer field cage 
    of the TPC.  They are shifted along the Z axis by about 1 mm.  This causes a tilting 
    of the equi-potential lines inside the TPC and therefore a DCA error at the vertex.  
    The distortion is anti- symmetric in Z. 
    Electrostatic equations solved in Rectangular Coodinates by Jim Thomas
    Updated to work in cylindrical coordinates by Jamie Dunlop  11/01/2001
*/
void StMagUtilities::UndoIFCShiftDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{ 

  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  const   Int_t ORDER = 1 ;                         // Linear interpolation = 1, Quadratic = 2         

  if ( DoOnce )
    {
      cout << "StMagUtilities::IFCShift   Please wait for the tables to fill ...  ~5 seconds" << endl ;
      Int_t Nterms = 100 ;
      Double_t Denominator[100];
      memset(Denominator,0,100*sizeof(Double_t));
      for ( Int_t i = 0 ; i < EMap_nZ ; ++i ) 
	{
	  z = TMath::Abs( eZList[i] ) ;
	  for ( Int_t j = 0 ; j < EMap_nR ; ++j ) 
	    {
	      r = eRList[j] ;
	      shiftEr[i][j] = 0.0 ; 	    
              if (r < IFCRadius) continue; //VP defence against divergency. Not sure if correct.  JT - Yes, OK.
              if (r > OFCRadius) continue; //VP defence against divergency. Not sure if correct.  JT - Yes, OK.
              if (z > TPC_Z0)    continue; //VP defence against divergency. Not sure if correct.  JT - Yes, OK.
	      Double_t IntegralOverZ = 0.0 ;
	      for ( Int_t n = 1 ; n < Nterms ; ++n ) 
		{
		  Double_t k  = (2*n-1) * TMath::Pi() / TPC_Z0 ;
		  Double_t Cn = -4.0 * IFCShift / ( k * TPC_Z0 ) ;
		  Double_t Numerator =
		    TMath::BesselK0( k*OFCRadius ) * TMath::BesselI1( k*r ) +
		    TMath::BesselK1( k*r )         * TMath::BesselI0( k*OFCRadius ) ;
		  if (Denominator[n] == 0) Denominator[n] =
		    TMath::BesselK0( k*OFCRadius ) * TMath::BesselI0( k*IFCRadius ) -
		    TMath::BesselK0( k*IFCRadius ) * TMath::BesselI0( k*OFCRadius ) ;
		  Double_t zterm = 1 + TMath::Cos( k*z ) ;
		  Double_t qwe = Numerator / Denominator[n] ;
		  IntegralOverZ += Cn * zterm * qwe ;
	          if ( n>10 && fabs(IntegralOverZ)*1.e-10 > fabs(qwe) ) break;
		}
	      if  ( eZList[i] < 0 )  IntegralOverZ = -1 * IntegralOverZ ;  // Force AntiSymmetry of solutions in Z
	      shiftEr[i][j] = IntegralOverZ ; 	    }
	}
    }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;            // Table uses phi from 0 to 2*Pi
  z = LimitZ( Sector, x ) ;                         // Protect against discontinuity at CM

  Interpolate2DEdistortion( ORDER, r, z, shiftEr, Er_integral ) ;
  Ephi_integral = 0.0 ;  // Efield is symmetric in phi

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;

}


//________________________________________


/// Space Charge entry function
/*!
    Call the appropriate Space Charge function based on distortion mode
*/
void StMagUtilities::UndoSpaceChargeDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  if ((mDistortionMode & kSpaceCharge) && (mDistortionMode & kSpaceChargeR2)) {
      cout << "StMagUtilities ERROR **** Do not use kSpaceCharge and kSpaceChargeR2 at the same time" << endl ;
      cout << "StMagUtilities ERROR **** These routines have overlapping functionality." << endl ;
      exit(1) ;
  }

  if (mDistortionMode & kSpaceCharge) { 
      UndoSpaceChargeR0Distortion ( x, Xprime, Sector ) ;
  } else if (mDistortionMode & kSpaceChargeR2) { 
      UndoSpaceChargeR2Distortion ( x, Xprime, Sector ) ;
  }

}


//________________________________________


/// Space Charge Correction 
/*!
    Space Charge distortion assuming a uniform distribution of charge per unit volume
    in the TPC.  We now know that this is not a good assumption but the code is here
    for legacy reasons.  Electrostatic equations solved by Jamie Dunlop  11/01/2001
    Updated to include linear increase of charge from endcap to CM by Jim Thomas 12/18/2001
*/
void StMagUtilities::UndoSpaceChargeR0Distortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{ 
  
  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  const   Int_t ORDER = 1 ;                         // Linear interpolation = 1, Quadratic = 2         

  if ( DoOnce )
    {
      Int_t Nterms = 100 ;
      Double_t Denominator[100];
      memset(Denominator,0,100*sizeof(Double_t));
      for ( Int_t i = 0 ; i < EMap_nZ ; ++i ) 
	{
	  z = TMath::Abs( eZList[i] ) ;
	  for ( Int_t j = 0 ; j < EMap_nR ; ++j ) 
	    {
	      r = eRList[j] ;
	      spaceEr[i][j] = 0.0 ; 
              if (r < IFCRadius) continue; //VP defence against divergency. Not sure if correct.  JT - Yes, OK.
              if (r > OFCRadius) continue; //VP defence against divergency. Not sure if correct.  JT - Yes, OK.
              if (z > TPC_Z0)    continue; //VP defence against divergency. Not sure if correct.  JT - Yes, OK.
	      Double_t IntegralOverZ = 0.0 ;
	      for ( Int_t n = 1 ; n < Nterms ; ++n ) 
		{
		  Double_t k  = n * TMath::Pi() / TPC_Z0 ;  // Integrated Charge Density
		  Double_t zterm = TMath::Power(-1,(n+1)) * ( 1.0 - TMath::Cos( k * ( TPC_Z0 - z ) ) ) ;
		  //Double_t k  = (2*n-1) * TMath::Pi() / TPC_Z0 ;  // Uniform Charge Density
		  //Double_t zterm = 1.0 + TMath::Cos( k *  z ) ;   // Uniform Charge Density
		  Double_t Cn = -4.0 / ( k*k*k * TPC_Z0 * StarMagE ) ;
		  Double_t Numerator =
		    TMath::BesselI1( k*r )         * TMath::BesselK0( k*OFCRadius ) -
		    TMath::BesselI1( k*r )         * TMath::BesselK0( k*IFCRadius ) +
		    TMath::BesselK1( k*r )         * TMath::BesselI0( k*OFCRadius ) -
		    TMath::BesselK1( k*r )         * TMath::BesselI0( k*IFCRadius ) ;
		  if (Denominator[n] == 0) Denominator[n] =
		    TMath::BesselK0( k*OFCRadius ) * TMath::BesselI0( k*IFCRadius ) -
		    TMath::BesselK0( k*IFCRadius ) * TMath::BesselI0( k*OFCRadius ) ;
		  Double_t qwe = Numerator / Denominator[n] ;
		  IntegralOverZ += Cn * zterm * qwe ;
	          if ( n>10 && fabs(IntegralOverZ)*1.e-10 > fabs(qwe) ) break;
		}
	      spaceEr[i][j] = IntegralOverZ ; 
	    }
	}
    }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;            // Table uses phi from 0 to 2*Pi
  z = LimitZ( Sector, x ) ;                         // Protect against discontinuity at CM

  Interpolate2DEdistortion( ORDER, r, z, spaceEr, Er_integral ) ;
  Ephi_integral = 0.0 ;  // E field is symmetric in phi

  // Get Space Charge **** Every Event (JCD This is actually per hit)***
  // Need to reset the instance every hit.  May be slow, but there's no per-event hook.  
  if (fSpaceCharge) GetSpaceCharge(); // need to reset it. 

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      Float_t Weight = SpaceCharge * (doingDistortion ? SmearCoefSC : 1.0);
      phi =  phi - Weight * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - Weight * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;

}

  
//________________________________________


/// 1/R**2 SpaceCharge Distortion
/*!
  Space Charge distortion using space charge from a real event.  Any charge distribution can 
  be simulated by this method.  However, the best charge distribution is Howard's fit to 
  HiJet events.  It is approximately independent of Z due to the Bjorken Plateau a mid-rapidity.  The 
  radial distribution is approximately 1/R**2, however we use a better parameterization in the code.
  Many different charge distributions are hidden in the comments of the code.  All candidate distributions
  have been integrated over Z to simulate the linear increase of space charge in Z due to the slow 
  drift velocity of the ions.  Electrostatic equations solved by relaxtion.  
  Note that on 3/26/2008, we added a new element to the DB so that the space charge in the East and
  West halves of the TPC can be different by a constant factor.  The constant is called "SpaceChargeEWRatio"
  and is greater than 1.0 when there is more charge in the East half to the TPC.
  Original work by H. H. Wieman, N. Smirnov, and J. Thomas 
*/
void StMagUtilities::UndoSpaceChargeR2Distortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{ 
  
  const Int_t     ORDER       =    1 ;  // Linear interpolation = 1, Quadratic = 2         

  Float_t   Er_integral, Ephi_integral ;
  Double_t  r, phi, z ;

  if (fSpaceChargeR2) { GetSpaceChargeR2();} // need to reset it. 

  if ( DoOnce )
    {
      cout << "StMagUtilities::UndoSpace  Please wait for the tables to fill ...  ~5 seconds" << endl ;
      const Int_t     ROWS        =  257 ;  // (2**n + 1)    
      const Int_t     COLUMNS     =  129 ;  // (2**m + 1) 
      const Int_t     ITERATIONS  =  100 ;  // About 0.05 seconds per iteration
      const Double_t  GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
      const Double_t  GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;
      TMatrix  ArrayV(ROWS,COLUMNS), Charge(ROWS,COLUMNS) ;
      TMatrix  EroverEz(ROWS,COLUMNS) ;
      Float_t  Rlist[ROWS], Zedlist[COLUMNS] ;
      //Fill arrays with initial conditions.  V on the boundary and Charge in the volume.      

      for ( Int_t j = 0 ; j < COLUMNS ; j++ )  
	{
	  Double_t zed = j*GRIDSIZEZ ;
	  Zedlist[j] = zed ;
	  for ( Int_t i = 0 ; i < ROWS ; i++ )  
	    {
	      Double_t Radius = IFCRadius + i*GRIDSIZER ;
	      ArrayV(i,j) = 0 ;
	      Charge(i,j) = 0 ;
	      Rlist[i] = Radius ;
	    }
	}      

      for ( Int_t j = 1 ; j < COLUMNS-1 ; j++ )  
	{
	  Double_t zed = j*GRIDSIZEZ ;
	  for ( Int_t i = 1 ; i < ROWS-1 ; i++ ) 
	    { 
	      Double_t Radius = IFCRadius + i*GRIDSIZER ;
	      Double_t zterm = (TPC_Z0-zed) * (OFCRadius*OFCRadius - IFCRadius*IFCRadius) / TPC_Z0 ;
	      // Next line is for Uniform charge deposition in the TPC; then integrated in Z due to drifting ions
	      // Charge(i,j) =  2. * zterm / (OFCRadius*OFCRadius - IFCRadius*IFCRadius) ;  
	      // Next few lines are for linearly decreasing charge deposition in R; then integrated in Z 
	      // Double_t IORatio = 4.0 ;  // Ratio of charge density at IFC divided by charge density at OFC
	      // Charge(i,j) = zterm * ( 1 - Radius*(IORatio-1)/(IORatio*OFCRadius-IFCRadius) ) / 
	      //  ( (OFCRadius-IFCRadius)*(OFCRadius-IFCRadius)*(OFCRadius-IFCRadius)*(IORatio-1) /
	      //  ( -3. * (IORatio*OFCRadius-IFCRadius) ) + 
	      //  0.5*(OFCRadius*OFCRadius-IFCRadius*IFCRadius) ) ; 
	      // Next line is for 1/R charge deposition in the TPC; then integrated in Z due to drifting ions
	      // Charge(i,j) = zterm / ( ( OFCRadius - IFCRadius ) * Radius ) ; 
	      // Next line is Wiemans fit to the HiJet Charge distribution; then integrated in Z due to drifting ions
	      // Charge(i,j) = zterm * ( 3191/(Radius*Radius) + 122.5/Radius - 0.395 ) / 15823 ;
	      Charge(i,j) = zterm * SpaceChargeRadialDependence(Radius) ; // currently uses Wieman's fit to HIJET
	      // Next line can be used in addition to the previus "Wieman" distribution in order to do d-Au assymetric distributions
	      // JT Test Charge(i,j) *= ( 1.144 + 0.144*zed/TPC_Z0 ) ; // Note that this does the Au splash side ... not the deuteron side
	      // JT Test - Do not use the previous line for production work.  It is only for testing purposes.
              // JT Test - Real d-Au assymetries should be done by pulling d-Au spacecharge factors from the DB.
	      // Next line is for 1/R**2 charge deposition in the TPC; then integrated in Z due to drifting ions
	      // Charge(i,j) = zterm / ( TMath::Log(OFCRadius/IFCRadius) * ( Radius*Radius ) ) ; 
	      // Next line is for 1/R**3 charge deposition in the TPC; then integrated in Z due to drifting ions
	      // Charge(i,j) = zterm / ( ( 1/IFCRadius - 1/OFCRadius) * ( Radius*Radius*Radius ) ) ; 
	      // Next few lines are for a 1/R**N distribution where N may be any real number but not equal to 2.0
	      // Float_t N = 1.65 ;  // 1.65 is a fit to real charge distribution by GVB on 11/4/2004
	      // Charge(i,j) = zterm * (2-N) /
	      //            ( ( TMath::Power(OFCRadius,2-N) - TMath::Power(IFCRadius,2-N) ) * TMath::Power(Radius,N) ) ;
	    } // All cases normalized to have same total charge as the Uniform Charge case == 1.0 * Volume of West End of TPC
	}

      PoissonRelaxation( ArrayV, Charge, EroverEz, ITERATIONS ) ;

      //Interpolate results onto standard grid for Electric Fields
      Int_t ilow=0, jlow=0 ;
      Float_t save_Er[2] ;	      
      for ( Int_t i = 0 ; i < EMap_nZ ; ++i ) 
	{
	  z = TMath::Abs( eZList[i] ) ;
	  for ( Int_t j = 0 ; j < EMap_nR ; ++j ) 
	    { // Linear interpolation
	      r = eRList[j] ;
	      Search( ROWS,   Rlist, r, ilow ) ;  // Note switch - R in rows and Z in columns
	      Search( COLUMNS, Zedlist, z, jlow ) ;
	      if ( ilow < 0 ) ilow = 0 ;  // artifact of Root's binsearch, returns -1 if out of range
	      if ( jlow < 0 ) jlow = 0 ;   
	      if ( ilow + 1  >=  ROWS - 1 ) ilow =  ROWS - 2 ;	      
	      if ( jlow + 1  >=  COLUMNS - 1 ) jlow =  COLUMNS - 2 ; 
	      save_Er[0] = EroverEz(ilow,jlow) + (EroverEz(ilow,jlow+1)-EroverEz(ilow,jlow))*(z-Zedlist[jlow])/GRIDSIZEZ ;
	      save_Er[1] = EroverEz(ilow+1,jlow) + (EroverEz(ilow+1,jlow+1)-EroverEz(ilow+1,jlow))*(z-Zedlist[jlow])/GRIDSIZEZ ;
	      spaceR2Er[i][j] = save_Er[0] + (save_Er[1]-save_Er[0])*(r-Rlist[ilow])/GRIDSIZER ;
	    }
	}

    }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;            // Table uses phi from 0 to 2*Pi
  z = LimitZ( Sector, x ) ;                         // Protect against discontinuity at CM

  Interpolate2DEdistortion( ORDER, r, z, spaceR2Er, Er_integral ) ;
  Ephi_integral = 0.0 ;  // E field is symmetric in phi

  // Get Space Charge **** Every Event (JCD This is actually per hit)***
  // Need to reset the instance every hit.  May be slow, but there's no per-event hook.
  //if (fSpaceChargeR2) GetSpaceChargeR2(); // need to reset it. 

  // Subtract to Undo the distortions and apply the EWRatio on the East end of the TPC 
  if ( r > 0.0 ) 
    {
      double Weight = SpaceChargeR2 * (doingDistortion ? SmearCoefSC : 1.0);
      if ( z < 0) Weight *= SpaceChargeEWRatio ;
      phi =  phi - Weight * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - Weight * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;

}

  
//________________________________________
  

/// Abort Gap Cleaning Cycle space charge correction
/*!
  Calculate the 3D distortions due to the charge introduced by an Abort Gap Cleaning Cycle. 
  As we understand the process, the abort gap cleaning cycle causes a burst of charge to appear 
  in the TPC every few seconds (cycle time predefined by MCR).  The positive charge then dissapates
  by drifting from the Central Membrane to the TPC readout planes. However, the drift time of the 
  ions is very slow and so it takes approximately 0.75 seconds (279 cm/sec) to drift from CM to readout.

  Gene proposed that we solve this problem by calculating the spacecharge in the TPC every 0.05 seconds
  (or so) as the charge drifts to the readout plane.  Thus, a discretized solution.

  Charge distribution: we do not know the distribution of charge in the TPC due to an abort gap
  cleaning cycle.  We are going to start by assuming that the charge distribution has the same shape
  as a normal event (approximately 1/R**2).  This assumption will hold at t=0, but then we will move 
  the charge distribution in Z as time evolves. Thus, at 0.05 seconds the charge distribution in Z will
  be shifted towards the endcaps by (about) 14 cm.  Repeat and shift every 0.05 seconds.

  "const Float_t Time" is the time since the last cleaning of the abort gap. Typically 0 to 0.75 seconds
  because 0.75 seconds is an estimate of the full length ion drift time.  Time > 0.75 s is a no-op.

  This codes does not stack events.  It assumes that the time between Abort Gap cleaning cycles is > 0.75 seconds.
  
  This code is an extension of the "1/R**2 SpaceCharge Distortion" but allows for the possibility
  of storing and applying N maps (where N ~ 15).  The following comments are excerpted from that code base:

  Space Charge distortion using space charge from a real event (an assumption).  The best charge 
  distribution for one event is Howard's fit to HiJet events. The radial distribution is approximately 
  1/R**2, however we use a better parameterization in the code. The charge distribution has been
  integrated over Z to simulate the linear increase of space charge in Z due to the slow drift velocity 
  of the ions.  Electrostatic equations solved by relaxtion.  
  Note that on 3/26/2008, we added a new element to the DB so that the space charge in the East and
  West halves of the TPC can be different by a constant factor.  The constant is called "SpaceChargeEWRatio"
  and is greater than 1.0 when there is more charge in the East half to the TPC.

  Original work by Gene VanBuren, Benjamin Kimelman and Jim Thomas 
*/

void StMagUtilities::UndoAbortGapDistortion( const Float_t x[], Float_t Xprime[], Int_t Sector, Float_t TimeSinceDeposition ) 
{
  
  const Int_t       ORDER     =    1 ;   // Linear interpolation = 1, Quadratic = 2   
  
  static  Bool_t DoOnceLocal = true ;
  Float_t   Er_integral, Ephi_integral ;
  Double_t  r, phi, z ;
  const Int_t     TIMEBINS    =  20  ;  // Each time bin is 0.05 seconds          

  static Float_t abortR2Er[TIMEBINS][EMap_nZ][EMap_nR] ;
  //  static Float_t abortR2Er_Calc[EMap_nZ][EMap_nR] ;

  const Int_t     ROWS        =  257 ;  // (2**n + 1)                             
  const Int_t     COLUMNS     =  129 ;  // (2**m + 1)                             

  //cout << "StMagUtilities::UndoAbortGap TimeSinceDeposition=" << TimeSinceDeposition << endl;

  Float_t AbortGapCharge = 0.0;
  Int_t AbortGapChargeSize = 1;

  Bool_t testCase = (TimeSinceDeposition >= 0);
  if (!testCase) {
    if (fAbortGapCharge) GetAbortGapCharge();
    AbortGapChargeSize = AbortGapCharges->GetSize();
    if (AbortGapChargeSize == 0) { memcpy(Xprime,x,threeFloats);  return ; }
  }
  

  Double_t fullDriftTime = TPC_Z0 / IonDriftVel;

  if (DoOnceLocal )
    {
      //      cout << "fullDriftTime: " << fullDriftTime << endl;
      cout << "StMagUtilities::UndoAbortGap  Please wait for the tables to fill ... ~5 seconds" << endl;
      const Int_t     ITERATIONS  =  100 ;  // About 0.05 seconds per iteration       
      const Double_t  GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
      const Double_t  GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;
      Float_t  Rlist[ROWS], Zedlist[COLUMNS] ;
     
      Double_t timeBinLength = TPC_Z0 / TIMEBINS;
      Double_t zterm = OFCRadius*OFCRadius - IFCRadius*IFCRadius ;

      TMatrix ArrayV(ROWS,COLUMNS), Charge(ROWS,COLUMNS), EroverEz(ROWS,COLUMNS) ;
      //Fill arrays with initial conditions.  V on the boundary and Charge in the volume.                                                                                   
      for ( Int_t k = 0 ; k < TIMEBINS ; k++ )
	{
	  Double_t driftZ = k * timeBinLength ;

	  for ( Int_t j = 0 ; j < COLUMNS ; j++ )
	    {
	      Double_t zed = j*GRIDSIZEZ ;
	      Zedlist[j] = zed ;
	      for ( Int_t i = 0 ; i < ROWS ; i++ )
		{
		  Double_t Radius = IFCRadius + i*GRIDSIZER ;
		  ArrayV(i,j) = 0 ;
		  Charge(i,j) = 0 ;
		  EroverEz(i,j) = 0 ;
		  Rlist[i] = Radius ;
		}
	    }
	  // Set charge distribution                                                  
	  for ( Int_t j = 1 ; j < COLUMNS-1 ; j++ )
	    {
	      Double_t zed = j*GRIDSIZEZ ;
	      if ( zed <= TPC_Z0 - driftZ ) 
                {
	          for ( Int_t i = 1 ; i < ROWS-1 ; i++ )
		    {
		      Double_t Radius = IFCRadius + i*GRIDSIZER ;
		      Charge(i,j) = zterm * SpaceChargeRadialDependence(Radius) ;
		    }
		} else {
	          for ( Int_t i = 1 ; i < ROWS-1 ; i++ ) Charge(i,j) = 0.0 ;
		}
	    }
	  
	  // Do Poisson relaxation for each time slice                                
	  PoissonRelaxation( ArrayV, Charge, EroverEz, ITERATIONS ) ;
	  //Interpolate results onto standard grid for Electric Fields                
	  Int_t ilow=0, jlow=0 ;
	  Float_t save_Er[2] ;
	  for ( Int_t i = 0 ; i < EMap_nZ ; ++i )
	    {
	      z = TMath::Abs( eZList[i] ) ;
	      for ( Int_t j = 0 ; j <EMap_nR ; ++j )
		{ // Linear interpolation                                             
		  r = eRList[j] ;
		  Search( ROWS,    Rlist, r, ilow ) ;
		  Search( COLUMNS, Zedlist, z, jlow ) ;
		  if ( ilow < 0 ) ilow = 0 ;
		  if ( jlow < 0 ) jlow = 0 ;
		  if ( ilow + 1  >=  ROWS - 1 ) ilow =  ROWS - 2 ;
		  if ( jlow + 1  >=  COLUMNS - 1 ) jlow =  COLUMNS - 2 ;
		  save_Er[0] = EroverEz(ilow,jlow) + (EroverEz(ilow,jlow+1)-EroverEz(ilow,jlow))*(z-Zedlist[jlow])/GRIDSIZEZ ;
		  save_Er[1] = EroverEz(ilow+1,jlow) + (EroverEz(ilow+1,jlow+1)-EroverEz(ilow+1,jlow))*(z-Zedlist[jlow])/GRIDSIZEZ ;
		  abortR2Er[k][i][j] = save_Er[0] + (save_Er[1]-save_Er[0])*(r-Rlist[ilow])/GRIDSIZER ;
		}
	    }
	}
      
      DoOnceLocal = false ;
    }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;      // Table uses phi from 0 to 2*Pi        
  z = LimitZ( Sector, x ) ;                   // Protect against discontinuity at CM  
  
  Double_t timeBinDuration = fullDriftTime / TIMEBINS;

  for ( Int_t i=0 ; i<AbortGapChargeSize ; i++)
    {
      if (testCase)
        {
          if (fSpaceChargeR2) { GetSpaceChargeR2();} // need to reset it. 
          AbortGapCharge = SpaceChargeR2; // test charge
        } else {
          TimeSinceDeposition = (Float_t) (*AbortGapTimes)[i];
          if ( TimeSinceDeposition >= fullDriftTime ) continue;
          AbortGapCharge = AbortGapChargeCoef * (*AbortGapCharges)[i];
        }

      // Determine which time slice is closest                                            
      Int_t timeSlice = TIMEBINS;
      Double_t closestTime = 25.0 ;

      for ( Int_t k = 0 ; k < TIMEBINS ; k++ )
        {
          Double_t timeSliceTime = k * timeBinDuration;
          Double_t timeDiff = abs(TimeSinceDeposition - timeSliceTime ) ;
          if ( timeDiff < abs(TimeSinceDeposition - closestTime) )
            {
              closestTime = timeSliceTime;
              timeSlice = k ;
            }
        }

      if ( timeSlice >=0 && timeSlice < TIMEBINS)
        {
	  //	  cout << "timeSlice: " << timeSlice << endl;
          Interpolate2DEdistortion( ORDER, r, z, abortR2Er[timeSlice], Er_integral ) ;
          Ephi_integral = 0.0 ;  // E field is symmetric in phi                               
          // Subtract to Undo the distortions and apply the EWRatio on the East end of the TPC
          if ( r > 0.0 ) 
            {
              double Weight = AbortGapCharge * (doingDistortion ? SmearCoefSC : 1.0);
              phi =  phi - Weight * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;     
              r   =  r   - Weight * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
            }
        } 
    }
  
  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2];

}


//________________________________________


/// Shorted Ring Distortion
/*!
    This code assumes that information about shorted rings in the TPC field cage will come from the DB.  
    The DB returns rows from a table and the columns have the following meaning:
  
    Side(E=0/W=1)  Cage(IFC=0/OFC=1)  Ring(# from CM)  MissingResistance(MOhm)  ExtraResistance(MOhm)
    
    0  0  169.5  2.0  2.0
    0  0  115.5  2.0  0.0
    1  1  150.5  1.0  1.0
    0  0    0.0  0.0  0.0
    etc.

    The table indicates that there are two shorted rings on the IFC of the East end of the TPC.  
    One of these shorts has a 2.0 MegOhm compensating resistor installed in the external resistor chain.
    There is also a short on the OFC of the West end of the TPC.  It is a non-standard short of 1 MOhm. 
    It has an external compensating resistor of 1.0 MOhm.  The row of zeros indicates that there are 
    no more shorts in the TPC. Counting of the rings starts at the Central Membrane; the CM is ring zero, 
    and 150.5 means the short is between rings 150 and 151.

    Electrostatic Equations from SN0253 by Howard Wieman.
    Note that we use Howard's funny coordinate system where Z==0 at the GG.
*/
void StMagUtilities::UndoShortedRingDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{ 
  
  const   Int_t   ORDER     = 1     ;            // Linear interpolation = 1, Quadratic = 2         
  
  static  Bool_t DoOnceLocal = true ;
  static  Int_t  NumberOfEastInnerShorts = 0, NumberOfEastOuterShorts = 0 , NumberOfWestInnerShorts = 0, NumberOfWestOuterShorts = 0 ;
 
  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  if (fTpcVolts) {DoOnceLocal = UpdateShortedRing() ;}

  if ( DoOnceLocal )
    {

      cout << "StMagUtilities::UndoShort  Please wait for the tables to fill ...  ~5 seconds" << endl ;

      // Parse the Table and separate out the four different resistor chains
      // Definition: A "missing" resistor is a shorted resistor, an "extra" resistor is a compensating resistor added at the end
      const   Float_t Z01       = 1.225 ;            // Distance from CM to center of first ring (cm)

      Float_t EastInnerMissingSum = 0,     EastOuterMissingSum = 0,      WestInnerMissingSum = 0,     WestOuterMissingSum = 0 ; 
      Float_t EastInnerExtraSum   = 0,     EastOuterExtraSum   = 0,      WestInnerExtraSum   = 0,     WestOuterExtraSum   = 0 ;   
      Float_t EastInnerMissingOhms[10],    EastOuterMissingOhms[10],     WestInnerMissingOhms[10],    WestOuterMissingOhms[10] ; 
      Float_t EastInnerShortZ[10],         EastOuterShortZ[10],          WestInnerShortZ[10],         WestOuterShortZ[10] ;
      
      NumberOfEastInnerShorts = 0; NumberOfEastOuterShorts = 0 ; NumberOfWestInnerShorts = 0; NumberOfWestOuterShorts = 0 ;

      for ( Int_t i = 0 ; i < ShortTableRows ; i++ )
	{
	  if ( ( Side[i] + Cage[i] + Ring[i] + TMath::Abs(MissingResistance[i]) + TMath::Abs(Resistor[i]) ) == 0.0 ) continue ;
	  if ( Side[i] == 0 && Cage[i] == 0 ) 
	    { NumberOfEastInnerShorts++ ; EastInnerMissingSum += MissingResistance[i] ; EastInnerExtraSum += Resistor[i] ; 
	    EastInnerMissingOhms[NumberOfEastInnerShorts-1]  = MissingResistance[i] ; 
	    EastInnerShortZ[NumberOfEastInnerShorts-1]  = TPC_Z0 - ( Z01 + (Ring[i]-1)*RPitch) ; }
	  if ( Side[i] == 0 && Cage[i] == 1 ) 
	    { NumberOfEastOuterShorts++ ; EastOuterMissingSum += MissingResistance[i] ; EastOuterExtraSum += Resistor[i] ; 
	    EastOuterMissingOhms[NumberOfEastOuterShorts-1]  = MissingResistance[i] ; 
	    EastOuterShortZ[NumberOfEastOuterShorts-1]  = TPC_Z0 - ( Z01 + (Ring[i]-1)*RPitch) ; }
	  if ( Side[i] == 1 && Cage[i] == 0 ) 
	    { NumberOfWestInnerShorts++ ; WestInnerMissingSum += MissingResistance[i] ; WestInnerExtraSum += Resistor[i] ; 
	    WestInnerMissingOhms[NumberOfWestInnerShorts-1]  = MissingResistance[i] ; 
	    WestInnerShortZ[NumberOfWestInnerShorts-1]  = TPC_Z0 - ( Z01 + (Ring[i]-1)*RPitch) ; }
	  if ( Side[i] == 1 && Cage[i] == 1 ) 
	    { NumberOfWestOuterShorts++ ; WestOuterMissingSum += MissingResistance[i] ; WestOuterExtraSum += Resistor[i] ; 
	    WestOuterMissingOhms[NumberOfWestOuterShorts-1]  = MissingResistance[i] ; 
	    WestOuterShortZ[NumberOfWestOuterShorts-1]  = TPC_Z0 - ( Z01 + (Ring[i]-1)*RPitch) ; }
	}
      
      // Don't fill the tables if there aren't any shorts
      if ( (NumberOfEastInnerShorts + NumberOfEastOuterShorts + NumberOfWestInnerShorts + NumberOfWestOuterShorts) == 0 ) 
	  { memcpy(Xprime,x,threeFloats);  return ; }

      Float_t EastInnerRtot = Rtot + EastInnerExtraSum - EastInnerMissingSum ;  // Total resistance of the real resistor chain
      Float_t EastOuterRtot = Rtot + EastOuterExtraSum - EastOuterMissingSum ;  // Total resistance of the real resistor chain
      Float_t WestInnerRtot = Rtot + WestInnerExtraSum - WestInnerMissingSum ;  // Total resistance of the real resistor chain
      Float_t WestOuterRtot = Rtot + WestOuterExtraSum - WestOuterMissingSum ;  // Total resistance of the real resistor chain
      
      //Float_t deltaV    = GG*0.99 - CathodeV * (1.0-TPC_Z0*RStep/(RPitch*Rtot)) ;    // (test) Error on GG voltage from nominal (99% effective)
      //    GVB (2013-12-11): Superseded by UndoGGVoltErrorDistortion()
      
      Int_t Nterms = 100 ;
      Double_t Denominator[100];
      memset(Denominator,0,100*sizeof(Double_t));
      for ( Int_t i = 0 ; i < EMap_nZ ; ++i ) 
	{
	  z = eZList[i] ;
	  for ( Int_t j = 0 ; j < EMap_nR ; ++j ) 
	    {
	      r = eRList[j] ;
	      shortEr[i][j] = 0.0 ; 	    
              if (r < IFCRadius) continue; //VP defence against divergency. Not sure if correct.  JT - Yes, OK.
              if (r > OFCRadius) continue; //VP defence against divergency. Not sure if correct.  JT - Yes, OK.
              if (TMath::Abs(z) > TPC_Z0)  continue; //VP defence against divergency. 
	      Double_t IntegralOverZ = 0.0 ;
	      for ( Int_t n = 1 ; n < Nterms ; ++n ) 
		{
		  Double_t k    =  n * TMath::Pi() / TPC_Z0 ;
		  Double_t Ein  =  0 ;                    // Error potential on the IFC
		  Double_t Eout =  0 ;                    // Error potential on the OFC
		  Double_t sum  =  0 ;                    // Working variable
		  if ( z < 0 ) 
		    {
		      sum  = 0.0 ;
		      for  ( Int_t m = 0 ; m < NumberOfEastInnerShorts ; m++ ) 
			sum += ( 1 - Rfrac - TMath::Cos(k*EastInnerShortZ[m]) ) * EastInnerMissingOhms[m] ; 
		      Ein  = 2 * ( Rfrac*EastInnerExtraSum + sum ) / (k*EastInnerRtot) ;		
		      sum  = 0.0 ;
		      for  ( Int_t m = 0 ; m < NumberOfEastOuterShorts ; m++ ) 
			sum += ( 1 - Rfrac - TMath::Cos(k*EastOuterShortZ[m]) ) * EastOuterMissingOhms[m] ; 
		      Eout = 2 * ( Rfrac*EastOuterExtraSum + sum ) / (k*EastOuterRtot) ;		
		    }
		  if ( z == 0 ) continue ;
		  if ( z > 0 ) 
		    {
		      sum  = 0.0 ;
		      for  ( Int_t m = 0 ; m < NumberOfWestInnerShorts ; m++ ) 
			sum += ( 1 - Rfrac - TMath::Cos(k*WestInnerShortZ[m]) ) * WestInnerMissingOhms[m] ; 
		      Ein  = 2 * ( Rfrac*WestInnerExtraSum + sum ) / (k*WestInnerRtot) ;		
		      sum  = 0.0 ;
		      for  ( Int_t m = 0 ; m < NumberOfWestOuterShorts ; m++ ) 
			sum += ( 1 - Rfrac - TMath::Cos(k*WestOuterShortZ[m]) ) * WestOuterMissingOhms[m] ; 
		      Eout = 2 * ( Rfrac*WestOuterExtraSum + sum ) / (k*WestOuterRtot) ;		
		    }
		  //if ( z > 0 )                       // (test) Gating Grid studies
                  //  GVB (2013-12-11): Superseded by UndoGGVoltErrorDistortion()
                  //                    Previous math here was incorrect anyhow! Fixing...
                  //  {
		  //    Ein   =  2 * -1*deltaV / ( k * Rfrac * CathodeV ) ;  // (test) Gating Grid studies (note -1)
		  //    Eout  =  2 * -1*deltaV / ( k * Rfrac * CathodeV ) ;  // (test) Gating Grid studies (note -1)
                  //  }
                  //else { Ein = 0.0 ; Eout = 0.0 ; }  // (test) Gating Grid studies
		  Double_t An   =  Ein  * TMath::BesselK0( k*OFCRadius ) - Eout * TMath::BesselK0( k*IFCRadius ) ;
		  Double_t Bn   =  Eout * TMath::BesselI0( k*IFCRadius ) - Ein  * TMath::BesselI0( k*OFCRadius ) ;
		  Double_t Numerator =
		    An * TMath::BesselI1( k*r ) - Bn * TMath::BesselK1( k*r ) ;
		  if (Denominator[n] == 0) Denominator[n] =
		    TMath::BesselK0( k*OFCRadius ) * TMath::BesselI0( k*IFCRadius ) -
		    TMath::BesselK0( k*IFCRadius ) * TMath::BesselI0( k*OFCRadius ) ;
		  Double_t zterm = TMath::Cos( k*(TPC_Z0-TMath::Abs(z)) ) - 1 ;
		  Double_t qwe = Numerator / Denominator[n] ;
		  IntegralOverZ += zterm * qwe ;
	          if ( n>10 && fabs(IntegralOverZ)*1.e-10 > fabs(qwe) ) break;   // Assume series converges, break if small terms
		}
	      shortEr[i][j] = IntegralOverZ ;
 	    }
	}
      DoOnceLocal = false ;
    }
  
  if ( SectorSide( Sector, x ) > 0 ) {
         if ( (NumberOfWestInnerShorts + NumberOfWestOuterShorts) == 0 ) 
	  { memcpy(Xprime,x,threeFloats);  return ; }
  } else if ( (NumberOfEastInnerShorts + NumberOfEastOuterShorts) == 0 ) 
	  { memcpy(Xprime,x,threeFloats);  return ; }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;            // Table uses phi from 0 to 2*Pi
  z = LimitZ( Sector, x ) ;                         // Protect against discontinuity at CM

  Interpolate2DEdistortion( ORDER, r, z, shortEr, Er_integral ) ;
  Ephi_integral = 0.0 ;  // Efield is symmetric in phi

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;

}

  
//________________________________________


/// Gated Grid Voltage Error
/*!
    This code assumes that information about the GG voltage errors will come from the DB.  
    
    Calculate the effect of having an incorrect voltage on the East or West Gated Grids.

    Electrostatic Equations from SN0253 by Howard Wieman.
    Note that we use Howard's funny coordinate system where Z==0 at the GG.
*/

void StMagUtilities::UndoGGVoltErrorDistortion( const Float_t x[], Float_t Xprime[], Int_t Sector )
{ 
  
  const   Int_t   ORDER     = 1     ;               // Linear interpolation = 1, Quadratic = 2         

  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  // if (fTpcVolts) DoOnceLocal = UpdateGGVoltError() ;  // Reserved for Gene VB to do this correctly

  if ( DoOnce )
    {

      cout << "StMagUtilities::UndoGG VE  Please wait for the tables to fill ...  ~5 seconds" << endl ;

      Int_t Nterms = 100 ;
      Double_t Denominator[100];
      memset(Denominator,0,100*sizeof(Double_t));
      for ( Int_t i = 0 ; i < EMap_nZ ; ++i ) 
	{
	  z = eZList[i] ;
	  for ( Int_t j = 0 ; j < EMap_nR ; ++j ) 
	    {
	      r = eRList[j] ;
	      GGVoltErrorEr[i][j] = 0.0 ; 	    
              if (r < IFCRadius) continue; 
              if (r > OFCRadius) continue; 
              if (TMath::Abs(z) > TPC_Z0)  continue;
	      Double_t IntegralOverZ = 0.0 ;
	      for ( Int_t n = 1 ; n < Nterms ; ++n ) 
		{
		  if ( z == 0 ) continue ;
		  Double_t k    =  n * TMath::Pi() / TPC_Z0 ;
                  Double_t Ein  =  -2.0 * (z < 0 ? deltaVGGEast : deltaVGGWest) * deltaGGeffectiveness /
                           (k * (CathodeV - GGideal));  // Error potential on the IFC
                  Double_t Eout =  Ein ;                // Error potential on the OFC

		  Double_t An   =  Ein  * TMath::BesselK0( k*OFCRadius ) - Eout * TMath::BesselK0( k*IFCRadius ) ;
		  Double_t Bn   =  Eout * TMath::BesselI0( k*IFCRadius ) - Ein  * TMath::BesselI0( k*OFCRadius ) ;
		  Double_t Numerator =
		    An * TMath::BesselI1( k*r ) - Bn * TMath::BesselK1( k*r ) ;
		  if (Denominator[n] == 0) Denominator[n] =
		    TMath::BesselK0( k*OFCRadius ) * TMath::BesselI0( k*IFCRadius ) -
		    TMath::BesselK0( k*IFCRadius ) * TMath::BesselI0( k*OFCRadius ) ;
		  Double_t zterm = TMath::Cos( k*(TPC_Z0-TMath::Abs(z)) ) - 1 ;
		  Double_t qwe = Numerator / Denominator[n] ;
		  IntegralOverZ += zterm * qwe ;
	          if ( n>10 && fabs(IntegralOverZ)*1.e-10 > fabs(qwe) ) break;   // Assume series converges, break if small terms
		}
	      GGVoltErrorEr[i][j] = IntegralOverZ ;
 	    }
	}
    }
  
  if ( deltaVGGEast == 0.0 && deltaVGGWest == 0 ) 
       { memcpy(Xprime,x,threeFloats) ; return ; }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;            // Table uses phi from 0 to 2*Pi
  z = LimitZ( Sector, x ) ;                         // Protect against discontinuity at CM

  Interpolate2DEdistortion( ORDER, r, z, GGVoltErrorEr, Er_integral ) ;
  Ephi_integral = 0.0 ;  // Efield is symmetric in phi

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;

}

//________________________________________

/// Interpolate a 2D table - 2D interpolation within a 2D TMatrix
Float_t StMagUtilities::Interpolate2DTable( const Int_t ORDER, const Float_t x, const Float_t y, const Int_t nx, const Int_t ny, 
					    const Float_t XV[], const Float_t YV[], const TMatrix &Array )
{

  static  Int_t jlow = 0, klow = 0 ;
  Float_t save_Array[ORDER+1]  ;

  Search( nx,  XV,  x,   jlow  ) ;
  Search( ny,  YV,  y,   klow  ) ;
  if ( jlow < 0 ) jlow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( klow < 0 ) klow = 0 ;
  if ( jlow + ORDER  >=    nx - 1 ) jlow =   nx - 1 - ORDER ;
  if ( klow + ORDER  >=    ny - 1 ) klow =   ny - 1 - ORDER ;

  for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
    {
      Float_t *ajkl = &((TMatrix&)Array)(j,klow);
      save_Array[j-jlow]  = Interpolate( &YV[klow], ajkl , ORDER, y )   ;
    }

  return( Interpolate( &XV[jlow], save_Array, ORDER, x ) )   ;

}

/// Interpolate the E field map - 2D interpolation
void StMagUtilities::Interpolate2DEdistortion( const Int_t ORDER, const Float_t r, const Float_t z, 
 					       const Float_t Er[EMap_nZ][EMap_nR], Float_t &Er_value )
{

  static  Int_t jlow = 0, klow = 0 ;
  Float_t save_Er[ORDER+1] ;

  Search( EMap_nZ,   eZList,  z,   jlow   ) ;
  Search( EMap_nR,   eRList,  r,   klow   ) ;
  if ( jlow < 0 ) jlow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( klow < 0 ) klow = 0 ;
  if ( jlow + ORDER  >=    EMap_nZ - 1 ) jlow =   EMap_nZ - 1 - ORDER ;
  if ( klow + ORDER  >=    EMap_nR - 1 ) klow =   EMap_nR - 1 - ORDER ;

  for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
    {
      save_Er[j-jlow]     = Interpolate( &eRList[klow], &Er[j][klow], ORDER, r )   ;
    }
  Er_value = Interpolate( &eZList[jlow], save_Er, ORDER, z )   ;

}

/// Interpolate the E field map - 3D interpolation
void StMagUtilities::Interpolate3DEdistortion( const Int_t ORDER, const Float_t r, const Float_t phi, const Float_t z,
					     const Float_t Er[EMap_nZ][EMap_nPhi][EMap_nR], const Float_t Ephi[EMap_nZ][EMap_nPhi][EMap_nR], 
                                             Float_t &Er_value, Float_t &Ephi_value )
{

  static  Int_t ilow = 0, jlow = 0, klow = 0 ;
  Float_t save_Er[ORDER+1],   saved_Er[ORDER+1] ;
  Float_t save_Ephi[ORDER+1], saved_Ephi[ORDER+1] ;

  Search( EMap_nZ,   eZList,   z,   ilow   ) ;
  Search( EMap_nPhi, ePhiList, phi, jlow   ) ;
  Search( EMap_nR,   eRList,   r,   klow   ) ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow < 0 ) klow = 0 ;

  if ( ilow + ORDER  >=    EMap_nZ - 1 ) ilow =   EMap_nZ - 1 - ORDER ;
  if ( jlow + ORDER  >=  EMap_nPhi - 1 ) jlow = EMap_nPhi - 1 - ORDER ;
  if ( klow + ORDER  >=    EMap_nR - 1 ) klow =   EMap_nR - 1 - ORDER ;

  for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
      for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
	{
	  save_Er[j-jlow]     = Interpolate( &eRList[klow], &Er[i][j][klow], ORDER, r )   ;
	  save_Ephi[j-jlow]   = Interpolate( &eRList[klow], &Ephi[i][j][klow], ORDER, r )   ;
	}
      saved_Er[i-ilow]     = Interpolate( &ePhiList[jlow], save_Er, ORDER, phi )   ; 
      saved_Ephi[i-ilow]   = Interpolate( &ePhiList[jlow], save_Ephi, ORDER, phi )   ; 
    }
  Er_value     = Interpolate( &eZList[ilow], saved_Er, ORDER, z )    ;
  Ephi_value   = Interpolate( &eZList[ilow], saved_Ephi, ORDER, z )  ;
 
}


/// Interpolate a 3D Table - 3D interpolation within a 3D TMatrix
Float_t StMagUtilities::Interpolate3DTable ( const Int_t ORDER, const Float_t x,    const Float_t y,    const Float_t z,
					     const Int_t  nx,    const Int_t  ny,    const Int_t  nz,
					     const Float_t XV[], const Float_t YV[], const Float_t ZV[],
					     TMatrix **ArrayofArrays )
{

  static  Int_t ilow = 0, jlow = 0, klow = 0 ;
  Float_t save_Array[ORDER+1],  saved_Array[ORDER+1] ;

  Search( nx, XV, x, ilow   ) ;
  Search( ny, YV, y, jlow   ) ;
  Search( nz, ZV, z, klow   ) ;  

  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow < 0 ) klow = 0 ;

  if ( ilow + ORDER  >=    nx - 1 ) ilow =   nx - 1 - ORDER ;
  if ( jlow + ORDER  >=    ny - 1 ) jlow =   ny - 1 - ORDER ;
  if ( klow + ORDER  >=    nz - 1 ) klow =   nz - 1 - ORDER ;

  for ( Int_t k = klow ; k < klow + ORDER + 1 ; k++ )
    {
      TMatrix &Table = *ArrayofArrays[k] ;
      for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
	{
	  save_Array[i-ilow] = Interpolate( &YV[jlow], &Table(i,jlow), ORDER, y )   ;
	}
      saved_Array[k-klow] = Interpolate( &XV[ilow], save_Array, ORDER, x )   ; 
    }
  return( Interpolate( &ZV[klow], saved_Array, ORDER, z ) )   ;
 
}


//________________________________________

//________________________________________


/// Solve Poisson's Equation by Relaxation Technique
/*!
    Solve Poissons equation in a cylindrical coordinate system.  The ArrayV matrix must be filled with the boundary
    conditions on the first and last rows, and the first and last columns.  The remainder of the array can be blank
    or contain a preliminary guess at the solution.  The Charge matrix contains the enclosed spacecharge density at 
    each point.  The charge density matrix can be full of zero's if you wish to solve Laplaces equation however
    it should not contain random numbers or you will get random numbers back as a solution.

    Poisson's equation is solved by iteratively relaxing the matrix to the final solution.  In order to speed up the
    convergence to the best solution, this algorithm does a binary expansion of the solution space.  First it solves
    the problem on a very sparse grid by skipping rows and columns in the original matrix.  Then it doubles the number 
    of points and solves the problem again.  Then it doubles the number of points and solves the problem again.  This 
    happens several times until the maximum number of points has been included in the array.  
  
    NOTE: In order for this algorith to work, the number of rows and columns must be a power of 2 plus one.  
    So ROWS == 2**M + 1 and COLUMNS == 2**N + 1.  The number of ROWS and COLUMNS can be different.
 */
void StMagUtilities::PoissonRelaxation( TMatrix &ArrayVM, TMatrix &ChargeM, TMatrix &EroverEzM,
					const Int_t ITERATIONS )
{

  const Int_t    ROWS        =  ArrayVM.GetNrows() ;
  const Int_t    COLUMNS     =  ArrayVM.GetNcols() ;
  const Float_t  GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
  const Float_t  GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;
  const Float_t  Ratio       =  GRIDSIZER*GRIDSIZER / (GRIDSIZEZ*GRIDSIZEZ) ;
  //const Float_t  Four        =  2.0 + 2.0*Ratio ;

  //Check that number of ROWS and COLUMNS is suitable for a binary expansion

  if ( !IsPowerOfTwo(ROWS-1) )
    { cout << "StMagUtilities::PoissonRelaxation - Error in the number of ROWS.  Must be 2**M - 1" << endl ; exit(1) ; }
  if ( !IsPowerOfTwo(COLUMNS-1) )
    { cout << "StMagUtilities::PoissonRelaxation - Error in the number of COLUMNS.  Must be 2**N - 1" << endl ; exit(1) ; }
  
  // Because performance of this relaxation is important, we access the arrays directly
  Float_t *ArrayE,*ArrayV,*Charge,*SumCharge,*EroverEz ;

  TMatrix  ArrayEM(ROWS,COLUMNS) ;
  TMatrix  SumChargeM(ROWS,COLUMNS) ;
  ArrayE    = ArrayEM.GetMatrixArray() ;
  SumCharge = SumChargeM.GetMatrixArray() ;
  ArrayV    = ArrayVM.GetMatrixArray() ;
  Charge    = ChargeM.GetMatrixArray() ;
  EroverEz  = EroverEzM.GetMatrixArray() ;

  //Solve Poisson's equation in cylindrical coordinates by relaxation technique
  //Allow for different size grid spacing in R and Z directions
  //Use a binary expansion of the matrix to speed up the solution of the problem

  Int_t i_one = (ROWS-1)/4 ;
  Int_t j_one = (COLUMNS-1)/4 ;
  Int_t loops = 1 + (int) ( 0.5 + TMath::Log2( (double) TMath::Max(i_one,j_one) ) ) ;  // Solve for N in 2**N and add one

  Float_t coef1[ROWS],coef2[ROWS];
  memset(coef1,0,ROWS*sizeof(Float_t));
  memset(coef2,0,ROWS*sizeof(Float_t));

  for ( Int_t count = 0 ; count < loops ; count++ ) {  // Do several loops as the matrix expands and the resolution increases

    // array index offsets ending in '__' are units of rows
    // e.g. one__ == 1 row, i__ == i rows
    // array index offset with '__' in the middle are units of rows and columns
    // e.g. i__j == i rows and j columns
    Int_t one__  = i_one*COLUMNS;
    Int_t half__ = one__ / 2;
    Int_t half   = j_one / 2;
        
    Float_t tempGRIDSIZER = GRIDSIZER * i_one ;
    Float_t tempRatio     = Ratio * i_one * i_one / ( j_one * j_one ) ;
    Float_t tempFourth    = 1.0 / (2.0 + 2.0*tempRatio) ;

    for ( Int_t i = i_one ; i < ROWS-1 ; i+=i_one )  {
      Float_t Radius = IFCRadius + i*GRIDSIZER ;
      coef1[i] = 1.0 + tempGRIDSIZER/(2*Radius);
      coef2[i] = 1.0 - tempGRIDSIZER/(2*Radius);
    }

    for ( Int_t i = i_one ; i < ROWS-1 ; i += i_one ) {
      Int_t i__ = i*COLUMNS;
      Float_t Radius = IFCRadius + i*GRIDSIZER ;
      for ( Int_t j = j_one ; j < COLUMNS-1 ; j += j_one ) {
        Int_t i__j = i__ + j;
        if ( i_one == 1 && j_one == 1 ) SumCharge[i__j] = Charge[i__j] ;
        else {           // Add up all enclosed charge within 1/2 unit in all directions
          Float_t weight = 0.0 ;
          Float_t sum    = 0.0 ;
          SumCharge[i__j]= 0.0 ;
          for ( Int_t ii = i-i_one/2 ; ii <= i+i_one/2 ; ii++ ) {
            for ( Int_t jj = j-j_one/2 ; jj <= j+j_one/2 ; jj++ ) {
              if ( ii == i-i_one/2 || ii == i+i_one/2 || jj == j-j_one/2 || jj == j+j_one/2 ) weight = 0.5*Radius ;
              else weight = Radius ;
              SumCharge[i__j]  += Charge[ii*COLUMNS+jj]*weight ;   // Note that this is cylindrical geometry
              sum += weight ;
            }
          }
          SumCharge[i__j] /= sum ;
        }
        SumCharge[i__j] *= tempGRIDSIZER*tempGRIDSIZER; // just saving a step later on
       }
    }

    for ( Int_t k = 1 ; k <= ITERATIONS; k++ ) {               // Solve Poisson's Equation

    //Float_t OverRelax   = 1.0 + TMath::Sqrt( TMath::Cos( (k*TMath::PiOver2())/ITERATIONS ) ) ; // Over-relaxation index, >= 1 but < 2
      Float_t OverRelaxM1 = TMath::Sqrt( TMath::Cos( (k*TMath::PiOver2())/ITERATIONS ) ) ;
      Float_t OverRelaxtempFourth = (1.0 + OverRelaxM1) * tempFourth ;

      for ( Int_t i = i_one ; i < ROWS-1 ; i += i_one ) {
        Int_t i__ = i*COLUMNS;
        for ( Int_t j = j_one ; j < COLUMNS-1 ; j += j_one ) {
          Int_t i__j = i__ + j;

          ArrayV[i__j] = (  coef2[i]       *   ArrayV[i__j - one__]
                          + tempRatio      * ( ArrayV[i__j - j_one] + ArrayV[i__j + j_one] )
                          + coef1[i]       *   ArrayV[i__j + one__] 
                          + SumCharge[i__j] 
                        ) * OverRelaxtempFourth
                        - OverRelaxM1 * ArrayV[i__j] ;

        }
      }
    }

    // After full solution is achieved, copy low resolution solution into higher res array
    if ( i_one > 1 || j_one > 1 ) {
      for ( Int_t i = i_one ; i < ROWS-1 ; i += i_one ) {
        Int_t i__ = i*COLUMNS;
        for ( Int_t j = j_one ; j < COLUMNS-1 ; j += j_one ) {
          Int_t i__j = i__ + j;

          if ( i_one > 1 ) {              
            ArrayV[i__j + half__]            =  ( ArrayV[i__j + one__]        + ArrayV[i__j]        ) / 2 ;
            if ( i == i_one )
              ArrayV[i__j - half__]          =  ( ArrayV[j]                   + ArrayV[one__ + j]   ) / 2 ;
          }
          if ( j_one > 1 ) {
            ArrayV[i__j + half]              =  ( ArrayV[i__j + j_one]        + ArrayV[i__j]        ) / 2 ;
            if ( j == j_one )
              ArrayV[i__j - half]            =  ( ArrayV[i__]                 + ArrayV[i__ + j_one] ) / 2 ;

            if ( i_one > 1 ) { // i_one > 1 && j_one > 1
              ArrayV[i__j + half__ + half]   = ( ArrayV[i__j + one__ + j_one] + ArrayV[i__j]        ) / 2 ;
              if ( i == i_one )
                ArrayV[i__j - half__ - half] = ( ArrayV[j - j_one]            + ArrayV[one__ + j]   ) / 2 ;
              if ( j == j_one )
                ArrayV[i__j - half__ - half] = ( ArrayV[i__ - one__]          + ArrayV[i__ + j_one] ) / 2 ;
              // Note that this leaves a point at the upper left and lower right corners uninitialized.  Not a big deal.
            }
          }
        }
      }
    }


    /* //JT test block for viewing histogams and solutions
       TCanvas*  c1 =  new TCanvas("Volts","Volts",50,50,840,600) ;  // JT test
       c1 -> cd() ;        // JT test
       c1 -> Clear() ;
       TH2F* h1  = new TH2F(ArrayVM)  ;  // JT test  
       h1 -> DrawCopy("lego") ;      // JT test
       ArrayVM -> Draw("lego") ; // JT test
       c1 -> Update() ;
       cout << "Hit any key to continue" << endl ;  // JT test
       Char_t anychar ;    // JT test
       cin  >> anychar ;   // JT test
    */ //End JT test block

    // For asymmetric grid sizes, keep them coarse as long as possible to avoid excessive calculations
    if (i_one > j_one / 2) { i_one = i_one / 2 ; if ( i_one < 1 ) i_one = 1 ; }
    if (j_one > i_one    ) { j_one = j_one / 2 ; if ( j_one < 1 ) j_one = 1 ; }

  }      

  Float_t coef10,coef12 ;
  coef10 = -0.5 / GRIDSIZER ;                // For differential in r
  coef12 = (GRIDSIZEZ/3.0) / (-1*StarMagE) ; // For integrals over z

  Int_t one__ = COLUMNS;

  //Differentiate V(r) and solve for E(r) using special equations for the first and last row
  //Integrate E(r)/E(z) from point of origin to pad plane

  for ( Int_t j = COLUMNS-1 ; j >= 0 ; j-- )  // Count backwards to facilitate integration over Z
    {
      // Differentiate in R
      for ( Int_t i = 1 ; i < ROWS-1 ; i++ ) {
        Int_t i__j = i*COLUMNS + j;
        ArrayE[i__j] = coef10 * ( ArrayV[i__j + one__] - ArrayV[i__j - one__] ) ;
      }
      Int_t r__j = (ROWS-1)*one__ + j;
      ArrayE[j]    = coef10 * ( - ArrayV[2*one__ + j] + 4*ArrayV[one__ + j]    - 3*ArrayV[j]              ) ;
      ArrayE[r__j] = coef10 * ( 3*ArrayV[r__j]        - 4*ArrayV[r__j - one__] +   ArrayV[r__j - 2*one__] ) ; 
      // Integrate over Z
      for ( Int_t i = 0 ; i < ROWS ; i++ ) 
        {
          Int_t Index = 1 ;   // Simpsons rule if N=odd.  If N!=odd then add extra point by trapezoidal rule.  
          Int_t i__  = i*COLUMNS;
          Int_t i__j = i__ + j;
          Int_t i__c = i__ + COLUMNS-1;
          EroverEz[i__j] = 0.0 ;
          if ( j == COLUMNS-2 ) EroverEz[i__j] = coef12 * 1.5 * (ArrayE[i__c - 1] + ArrayE[i__c]) ;
          else if ( j != COLUMNS-1 )
          {
            for ( Int_t k = i__j ; k <= i__c ; k++ ) 
            { 
              EroverEz[i__j]  +=  Index*ArrayE[k] ;
              if ( Index != 4 )  Index = 4; else Index = 2 ;
            }
            if ( Index == 4 )      EroverEz[i__j] -= ArrayE[i__c] ;
            else if ( Index == 2 ) EroverEz[i__j] += (0.5*ArrayE[i__c - 1] - 2.5*ArrayE[i__c]) ;
            EroverEz[i__j] *= coef12 ;
          }
        }
    }

}


//________________________________________


/// 3D - Solve Poisson's Equation in 3D by Relaxation Technique
/*!
    NOTE: In order for this algorith to work, the number of rows and columns must be a power of 2 plus one.  
    The number of ROWS and COLUMNS can be different.

    ROWS       ==  2**M + 1  
    COLUMNS    ==  2**N + 1  
    PHISLICES  ==  Arbitrary but greater than 3

    DeltaPhi in Radians

    SYMMETRY = 0 if no phi symmetries, and no phi boundary conditions
             = 1 if we have reflection symmetry at the boundaries (eg. sector symmetry or half sector symmetries).

 */
void StMagUtilities::Poisson3DRelaxation( TMatrix **ArrayofArrayV, TMatrix **ArrayofCharge, TMatrix **ArrayofEroverEz, 
                                          TMatrix **ArrayofEPhioverEz,
                                          const Int_t PHISLICES, const Float_t DELTAPHI, 
                                          const Int_t ITERATIONS, const Int_t SYMMETRY )
{

  const Int_t    ROWS        =  ArrayofArrayV[0]->GetNrows();
  const Int_t    COLUMNS     =  ArrayofArrayV[0]->GetNcols();  
  const Float_t  GRIDSIZEPHI =  DELTAPHI ;
  const Float_t  GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
  const Float_t  GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;
  const Float_t  RatioPhi    =  GRIDSIZER*GRIDSIZER / (GRIDSIZEPHI*GRIDSIZEPHI) ;
  const Float_t  RatioZ      =  GRIDSIZER*GRIDSIZER / (GRIDSIZEZ*GRIDSIZEZ) ;

  //Check that the number of ROWS and COLUMNS is suitable for a binary expansion
  if ( !IsPowerOfTwo((ROWS-1))    )
  { cout << "StMagUtilities::Poisson3DRelaxation - Error in the number of ROWS.  Must be 2**M - 1" << endl ; exit(1) ; }
  if ( !IsPowerOfTwo((COLUMNS-1)) )
  { cout << "StMagUtilities::Poisson3DRelaxation - Error in the number of COLUMNS.  Must be 2**N - 1" << endl ; exit(1) ; }
  if ( PHISLICES <= 3   )
  { cout << "StMagUtilities::Poisson3DRelaxation - Error in the number of PHISLICES.  Must be larger than 3" << endl ; exit(1) ; }
  
  // Because performance of this relaxation is important, we access the arrays directly
  Float_t *ArrayE,*ArrayV,*ArrayVM,*ArrayVP,*Charge,*SumCharge,*EroverEz,*EPhioverEz ;

  TMatrix ArrayEM(ROWS,COLUMNS) ;
  ArrayE = ArrayEM.GetMatrixArray() ;

  //Solve Poisson's equation in cylindrical coordinates by relaxation technique
  //Allow for different size grid spacing in R and Z directions
  //Use a binary expansion of the matrix to speed up the solution of the problem

  Int_t loops, m_plus, m_minus ;
  Int_t i_one = (ROWS-1)/4 ;
  Int_t j_one = (COLUMNS-1)/4 ;
  loops = TMath::Max(i_one, j_one) ;      // Calculate the number of loops for the binary expansion
  loops = 1 + (int) ( 0.5 + TMath::Log2((double)loops) ) ;  // Solve for N in 2**N
  
  TMatrix *ArrayofSumCharge[1000] ;    // Create temporary arrays to store low resolution charge arrays
  if  ( PHISLICES > 1000 ) { cout << "StMagUtilities::Poisson3D  PHISLICES > 1000 is not allowed (nor wise) " << endl ; exit(1) ; }  
  for ( Int_t i = 0 ; i < PHISLICES ; i++ ) { ArrayofSumCharge[i] = new TMatrix(ROWS,COLUMNS) ; }
  Float_t OverRelaxers[ITERATIONS] ; 
  for ( Int_t k = 1 ; k <= ITERATIONS; k++ ) {
    OverRelaxers[k-1] = 1.0 + TMath::Sqrt( TMath::Cos( (k*TMath::PiOver2())/ITERATIONS ) ) ;    // Over-relaxation index, >= 1 but < 2
  }

  Float_t coef1[ROWS],coef2[ROWS],coef3[ROWS],coef4[ROWS],OverRelaxcoef4[ROWS];
  memset(coef1,0,ROWS*sizeof(Float_t));
  memset(coef2,0,ROWS*sizeof(Float_t));
  memset(coef3,0,ROWS*sizeof(Float_t));
  memset(coef4,0,ROWS*sizeof(Float_t));
  memset(OverRelaxcoef4,0,ROWS*sizeof(Float_t));

  for ( Int_t count = 0 ; count < loops ; count++ ) {  // START the master loop and do the binary expansion
   
    // array index offsets ending in '__' are units of rows
    // e.g. one__ == 1 row, i__ == i rows
    // array index offset with '__' in the middle are units of rows and columns
    // e.g. i__j == i rows and j columns
    Int_t one__  = i_one*COLUMNS;
    Int_t half__ = one__ / 2;
    Int_t half   = j_one / 2;
    Bool_t iWillDivide = (i_one > 1 && i_one >= j_one / 2) ;
    Bool_t jWillDivide = (j_one > 1 && j_one >= i_one / 2) ;
    
    Float_t  tempGRIDSIZER   =  GRIDSIZER  * i_one ;
    Float_t  tempRatioPhi    =  RatioPhi * i_one * i_one ; // Used to be divided by ( m_one * m_one ) when m_one was != 1
    Float_t  tempRatioZ      =  RatioZ   * i_one * i_one / ( j_one * j_one ) ;

    for ( Int_t i = i_one ; i < ROWS-1 ; i+=i_one )  {
      Float_t Radius = IFCRadius + i*GRIDSIZER ;
      coef1[i] = 1.0 + tempGRIDSIZER/(2*Radius);
      coef2[i] = 1.0 - tempGRIDSIZER/(2*Radius);
      coef3[i] = tempRatioPhi/(Radius*Radius);
      coef4[i] = 0.5 / (1.0 + tempRatioZ + coef3[i]);
    }

    for ( Int_t m = 0 ; m < PHISLICES ; m++ ) {
      Charge    = ArrayofCharge[m]->GetMatrixArray() ;
      SumCharge = ArrayofSumCharge[m]->GetMatrixArray() ;
      for ( Int_t i = i_one ; i < ROWS-1 ; i += i_one ) {
        Int_t i__ = i*COLUMNS;
        Float_t Radius = IFCRadius + i*GRIDSIZER ;
        for ( Int_t j = j_one ; j < COLUMNS-1 ; j += j_one ) {
          Int_t i__j = i__ + j;
          if ( i_one == 1 && j_one == 1 ) SumCharge[i__j] = Charge[i__j] ;
          else {           // Add up all enclosed charge within 1/2 unit in all directions
            Float_t weight = 0.0 ;
            Float_t sum    = 0.0 ;
            SumCharge[i__j]= 0.0 ;
            for ( Int_t ii = i-i_one/2 ; ii <= i+i_one/2 ; ii++ ) {
              for ( Int_t jj = j-j_one/2 ; jj <= j+j_one/2 ; jj++ ) {
                if ( ii == i-i_one/2 || ii == i+i_one/2 || jj == j-j_one/2 || jj == j+j_one/2 ) weight = 0.5*Radius ;
                else weight = Radius ; 
                SumCharge[i__j] += Charge[ii*COLUMNS+jj]*weight ;
                sum += weight ;
              }
            }
            SumCharge[i__j] /= sum ;
          }
          SumCharge[i__j] *= tempGRIDSIZER*tempGRIDSIZER; // just saving a step later on
        }
      }
    }

    for ( Int_t k = 1 ; k <= ITERATIONS; k++ ) {
      Float_t OverRelax   = OverRelaxers[k-1];
      Float_t OverRelaxM1 = OverRelax - 1.0 ;
      for ( Int_t i = i_one ; i < ROWS-1 ; i+=i_one ) { 
        OverRelaxcoef4[i] = OverRelax * coef4[i] ;
      }


      for ( Int_t m = 0 ; m < PHISLICES ; m++ ) {

        m_plus  = m + 1;
        m_minus = m - 1 ; 
        if (SYMMETRY==1) {  // Reflection symmetry in phi (e.g. symmetry at sector boundaries, or half sectors, etc.)
          if ( m_plus  > PHISLICES-1 ) m_plus  = PHISLICES - 2 ;
          if ( m_minus < 0 )           m_minus = 1 ;
        }
        else { // No Symmetries in phi, no boundaries, the calculation is continuous across all phi
          if ( m_plus  > PHISLICES-1 ) m_plus  = 0 ;
          if ( m_minus < 0 )           m_minus = PHISLICES - 1 ;
        }
        ArrayV  = ArrayofArrayV[m]->GetMatrixArray();
        ArrayVP = ArrayofArrayV[m_plus]->GetMatrixArray();
        ArrayVM = ArrayofArrayV[m_minus]->GetMatrixArray();
        SumCharge = ArrayofSumCharge[m]->GetMatrixArray() ;
        for ( Int_t i = i_one ; i < ROWS-1 ; i+=i_one )  {
          Int_t i__ = i*COLUMNS;
          for ( Int_t j = j_one ; j < COLUMNS-1 ; j+=j_one ) {
            Int_t i__j = i__ + j;

            ArrayV[i__j] = (  coef2[i]          *   ArrayV[i__j - one__]
                            + tempRatioZ        * ( ArrayV[i__j - j_one]  + ArrayV[i__j + j_one] )
                            + coef1[i]          *   ArrayV[i__j + one__]
                            + coef3[i]          * ( ArrayVP[i__j]         + ArrayVM[i__j]        )
                            + SumCharge[i__j]
                           ) * OverRelaxcoef4[i]
                           - OverRelaxM1*ArrayV[i__j] ;     // Note: over-relax the solution at each step.  This speeds up the convergance.

          }
        }

        if ( k == ITERATIONS && ( iWillDivide || jWillDivide ) ) {       // After full solution is achieved, copy low resolution solution into higher res array
          for ( Int_t i = i_one ; i < ROWS-1 ; i+=i_one )  {
            Int_t i__ = i*COLUMNS;
            for ( Int_t j = j_one ; j < COLUMNS-1 ; j+=j_one ) {
              Int_t i__j = i__ + j;
              
              if ( iWillDivide ) {
                ArrayV[i__j + half__]            =  ( ArrayV[i__j + one__]        + ArrayV[i__j]         ) / 2 ;
                if ( i == i_one )
                  ArrayV[i__j - half__]          =  ( ArrayV[j]                   + ArrayV[one__ + j]    ) / 2 ;
              }

              if ( jWillDivide ) {
                ArrayV[i__j + half]              =  ( ArrayV[i__j + j_one]        + ArrayV[i__j]         ) / 2 ;
                if ( j == j_one )
                  ArrayV[i__j - half]            =  ( ArrayV[i__]                 + ArrayV[i__ + j_one]  ) / 2 ;

                if ( iWillDivide ) { // i_one > 1 && j_one > 1
                  ArrayV[i__j + half__ + half]   = ( ArrayV[i__j + one__ + j_one] + ArrayV[i__j]
                                                   + ArrayV[i__j + one__ ]        + ArrayV[i__j + j_one] ) / 4 ;
                  if ( i == i_one )
                    ArrayV[i__j - half__ - half] = ( ArrayV[j - j_one]            + ArrayV[i__j]
                                                   + ArrayV[j]                    + ArrayV[i__j - j_one] ) / 4 ;
                  else if ( j == j_one )
                    ArrayV[i__j - half__ - half] = ( ArrayV[i__ - one__]          + ArrayV[i__j]
                                                   + ArrayV[i__]                  + ArrayV[i__j - one__] ) / 4 ;
                  // Note that this leaves a point at the upper left and lower right corners uninitialized.  Not a big deal.
                }
              }

            }
          }
        }

      } // m phi slices for-loop
    } // k iterations for-loop

    // For asymmetric grid sizes, keep them coarse as long as possible to avoid excessive calculations
    if (iWillDivide) { i_one = i_one / 2 ; if ( i_one < 1 ) i_one = 1 ; }
    if (jWillDivide) { j_one = j_one / 2 ; if ( j_one < 1 ) j_one = 1 ; }

  }
  

  Float_t coef10,coef11,coef12 ;
  coef10 = -0.5 / GRIDSIZER ;                // For differential in r
  coef11 = -0.5 / GRIDSIZEPHI ;              // For differential in phi
  coef12 = (GRIDSIZEZ/3.0) / (-1*StarMagE) ; // For integrals over z

  Int_t one__ = COLUMNS;

  //Differentiate V(r) and solve for E(r) using special equations for the first and last row
  //Integrate E(r)/E(z) from point of origin to pad plane

  for ( Int_t m = 0 ; m < PHISLICES ; m++ )
    {
      ArrayV = ArrayofArrayV[m]->GetMatrixArray();
      EroverEz = ArrayofEroverEz[m]->GetMatrixArray();
      
      for ( Int_t j = COLUMNS-1 ; j >= 0 ; j-- )  // Count backwards to facilitate integration over Z
        {
          // Differentiate in R
          for ( Int_t i = 1 ; i < ROWS-1 ; i++ ) {
            Int_t i__j = i*COLUMNS + j;
            ArrayE[i__j] = coef10 * ( ArrayV[i__j + one__] - ArrayV[i__j - one__] ) ;
          }
          Int_t r__j = (ROWS-1)*one__ + j;
          ArrayE[j]    = coef10 * ( - ArrayV[2*one__ + j] + 4*ArrayV[one__ + j]    - 3*ArrayV[j]              ) ;
          ArrayE[r__j] = coef10 * ( 3*ArrayV[r__j]        - 4*ArrayV[r__j - one__] +   ArrayV[r__j - 2*one__] ) ;
          // Integrate over Z
          for ( Int_t i = 0 ; i < ROWS ; i++ ) 
            {
              Int_t Index = 1 ;   // Simpsons rule if N=odd.  If N!=odd then add extra point by trapezoidal rule.  
              Int_t i__  = i*COLUMNS;
              Int_t i__j = i__ + j;
              Int_t i__c = i__ + COLUMNS-1;
              EroverEz[i__j] = 0.0;
              if ( j == COLUMNS-2 ) EroverEz[i__j] = coef12 * 1.5 * (ArrayE[i__c - 1] + ArrayE[i__c]) ;
              else if ( j != COLUMNS-1 )
                {
                  for ( Int_t k = i__j ; k <= i__c ; k++ ) 
                    { 
                      EroverEz[i__j]  +=  Index*ArrayE[k] ;
                      if ( Index != 4 )  Index = 4; else Index = 2 ;
                    }
                  if ( Index == 4 )      EroverEz[i__j] -= ArrayE[i__c] ;
                  else if ( Index == 2 ) EroverEz[i__j] += (0.5*ArrayE[i__c - 1] - 2.5*ArrayE[i__c]) ;
                  EroverEz[i__j] *= coef12 ;
                }
            }
        }
      // if ( m == 0 ) { TCanvas*  c1 =  new TCanvas("ErOverEz","ErOverEz",50,50,840,600) ;  c1 -> cd() ;
      // ArrayofEroverEz[m]->Draw("surf") ; } // JT test
    }

  //Differentiate V(r) and solve for E(phi) 
  //Integrate E(r)/E(z) from point of origin to pad plane

  for ( Int_t m = 0 ; m < PHISLICES ; m++ )
    {
      m_plus  = m + 1;
      m_minus = m - 1 ; 
      if (SYMMETRY==1) { // Reflection symmetry in phi (e.g. symmetry at sector boundaries, or half sectors, etc.)
        if ( m_plus  > PHISLICES-1 ) m_plus  = PHISLICES - 2 ;
        if ( m_minus < 0 )           m_minus = 1 ;
      }
      else { // No Symmetries in phi, no boundaries, the calculations is continuous across all phi
        if ( m_plus  > PHISLICES-1 ) m_plus  = 0 ;
        if ( m_minus < 0 )           m_minus = PHISLICES - 1 ;
      }
      ArrayVP =  ArrayofArrayV[m_plus]->GetMatrixArray() ;
      ArrayVM =  ArrayofArrayV[m_minus]->GetMatrixArray() ;
      EPhioverEz =  ArrayofEPhioverEz[m]->GetMatrixArray() ;
      for ( Int_t j = COLUMNS-1 ; j >= 0 ; j-- )  // Count backwards to facilitate integration over Z
        {
          // Differentiate in Phi
          for ( Int_t i = 0 ; i < ROWS ; i++ )  
            {
              Int_t i__j = i*COLUMNS + j;
              Float_t Radius = IFCRadius + i*GRIDSIZER ;
              ArrayE[i__j] = coef11 * ( ArrayVP[i__j] - ArrayVM[i__j] ) / Radius ;
            }
          // Integrate over Z
          for ( Int_t i = 0 ; i < ROWS ; i++ ) 
            {
              Int_t i__  = i*COLUMNS;
              Int_t i__j = i__ + j;
              Int_t i__c = i__ + COLUMNS-1;
              Int_t Index = 1 ;   // Simpsons rule if N=odd.  If N!=odd then add extra point by trapezoidal rule.  
              EPhioverEz[i__j] = 0.0 ;
              if ( j == COLUMNS-2 ) EPhioverEz[i__j] = coef12 * 1.5 * (ArrayE[i__c - 1] + ArrayE[i__c]) ;
              else if ( j != COLUMNS-1 )
                {
                  for ( Int_t k = i__j ; k <= i__c ; k++ ) 
                    { 
                      EPhioverEz[i__j]  +=  Index*ArrayE[k] ;
                      if ( Index != 4 )  Index = 4; else Index = 2 ;
                    }
                  if ( Index == 4 )      EPhioverEz[i__j] -= ArrayE[i__c] ;
                  else if ( Index == 2 ) EPhioverEz[i__j] +=  (0.5*ArrayE[i__c - 1] - 2.5*ArrayE[i__c]) ;
                  EPhioverEz[i__j] *= coef12 ;
                }
            }
        }
      // if ( m == 5 ) { TCanvas* c2 =  new TCanvas("ArrayE","ArrayE",50,50,840,600) ;  c2 -> cd() ;
      // ArrayEM.Draw("surf") ; } // JT test
    }
  

  for ( Int_t m = 0 ; m < PHISLICES ; m++ )
    {
      ArrayofSumCharge[m] -> Delete() ;
    }
  
}

//________________________________________


/// Convert from the old (Uniform) space charge correction to the new (1/R**2) space charge correction. 
/*! 
  Applicable to 200 GeV Au+Au data that is on the P02ge (and other) microDSTs.
  Given the charge and momentum of a particle and a point on the circular path described by the particle , 
  this function returns the new position of the point (cm) and the new momentum of the particle (GeV).  This 
  is done by undoing the old space charge corrections and then applying the new space charge corrections.
  
  Input x[3], p[3] and return x_new[3], p_new[3].        x[3] in cm and p[3] in GeV.
  
  The program works by calculating the hits on the TPC rows for
  the input track, distorts the hits according to the new presciption, and then refits the new hits to find
  the new track parameters.  If the track is a primary track (PrimaryOrGlobal == 0) then x[3] is assumed to
  be the vertex and it is included in the refit.  If the track is a global track (PrimaryOrGlobal == 1) then
  x[3] is assumed to lie somewhere (anywhere) on the track but it is not included in the fit.  For a global
  track, x[3] must lie on the track because it is used to determine where the track flies (ie. angle phi).
  
  PrimaryOrGlobal = 0   for a primary track.
  PrimaryOrGlobal = 1   for a global track.  You can also use the "Prime" enumeration in the .h file.
  
  The code attempts to be as realistic as possible when it does the refit.  Therefore, it asks you for
  the hit masks from the microDSTs.  These masks tell you which TPC rows were used in the original track fit.
  For future reference, the masks come in two words.  The first word covers TPC rows 1-24 and the second 
  word covers rows 25-45.  The first 8 bits of the first word are reserved for the FTPC and therefore
  0xFFFFFF00, 0x1FFFFF represent all 45 rows of the TPC and no SVT or SSD hits. (NB: this is dependent
  on the implementation of the topology map, and may be subject to change!)
  
  VertexError is quoted in cm (RMS). It is for experts.  If you are working with primary tracks, the vertex
  is included in the fit.  The true error bar is multiplcity dependent.  (sigma**2 increase linearly with mult).
  So you can calculate this, external to the function, and then work with a realistic vertex error bar if
  you wish to do it.  200 microns error is a good average value for central Au-Au events.
*/
void StMagUtilities::FixSpaceChargeDistortion ( const Int_t Charge, const Float_t x[3], const Float_t p[3], 
					        const Prime PrimaryOrGlobal, Float_t x_new[3], Float_t p_new[3],
		         const unsigned int RowMask1  , const unsigned int RowMask2 ,const Float_t VertexError)
{

  x_new[0] = x[0] ; x_new[1] = x[1] ; x_new[2] = x[2] ;          // Default is to do nothing
  p_new[0] = p[0] ; p_new[1] = p[1] ; p_new[2] = p[2] ;
  Int_t sec;
  SectorNumber( sec, x ) ;

  // Return default values if passed a whacko input value (i.e. infinite or NaN)
  if ( finite((double)Charge)*finite(x[0])*finite(x[1])*finite(x[2])*finite(p[0])*finite(p[1])*finite(p[2]) == 0 ) return ;

  const Int_t   ROWS   = TPCROWS[sec-1] ;               // Total number of TPC rows per sector (Inner + Outer)
  const Float_t TestRadius =  77.00 ;      // A random test radius inside the TPC to compare which way the track is going

  Int_t    ChargeB ;
  Float_t  B[3], Rotation, Direction, xx[3], xxprime[3] ;
  Double_t Xtrack[ROWS], Ytrack[ROWS], Ztrack[ROWS] ;
  Double_t Xtrack1[ROWS], Ytrack1[ROWS], Ztrack1[ROWS] ;
  Double_t R[ROWS], dX[ROWS], dY[ROWS], C0, X0, Y0, R0, Pt, R2, theta, theta0, DeltaTheta ;
  Double_t Xprime[ROWS+1], Yprime[ROWS+1], eX[ROWS+1], eY[ROWS+1] ;  // Extra index is to accomodate the vertex in the fit for primaries
 
  BFieldTpc(x,B) ;
  ChargeB  = Charge * TMath::Sign((int)1,(int)(B[2]*1000)) ;
  Pt = TMath::Sqrt( p[0]*p[0] + p[1]*p[1] ) ;
  R0 = TMath::Abs( 1000.0 * Pt / ( 0.299792 * B[2] ) ) ;     // P in GeV, R in cm, B in kGauss
  X0 = x[0] + ChargeB * p[1] * R0 / Pt ;
  Y0 = x[1] - ChargeB * p[0] * R0 / Pt ; 
  Rotation = TMath::Sign( (double)1.0, (x[0]-X0)*p[1] - (x[1]-Y0)*p[0] ) ; 

  memcpy(R,TPCROWR[sec-1],ROWS*sizeof(Double_t));
  // Not correct because TPC rows aren't circles ... but we dont' care

  if (Y0 == 0.0)  Direction = TMath::Sign((float)1.0,p[1]) ;
  else
    {
      Direction = 1.0 ;
      R2 = TestRadius * TestRadius ;
      C0 = ( R2 - R0*R0 + X0*X0 + Y0*Y0 ) ;                                // Intermediate constant
      Double_t X1 = 0.5 * ( C0*X0 - TMath::Sqrt( TMath::Abs( C0*C0*X0*X0 - (C0*C0 - 4*Y0*Y0*R2) * (X0*X0+Y0*Y0) ) ) ) / (X0*X0 + Y0*Y0) ;
      Double_t Y1 = ( R2 - R0*R0 - 2*X0*X1 + X0*X0 + Y0*Y0 ) / ( 2 * Y0 ) ;
      Double_t X2 = 0.5 * ( C0*X0 + TMath::Sqrt( TMath::Abs( C0*C0*X0*X0 - (C0*C0 - 4*Y0*Y0*R2) * (X0*X0+Y0*Y0) ) ) ) / (X0*X0 + Y0*Y0) ;
      Double_t Y2 = ( R2 - R0*R0 - 2*X0*X2 + X0*X0 + Y0*Y0 ) / ( 2 * Y0 ) ;
      if ( X2*p[0] +  Y2*p[1]  <  X1*p[0] + Y1*p[1] ) Direction = -1.0 ;   // Test which of two directions the particle goes on the circle
    }
  
  theta0    = TMath::ATan2( (x[1]-Y0) , (x[0]-X0) ) ;  // Assume that x[3] is the vertex if its a primary track
  Xprime[0] = theta0 ;
  Yprime[0] = 0.0 ;
  eX[0] = 0.5 / R0 ;
  eY[0] = VertexError ;    // In centimeters.  GVB studies suggest average vertex resolution 2x worse than TPC point

  Int_t index = -1 ;
  unsigned int OneBit = 1 ;
  for ( Int_t i = 0 ; i < ROWS ; i++ )
    {
      if ( ( i < 24 ) && ( ( RowMask1 & OneBit<<(i+8) ) == 0 ) ) continue ;
      if ( ( i >= 24 ) && ( ( RowMask2 & OneBit<<(i-24) ) == 0 ) ) continue ;
      index++ ;
      C0 = ( R[i]*R[i] - R0*R0 + X0*X0 + Y0*Y0 ) ;     // Intermediate constant
      if (Y0 == 0.0) Xtrack[index]  =  0.5 * C0 / X0 ;
      else           Xtrack[index]  =  0.5*( C0*X0 + Direction*TMath::Sqrt( TMath::Abs( C0*C0*X0*X0 - (C0*C0-4*Y0*Y0*R[i]*R[i])*(X0*X0+Y0*Y0) )) ) 
		                     / (X0*X0+Y0*Y0) ;
      if (Y0 == 0.0) Ytrack[index]  =  Direction * TMath::Sqrt( TMath::Abs( R[i]*R[i] - Xtrack[index]*Xtrack[index] ) ) ;
      else           Ytrack[index]  =  ( R[i]*R[i] - R0*R0 - 2*X0*Xtrack[index] + X0*X0 + Y0*Y0 ) / ( 2 * Y0 ) ;
      DeltaTheta  =  TMath::ATan2(x[1]-Y0,x[0]-X0) - TMath::ATan2(Ytrack[index]-Y0,Xtrack[index]-X0) ;
      while ( DeltaTheta < -1*TMath::Pi() ) DeltaTheta += TMath::TwoPi() ; 
      while ( DeltaTheta >=   TMath::Pi() ) DeltaTheta -= TMath::TwoPi() ; 
      Ztrack[index]  =   x[2] - Rotation*DeltaTheta*R0*p[2] / Pt ;
      xx[0] = Xtrack[index] ; xx[1] = Ytrack[index] ; xx[2] = Ztrack[index] ;
      UndoSpaceChargeR0Distortion(xx,xxprime) ;
      xx[0] = Xtrack[index] - (xxprime[0]-xx[0]) ; xx[1] = Ytrack[index] - (xxprime[1]-xx[1]) ; xx[2] = Ztrack[index] - (xxprime[2]-xx[2]) ;
      UndoSpaceChargeR2Distortion(xx,xxprime) ;
      Xtrack1[index] = xxprime[0] ; Ytrack1[index] = xxprime[1] ; Ztrack1[index] = xxprime[2] ;
      theta = TMath::ATan2( (Ytrack[index]-Y0) , (Xtrack[index]-X0) ) ; // Note (theta-theta0) must stay in range -pi,pi 
      while ( (theta - theta0) <  -1*TMath::Pi() )   theta = theta + 2*TMath::Pi() ;
      while ( (theta - theta0) >=    TMath::Pi() )   theta = theta - 2*TMath::Pi() ;
      dX[index] = Xtrack1[index] - Xtrack[index] ;
      dY[index] = Ytrack1[index] - Ytrack[index] ;
      Xprime[index+1] = theta ;          // First location in these arrays used for the vertex if its a primary track
      Yprime[index+1] = dY[index]*TMath::Sin(theta) + dX[index]*TMath::Cos(theta) ;
      eX[index+1] = 0.5 / R0 ;
      eY[index+1] = 0.0100 ;
    }
  if ( index == -1 ) return ;

  TGraphErrors gre(index-PrimaryOrGlobal+2,&Xprime[PrimaryOrGlobal],&Yprime[PrimaryOrGlobal],&eX[PrimaryOrGlobal],&eY[PrimaryOrGlobal]) ;
  TF1 FIT("myFIT", "[0] + [1]*sin(x) + [2]*cos(x)" );
  FIT.SetParameter( 0, 0. );  
  FIT.SetParameter( 1, 0. );  
  FIT.SetParameter( 2, 0. );  
  gre.Fit("myFIT","NQ") ;
  /*
  // Begin debugging plots
  gre.Fit("myFIT","Q") ;  // Comment out previous gre.fit in order to see the fit on the plots
  TCanvas* c1 = new TCanvas("A Simple Fit","The Fit", 250, 10, 700, 500) ;
  c1  -> cd() ;
  gre.Draw("A*") ;
  c1  -> Update() ;

  TCanvas* c2  = new TCanvas("The circles are OK","The circles ", 250, 800, 700, 500) ;
  TGraph* gra  = new TGraph(index+1,Xtrack,Ytrack) ;
  c2  -> cd() ;
  gra -> SetMaximum(200) ;
  gra -> SetMinimum(-200) ;
  gra -> Draw("A*") ;  // Have to draw twice in order to get the Axis (?)
  gra -> GetXaxis() -> SetLimits(-200.,200.) ;
  gra -> Draw("A*") ;
  c2  -> Update() ;
  // End debugging plots
  */
  Double_t X0_new  =  X0 + FIT.GetParameter( 2 ) ;
  Double_t Y0_new  =  Y0 + FIT.GetParameter( 1 ) ;
  Double_t R0_new  =  R0 + FIT.GetParameter( 0 ) ;  
  Double_t Pt_new  =  TMath::Abs( R0_new * 0.299792 * B[2] / 1000. ) ;     // P in GeV, R in cm, B in kGauss

  if ( TMath::Sqrt( x[0]*x[0]+x[1]*x[1] ) <= IFCRadius ) 
    {  x_new[0] = x[0] ;  x_new[1] = x[1] ;  x_new[2] = x[2] ; } 
  else
    {
      UndoSpaceChargeR0Distortion(x,xxprime) ;
      xx[0] = x[0] - (xxprime[0]-x[0]) ;  xx[1] = x[1] - (xxprime[1]-x[1]) ;  xx[2] = x[2] - (xxprime[2]-x[2]) ;
      UndoSpaceChargeR2Distortion(xx,x_new) ;
    }

  Int_t count = 0 ;  p_new[2] = 0.0 ;
  for ( Int_t i = 0 ; i < index+1 ; i++ )
    {
      DeltaTheta  =  (TMath::ATan2(Ytrack1[i]-Y0_new,Xtrack1[i]-X0_new)-TMath::ATan2(x_new[1]-Y0_new,x_new[0]-X0_new)) ;
      while ( DeltaTheta < -1*TMath::Pi() ) DeltaTheta += TMath::TwoPi() ; 
      while ( DeltaTheta >=   TMath::Pi() ) DeltaTheta -= TMath::TwoPi() ; 
      if ( DeltaTheta != 0 )  {  p_new[2] += (Ztrack1[i]-x_new[2]) / DeltaTheta ;   count += 1 ;  }
    }

  p_new[0]  = Pt_new * ( x_new[1] - Y0_new ) / ( ChargeB * R0_new ) ;   
  p_new[1]  = Pt_new * ( X0_new - x_new[0] ) / ( ChargeB * R0_new ) ;
  p_new[2] *= Pt_new / ( Rotation * R0_new * count ) ;

}


//________________________________________



/// Apply the (1/R**2) space charge correction to selected data from the microDSTs. 
/*! 
  Given the charge and momentum of a particle and a point on the circular path described by the particle , 
  this function returns the new position of the point (cm) and the new momentum of the particle (GeV).  
  The momentum p[] must be the momentum at the point x[].
  
  Input x[], p[] and return x_new[], p_new[].        x[] in cm and p[] in GeV.
  
  The program works by calculating the hits on the TPC rows for the input track, removes the distortion 
  from the hits according to the 1/R**2 spacecharge presciption, and then refits the new hits to find
  the new track parameters.  If the track is a primary track (PrimaryOrGlobal == 0) then x[] is assumed to
  be the vertex and it is included in the refit.  If the track is a global track (PrimaryOrGlobal == 1) then
  x[] is assumed to lie somewhere (anywhere) on the track but it is not included in the fit.  For a global
  track, x[] must lie on the track because it is used to determine where the track flies (ie. angle phi).
  
  PrimaryOrGlobal = 0   for a primary track.
  PrimaryOrGlobal = 1   for a global track.  You can also use the "Prime" enumeration in the .h file.
  
  The code attempts to be as realistic as possible when it does the refit.  Therefore, it asks you for
  the hit masks from the microDSTs.  These masks tell you which TPC rows were used in the original track fit.
  For future reference, the masks come in two words.  The first word covers TPC rows 1-24 and the second 
  word covers rows 25-45.  The first 8 bits of the first word are reserved for the FTPC and therefore
  0xFFFFFF00, 0x1FFFFF represent all 45 rows of the TPC and no SVT or SSD hits. (NB: this is dependent
  on the implementation of the topology map, and may be subject to change!)
  
  VertexError is quoted in cm (RMS). It is for experts.  If you are working with primary tracks, the vertex
  is included in the fit.  The true error bar is multiplcity dependent.  (sigma**2 increase linearly with mult).
  So you can calculate this, external to the function, and then work with a realistic vertex error bar if
  you wish to do it.  200 microns error is a good average value for central Au-Au events.
*/
void StMagUtilities::ApplySpaceChargeDistortion (const Double_t sc, const Int_t Charge, const Float_t x[3], const Float_t p[3],
                                            const Prime PrimaryOrGlobal, Int_t &new_Charge, Float_t x_new[3], Float_t p_new[3],
				   const unsigned int RowMask1, const unsigned int RowMask2, const Float_t VertexError )
{

   x_new[0] = x[0] ; x_new[1] = x[1] ; x_new[2] = x[2] ;         //  Default is to do nothing
   p_new[0] = p[0] ; p_new[1] = p[1] ; p_new[2] = p[2] ;
   Int_t sec;
   SectorNumber(sec, x);
   // Return default values if passed a whacko input value (i.e. infinite or NaN)
   if ( finite((double)Charge)*finite(x[0])*finite(x[1])*finite(x[2])*finite(p[0])*finite(p[1])*finite(p[2]) == 0 ) return ;

   const Float_t InnerOuterRatio = 0.6 ; // Ratio of size of the inner pads to the outer pads (real world == 0.5, GVB likes 0.6)
   const Int_t   ROWS     = TPCROWS[sec-1]  ;        // Total number of TPC rows per sector (Inner + Outer)
   const Int_t   RefIndex =  7  ;        // Refindex 7 (TPCRow 8) is about where 1/R**2 has no effect on points (~97 cm radius).
   const Int_t   MinHits  = 15  ;        // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   DEBUG    =  0  ;        // Turn on debugging statements and plots

   Int_t    ChargeB ;
   Float_t  B[3], Direction, xx[3], xxprime[3] ;
   Double_t Xreference, Yreference ;
   Double_t Xtrack[ROWS], Ytrack[ROWS], Ztrack[ROWS] ;
   Double_t R[ROWS], C0, X0, Y0, R0, Pt, R2, DeltaTheta ;
   Double_t Xprime[ROWS+1], Yprime[ROWS+1], Zprime[ROWS+1], dX[ROWS+1], dY[ROWS+1] ;  
   Double_t U[ROWS+1], V[ROWS+1], eU[ROWS+1], eV[ROWS+1] ;  
   // Extra index is to accomodate the vertex in the fit for primaries

   // Temporarily overide settings for space charge data (only)
   St_spaceChargeCorC* tempfSpaceChargeR2 = fSpaceChargeR2 ;
   Double_t tempSpaceChargeR2 = SpaceChargeR2 ;
   ManualSpaceChargeR2(sc,SpaceChargeEWRatio); // Set a custom value of the spacecharge parameter but keep previous E/W ratio
   
   BFieldTpc(x,B) ;
   ChargeB  = Charge * TMath::Sign((int)1,(int)(B[2]*1000)) ;
   Pt = TMath::Sqrt( p[0]*p[0] + p[1]*p[1] ) ;
   R0 = TMath::Abs( 1000.0 * Pt / ( 0.299792 * B[2] ) ) ;     // P in GeV, R in cm, B in kGauss
   X0 = x[0] + ChargeB * p[1] * R0 / Pt ;
   Y0 = x[1] - ChargeB * p[0] * R0 / Pt ;

   memcpy(R,TPCROWR[sec-1],ROWS*sizeof(Double_t));
   // Not correct because TPC rows aren't circles ... but we dont' care

   // Test which of the two directions the particle goes on the circle
   if (TMath::Abs(Y0) < 0.001 )  Direction = TMath::Sign( (float)1.0, p[1] ) ;  
   else
     {
       Direction = 1.0 ;
       R2 = R[RefIndex]*R[RefIndex] ;
       C0 = ( R2 - R0*R0 + X0*X0 + Y0*Y0 ) ;                                // Intermediate constant
       Double_t X1 = 0.5 * ( C0*X0 - TMath::Sqrt( TMath::Abs( C0*C0*X0*X0 - (C0*C0 - 4*Y0*Y0*R2) * (X0*X0+Y0*Y0) ) ) ) 
	                   / (X0*X0 + Y0*Y0) ;
       Double_t Y1 = ( R2 - R0*R0 - 2*X0*X1 + X0*X0 + Y0*Y0 ) / ( 2 * Y0 ) ;
       Double_t X2 = 0.5 * ( C0*X0 + TMath::Sqrt( TMath::Abs( C0*C0*X0*X0 - (C0*C0 - 4*Y0*Y0*R2) * (X0*X0+Y0*Y0) ) ) ) 
	                   / (X0*X0 + Y0*Y0) ;
       Double_t Y2 = ( R2 - R0*R0 - 2*X0*X2 + X0*X0 + Y0*Y0 ) / ( 2 * Y0 ) ;
       if ( X2*p[0] +  Y2*p[1]  <  X1*p[0] + Y1*p[1] ) Direction = -1.0 ;   
     }

   Xreference = Yreference = 0.0 ;
   for ( Int_t i = 0 ; i < ROWS ; i++ )
     {
       C0 = ( R[i]*R[i] - R0*R0 + X0*X0 + Y0*Y0 ) ;     // Intermediate constant
       if ( TMath::Abs(Y0) < 0.001 ) Xtrack[i]  =  0.5 * C0 / X0 ;     // Create circular tracks and record hits on pad rows
       else           Xtrack[i]  =  0.5*( C0*X0 + Direction*TMath::Sqrt( TMath::Abs( C0*C0*X0*X0 - 
					    (C0*C0-4*Y0*Y0*R[i]*R[i])*(X0*X0+Y0*Y0) )) ) / (X0*X0+Y0*Y0) ;
       if ( TMath::Abs(Y0) < 0.001 ) Ytrack[i]  =  Direction * TMath::Sqrt( TMath::Abs( R[i]*R[i] - Xtrack[i]*Xtrack[i] ) ) ;
       else           Ytrack[i]  =  ( R[i]*R[i] - R0*R0 - 2*X0*Xtrack[i] + X0*X0 + Y0*Y0 ) / ( 2 * Y0 ) ;
       DeltaTheta  =  TMath::ATan2(x[1]-Y0,x[0]-X0) - TMath::ATan2(Ytrack[i]-Y0,Xtrack[i]-X0) ;
       while ( DeltaTheta < -1*TMath::Pi() ) DeltaTheta += TMath::TwoPi() ;
       while ( DeltaTheta >=   TMath::Pi() ) DeltaTheta -= TMath::TwoPi() ;
       Ztrack[i]  =   x[2] + ChargeB*DeltaTheta*R0*p[2] / Pt ;
       xx[0] = Xtrack[i] ; xx[1] = Ytrack[i] ; xx[2] = Ztrack[i] ;
       //UndoShortedRingDistortion(xx,xxprime) ;        // JT test of shorted ring distortion
       UndoSpaceChargeR2Distortion(xx,xxprime) ;        // Undo the distortion for the hits
       // JT Test ... Note that GridLeak/3DGridLeak do not appear here ... should they?
       Xtrack[i] = xxprime[0] ; Ytrack[i] = xxprime[1] ;  Ztrack[i] = xxprime[2] ;  // This line to undo the distortion
       //Xtrack[i] = 2*xx[0] - xxprime[0] ; Ytrack[i] = 2*xx[1] - xxprime[1] ;  Ztrack[i] = 2*xx[2] - xxprime[2] ; // JT test
       if ( i == RefIndex )
	 {
	   Xreference = Xtrack[i] ;  // This point on the circle is the reference for the rest of the fit
	   Yreference = Ytrack[i] ;  // Note: we must run through all TPC Rows to find this point.  Can't skip a row.
	 }
     }
   
   Int_t Index = 0 ;
   unsigned int OneBit = 1 ;
   for ( Int_t i = 0 ; i < ROWS ; i++ )  // Delete rows not in the bit masks
     {
       if ( ( i < 24  ) && (( RowMask1 & OneBit<<(i+8)  ) == 0 )) continue ;  // Skip this row if not in bit mask
       if ( ( i >= 24 ) && (( RowMask2 & OneBit<<(i-24) ) == 0 )) continue ;  // Skip this row if not in bit mask
       Index++ ;    // Start counting at 1 to leave room for the vertex point.
       Xprime[Index] = Xtrack[i] ; Yprime[Index] = Ytrack[i] ; Zprime[Index] = Ztrack[i] ;
       dX[Index] = 0.2 ; dY[Index] = 1.0 ;
       // Inner pads are smaller, but noisier, in the real world. Toy model requires adjustment to match STAR tracker.
       if ( i < INNER[sec-1] ) { dX[Index] *= InnerOuterRatio ; dY[Index] *= InnerOuterRatio ; } ;  
     }
   
   // Fill in the vertex location.  These will only be used if we have a primary track.
   Xprime[0] = x[0] ;  Yprime[0] = x[1] ; Zprime[0] = x[2] ; dX[0] = VertexError ; dY[0] = VertexError ; 

   // Transform into U,V space so circles in x,y space lie on a straight line in U,V space
   Int_t count = -1 ;                                       // Count number of active rows
   for ( Int_t i = PrimaryOrGlobal ; i < Index+1 ; i++ )  
     {
       Double_t zero = 0.001 ;         // Check divide by zero ... not a very tight constraint in this case.
       Double_t displacement2 ;

       displacement2 =
	 (Xprime[i]-Xreference)*(Xprime[i]-Xreference) + (Yprime[i]-Yreference)*(Yprime[i]-Yreference) ;
       
       if ( displacement2 > zero )  
	 {
	   count ++ ;            // reference point not included in the arrays for fitting (below)
	   U[count]  = ( Xprime[i] - Xreference ) / displacement2 ;
	   V[count]  = ( Yprime[i] - Yreference ) / displacement2 ;
	   eU[count] = dX[i] / displacement2 ;
	   eV[count] = dY[i] / displacement2 ; 
	 }
     }

   if ( count < MinHits ) return ;                      // No action if too few hits
   
   TGraphErrors gre( count+1, U, V, eU, eV ) ;     
   gre.Fit("pol1","Q") ;
   TF1 *fit = gre.GetFunction("pol1" ) ;
   
   if ( DEBUG ) 
     { // Begin debugging plots
       TCanvas* c1  = new TCanvas("A Simple Fit","The Fit", 250, 10, 700, 500) ;
       c1  -> cd() ;
       gre.Draw("A*") ;
       c1  -> Update() ;
       TCanvas* c2  = new TCanvas("The circles are OK","The circles ", 250, 800, 700, 500) ;
       TGraph* gra  = new TGraph( Index-PrimaryOrGlobal+1, &Xprime[PrimaryOrGlobal], &Yprime[PrimaryOrGlobal] ) ;
       c2  -> cd() ;
       gra -> SetMaximum(200)  ;
       gra -> SetMinimum(-200) ;
       gra -> Draw("A*") ;  // Have to draw twice in order to get the Axis (?)
       gra -> GetXaxis() -> SetLimits(-200.,200.) ;
       gra -> Draw("A*") ;
       c2  -> Update() ;
     } // End debugging plots 
       
   Double_t X0_new  =  Xreference - ( fit->GetParameter(1) / ( 2.0 * fit->GetParameter(0) ) )  ;
   Double_t Y0_new  =  Yreference + ( 1.0 / ( 2.0 * fit->GetParameter(0) ) ) ;
   Double_t R0_new  =  TMath::Sqrt( (Xreference-X0_new)*(Xreference-X0_new) + (Yreference-Y0_new)*(Yreference-Y0_new) ) ;
   Double_t Pt_new  =  TMath::Abs ( R0_new * 0.299792 * B[2] / 1000. ) ;   
   //Double_t DCA_new =  TMath::Sqrt( X0_new*X0_new + Y0_new*Y0_new ) - R0_new ;  // Negative means (0,0) is inside the circle

   //cout << "DCA (from inside) = " << DCA_new << endl ; // JT test

   // P in GeV, R in cm, B in kGauss
   if ( TMath::Sqrt( x[0]*x[0]+x[1]*x[1] ) <= IFCRadius )
     {  x_new[0] = x[0] ;  x_new[1] = x[1] ;  x_new[2] = x[2] ; }
   else
     {
       UndoSpaceChargeR2Distortion(x,x_new) ;
     }

   Int_t icount = 0 ;  p_new[2] = 0.0 ;
   for ( Int_t i = 0 ; i < Index+1 ; i++ )
     {
       DeltaTheta  = (TMath::ATan2(Yprime[i]-Y0_new,Xprime[i]-X0_new)-TMath::ATan2(x_new[1]-Y0_new,x_new[0]-X0_new)) ;
       while ( DeltaTheta < -1*TMath::Pi() ) DeltaTheta += TMath::TwoPi() ;
       while ( DeltaTheta >=   TMath::Pi() ) DeltaTheta -= TMath::TwoPi() ;
       if ( DeltaTheta != 0 )  {  p_new[2] += (Zprime[i]-x_new[2]) / DeltaTheta ;   icount += 1 ;  }
     }

   p_new[0]   = Pt_new * ( x_new[1] - Y0_new ) / ( ChargeB * R0_new ) ;
   p_new[1]   = Pt_new * ( X0_new - x_new[0] ) / ( ChargeB * R0_new ) ;
   p_new[2]  *= Pt_new / ( -1 * ChargeB * R0_new * icount ) ;

   // Check if the charge of the track changed due to the distortions
   Float_t change = TMath::Abs( TMath::ATan2(Y0,X0) - TMath::ATan2(Y0_new,X0_new) ) ;
   if ( change > 0.9*TMath::Pi() && change < 1.1*TMath::Pi() ) new_Charge = -1 * Charge ;
   else  new_Charge = Charge ;

   // Restore settings for spacechargeR2
   fSpaceChargeR2 = tempfSpaceChargeR2 ;
   SpaceChargeR2 = tempSpaceChargeR2 ;
   
}


/// PredictSpaceCharge - Input Physical-Signed DCA and get back spacecharge parameter plus a success or failure flag.
/*!
Add comments here.
TPC Hits only.  Does not include SVT or SSD or any other inner tracking detectors.
*/
Int_t StMagUtilities::PredictSpaceChargeDistortion (Int_t sec, Int_t Charge, Float_t Pt, Float_t VertexZ, Float_t PseudoRapidity, 
	       Float_t DCA,  const unsigned int RowMask1, const unsigned int RowMask2, Float_t &pSpace )
{
   const Int_t   ROWS             =  TPCROWS[sec-1]  ;       // Total number of TPC rows per sector (Inner + Outer)
   const Int_t   RefIndex         =   7  ;       // Refindex 7 (TPCRow 8) is about where 1/R**2 has no effect on points (~97 cm)
   const Int_t   MinInnerTPCHits  =   5  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   MinOuterTPCHits  =  10  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   DEBUG            =   0  ;       // Turn on debugging statements and plots

   unsigned int OneBit = 1 ;
   Int_t InnerTPCHits = 0, OuterTPCHits = 0 ;
   for ( Int_t i = 0 ; i < ROWS ; i++ ) 
     {
       if ( i < INNER[sec-1] )
	 {
	   if ( ( i < 24  ) && (( RowMask1 & OneBit<<(i+8)  ) != 0 )) InnerTPCHits++ ;  
	   if ( ( i >= 24 ) && (( RowMask2 & OneBit<<(i-24) ) != 0 )) InnerTPCHits++ ;  
	 }
       else
	 {
	   if ( ( i < 24  ) && (( RowMask1 & OneBit<<(i+8)  ) != 0 )) OuterTPCHits++ ;  
	   if ( ( i >= 24 ) && (( RowMask2 & OneBit<<(i-24) ) != 0 )) OuterTPCHits++ ;  
	 }
     }

   pSpace  = 0 ;

   if ( (Pt < 0.3) || (Pt > 2.0) )                           return(1) ; // Fail
   if ( (VertexZ < -50) || (VertexZ > 50) )                  return(2) ; // Fail
   if ( (PseudoRapidity < -1.0) || (PseudoRapidity > 1.0) )  return(3) ; // Fail
   if ( (Charge != 1) && (Charge != -1) )                    return(4) ; // Fail
   if ( (DCA < -4.0) || (DCA > 4.0) )                        return(5) ; // Fail
   if ( InnerTPCHits < MinInnerTPCHits )                     return(6) ; // No action if too few hits in the TPC   
   if ( OuterTPCHits < MinOuterTPCHits )                     return(7) ; // No action if too few hits in the TPC   

   Int_t    ChargeB ;
   Float_t  B[3], Direction, xx[3], xxprime[3] ;
   Double_t Xreference, Yreference ;
   Double_t Xtrack[ROWS], Ytrack[ROWS], Ztrack[ROWS] ;
   Double_t R[ROWS], C0, X0, Y0, R0, Pz_over_Pt, Z_coef, DeltaTheta ;
   Double_t Xprime[ROWS+1], Yprime[ROWS+1], /* Zprime[ROWS+1], */ dX[ROWS+1], dY[ROWS+1] ;  
   Double_t U[ROWS+1], V[ROWS+1], eU[ROWS+1], eV[ROWS+1] ;  

   // Temporarily overide settings for space charge data (only)
   St_spaceChargeCorC* tempfSpaceChargeR2 = fSpaceChargeR2 ;
   Double_t tempSpaceChargeR2 = SpaceChargeR2 ;
   if (!useManualSCForPredict) ManualSpaceChargeR2(0.01,SpaceChargeEWRatio); // Set "medium to large" value of the spacecharge parameter for tests, not critical.
                                                 // but keep EWRatio that was previously defined
   Float_t x[3] = { 0, 0, 0 } ;
   BFieldTpc(x,B) ;
   ChargeB  = Charge * TMath::Sign((int)1,(int)(B[2]*1000)) ;
   R0 = TMath::Abs( 1000.0 * Pt / ( 0.299792 * B[2] ) ) ;     // P in GeV, R in cm, B in kGauss
   X0 = ChargeB *  0.707107 * R0  ;   // Assume a test particle that shoots out at 45 degrees
   Y0 = ChargeB * -0.707107 * R0  ;
   Pz_over_Pt = TMath::SinH(PseudoRapidity) ;
   Z_coef = ChargeB*R0*Pz_over_Pt ;
 
   memcpy(R,TPCROWR[sec-1],ROWS*sizeof(Double_t));
   // Not correct because TPC rows aren't circles ... but we dont' care

   Float_t InnerOuterRatio = 0.0 ; // JT test. Ratio of size of the inner pads to the outer pads (Set after daisy chain, below)
   Xreference = Yreference = 0.0 ;
   Direction = 1.0 ; // Choose sqrt solution so ray shoots out at 45 degrees
   for ( Int_t i = 0 ; i < ROWS ; i++ )
     {
       C0 = ( R[i]*R[i] - R0*R0 + X0*X0 + Y0*Y0 ) ;     // Intermediate constant
       if ( TMath::Abs(Y0) < 0.001 ) Xtrack[i]  =  0.5 * C0 / X0 ;     // Create circular tracks and record hits on pad rows
       else           Xtrack[i]  =  0.5*( C0*X0 + Direction*TMath::Sqrt( TMath::Abs( C0*C0*X0*X0 - 
					    (C0*C0-4*Y0*Y0*R[i]*R[i])*(X0*X0+Y0*Y0) )) ) / (X0*X0+Y0*Y0) ;
       if ( TMath::Abs(Y0) < 0.001 ) Ytrack[i]  =  Direction * TMath::Sqrt( TMath::Abs( R[i]*R[i] - Xtrack[i]*Xtrack[i] ) ) ;
       else           Ytrack[i]  =  ( R[i]*R[i] - R0*R0 - 2*X0*Xtrack[i] + X0*X0 + Y0*Y0 ) / ( 2 * Y0 ) ;
       DeltaTheta  =  TMath::ATan2(-1*Y0,-1*X0) - TMath::ATan2(Ytrack[i]-Y0,Xtrack[i]-X0) ;
       while ( DeltaTheta < -1*TMath::Pi() ) DeltaTheta += TMath::TwoPi() ;
       while ( DeltaTheta >=   TMath::Pi() ) DeltaTheta -= TMath::TwoPi() ;
       Ztrack[i]  =   VertexZ + DeltaTheta*Z_coef ;
       xx[0] = Xtrack[i] ; xx[1] = Ytrack[i] ; xx[2] = Ztrack[i] ;

       if (mDistortionMode & (kSpaceCharge | kSpaceChargeR2)) {    // Daisy Chain all possible distortions and sort on flags
	 UndoSpaceChargeDistortion ( xx, xxprime ) ;
	 InnerOuterRatio = 1.3 ;  // JT test.  Without the GridLeak, GVB prefers 1.3  (real world == 0.5)  
	 for ( unsigned int j = 0 ; j < 3; ++j ) 
	   {
	     xx[j] = xxprime[j];
	   }
       }
       if (mDistortionMode & (kGridLeak | k3DGridLeak | kFullGridLeak)) { 
	 UndoGridLeakDistortion ( xx, xxprime ) ;
	 InnerOuterRatio = 0.6 ; // JT test.  With the GridLeak, GVB prefers 0.6  (note that order is important in this loop).
	 for ( unsigned int j = 0 ; j < 3 ; ++j ) 
	   {
	     xx[j] = xxprime[j];
	   }
       }

       Xtrack[i] = 2*Xtrack[i] - xx[0] ; 
       Ytrack[i] = 2*Ytrack[i] - xx[1] ;  
       Ztrack[i] = 2*Ztrack[i] - xx[2] ; 

       if ( i == RefIndex )
	 {
	   Xreference = Xtrack[i] ;  // This point on the circle is the reference for the rest of the fit
	   Yreference = Ytrack[i] ;  // Note: we must run through all TPC Rows to find this point.  Can't skip a row.
	 }
     }
   
   Int_t Index = -1 ;
   for ( Int_t i = 0 ; i < ROWS ; i++ )  // Delete rows not in the bit masks
     {
       if ( ( i < 24  ) && (( RowMask1 & OneBit<<(i+8)  ) == 0 )) continue ;  // Skip this row if not in bit mask
       if ( ( i >= 24 ) && (( RowMask2 & OneBit<<(i-24) ) == 0 )) continue ;  // Skip this row if not in bit mask
       Index++ ;   
       Xprime[Index] = Xtrack[i] ; Yprime[Index] = Ytrack[i] ; // Zprime[Index] = Ztrack[i] ;
       dX[Index] = 0.2 ; dY[Index] = 1.0 ;
       // Inner pads are smaller, but noisier, in the real world. Toy model requires adjustment to match STAR tracker.
       if ( i < INNER[sec-1] ) { dX[Index] *= InnerOuterRatio ; dY[Index] *= InnerOuterRatio ; } ;  
     }
   
   // Transform into U,V space so circles in x,y space lie on a straight line in U,V space
   Int_t count = -1 ;                                       // Count number of active rows
   for ( Int_t i = 0 ; i < Index+1 ; i++ )  
     {
       Double_t zero = 0.001 ;         // Check divide by zero ... not a very tight constraint in this case.
       Double_t displacement2 ;

       displacement2 =
	 (Xprime[i]-Xreference)*(Xprime[i]-Xreference) + (Yprime[i]-Yreference)*(Yprime[i]-Yreference) ;
       
       if ( displacement2 > zero )  
	 {
	   count ++ ;            // reference point not included in the arrays for fitting (below)
	   U[count]  = ( Xprime[i] - Xreference ) / displacement2 ;
	   V[count]  = ( Yprime[i] - Yreference ) / displacement2 ;
	   eU[count] = dX[i] / displacement2 ;
	   eV[count] = dY[i] / displacement2 ; 
	 }
     }

   TGraphErrors gre( count+1, U, V, eU, eV ) ;     
   gre.Fit("pol1","Q") ;
   TF1 *fit = gre.GetFunction("pol1" ) ;
   
   if ( DEBUG ) 
     { // Begin debugging plots
       TCanvas* c1  = new TCanvas("A Simple Fit","The Fit", 250, 10, 700, 500) ;
       c1  -> cd() ;
       gre.Draw("A*") ;
       c1  -> Update() ;
       TCanvas* c2  = new TCanvas("The circles are OK","The circles ", 250, 800, 700, 500) ;
       TGraph* gra  = new TGraph( Index+1, Xprime, Yprime ) ;
       c2  -> cd() ;
       gra -> SetMaximum(200)  ;
       gra -> SetMinimum(-200) ;
       gra -> Draw("A*") ;  // Have to draw twice in order to get the Axis (?)
       gra -> GetXaxis() -> SetLimits(-200.,200.) ;
       gra -> Draw("A*") ;
       c2  -> Update() ;
     } // End debugging plots 
       
   Double_t X0_new  =  Xreference - ( fit->GetParameter(1) / ( 2.0 * fit->GetParameter(0) ) )  ;
   Double_t Y0_new  =  Yreference + ( 1.0 / ( 2.0 * fit->GetParameter(0) ) ) ;
   Double_t R0_new  =  TMath::Sqrt( (Xreference-X0_new)*(Xreference-X0_new) + (Yreference-Y0_new)*(Yreference-Y0_new) ) ;
   Double_t DCA_new =  TMath::Sqrt( X0_new*X0_new + Y0_new*Y0_new ) - R0_new ;  // Negative means (0,0) is inside the circle
   
   pSpace  =  (DCA * ChargeB) * SpaceChargeR2 / DCA_new ;    // Work with Physical-Signed DCA from Chain or MuDST 

   // Restore settings for spacechargeR2
   fSpaceChargeR2 = tempfSpaceChargeR2 ;
   SpaceChargeR2 = tempSpaceChargeR2 ;
   
   return(0) ; // Success 

}


/// PredictSpaceCharge - Input Physical-Signed DCA and get back spacecharge estimate.  Includes the SVT and SSD.
/*!
  Input the parameters for a global track fit and get back an estimate of the spacecharge that distorted the track.

  Input for the track includes the Charge, Pt, VertexZ, PseudoRapidity, and measured DCA. 

  The code attempts to be as realistic as possible when it does the refit.  Therefore, it asks you for
  the hit masks from the microDSTs.  These masks tell you which TPC rows were used in the original track fit
  and whether or not there were hits in the SVT and/or SSD detectors.  It the masks have bits set for the
  SVT then this will affect the track and will pull the fits. For future reference, the masks come in two words.  
  The first 8 bits of the first word describe the vertex, then six layers of the SVT, and finally the SSD.  
  The vertex bit is only set for primary tracks and is not used by this routine.  You should only input global tracks.
  The remaining bits of the first word cover TPC rows 1-24.  The second word covers rows 25-45.  So, for example
  0xFFFFFF00, 0x1FFFFF represent all 45 rows of the TPC and no SVT or SSD hits. (NB: this is dependent
  on the implementation of the topology map, and may be subject to change!)

  You must provide the hit errors by instantiating with two vectors that include the track errors in both X and Y.  
  RowMaskErrorR[]    is the array of hit errors in the radial direction (ie in the direction of a high pt track)
  RowMaskErrorRPhi[] is the array of hit errors in the r-phi direction  (ie tranvsverse to the direction of a high pt track)
  
  pSpace contains the estimate of the space charge in the TPC that caused the distortion on the original track.

  The function returns zero if it succeeds and pSpace will be finite.
  The function returns non-zero if it fails and pSpace will be zero.  
  The function returns non-zero if there are too few hits in the TPC inner and outer sectors.  
  There are also cuts on Pt and rapdity, etc, that can cause the funtion to return non-zero.

*/
Int_t StMagUtilities::PredictSpaceChargeDistortion (Int_t sec, Int_t Charge, Float_t Pt, Float_t VertexZ,
                                                    Float_t PseudoRapidity, Float_t Phi, Float_t DCA,
						    const unsigned long long RowMask1,
                                                    const unsigned long long RowMask2,
						    Float_t RowMaskErrorR[64], Float_t RowMaskErrorRPhi[64], Float_t &pSpace )
{

   const Int_t   INNERDETECTORS   =   6  ;       // Number of inner detector rows in represented in the bit masks
   const Int_t   SSDLAYERS        =   1  ;       // Number of SSD layers
   const Int_t   MinInnerTPCHits  =   5  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   MinOuterTPCHits  =  10  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   DEBUG            =   0  ;       // Turn on debugging statements and plots

   const Int_t   TPCOFFSET = INNERDETECTORS + SSDLAYERS + 1 ;   // Add one for the vertex in 0th position in RowMasks
   const Int_t   BITS      = INNERDETECTORS + TPCROWS[sec-1] + SSDLAYERS + 1 ;  // Number of bits in the row masks (TPC Rows + etc.)

   unsigned int OneBit = 1U ;
   Int_t InnerTPCHits = 0, OuterTPCHits = 0 ;
   for ( Int_t i = 0 ; i < TPCROWS[sec-1] ; i++ ) 
     {
       if ( i < INNER[sec-1] )
	 {
	   if ( ( i < 24  ) && (( RowMask1 & OneBit<<(i+8)  ) != 0 )) InnerTPCHits++ ;  
	   if ( ( i >= 24 ) && (( RowMask2 & OneBit<<(i-24) ) != 0 )) InnerTPCHits++ ;  
	 }
       else
	 {
	   if ( ( i < 24  ) && (( RowMask1 & OneBit<<(i+8)  ) != 0 )) OuterTPCHits++ ;  
	   if ( ( i >= 24 ) && (( RowMask2 & OneBit<<(i-24) ) != 0 )) OuterTPCHits++ ;  
	 }
     }

   pSpace  = 0 ;

   if ( (Pt < 0.3) || (Pt > 2.0) )                           return(1) ; // Fail
   if ( (VertexZ < -50) || (VertexZ > 50) )                  return(2) ; // Fail
   if ( (PseudoRapidity < -1.0) || (PseudoRapidity > 1.0) )  return(3) ; // Fail
   if ( (Charge != 1) && (Charge != -1) )                    return(4) ; // Fail
   if ( (DCA < -4.0) || (DCA > 4.0) )                        return(5) ; // Fail
   if ( InnerTPCHits < MinInnerTPCHits )                     return(6) ; // No action if too few hits in the TPC   
   if ( OuterTPCHits < MinOuterTPCHits )                     return(7) ; // No action if too few hits in the TPC   

   Int_t    ChargeB, HitSector ;
   Float_t  B[3], xx[3], xxprime[3] ;
   Double_t Xtrack[BITS], Ytrack[BITS], Ztrack[BITS] ;
   Double_t R[BITS], X0, Y0, X0Prime, Y0Prime, R0, Pz_over_Pt, Z_coef, DeltaTheta ;
   Double_t Xprime[BITS+1], Yprime[BITS+1], /* Zprime[BITS+1], */ dX[BITS+1], dY[BITS+1] ;  
   Float_t PhiPrime, HitPhi, HitLocalPhi ;
   Double_t cosPhi, sinPhi, cosPhiPrime, sinPhiPrime, cosPhiMPrime, sinPhiMPrime ;

   // Temporarily overide settings for space charge data (only)
   St_spaceChargeCorC* tempfSpaceChargeR2 = fSpaceChargeR2 ;
   Double_t tempSpaceChargeR2 = SpaceChargeR2 ;
   if (!useManualSCForPredict) ManualSpaceChargeR2(0.01,SpaceChargeEWRatio); // Set "medium to large" value of the spacecharge parameter for tests, not critical.
                                                   // but keep EWRatio that was previously defined 
   if (DoOnce) {
     xx[0] = TPCROWR[2][0]; xx[1] = 0; xx[2] = 50; // a dummy point in sector 3
     DoDistortion ( xx, xxprime, 3 ) ;
   }
   Int_t tempDistortionMode = mDistortionMode;
   mDistortionMode = (tempDistortionMode & kSpaceChargeR2);
        if (tempDistortionMode & kFullGridLeak) mDistortionMode |= kFullGridLeak ;
   else if (tempDistortionMode & k3DGridLeak  ) mDistortionMode |= k3DGridLeak   ;
   else if (tempDistortionMode & kGridLeak    ) mDistortionMode |= kGridLeak     ;
 
   Float_t x[3] = { 0, 0, 0 } ;  // Get the B field at the vertex 
   BFieldTpc(x,B) ;
   ChargeB = Charge * TMath::Sign((int)1,(int)(B[2]*1000)) ;
   R0 = TMath::Abs( 1000.0 * Pt / ( 0.299792 * B[2] ) ) ;     // P in GeV, R in cm, B in kGauss
   X0 = ChargeB *  0.0 * R0  ;   // Assume a test particle that shoots out at 0 degrees
   Y0 = ChargeB * -1.0 * R0  ;
   Pz_over_Pt = TMath::SinH(PseudoRapidity) ;
   Z_coef = ChargeB*R0*Pz_over_Pt ;

   PhiPrime = Phi; // Phi is the pointing angle at the vertex
   while ( PhiPrime < 0 ) PhiPrime += PiOver6 ;
   PhiPrime = fmod(PhiPrime + PiOver12,PiOver6) - PiOver12; // PhiPrime is the pointing angle within sector
   cosPhi = TMath::Cos(Phi);
   sinPhi = TMath::Sin(Phi);
   cosPhiPrime = TMath::Cos(PhiPrime);
   sinPhiPrime = TMath::Sin(PhiPrime);
   cosPhiMPrime = cosPhi*cosPhiPrime+sinPhi*sinPhiPrime; // cos(Phi - PhiPrime)
   sinPhiMPrime = sinPhi*cosPhiPrime-cosPhi*sinPhiPrime; // sin(Phi - PhiPrime)

   X0Prime = cosPhiPrime*X0 - sinPhiPrime*Y0; // Rotate helix center by PhiPrime
   Y0Prime = sinPhiPrime*X0 + cosPhiPrime*Y0;
   
   //JT Test Hardwire the radius of the vertex - this is not real and not used.  Vertex is not compatible with DCA input.
   R[0] = 0.0    ;  // This is a place holder so sequence and order matches that in the ROWmask.  Not Used.

   //JT TEST Hardwire the radii for the SVT.  Note that this is a disgusting kludge.
   R[1] =  6.37  ;  // SVT layer 1          (2nd bit)
   R[2] =  7.38  ;  // SVT layer 1 prime
   R[3] = 10.38  ;  // SVT layer 2
   R[4] = 11.27  ;  // SVT layer 2 prime
   R[5] = 14.19  ;  // SVT layer 3 
   R[6] = 15.13  ;  // SVT layer 3 prime    (7th bit)
    
   //JT TEST Hardwire the radii for the SSD.  Note that this is a disgusting kludge. 
   R[7] = 22.8   ;  // SSD (average) Radius (8th bit)

   // JT TEST Add the radii for the TPC Rows.  Note the hardwired offsets.
   memcpy(&(R[TPCOFFSET]),TPCROWR[sec-1],TPCROWS[sec-1]*sizeof(Double_t));
   // Not completly correct because TPC rows are flat.

   for ( Int_t i = 0 ; i < BITS ; i++ )
     {

       if ( ( i > 0 && i <  32 ) && (( RowMask1 & OneBit<<(i)    ) == 0 )) continue ;  // Skip this row if not in bit mask
       if ( ( i > 0 && i >= 32 ) && (( RowMask2 & OneBit<<(i-32) ) == 0 )) continue ;  // Skip this row if not in bit mask

       if ( R[i] < IFCRadius || R[i] > OFCRadius ) { // Check if point is outside the TPC
         Ytrack[i] = -1 * ChargeB * ( R[i]*R[i]/(2*R0) ) ;
         Xtrack[i] = TMath::Sqrt( R[i]*R[i] - Ytrack[i]*Ytrack[i] ) ;
         DeltaTheta  =  TMath::ATan2(-1*Y0,-1*X0) - TMath::ATan2(Ytrack[i]-Y0,Xtrack[i]-X0) ;
         while ( DeltaTheta < -1*TMath::Pi() ) DeltaTheta += TMath::TwoPi() ;
         while ( DeltaTheta >=   TMath::Pi() ) DeltaTheta -= TMath::TwoPi() ;
         Ztrack[i]  =   VertexZ + DeltaTheta*Z_coef;
         continue ;   // Do nothing if point is outside the TPC
       }

       // Throw track at 0 + PhiPrime
       // Handle non-circular padrows
       Xtrack[i] = R[i] ;
       Ytrack[i] = ChargeB * TMath::Sqrt( R0*R0 - (Xtrack[i]-X0Prime)*(Xtrack[i]-X0Prime) ) + Y0Prime ;
       HitLocalPhi = TMath::ATan2(Ytrack[i],Xtrack[i]);
       while (TMath::Abs(HitLocalPhi) > PiOver12) {
         // Jump local coordinates to neighboring sector
         PhiPrime -= TMath::Sign(PiOver6,HitLocalPhi);
         cosPhiPrime = TMath::Cos(PhiPrime);
         sinPhiPrime = TMath::Sin(PhiPrime);
         cosPhiMPrime = cosPhi*cosPhiPrime+sinPhi*sinPhiPrime; // cos(Phi - PhiPrime)
         sinPhiMPrime = sinPhi*cosPhiPrime-cosPhi*sinPhiPrime; // sin(Phi - PhiPrime)

         X0Prime = cosPhiPrime*X0 - sinPhiPrime*Y0; // Rotate helix center by PhiPrime
         Y0Prime = sinPhiPrime*X0 + cosPhiPrime*Y0;

         Ytrack[i] = ChargeB * TMath::Sqrt( R0*R0 - (Xtrack[i]-X0Prime)*(Xtrack[i]-X0Prime) ) + Y0Prime ;
         HitLocalPhi = TMath::ATan2(Ytrack[i],Xtrack[i]);
       }

       DeltaTheta  =  TMath::ATan2(-1*Y0Prime,-1*X0Prime) - TMath::ATan2(Ytrack[i]-Y0Prime,Xtrack[i]-X0Prime) ;
       while ( DeltaTheta < -1*TMath::Pi() ) DeltaTheta += TMath::TwoPi() ;
       while ( DeltaTheta >=   TMath::Pi() ) DeltaTheta -= TMath::TwoPi() ;
       Ztrack[i]  =   VertexZ + DeltaTheta*Z_coef;
       
       // Rotate by Phi - PhiPrime
       xx[0] = cosPhiMPrime*Xtrack[i] - sinPhiMPrime*Ytrack[i];
       xx[1] = sinPhiMPrime*Xtrack[i] + cosPhiMPrime*Ytrack[i];
       xx[2] = Ztrack[i];

       if (mDistortionMode & (kGridLeak | k3DGridLeak | kFullGridLeak)) {
         HitPhi = TMath::ATan2(xx[1],xx[0]) ;
         while ( HitPhi < 0 ) HitPhi += TMath::TwoPi() ;
         while ( HitPhi >= TMath::TwoPi() ) HitPhi -= TMath::TwoPi() ;
         HitSector = 0;
         SectorNumber ( HitSector, HitPhi, xx[2] );
         if ( GLWeights[HitSector] < 0) {
           // Restore settings for spacechargeR2
           fSpaceChargeR2  =  tempfSpaceChargeR2 ;
           SpaceChargeR2   =  tempSpaceChargeR2  ;
           mDistortionMode =  tempDistortionMode ;
           return(8); // Fail on unknown GridLeak weights
         }
       }

       DoDistortion ( xx, xxprime) ; // Distort the tracks
       Xtrack[i] =  cosPhi*xxprime[0] + sinPhi*xxprime[1] ; // Rotate by -Phi
       Ytrack[i] = -sinPhi*xxprime[0] + cosPhi*xxprime[1] ;
       Ztrack[i] = xxprime[2] ;
       
     }
   
   Int_t Index = -1 ;                    // Count number of 'hits' in the bit mask
   Int_t TPCIndex = -1 ;                 // Count the number of these hits in the TPC
   for ( Int_t i = 1 ; i < BITS ; i++ )  // Delete rows not in the bit masks.  Note i=1 to skip the vertex.
     {
       if ( ( i <  32 ) && (( RowMask1 & OneBit<<(i)    ) == 0 )) continue ;  // Skip this row if not in bit mask
       if ( ( i >= 32 ) && (( RowMask2 & OneBit<<(i-32) ) == 0 )) continue ;  // Skip this row if not in bit mask
       Index++ ;   if ( i >= TPCOFFSET ) TPCIndex++ ;
       Xprime[Index] = Xtrack[i] ;        Yprime[Index] = Ytrack[i] ;         // Zprime[Index] = Ztrack[i] ;
       dX[Index]     = RowMaskErrorR[i] ;     dY[Index] = RowMaskErrorRPhi[i] ;   
       // printf("%9.2f  %9.2f  %9.2f  %9.2f  %9.2f \n", Xprime[Index], Yprime[Index], Zprime[Index], dX[Index], dY[Index] ) ; // JT Test
     }

   TGraphErrors gre(Index+1,Xprime,Yprime,dX,dY) ;  

   // Note that circle fitting has ambiguities which can be settled via ChargeB.
   //   The "+" solution is for Y points greater than Y0.  "-" for Y < Y0.
   TF1 newDCA ("newDCA" , "( [1] + [3] * sqrt( [1]**2 - x**2 + 2*x*[0] + [2]*[2] - [2]*(2*sqrt([0]**2+[1]**2))) )" );
   newDCA.SetParameters( X0, Y0, 0.0, ChargeB );
   newDCA.FixParameter(3, ChargeB);
   gre.Fit("newDCA","Q") ;
   Double_t DCA_new = newDCA.GetParameter( 2 ) ;  // Negative means that (0,0) is inside the circle
   // End of circle fitting

   if ( DEBUG ) 
     { // Begin debugging plots
       TCanvas* c1  = new TCanvas("A Simple Fit","The Fit", 250, 10, 700, 500) ;
       TCanvas* c2  = new TCanvas("The circles are OK","The circles ", 250, 800, 700, 500) ;
       c1  -> cd() ;
       gre.Draw("A*") ;
       c1  -> Update() ;
       TGraph* gra  = new TGraph( Index+1, Xprime, Yprime ) ;
       c2  -> cd() ;
       gra -> SetMaximum(200)  ;
       gra -> SetMinimum(-200) ;
       //gra -> Draw("A*") ;  // Have to draw twice in order to get the Axis (?)
       gra -> GetXaxis() -> SetLimits(-200.,200.) ;
       gra -> Draw("A*") ;
       c2  -> Update() ;
     } // End debugging plots 
   
   pSpace  =  (DCA * ChargeB) * SpaceChargeR2 / DCA_new ;    // Work with Physical-Signed DCA from Chain or MuDST 

   // Restore settings for spacechargeR2
   fSpaceChargeR2  =  tempfSpaceChargeR2 ;
   SpaceChargeR2   =  tempSpaceChargeR2  ;
   mDistortionMode =  tempDistortionMode ;
   
   return(0) ; // Success 

}


/// PredictSpaceCharge - Input Physical-Signed DCA and get back spacecharge estimate.
/*!
  Input the parameters for a global track fit and get back an estimate of the spacecharge that distorted the track.

  Input for the track includes the Charge, Pt, VertexZ, PseudoRapidity, and measured DCA. 

  The code attempts to be as realistic as possible when it does the refit and takes as its primary input
  the radii of hits on the track. Hits that are not within the dimensions of the TPC are not moved.
  You must provide the hit errors by instantiating with two vectors that include the track errors in both r and phi
  R[]         is the array of hit radii (ie in the direction of a high pt track)
  ErrorR[]    is the array of hit errors in the radial direction (ie in the direction of a high pt track)
  ErrorRPhi[] is the array of hit errors in the r-phi direction  (ie tranvsverse to the direction of a high pt track)
  
  pSpace contains the estimate of the space charge in the TPC that caused the distortion on the original track.

  The function returns zero if it succeeds and pSpace will be finite.
  The function returns non-zero if it fails and pSpace will be zero.  
  The function returns non-zero if there are too few hits in the TPC inner and outer sectors.  
  There are also cuts on Pt and rapdity, etc, that can cause the funtion to return non-zero.

*/
Int_t StMagUtilities::PredictSpaceChargeDistortion (Int_t NHits, Int_t Charge, Float_t Pt, Float_t VertexZ,
                                                    Float_t PseudoRapidity, Float_t Phi, Float_t DCA,
						    Double_t R[128],
						    Double_t ErrorR[128],
                                                    Double_t ErrorRPhi[128], Float_t &pSpace )
{

   const Int_t   MinInnerTPCHits  =   5  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   MinOuterTPCHits  =  10  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   DEBUG            =   0  ;       // Turn on debugging statements and plots

   pSpace  = 0 ;

   if ( (Pt < 0.3) || (Pt > 2.0) )                           return(1) ; // Fail
   if ( (VertexZ < -50) || (VertexZ > 50) )                  return(2) ; // Fail
   if ( (PseudoRapidity < -1.0) || (PseudoRapidity > 1.0) )  return(3) ; // Fail
   if ( (Charge != 1) && (Charge != -1) )                    return(4) ; // Fail
   if ( (DCA < -4.0) || (DCA > 4.0) )                        return(5) ; // Fail

   Int_t InnerTPCHits = 0, OuterTPCHits = 0 ;
   for ( Int_t i = 0 ; i < NHits ; i++ ) 
     {
       // non-strict counting to return quickly if it's clear, but will count strictly later
       if ( R[i] > IFCRadius && R[i] < 1.035*GAPRADIUS ) InnerTPCHits++ ;
       if ( R[i] < OFCRadius && R[i] > 0.990*GAPRADIUS ) OuterTPCHits++ ;
     }

   if ( InnerTPCHits < MinInnerTPCHits )                     return(6) ; // No action if too few hits in the TPC   
   if ( OuterTPCHits < MinOuterTPCHits )                     return(7) ; // No action if too few hits in the TPC   
   InnerTPCHits = 0 ; OuterTPCHits = 0 ;

   Int_t    ChargeB, HitSector ;
   Float_t  B[3], xx[3], xxprime[3] ;
   Double_t Xtrack[NHits], Ytrack[NHits], Ztrack[NHits] ;
   Double_t X0, Y0, R0, Pz_over_Pt, Z_coef, DeltaTheta ;
   Float_t HitPhi ;
   Double_t cosPhi, sinPhi ;

   // Temporarily overide settings for space charge data (only)
   St_spaceChargeCorC* tempfSpaceChargeR2 = fSpaceChargeR2 ;
   Double_t tempSpaceChargeR2 = SpaceChargeR2 ;
   if (!useManualSCForPredict) ManualSpaceChargeR2(0.01,SpaceChargeEWRatio); // Set "medium to large" value of the spacecharge parameter for tests, not critical.
                                                   // but keep EWRatio that was previously defined 
   if (DoOnce) UndoDistortion(0,0,0); // make sure everything is initialized for mDistortionMode
   Int_t tempDistortionMode = mDistortionMode;
   mDistortionMode = (tempDistortionMode & (kSpaceCharge | kSpaceChargeR2 | kGridLeak | k3DGridLeak | kFullGridLeak));
 
   memset(xx,0,3*sizeof(Float_t));  // Get the B field at the vertex 
   BFieldTpc(xx,B) ;
   ChargeB = Charge * TMath::Sign((int)1,(int)(B[2]*1000)) ;
   R0 = TMath::Abs( 1000.0 * Pt / ( 0.299792 * B[2] ) ) ;     // P in GeV, R in cm, B in kGauss
   X0 = ChargeB *  0.0 * R0  ;   // Assume a test particle that shoots out at 0 degrees
   Y0 = ChargeB * -1.0 * R0  ;
   Pz_over_Pt = TMath::SinH(PseudoRapidity) ;
   Z_coef = ChargeB*R0*Pz_over_Pt ;

   cosPhi = TMath::Cos(Phi);
   sinPhi = TMath::Sin(Phi);

   for ( Int_t i = 0 ; i < NHits ; i++ )
     {

       Ytrack[i] = -1 * ChargeB * ( R[i]*R[i]/(2*R0) ) ;
       Xtrack[i] = TMath::Sqrt( R[i]*R[i] - Ytrack[i]*Ytrack[i] ) ;
       DeltaTheta  =  TMath::ATan2(-1*Y0,-1*X0) - TMath::ATan2(Ytrack[i]-Y0,Xtrack[i]-X0) ;
       while ( DeltaTheta < -1*TMath::Pi() ) DeltaTheta += TMath::TwoPi() ;
       while ( DeltaTheta >=   TMath::Pi() ) DeltaTheta -= TMath::TwoPi() ;
       Ztrack[i]  =   VertexZ + DeltaTheta*Z_coef;
       // Do nothing if point is outside the TPC drift volume
       if ( R[i] < IFCRadius || R[i] > OFCRadius ||
            TMath::Abs(Ztrack[i]) > TPC_Z0-0.5) continue ;
            // GVB: excluding a 5 mm around the GG plane in case alignment moves a
            // prompt hit there. Other PredictSpaceCharge() functions did not exclude
            // prompt hits, but DoDistortion should do nothing to those anyhow.

       // Distortion must be applied at (approximately) true positions in space
       // to account for the TPC distortions not being azimuthally symmetric,
       // then rotated back to fitting frame

       // Rotate by Phi to the true angle
       xx[0] = cosPhi*Xtrack[i] - sinPhi*Ytrack[i];
       xx[1] = sinPhi*Xtrack[i] + cosPhi*Ytrack[i];
       xx[2] = Ztrack[i];

       HitPhi = TMath::ATan2(xx[1],xx[0]) ;
       while ( HitPhi < 0 ) HitPhi += TMath::TwoPi() ;
       while ( HitPhi >= TMath::TwoPi() ) HitPhi -= TMath::TwoPi() ;
       HitSector = 0;
       SectorNumber ( HitSector, HitPhi, xx[2] );

       if (mDistortionMode & (kGridLeak | k3DGridLeak | kFullGridLeak)) {
         if ( GLWeights[HitSector] < 0) {
           // Restore settings for spacechargeR2
           fSpaceChargeR2  =  tempfSpaceChargeR2 ;
           SpaceChargeR2   =  tempSpaceChargeR2  ;
           mDistortionMode =  tempDistortionMode ;
           return(8); // Fail on unknown GridLeak weights
         }
       }
       HitPhi = fmod(HitPhi + PiOver12, PiOver6) - PiOver12;
       if ( R[i]*TMath::Cos(HitPhi) < GAPRADIUS ) InnerTPCHits++ ;
       else OuterTPCHits++ ;

       DoDistortion ( xx, xxprime, HitSector) ; // Distort the tracks
       Xtrack[i] =  cosPhi*xxprime[0] + sinPhi*xxprime[1] ; // Rotate by -Phi
       Ytrack[i] = -sinPhi*xxprime[0] + cosPhi*xxprime[1] ;
       Ztrack[i] = xxprime[2] ;
       
     }
   
   if ( InnerTPCHits < MinInnerTPCHits ) {
     // Restore settings for spacechargeR2
     fSpaceChargeR2  =  tempfSpaceChargeR2 ;
     SpaceChargeR2   =  tempSpaceChargeR2  ;
     mDistortionMode =  tempDistortionMode ;
     return(6) ; // No action if too few hits in the TPC   
   }
   if ( OuterTPCHits < MinOuterTPCHits ) {
     // Restore settings for spacechargeR2
     fSpaceChargeR2  =  tempfSpaceChargeR2 ;
     SpaceChargeR2   =  tempSpaceChargeR2  ;
     mDistortionMode =  tempDistortionMode ;
     return(7) ; // No action if too few hits in the TPC   
   }

   TGraphErrors gre(NHits,Xtrack,Ytrack,ErrorR,ErrorRPhi) ;  

   // Note that circle fitting has ambiguities which can be settled via ChargeB.
   //   The "+" solution is for Y points greater than Y0.  "-" for Y < Y0.
   static TF1* newDCA = 0;
   if (!newDCA) newDCA = new TF1("newDCA" , "( [1] + [3] * sqrt( [1]**2 - x**2 + 2*x*[0] + [2]*[2] - [2]*(2*sqrt([0]**2+[1]**2))) )" );
   newDCA->SetParameters( X0, Y0, 0.0, ChargeB );
   newDCA->FixParameter(3, ChargeB);
   gre.Fit(newDCA,"Q") ;
   Double_t DCA_new = newDCA->GetParameter( 2 ) ;  // Negative means that (0,0) is inside the circle
   // End of circle fitting

   if ( DEBUG ) 
     { // Begin debugging plots
       TCanvas* c1  = new TCanvas("A Simple Fit","The Fit", 250, 10, 700, 500) ;
       TCanvas* c2  = new TCanvas("The circles are OK","The circles ", 250, 800, 700, 500) ;
       c1  -> cd() ;
       gre.Draw("A*") ;
       c1  -> Update() ;
       TGraph* gra  = new TGraph( NHits, Xtrack, Ytrack ) ;
       c2  -> cd() ;
       gra -> SetMaximum(200)  ;
       gra -> SetMinimum(-200) ;
       //gra -> Draw("A*") ;  // Have to draw twice in order to get the Axis (?)
       gra -> GetXaxis() -> SetLimits(-200.,200.) ;
       gra -> Draw("A*") ;
       c2  -> Update() ;
     } // End debugging plots 
   
   pSpace  =  (DCA * ChargeB) * SpaceChargeR2 / DCA_new ;    // Work with Physical-Signed DCA from Chain or MuDST 

   // Restore settings for spacechargeR2
   fSpaceChargeR2  =  tempfSpaceChargeR2 ;
   SpaceChargeR2   =  tempSpaceChargeR2  ;
   mDistortionMode =  tempDistortionMode ;
   
   return(0) ; // Success 

}


//________________________________________

/// Check if integer is a power of 2

Int_t StMagUtilities::IsPowerOfTwo(Int_t i)
{
  Int_t j = 0;
  while( i > 0 ) { j += (i&1) ; i = (i>>1) ; }
  if ( j == 1 ) return(1) ;  // True
  return(0) ;                // False
}


//________________________________________

/// Calculate Sector Number from coordinate position if not already known.  
/*!
  Use this function only if sector number has not been passed into StMagUtilties by calling function.
  This function assumes that -z values are on the East end of the TPC.  This may not be true
  for pileup events.  SectorNumber from datatapes is better, if available.
*/

void StMagUtilities::SectorNumber( Int_t& Sector , const Float_t x[] )
{
  if ( Sector > 0 ) return              ;  // Already valid
  Float_t phi = (usingCartesian ? TMath::ATan2(x[1],x[0]) : x[1]) ;
  SectorNumber( Sector, phi, x[2] )     ;
}
void StMagUtilities::SectorNumber( Int_t& Sector , Float_t phi, const Float_t z )
{
  if ( Sector > 0 ) return              ;  // Already valid
  if ( phi < 0 ) phi += TMath::TwoPi()  ;  // Use range from 0-360
  Sector = ( ( 30 - (int)(phi/PiOver12) )%24 ) / 2 ;
  if ( z < 0 ) Sector = 24 - Sector     ;  // Note that order of these two if statements is important
  else if ( Sector == 0 ) Sector = 12   ;
}

/// Calculate Sector Side from Sector number (-1 for east, +1 for west)
Int_t StMagUtilities::SectorSide( Int_t& Sector , const Float_t x[] )
{
  return SectorSide(Sector, x[2]) ;
}
Int_t StMagUtilities::SectorSide( Int_t& Sector , const Float_t z )
{
  if ( Sector <= 0 ) SectorNumber(Sector,0,z);
  return (Sector <= 12 ? 1 : -1) ;
}


//________________________________________

/// Limit z based on Sector Number
/*!
  Use this function to protect against discontinuity at the CM
  by doing calculations no closer than 0.2 cm from it
*/

Float_t StMagUtilities::LimitZ( Int_t& Sector , const Float_t x[] )
{
  Float_t z = x[2];
  const Float_t zlimit = 0.2;
  Int_t sign = SectorSide(Sector, z);
  if (sign * z < zlimit) z = sign * zlimit;
  return z ;
}


//________________________________________

/// Grid Leakage entry function
/*!
  Call the appropriate GridLeak function based on distortion mode
*/
void StMagUtilities::UndoGridLeakDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{ 

  if (((mDistortionMode/kGridLeak)     & 1) +
      ((mDistortionMode/k3DGridLeak)   & 1) +
      ((mDistortionMode/kFullGridLeak) & 1) > 1) {
      cout << "StMagUtilities ERROR **** Do not use multiple GridLeak modes at the same time" << endl ;
      cout << "StMagUtilities ERROR **** These routines have overlapping functionality." << endl ;
      exit(1) ;
  }

  if (mDistortionMode & kGridLeak) { 
      Undo2DGridLeakDistortion ( x, Xprime, Sector ) ;
  } else if (mDistortionMode & k3DGridLeak) { 
      Undo3DGridLeakDistortion ( x, Xprime, Sector ) ;
  } else if (mDistortionMode & kFullGridLeak) { 
      UndoFullGridLeakDistortion ( x, Xprime, Sector ) ;
  }

}


//________________________________________

/// Grid Leakage Calculation
/*!
  Calculate the distortions due to charge leaking out of the gap between the inner and outer sectors
  as well as the gap between the IFC and the innersector, as well as outersector and OFC.
  Original work by Gene VanBuren, and J. Thomas 
  NOTE: This routine is obsolete: 10/31/2009  Recommend that you use Undo3DGridLeakDistortion, instead.
*/
void StMagUtilities::Undo2DGridLeakDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{ 
  
  const  Int_t     ORDER       =  1   ;  // Linear interpolation = 1, Quadratic = 2         
  const  Int_t     ROWS        =  513 ;  // ( 2**n + 1 )  eg. 65, 129, 257, 513, 1025  (513 or above for natural width gap)
  const  Int_t     COLUMNS     =  129 ;  // ( 2**m + 1 )  eg. 65, 129, 257, 513
  const  Int_t     ITERATIONS  =  750 ;  // About 1 second per iteration
  const  Double_t  GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
  const  Double_t  GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;


  static TMatrix   EroverEz(ROWS,COLUMNS)  ;            // Make static so we can use them later
  static Float_t   Rlist[ROWS], Zedlist[COLUMNS] ;

  Float_t   Er_integral, Ephi_integral ;
  Double_t  r, phi, z ;

  if ( InnerGridLeakStrength == 0 && MiddlGridLeakStrength == 0 && OuterGridLeakStrength == 0 )
    { memcpy(Xprime,x,threeFloats) ; return ; }

  if ( DoOnce )
    {
      cout << "StMagUtilities::UndoGridL  Please wait for the tables to fill ... ~30 seconds" << endl ;
      TMatrix  ArrayV(ROWS,COLUMNS), Charge(ROWS,COLUMNS) ;
      //Fill arrays with initial conditions.  V on the boundary and Charge in the volume.      

      for ( Int_t j = 0 ; j < COLUMNS ; j++ )  
	{
	  Double_t zed = j*GRIDSIZEZ ;
	  Zedlist[j] = zed ;
	  for ( Int_t i = 0 ; i < ROWS ; i++ )  
	    {
	      Double_t Radius = IFCRadius + i*GRIDSIZER ;
	      ArrayV(i,j) = 0 ;
	      Charge(i,j) = 0 ;
	      Rlist[i] = Radius ;
	    }
	}      

      // 2D Simulation of the charge coming out the gap between the inner and outer in the sectors.  
      // Note it is *not* integrated in Z. Grid leakage strength must be set manually.  
      // Once set, the correction will automatically go up and down with the Luminosity of the beam. 
      // This is an assumption that may or may not be true.  Note that the Inner sector and the
      // Outer sector have different gains.  This is taken into account in this code ... not in the DB.
      // Note that the grid leak is twice as wide as GridLeakWidth.  The width must be larger than life
      // for numerical reasons. (3.0 cm seems to work well for the halfwidth of this band but ROWS must be >= 256)
      // Global Grid Leaks are addressed by this simulation, too.  Use DB entries for Inner GridLeakRadius etc.
      Float_t GainRatio = 3.0 ;      // Gain ratio is approximately 3:1 See NIM Article. (larger in Inner Sector)
      for ( Int_t j = 1 ; j < COLUMNS-1 ; j++ )  
	{
	  for ( Int_t i = 1 ; i < ROWS-1 ; i++ ) 
	    { 
	      Double_t Radius = IFCRadius + i*GRIDSIZER ;
	      // Simulate GG leak over the full Inner sector
	      if ( (Radius >= InnerGridLeakRadius) && (Radius < MiddlGridLeakRadius) ) 
		Charge(i,j) += InnerGridLeakStrength / (1.0e-3*Radius*Radius) ;  // 1.0e-3 is arbitrary Normalization
	      // Charge leak in the gap between the inner and outer sectors
	      if ( (Radius >= MiddlGridLeakRadius-MiddlGridLeakWidth) && (Radius <= MiddlGridLeakRadius+MiddlGridLeakWidth) ) 
		Charge(i,j) += MiddlGridLeakStrength ;
	      // Simulate GG leak over the full Outer sector 
	      if ( (Radius >= MiddlGridLeakRadius) && (Radius < OuterGridLeakRadius) ) 
		Charge(i,j) += OuterGridLeakStrength / (GainRatio*1.0e-3*Radius*Radius) ; // Note GainRatio and Normlization
	    } 
	}

      PoissonRelaxation( ArrayV, Charge, EroverEz, ITERATIONS ) ;

    }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;             // Table uses phi from 0 to 2*Pi
  z = LimitZ( Sector, x ) ;                          // Protect against discontinuity at CM
  
  Float_t phi0     =  PiOver6 * ((Int_t)(0.499 + phi/PiOver6)) ;
  Float_t local_y  =  r * TMath::Cos( phi - phi0 ) ; // Cheat! Cheat! Use local y because charge is a flat sheet.

  // Assume symmetry in Z for call to table, below
  Er_integral   =  Interpolate2DTable( ORDER, local_y, TMath::Abs(z), ROWS, COLUMNS, Rlist, Zedlist, EroverEz ) ;
  Ephi_integral =  0.0 ;                             // E field is symmetric in phi

  // Get Space Charge 
  if (fSpaceChargeR2) GetSpaceChargeR2();            // need to reset it each event 

  // Subtract to Undo the distortions and apply the EWRatio factor to the data on the East end of the TPC
  if ( r > 0.0 ) 
    {
      Float_t Weight = SpaceChargeR2 * (doingDistortion ? SmearCoefSC*SmearCoefGL : 1.0);
      if (                z <  0) Weight *= SpaceChargeEWRatio ;
      phi =  phi - Weight * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - Weight * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;

}

  
//________________________________________

  
/// 3D GridLeak Distortion Calculation
/*!
  Calculate the 3D distortions due to charge leaking out of the gap between the inner and outer sectors.
  Original work by Gene VanBuren, and J. Thomas 
*/
void StMagUtilities::Undo3DGridLeakDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{ 
     
  const Int_t   ORDER       =    1  ;  // Linear interpolation = 1, Quadratic = 2         
  const Int_t   neR3D       =   73  ;  // Number of rows in the interpolation table for the Electric field
  const Int_t   ITERATIONS  =  100  ;  // Depends on setting for the OverRelaxation parameter ... check results carefully
  const Int_t   SYMMETRY    =    1  ;  // The Poisson problem to be solved has reflection symmetry (1) or not (0).
  const Int_t   ROWS        =  513  ;  // ( 2**n + 1 )  eg. 65, 129, 257, 513, 1025   (513 or above for a natural width gap)
  const Int_t   COLUMNS     =  129  ;  // ( 2**m + 1 )  eg. 65, 129, 257, 513
  const Int_t   PHISLICES   =    8  ;  // Note interaction with "GRIDSIZEPHI" and "SYMMETRY"
  const Float_t GRIDSIZEPHI =  (2 - SYMMETRY) * TMath::Pi() / (12.0*(PHISLICES-1)) ;
  const Float_t GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
  const Float_t GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;

  static TMatrix *ArrayofArrayV[PHISLICES], *ArrayofCharge[PHISLICES] ; 
  static TMatrix *ArrayofEroverEz[PHISLICES], *ArrayofEPhioverEz[PHISLICES] ; 

  static Float_t  Rlist[ROWS], Zedlist[COLUMNS] ;

  static TMatrix *ArrayoftiltEr[PHISLICES] ;
  static TMatrix *ArrayoftiltEphi[PHISLICES] ;

  // Use custom list of radii because we need extra resolution near the gap
  static Float_t eRadius[neR3D] = {   50.0,   60.0,   70.0,   80.0,   90.0, 
				    100.0,  104.0,  106.5,  109.0,  111.5, 
				    114.0,  115.0,  116.0,  117.0,  118.0,  
				    118.5,  118.75, 119.0,  119.25, 119.5, 
				    119.75, 120.0,  120.25, 120.5,  120.75, 
				    121.0,  121.25, 121.5,  121.75, 122.0,  
				    122.25, 122.5,  122.75, 123.0,  123.25, 
                                    123.5,  123.75, 124.0,  124.25, 124.5,  
                                    124.75, 125.0,  125.25, 125.5,  126.0, 
                                    126.25, 126.5,  126.75, 127.0,  127.25,  
				    127.5,  128.0,  128.5,  129.0,  129.5,  
				    130.0,  130.5,  131.0,  131.5,  132.0,  
				    133.0,  135.0,  137.5,  140.0,  145.0,  
				    150.0,  160.0,  170.0,  180.0,  190.0,  
				    195.0,  198.0,  200.0 } ;

  static Float_t Philist[PHISLICES] ; // Note that there will be rapid changes near 15 degrees on padrow 13

  for ( Int_t k = 0 ; k < PHISLICES ; k++ ) Philist[k] = GRIDSIZEPHI * k ;  

  Float_t  Er_integral, Ephi_integral ;
  Float_t  r, phi, z ;

  memcpy(Xprime,x,threeFloats) ;

  if ( MiddlGridLeakStrength == 0 ) return ; 

  if ( DoOnce )
    {
      cout << "StMagUtilities::Undo3DGrid Please wait for the tables to fill ...  ~5 seconds * PHISLICES" << endl ;
    
      for ( Int_t k = 0 ; k < PHISLICES ; k++ )
	{
	  ArrayoftiltEr[k]   =  new TMatrix(neR3D,EMap_nZ) ;
	  ArrayoftiltEphi[k] =  new TMatrix(neR3D,EMap_nZ) ;
	}

      for ( Int_t k = 0 ; k < PHISLICES ; k++ )
	{
	  ArrayofArrayV[k]     =  new TMatrix(ROWS,COLUMNS) ;
	  ArrayofCharge[k]     =  new TMatrix(ROWS,COLUMNS) ;
	  ArrayofEroverEz[k]   =  new TMatrix(ROWS,COLUMNS) ;
	  ArrayofEPhioverEz[k] =  new TMatrix(ROWS,COLUMNS) ;
	}

      for ( Int_t k = 0 ; k < PHISLICES ; k++ )
	{
	  TMatrix &ArrayV    =  *ArrayofArrayV[k] ;
	  TMatrix &Charge    =  *ArrayofCharge[k] ;
          Float_t cosPhiK    =  TMath::Cos(Philist[k]) ;

	  //Fill arrays with initial conditions.  V on the boundary and Charge in the volume.
	  for ( Int_t i = 0 ; i < ROWS ; i++ )  
	    {
	      Rlist[i] = IFCRadius + i*GRIDSIZER ;
	      for ( Int_t j = 0 ; j < COLUMNS ; j++ )  // Fill Vmatrix with Boundary Conditions
		{
		  Zedlist[j]  = j * GRIDSIZEZ ;
		  ArrayV(i,j) = 0.0 ; 
		  Charge(i,j) = 0.0 ;
		}
	    }      

	  for ( Int_t i = 1 ; i < ROWS-1 ; i++ ) 
	    { 
	      Float_t Radius = IFCRadius + i*GRIDSIZER ;
	      Float_t local_y_hi  = (Radius+GRIDSIZER/2.0) * cosPhiK ;
	      Float_t local_y_lo  = (Radius-GRIDSIZER/2.0) * cosPhiK ;
	      Float_t charge_y_hi =  OUTERGGFirst ;   // Use physical Gap dimensions
	      Float_t charge_y_lo =  INNERGGLast  ;   // Use physical Gap dimensions
	      for ( Int_t j = 1 ; j < COLUMNS-1 ; j++ )    
		{

		  Float_t top = 0, bottom = 0 ;

		  if (local_y_hi > charge_y_lo && local_y_hi < charge_y_hi) 
		    {
		      top    = local_y_hi ; 
		      if (local_y_lo > charge_y_lo && local_y_lo < charge_y_hi) 
			bottom = local_y_lo ;
		      else 
			bottom = charge_y_lo ;
		    } 

		  if (local_y_lo > charge_y_lo && local_y_lo < charge_y_hi) 
		    {
		      bottom    = local_y_lo ; 
		      if (local_y_hi > charge_y_lo && local_y_hi < charge_y_hi) 
			top = local_y_hi ;
		      else 
		        top = charge_y_hi ;
		    } 

		  Float_t Weight  =  1. / (local_y_hi*local_y_hi - local_y_lo*local_y_lo) ;
		  // Weight by ratio of volumes for a partially-full cell / full cell (in Cylindrical Coords).
		  // Note that Poisson's equation is using charge density ... so if rho = 1.0, then volume is what counts.
		  const Float_t BackwardsCompatibilityRatio = 3.75 ;       // DB uses different value for legacy reasons
		  Charge(i,j)  =  (top*top-bottom*bottom) * Weight * MiddlGridLeakStrength*BackwardsCompatibilityRatio ;

		}
	    }
	}      
      
      //Solve Poisson's equation in 3D cylindrical coordinates by relaxation technique
      //Allow for different size grid spacing in R and Z directions

      Poisson3DRelaxation( ArrayofArrayV, ArrayofCharge, ArrayofEroverEz, ArrayofEPhioverEz, PHISLICES, 
			   GRIDSIZEPHI, ITERATIONS, SYMMETRY) ;

      //Interpolate results onto a custom grid which is used just for the grid leak calculation.

      for ( Int_t k = 0 ; k < PHISLICES ; k++ )
	{
	  TMatrix &tiltEr   = *ArrayoftiltEr[k] ;
	  TMatrix &tiltEphi = *ArrayoftiltEphi[k] ;
	  phi = Philist[k] ;
	  for ( Int_t i = 0 ; i < EMap_nZ ; i++ ) 
	    {
	      z = TMath::Abs(eZList[i]) ;              // Assume a symmetric solution in Z that depends only on ABS(Z)
	      for ( Int_t j = 0 ; j < neR3D ; j++ ) 
		{ 
		  r = eRadius[j] ;
		  tiltEr(j,i)    = Interpolate3DTable( ORDER,r,z,phi,ROWS,COLUMNS,PHISLICES,Rlist,Zedlist,Philist,ArrayofEroverEz )   ;
		  tiltEphi(j,i)  = Interpolate3DTable( ORDER,r,z,phi,ROWS,COLUMNS,PHISLICES,Rlist,Zedlist,Philist,ArrayofEPhioverEz ) ;
		}
	    }
	}

      for ( Int_t k = 0 ; k < PHISLICES ; k++ )
	{
	  ArrayofArrayV[k]     -> Delete() ;
	  ArrayofCharge[k]     -> Delete() ;
	  ArrayofEroverEz[k]   -> Delete() ;  
	  ArrayofEPhioverEz[k] -> Delete() ;  
	}

    }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;            // Table uses phi from 0 to 2*Pi
  z = LimitZ( Sector, x ) ;                         // Protect against discontinuity at CM

  Float_t phi_prime, local_y, r_eff, FLIP = 1.0 ;
  phi_prime = phi ;
  if ( SYMMETRY == 1 ) 
    {
      Int_t   N = (int)(phi/PiOver12)  ;
      phi_prime = phi - N * PiOver12 ;
      if ( TMath::Power(-1,N) < 0 ) phi_prime = PiOver12 - phi_prime ; // Note that 
      if ( TMath::Power(-1,N) < 0 ) FLIP = -1 ;     // Note change of sign.  Assume reflection symmetry!!
    }

  r_eff   = r ;                                     // Do not allow calculation to go too near the gap
  local_y = r * TMath::Cos(phi_prime) ;             
  if ( local_y > GAPRADIUS - WIREGAP && local_y < GAPRADIUS ) r_eff = (GAPRADIUS - WIREGAP) / TMath::Cos(phi_prime) ;
  if ( local_y < GAPRADIUS + WIREGAP && local_y > GAPRADIUS ) r_eff = (GAPRADIUS + WIREGAP) / TMath::Cos(phi_prime) ;

  // Assume symmetry in Z when looking up data in tables, below
  Er_integral   = Interpolate3DTable( ORDER, r_eff, TMath::Abs(z), phi_prime, neR3D, EMap_nZ, PHISLICES, eRadius, eZList, Philist, ArrayoftiltEr )   ;
  Ephi_integral = Interpolate3DTable( ORDER, r_eff, TMath::Abs(z), phi_prime, neR3D, EMap_nZ, PHISLICES, eRadius, eZList, Philist, ArrayoftiltEphi ) ;
  Ephi_integral *= FLIP ;                           // Note possible change of sign if we have reflection symmetry!!

  if (fSpaceChargeR2) GetSpaceChargeR2();           // Get latest spacecharge values from DB 

  // Subtract to Undo the distortions and apply the EWRatio factor to the data on the East end of the TPC

  if ( r > 0.0 ) 
    {
      Float_t Weight = SpaceChargeR2 * (doingDistortion ? SmearCoefSC*SmearCoefGL : 1.0);
      if (GLWeights[Sector] >= 0) Weight *= GLWeights[Sector] ;
      if (                z <  0) Weight *= SpaceChargeEWRatio ;
      phi =  phi - Weight * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - Weight * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;

}

//________________________________________

  
/// Full GridLeak Distortion Calculation
/*!
  Calculate the 3D distortions due to charge leaking out around the edge of all wire grids
  4 locations per sector, all sectors. Mostly a copy of Undo3DGridLeakDistortion().
  Original work by Gene Van Buren and Irakli Chakaberia (GARFIELD simulations)
*/
void StMagUtilities::UndoFullGridLeakDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{ 
     
  const Int_t   ORDER       =    1  ;  // Linear interpolation = 1, Quadratic = 2         
  const Int_t   neR3D       =  132  ;  // Number of rows in the interpolation table for the Electric field
  const Int_t   ITERATIONS  =   80  ;  // Depends on setting for the OverRelaxation parameter ... check results carefully
  const Int_t   ROWS        =  513  ;  // ( 2**n + 1 )  eg. 65, 129, 257, 513, 1025   (513 or above for a natural width gap)
  const Int_t   COLUMNS     =   65  ;  // ( 2**m + 1 )  eg. 65, 129, 257, 513
  const Int_t   PHISLICES   =  180  ;  // ( 12*(2*n + 1) )  eg. 60, 84, 108 ; Note interaction with "GRIDSIZEPHI"
                                       //                   avoids phi at sector boundaries
  const Int_t   PHISLICES1  =  PHISLICES+1   ;  // ( 24*n )  eg. 24, 72, 144 ;
  const Float_t GRIDSIZEPHI =  TMath::TwoPi() / PHISLICES ;
  const Float_t GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
  const Float_t GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;

  static TMatrix *ArrayofArrayV[PHISLICES]   , *ArrayofCharge[PHISLICES]      ; 
  static TMatrix *ArrayofEroverEzW[PHISLICES], *ArrayofEPhioverEzW[PHISLICES] ; 
  static TMatrix *ArrayofEroverEzE[PHISLICES], *ArrayofEPhioverEzE[PHISLICES] ; 

  static Float_t  Rlist[ROWS], Zedlist[COLUMNS] ;

  static TMatrix *ArrayoftiltEr[PHISLICES1] ;
  static TMatrix *ArrayoftiltEphi[PHISLICES1] ;

  // Use custom list of radii because we need extra resolution near the edges
  static Float_t eRadius[neR3D] = {  50.0,   50.5,  51.0,   51.25,  51.5,
				     51.75,  52.0,  52.25,  52.5,   52.75,
				     53.0,   53.25, 53.5,   53.75,  54.0,
				     54.25,  54.75, 55.0,   55.25,  55.5,
				     56.0,   56.5,  57.0,   58.0,   60.0,
				     62.0,   66.0,  70.0,   80.0,   90.0,
				    100.0,  104.0,  106.5,  109.0,  111.5,
				    114.0,  115.0,  116.0,  117.0,  118.0,
				    118.5,  118.75, 119.0,  119.25, 119.5,
				    119.75, 120.0,  120.25, 120.5,  120.75,
				    121.0,  121.25, 121.5,  121.75, 122.0,
				    122.25, 122.5,  122.75, 123.0,  123.25,
				    123.5,  123.75, 124.0,  124.25, 124.5,
				    124.75, 125.0,  125.25, 125.5,  125.75,
				    126.0,  126.25, 126.5,  126.75, 127.0,
				    127.25, 127.5,  128.0,  128.5,  129.0,
				    129.5,  130.0,  130.5,  131.0,  131.5,
				    132.0,  133.0,  135.0,  137.5,  140.0,
				    145.0,  150.0,  160.0,  170.0,  180.0,
				    184.0,  186.5,  189.0,  190.0,  190.5,
				    190.75, 191.0,  191.25, 191.5,  191.75,
				    192.0,  192.25, 192.5,  192.75, 193.0,
				    193.25, 193.5,  193.75, 194.0,  194.25,
				    194.5,  194.75, 195.0,  195.25, 195.5,
				    196.0,  196.25, 196.5,  196.75, 197.0,
				    197.25, 197.5,  198.0,  198.5,  199.0,
				    199.5,  200.0 } ; 

  static Float_t Philist [PHISLICES1] ; // Note that there will be rapid changes near 15 degrees on padrow 13

  Float_t  Er_integral, Ephi_integral ;
  Float_t  r, phi, z ;

  memcpy(Xprime,x,threeFloats) ;

  if ( MiddlGridLeakStrength == 0 ) return ; 

  if ( DoOnce )
    {
      cout << "StMagUtilities::UndoFullGrid Please wait for the tables to fill ...  ~5 seconds * PHISLICES" << endl ;
      Int_t   SeclistW[PHISLICES1] ;
      Int_t   SeclistE[PHISLICES1] ;
      Float_t SecPhis [PHISLICES1] ;
      
      for ( Int_t i = 0 ; i < ROWS ; i++ ) Rlist[i] = IFCRadius + i*GRIDSIZER ;
      for ( Int_t j = 0 ; j < COLUMNS ; j++ ) Zedlist[j]  = j * GRIDSIZEZ ;
      
      for ( Int_t k = 0 ; k < PHISLICES1 ; k++ )
        {
          Philist[k] = GRIDSIZEPHI * k ;
          Int_t k2 = (12*k+PHISLICES/2)/PHISLICES;
          SeclistW[k] = (14-k2)%12+1;
          SeclistE[k] = (k2+8)%12+13;
          SecPhis[k] = GRIDSIZEPHI * (k2*PHISLICES/12-k) ;
        }
      
      for ( Int_t k = 0 ; k < PHISLICES1 ; k++ )
        {
          ArrayoftiltEr[k]   =  new TMatrix(neR3D,EMap_nZ) ;
          ArrayoftiltEphi[k] =  new TMatrix(neR3D,EMap_nZ) ;
        }
      
      for ( Int_t k = 0 ; k < PHISLICES ; k++ )
      {
        ArrayofArrayV[k]      =  new TMatrix(ROWS,COLUMNS) ;
        ArrayofCharge[k]      =  new TMatrix(ROWS,COLUMNS) ;
        ArrayofEroverEzW[k]   =  new TMatrix(ROWS,COLUMNS) ;
        ArrayofEPhioverEzW[k] =  new TMatrix(ROWS,COLUMNS) ;
        ArrayofEroverEzE[k]   =  new TMatrix(ROWS,COLUMNS) ;
        ArrayofEPhioverEzE[k] =  new TMatrix(ROWS,COLUMNS) ;
      }

      for ( Int_t m = -1; m < 2 ; m+=2 ) // west (m = -1), then east (m = 1)
        {
          TMatrix** ArrayofEroverEz   = ( m < 0 ? ArrayofEroverEzW   : ArrayofEroverEzE   ) ;
          TMatrix** ArrayofEPhioverEz = ( m < 0 ? ArrayofEPhioverEzW : ArrayofEPhioverEzE ) ;
          Int_t*    Seclist           = ( m < 0 ? SeclistW           : SeclistE           ) ;

          for ( Int_t k = 0 ; k < PHISLICES ; k++ )
    	    {
  	      TMatrix &ArrayV    =  *ArrayofArrayV[k] ;
	      TMatrix &Charge    =  *ArrayofCharge[k] ;
              Float_t cosPhiK    =  TMath::Cos(SecPhis[k]) ;

	      //Fill arrays with initial conditions.  V on the boundary and Charge in the volume.
              ArrayV.Zero();  // Fill Vmatrix with Boundary Conditions
              Charge.Zero();

	      for ( Int_t i = 1 ; i < ROWS-1 ; i++ ) 
	        { 
	          Float_t Radius = IFCRadius + i*GRIDSIZER ;
	          Float_t local_y_hi  = (Radius+GRIDSIZER/2.0) * cosPhiK ;
	          Float_t local_y_lo  = (Radius-GRIDSIZER/2.0) * cosPhiK ;

		  // Here we are finding rho in the cell of volume V to be (1/V)*Integral(local_rho * dV)
		  // In the below formulas...
		  // 'Weight' will be 1/V
		  // dV will be the volume between 'top' and 'bottom'
		  // 'SCscale * GLWeights' will be the local_rho
		  // 'local_charge' will be local_rho*dV

                  // Scale by SpaceCharge at this radius relative to SpaceCharge at GAPRADIUS
                  // as the electrons that feed GridLeak have the same radial dependence as SpaceCharge
                  // (Garfield simulations assumed flat radial dependence).
                  // Note that this dlightly destroys getting the total charge in the middle sheet
                  // to be equal to that in 3DGridLeak, but this is more accurate.
	          Float_t SCscale = SpaceChargeRadialDependence(Radius)/SpaceChargeRadialDependence(GAPRADIUS) ;

		  Float_t top = 0, bottom = 0 ;
                  Float_t local_charge = 0;

                  for (Int_t l = 0; l < 4 ; l++ )
                    {

                      if (local_y_hi < GL_charge_y_lo[l] || local_y_lo > GL_charge_y_hi[l]) continue;
                      top    = (local_y_hi > GL_charge_y_hi[l] ? GL_charge_y_hi[l] : local_y_hi);
                      bottom = (local_y_lo < GL_charge_y_lo[l] ? GL_charge_y_lo[l] : local_y_lo);
                      local_charge += SCscale * GLWeights[Seclist[k]-1 + l*24] * (top*top - bottom*bottom);

                    }

		  Float_t Weight  =  1.0 / (local_y_hi*local_y_hi - local_y_lo*local_y_lo) ;
		  // Weight by ratio of volumes for a partially-full cell / full cell (in Cylindrical Coords).
		  // Note that Poisson's equation is using charge density ... so if rho = 1.0, then volume is what counts.
 
		  const Float_t BackwardsCompatibilityRatio = 3.75 ;      // DB uses different value for legacy reasons

	          for ( Int_t j = 1 ; j < COLUMNS-1 ; j++ )    
		    {

		      Charge(i,j)  =  local_charge * Weight * MiddlGridLeakStrength * BackwardsCompatibilityRatio ;

		    }
		}
	    }
      
          //Solve Poisson's equation in 3D cylindrical coordinates by relaxation technique
          //Allow for different size grid spacing in R and Z directions

          Poisson3DRelaxation( ArrayofArrayV, ArrayofCharge, ArrayofEroverEz, ArrayofEPhioverEz, PHISLICES, 
			       GRIDSIZEPHI, ITERATIONS, 0) ;

	} // m (west/east) loop

      //Interpolate results onto a custom grid which is used just for the grid leak calculation.

      for ( Int_t k = 0 ; k < PHISLICES1 ; k++ )
        {
          TMatrix &tiltEr   = *ArrayoftiltEr[k] ;
          TMatrix &tiltEphi = *ArrayoftiltEphi[k] ;
          phi = Philist[k == PHISLICES ? 0 : k] ;
          for ( Int_t i = 0 ; i < EMap_nZ ; i++ ) 
            {
              z = TMath::Abs(eZList[i]) ;
              TMatrix** ArrayofEroverEz   = ( eZList[i] > 0 ? ArrayofEroverEzW   : ArrayofEroverEzE   ) ;
              TMatrix** ArrayofEPhioverEz = ( eZList[i] > 0 ? ArrayofEPhioverEzW : ArrayofEPhioverEzE ) ;
              for ( Int_t j = 0 ; j < neR3D ; j++ ) 
                { 
                  r = eRadius[j] ;
                  tiltEr(j,i)    = Interpolate3DTable( ORDER,r,z,phi,ROWS,COLUMNS,PHISLICES,Rlist,Zedlist,Philist,ArrayofEroverEz )   ;
                  tiltEphi(j,i)  = Interpolate3DTable( ORDER,r,z,phi,ROWS,COLUMNS,PHISLICES,Rlist,Zedlist,Philist,ArrayofEPhioverEz ) ;
                }
            }
        }

      for ( Int_t k = 0 ; k < PHISLICES ; k++ )
	{
	  ArrayofArrayV[k]     -> Delete() ;
	  ArrayofCharge[k]     -> Delete() ;
          ArrayofEroverEzW[k]   -> Delete() ;  
          ArrayofEPhioverEzW[k] -> Delete() ;  
          ArrayofEroverEzE[k]   -> Delete() ;  
          ArrayofEPhioverEzE[k] -> Delete() ;  
	}

    }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;            // Table uses phi from 0 to 2*Pi
  z = LimitZ( Sector, x ) ;                         // Protect against discontinuity at CM

  Float_t cos_phi_prime, local_y, r_eff ;
  cos_phi_prime = TMath::Cos(phi - PiOver6 * ((int) ((phi/PiOver6)+0.5))) ; // cos of local phi within sector
  
  r_eff   = r ;                                     // Do not allow calculation to go too near the gap
  local_y = r * cos_phi_prime ;
  // Undo3DGridLeak used a margin of WIREGAP(1.595)/2 = 0.7975 cm beyond the edge of the charge sheet
  // Here we will continue to use the same margin
  const Float_t margin = 0.7975 ;

  const Float_t middle_of_inner_sheet = 0.5 * (GL_charge_y_lo[0] + GL_charge_y_hi[0]);
  const Float_t middle_of_outer_sheet = 0.5 * (GL_charge_y_lo[3] + GL_charge_y_hi[3]);
       if ( local_y > GL_charge_y_lo[0] - margin && local_y < middle_of_inner_sheet) r_eff = (GL_charge_y_lo[0] - margin) / cos_phi_prime;
  else if ( local_y < GL_charge_y_hi[0] + margin && local_y > middle_of_inner_sheet) r_eff = (GL_charge_y_hi[0] + margin) / cos_phi_prime;
  else if ( local_y > GL_charge_y_lo[1] - margin && local_y < GL_charge_y_hi[1]    ) r_eff = (GL_charge_y_lo[1] - margin) / cos_phi_prime;
  else if ( local_y < GL_charge_y_hi[2] + margin && local_y > GL_charge_y_lo[2]    ) r_eff = (GL_charge_y_hi[2] + margin) / cos_phi_prime;
  else if ( local_y > GL_charge_y_lo[3] - margin && local_y < middle_of_outer_sheet) r_eff = (GL_charge_y_lo[3] - margin) / cos_phi_prime;
  else if ( local_y < GL_charge_y_hi[3] + margin && local_y > middle_of_outer_sheet) r_eff = (GL_charge_y_hi[3] + margin) / cos_phi_prime;

  Er_integral   = Interpolate3DTable( ORDER, r_eff, z, phi, neR3D, EMap_nZ, PHISLICES1, eRadius, eZList, Philist, ArrayoftiltEr )   ;
  Ephi_integral = Interpolate3DTable( ORDER, r_eff, z, phi, neR3D, EMap_nZ, PHISLICES1, eRadius, eZList, Philist, ArrayoftiltEphi ) ;

  if (fSpaceChargeR2) GetSpaceChargeR2();           // Get latest spacecharge values from DB 

  // Subtract to Undo the distortions and apply the EWRatio factor to the data on the East end of the TPC

  if ( r > 0.0 ) 
    {
      Float_t Weight = SpaceChargeR2 * (doingDistortion ? SmearCoefSC*SmearCoefGL : 1.0);
      if ( z < 0 ) Weight *= SpaceChargeEWRatio ;
      phi =  phi - Weight * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - Weight * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;

}

//________________________________________

  
/// 3D Sector Alignment Distortion Calculation
/*!
  Calculate the 3D distortions due to z displacements caused by mis-alignment of the inner and outer sectors.
  Original work by Gene VanBuren, and J. Thomas 
  Method:
  Determine the potential offsets at the innermost and outermost
  points of the Inner and Outer Sector grids along this phi angle,
  then interpolate to any poinr between the two.
  Also interpolate radially to zero out to the field cages.
 
*/
void StMagUtilities::UndoSectorAlignDistortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{ 
     
  const Int_t   ORDER       =    1  ;  // Linear interpolation = 1, Quadratic = 2         
  const Int_t   neR3D       =   73  ;  // Number of rows in the interpolation table for the Electric field
  const Int_t   ITERATIONS  =  100  ;  // Depends on setting for the OverRelaxation parameter ... check results carefully
  const Int_t   ROWS        =  257  ;  // ( 2**n + 1 )  eg. 65, 129, 257, 513, 1025
  const Int_t   COLUMNS     =   65  ;  // ( 2**m + 1 )  eg. 65, 129, 257, 513
  const Int_t   PHISLICES   =  180  ;  // ( 12*(2*n + 1) )  eg. 60, 84, 108 ; Note interaction with "GRIDSIZEPHI"
                                       //                   avoids phi at sector boundaries
  const Int_t   PHISLICES1  =  PHISLICES+1   ;  // ( 24*n )  eg. 24, 72, 144 ;
  const Float_t GRIDSIZEPHI =  TMath::TwoPi() / PHISLICES ;
  const Float_t GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
  const Float_t GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;

  const Float_t INNERGGSpan  = INNERGGLast - INNERGGFirst ;
  const Float_t OUTERGGSpan  = OUTERGGLast - OUTERGGFirst ;

  static TMatrix *ArrayofArrayV[PHISLICES]   , *ArrayofCharge[PHISLICES]      ; 
  static TMatrix *ArrayofEroverEzW[PHISLICES], *ArrayofEPhioverEzW[PHISLICES] ; 
  static TMatrix *ArrayofEroverEzE[PHISLICES], *ArrayofEPhioverEzE[PHISLICES] ; 

  static Float_t  Rlist[ROWS], Zedlist[COLUMNS] ;

  static TMatrix *ArrayoftiltEr[PHISLICES1] ;
  static TMatrix *ArrayoftiltEphi[PHISLICES1] ;

  // Use custom list of radii because we need extra resolution near the gap
  static Float_t eRadius[neR3D] = {   50.0,   60.0,   70.0,   80.0,   90.0, 
                                     100.0,  104.0,  106.5,  109.0,  111.5, 
                                     114.0,  115.0,  116.0,  117.0,  118.0,  
                                     118.5,  118.75, 119.0,  119.25, 119.5, 
                                     119.75, 120.0,  120.25, 120.5,  120.75, 
                                     121.0,  121.25, 121.5,  121.75, 122.0,  
                                     122.25, 122.5,  122.75, 123.0,  123.25, 
                                     123.5,  123.75, 124.0,  124.25, 124.5,  
                                     124.75, 125.0,  125.25, 125.5,  126.0, 
                                     126.25, 126.5,  126.75, 127.0,  127.25,  
                                     127.5,  128.0,  128.5,  129.0,  129.5,  
                                     130.0,  130.5,  131.0,  131.5,  132.0,  
                                     133.0,  135.0,  137.5,  140.0,  145.0,  
                                     150.0,  160.0,  170.0,  180.0,  190.0,  
                                     195.0,  198.0,  200.0 } ;

  static Float_t Philist [PHISLICES1] ; // Note that there will be rapid changes near 15 degrees on padrow 13

  Float_t  Er_integral, Ephi_integral ;
  Float_t  r, phi, z ;
  //Int_t lastSec = -1;


  memcpy(Xprime,x,threeFloats) ;

  if ( DoOnce )
    {
      cout << "StMagUtilities::UndoSectorAlign Please wait for the tables to fill ...  ~5 seconds * PHISLICES" << endl ;
      Int_t   SeclistW[PHISLICES1] ;
      Int_t   SeclistE[PHISLICES1] ;
      Float_t SecPhis [PHISLICES1] ;

      for ( Int_t i = 0 ; i < ROWS ; i++ ) Rlist[i] = IFCRadius + i*GRIDSIZER ;
      for ( Int_t j = 0 ; j < COLUMNS ; j++ ) Zedlist[j]  = j * GRIDSIZEZ ;
      
      for ( Int_t k = 0 ; k < PHISLICES1 ; k++ )
        {
          Philist[k] = GRIDSIZEPHI * k ;
          Int_t k2 = (12*k+PHISLICES/2)/PHISLICES;
          SeclistW[k] = (14-k2)%12+1;
          SeclistE[k] = (k2+8)%12+13;
          SecPhis[k] = GRIDSIZEPHI * (k2*PHISLICES/12-k) ;
        }
      
      for ( Int_t k = 0 ; k < PHISLICES1 ; k++ )
        {
          ArrayoftiltEr[k]   =  new TMatrix(neR3D,EMap_nZ) ;
          ArrayoftiltEphi[k] =  new TMatrix(neR3D,EMap_nZ) ;
        }
      
      for ( Int_t k = 0 ; k < PHISLICES ; k++ )
      {
        ArrayofArrayV[k]      =  new TMatrix(ROWS,COLUMNS) ;
        ArrayofCharge[k]      =  new TMatrix(ROWS,COLUMNS) ;
        ArrayofEroverEzW[k]   =  new TMatrix(ROWS,COLUMNS) ;
        ArrayofEPhioverEzW[k] =  new TMatrix(ROWS,COLUMNS) ;
        ArrayofEroverEzE[k]   =  new TMatrix(ROWS,COLUMNS) ;
        ArrayofEPhioverEzE[k] =  new TMatrix(ROWS,COLUMNS) ;
      }

      for ( Int_t m = -1; m < 2 ; m+=2 ) // west (m = -1), then east (m = 1)
        {
          TMatrix** ArrayofEroverEz   = ( m < 0 ? ArrayofEroverEzW   : ArrayofEroverEzE   ) ;
          TMatrix** ArrayofEPhioverEz = ( m < 0 ? ArrayofEPhioverEzW : ArrayofEPhioverEzE ) ;
          Int_t*    Seclist           = ( m < 0 ? SeclistW           : SeclistE           ) ;
          
          
          for ( Int_t k = 0 ; k < PHISLICES ; k++ )
            {
              TMatrix &ArrayV    =  *ArrayofArrayV[k] ;
              TMatrix &Charge    =  *ArrayofCharge[k] ;
              
              //Fill arrays with initial conditions.  V on the boundary and Charge in the volume.
              ArrayV.Zero();  // Fill Vmatrix with Boundary Conditions
              Charge.Zero();
              
              Double_t tanSecPhi = TMath::Tan(SecPhis[k]);
              Double_t cosSecPhi = TMath::Cos(SecPhis[k]);
              Double_t secSecPhi = 1.0/cosSecPhi;
              Double_t iOffsetFirst, iOffsetLast, oOffsetFirst, oOffsetLast;

              if (StTpcDb::instance())
                {
                  Double_t local[3] = {0,0,0};
#if 0		  
                  Double_t master[3];
                  

                  // To test internal sector alignment parameters only:
                  //const TGeoHMatrix& iAlignMatrix = StTpcDb::instance()->SubSInner2SupS(Seclist[k]);
                  //const TGeoHMatrix& oAlignMatrix = StTpcDb::instance()->SubSOuter2SupS(Seclist[k]);
                  // For sector alignment with respect to the TPC
                  const TGeoHMatrix& iAlignMatrix = StTpcDb::instance()->SubSInner2Tpc(Seclist[k]);
                  const TGeoHMatrix& oAlignMatrix = StTpcDb::instance()->SubSOuter2Tpc(Seclist[k]);
                  
                  // For debugging the rotation matrices
                  /*
                  if (SeclistW[k] != lastSec) {
                    iAlignMatrix.Print();
                    oAlignMatrix.Print();
                    lastSec=SeclistW[k];
                  }
                  */
                  
#else
		  static StTpcCoordinateTransform tran;
		  static StTpcLocalCoordinate locP;
		  Double_t *master = locP.position().xyz();
#endif
                  // For the alignment, 'local' is the ideal position of the point
                  // on the endcap, and 'master' is the real position of that point.
                  // For this distortion, we will only worry about z displacements.

                  local[0] = m * INNERGGFirst * tanSecPhi;
                  local[1] = INNERGGFirst;
#if 0
                  iAlignMatrix.LocalToMaster(local,master);
#else
		  StTpcLocalSectorCoordinate lSec(local[0],local[1],local[2],Seclist[k]);
		  tran(lSec,locP);
#endif
                  iOffsetFirst = (TPC_Z0 + m * master[2]) * StarMagE;
                  
                  local[0] = m * INNERGGLast  * tanSecPhi;
                  local[1] = INNERGGLast;
#if 0
                  iAlignMatrix.LocalToMaster(local,master);
#else
		  lSec = StTpcLocalSectorCoordinate(local[0],local[1],local[2],Seclist[k]);
		  tran(lSec,locP);
#endif
                  iOffsetLast  = (TPC_Z0 + m * master[2]) * StarMagE;
                  
                  local[0] = m * OUTERGGFirst * tanSecPhi;
                  local[1] = OUTERGGFirst;
#if 0
                  oAlignMatrix.LocalToMaster(local,master);
#else
		  lSec = StTpcLocalSectorCoordinate(local[0],local[1],local[2],Seclist[k]);
		  tran(lSec,locP);
#endif
                  oOffsetFirst = (TPC_Z0 + m * master[2]) * StarMagE;
                  
                  local[0] = m * OUTERGGLast  * tanSecPhi;
                  local[1] = OUTERGGLast;
#if 0
                  oAlignMatrix.LocalToMaster(local,master);
#else
		  lSec = StTpcLocalSectorCoordinate(local[0],local[1],local[2],Seclist[k]);
		  tran(lSec,locP);
#endif
                  oOffsetLast  = (TPC_Z0 + m * master[2]) * StarMagE;

                } else {

                  // toy models (not reading from DB)
                  // this model is 1 (0.5) mm at OUTERGGFirst of Sec 12 (24)
                  iOffsetFirst = 0;
                  iOffsetLast = 0;
                  oOffsetFirst = (Seclist[k] == 12 || Seclist[k] == 24 ?
                                  0.1 * StarMagE * (1.5 - Seclist[k]/24.) : 0);
                  oOffsetLast = 0;

                }


              
              for ( Int_t i = 1 ; i < ROWS-1 ; i++ ) 
                { 
                  Double_t Radius = IFCRadius + i*GRIDSIZER ;
                  Double_t local_y  = Radius * cosSecPhi ;
                  Double_t tempV;
                  if (local_y <= INNERGGFirst)
                    tempV = iOffsetFirst*(Radius - IFCRadius)/(INNERGGFirst*secSecPhi - IFCRadius);
                  else if (local_y <= INNERGGLast)
                    tempV = iOffsetFirst + (iOffsetLast -iOffsetFirst)*(local_y-INNERGGFirst)/INNERGGSpan;
                  else if (local_y <= OUTERGGFirst)
                    tempV = iOffsetLast  + (oOffsetFirst-iOffsetLast) *(local_y-INNERGGLast) /WIREGAP;
                  else if (local_y <= OUTERGGLast)
                    tempV = oOffsetFirst + (oOffsetLast -oOffsetFirst)*(local_y-OUTERGGFirst)/OUTERGGSpan;
                  else
                    tempV = oOffsetLast*(OFCRadius - Radius)/(OFCRadius - OUTERGGLast*secSecPhi);
                  ArrayV(i,COLUMNS-1) = tempV;
                }
            }      
            
          //Solve Poisson's equation in 3D cylindrical coordinates by relaxation technique
          //Allow for different size grid spacing in R and Z directions
          
          Poisson3DRelaxation( ArrayofArrayV, ArrayofCharge, ArrayofEroverEz, ArrayofEPhioverEz, PHISLICES, 
                              GRIDSIZEPHI, ITERATIONS, 0) ;
        } // m (west/east) loop
      
          
      //Interpolate results onto a custom grid which is used just for the sector align calculation.
      
      for ( Int_t k = 0 ; k < PHISLICES1 ; k++ )
        {
          TMatrix &tiltEr   = *ArrayoftiltEr[k] ;
          TMatrix &tiltEphi = *ArrayoftiltEphi[k] ;
          phi = Philist[k == PHISLICES ? 0 : k] ;
          for ( Int_t i = 0 ; i < EMap_nZ ; i++ ) 
            {
              z = TMath::Abs(eZList[i]) ;
              TMatrix** ArrayofEroverEz   = ( eZList[i] > 0 ? ArrayofEroverEzW   : ArrayofEroverEzE   ) ;
              TMatrix** ArrayofEPhioverEz = ( eZList[i] > 0 ? ArrayofEPhioverEzW : ArrayofEPhioverEzE ) ;
              for ( Int_t j = 0 ; j < neR3D ; j++ ) 
                { 
                  r = eRadius[j] ;
                  tiltEr(j,i)    = Interpolate3DTable( ORDER,r,z,phi,ROWS,COLUMNS,PHISLICES,Rlist,Zedlist,Philist,ArrayofEroverEz )   ;
                  tiltEphi(j,i)  = Interpolate3DTable( ORDER,r,z,phi,ROWS,COLUMNS,PHISLICES,Rlist,Zedlist,Philist,ArrayofEPhioverEz ) ;
                }
            }
        }
      
      for ( Int_t k = 0 ; k < PHISLICES ; k++ )
        {
          ArrayofArrayV[k]      -> Delete() ;
          ArrayofCharge[k]      -> Delete() ;
          ArrayofEroverEzW[k]   -> Delete() ;  
          ArrayofEPhioverEzW[k] -> Delete() ;  
          ArrayofEroverEzE[k]   -> Delete() ;  
          ArrayofEPhioverEzE[k] -> Delete() ;  
        }
          
    }
  
  if (usingCartesian) Cart2Polar(x,r,phi);
  else { r = x[0]; phi = x[1]; }
  if ( phi < 0 ) phi += TMath::TwoPi() ;            // Table uses phi from 0 to 2*Pi
  z = LimitZ( Sector, x ) ;                         // Protect against discontinuity at CM
  
  Er_integral   = Interpolate3DTable( ORDER, r, z, phi, neR3D, EMap_nZ, PHISLICES1, eRadius, eZList, Philist, ArrayoftiltEr )   ;
  Ephi_integral = Interpolate3DTable( ORDER, r, z, phi, neR3D, EMap_nZ, PHISLICES1, eRadius, eZList, Philist, ArrayoftiltEphi ) ;
  
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }
  
  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;
  
}


//________________________________________

// Must call IterationFailCount once to initialize it
// before it starts counting (will return a negative number)
// Each call resets the count to zero, so it gives the counts
// since the last time it was called.
Int_t StMagUtilities::IterationFailCount()
{ 

  Int_t temp = iterationFailCounter;
  iterationFailCounter = 0;
  return temp;

}
//________________________________________________________________________________
void StMagUtilities::BFieldTpc ( const Float_t xTpc[], Float_t BTpc[], Int_t Sector ) {
  if (StTpcDb::IsOldScheme()) {
      BField( xTpc, BTpc) ; 
  } else {
  // mag. field in Tpc local coordinate system
    Double_t Tpc[3] =  {xTpc[0], xTpc[1], xTpc[2]};
    Double_t coorG[3];
    StTpcDb::instance()->Tpc2GlobalMatrix().LocalToMaster(Tpc,coorG);
    Float_t xyzG[3] = {(Float_t) coorG[0], (Float_t) coorG[1], (Float_t) coorG[2]};
    Float_t BG[3];
    BField( xyzG, BG) ; 
    Double_t    BGD[3] = {BG[0], BG[1], BG[2]};
    Double_t    BTpcL[3];
    StTpcDb::instance()->Tpc2GlobalMatrix().MasterToLocalVect(BGD,BTpcL);
    BTpc[0] = BTpcL[0];
    BTpc[1] = BTpcL[1];
    BTpc[2] = BTpcL[2];
  }  
}
//________________________________________________________________________________
void StMagUtilities::B3DFieldTpc ( const Float_t xTpc[], Float_t BTpc[], Int_t Sector ) {
  if (StTpcDb::IsOldScheme()) {
    B3DField( xTpc, BTpc) ; 
  } else {
    BFieldTpc(xTpc, BTpc, Sector);
  }  
}
//________________________________________________________________________________
