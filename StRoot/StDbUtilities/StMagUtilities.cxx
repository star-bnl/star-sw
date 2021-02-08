/***********************************************************************
 *
 * $Id: StMagUtilities.cxx,v 1.111.2.1 2018/11/11 16:07:38 didenko Exp $
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
 * Revision 1.111.2.1  2018/11/11 16:07:38  didenko
 * branch updates for S18c_embed
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
  kFullGridLeak      = 0x80000   // Bit 20                            <br>
  kDistoSmearing     = 0x100000  // Bit 21                            <br>
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
#include "StDetectorDbMaker/St_tpcFieldCageShortC.h"
#include "StDetectorDbMaker/StTpcSurveyC.h"
#include "StDetectorDbMaker/St_trigDetSumsC.h"
#include "StDetectorDbMaker/St_tpcCalibResolutionsC.h"
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
  GetTPCVoltages( mode );    // Get the TPC Voltages from the DB
  GetHVPlanes()         ;    // Get the parameters that describe the HV plane errors (after GetTPCVoltages!)
  GetOmegaTau ()        ;    // Get Omega Tau parameters
  GetSpaceCharge()      ;    // Get the spacecharge variable from the DB
  GetSpaceChargeR2()    ;    // Get the spacecharge variable R2 from the DB and EWRatio
  GetShortedRing()      ;    // Get the parameters that describe the shorted ring on the field cage
  GetGridLeak( mode )   ;    // Get the parameters that describe the gating grid leaks
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
  St_tpcPadPlanesC*      pads = StTpcDb::instance()->PadPlaneGeometry();
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
  INNER          =  pads->innerPadRows(); // Use Sector 1 for default values
  TPCROWS        =  pads->padRows();
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
  for ( Int_t i = 0 ; i < TPCROWS ; i++ )
    TPCROWR[i] = pads->radialDistanceAtRow(i+1); // Use Sector 1 for default values
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
  fTpcVolts      =  StDetectorDbTpcVoltages::instance() ;  // Initialize the DB for TpcVoltages
  CathodeV       =  fTpcVolts->getCathodeVoltage() * 1000 ; 
  GG             =  fTpcVolts->getGGVoltage() ; 
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
      innerVs.Fill(anodeVolts->voltagePadrow(i,INNER));
      outerVs.Fill(anodeVolts->voltagePadrow(i,INNER+1));
    }
    double cmnInner = innerVs.GetBinCenter(innerVs.GetMaximumBin());
    double cmnOuter = outerVs.GetBinCenter(outerVs.GetMaximumBin());
    cout << "StMagUtilities assigning common anode voltages as " << cmnInner << " , " << cmnOuter << endl;
    for (Int_t i = 1 ; i < 25; i++ ) {
      GLWeights[i] = ( ( TMath::Abs(anodeVolts->voltagePadrow(i,INNER  ) - cmnInner) < stepsInner/2. ) &&
                       ( TMath::Abs(anodeVolts->voltagePadrow(i,INNER+1) - cmnOuter) < stepsOuter/2. ) ? 1 : -1 );
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
      GLWeights[i   ] = GL_rho_inner_of_innerSec(anodeVolts->voltagePadrow(i+1,      1)) * norm ;
      GLWeights[i+24] = GL_rho_outer_of_innerSec(anodeVolts->voltagePadrow(i+1,INNER  )) * norm ;
      GLWeights[i+48] = GL_rho_inner_of_outerSec(anodeVolts->voltagePadrow(i+1,INNER+1)) * norm ;
      GLWeights[i+72] = GL_rho_outer_of_outerSec(anodeVolts->voltagePadrow(i+1,TPCROWS)) * norm ;
    }
  }
  
}

void StMagUtilities::GetSpaceCharge ()  
{ 
  static spaceChargeCor_st* spaceTable = 0;
  static St_trigDetSumsC* scalers = 0;

  StDetectorDbSpaceCharge* spaceChair = StDetectorDbSpaceCharge::instance();
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

  StDetectorDbSpaceChargeR2* spaceChair = StDetectorDbSpaceChargeR2::instance();
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
  fOmegaTau  =  StDetectorDbTpcOmegaTau::instance();
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
   fGridLeak   =  StDetectorDbGridLeak::instance()  ;
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
      INNER          =  13   ;      // Number of TPC rows in the inner sectors
      TPCROWS        =  45   ;      // Total number of TPC rows per sector (Inner + Outer)
      IFCRadius   =    47.90 ;      // Radius of the Inner Field Cage
      OFCRadius   =    200.0 ;      // Radius of the Outer Field Cage
      INNERGGFirst =  53.0   ;      // Radius of the first Inner Gating Grid Wire
      INNERGGLast  = 121.0   ;      // Radius of the last Inner Gating Grid Wire
      OUTERGGFirst = 122.595 ;      // Radius of the first Outer Gating Grid Wire
      OUTERGGLast  = 191.395 ;      // Radius of the last Outer Gating Grid Wire
      GAPRADIUS   =    121.8 ;      // Radius of the gap between the inner and outer grids (cm) at sector centerline
      WIREGAP     =    1.595 ;      // Width of the gap between the inner and outer grids (cm)
      for ( Int_t i = 0 ; i < TPCROWS ; i++ )
        {
          if ( i < 8 ) 
	    TPCROWR[i] = 60.0 + i*4.8 ;
          else if ( i < INNER ) 
	    TPCROWR[i] = 93.6 + (i-8+1)*5.2 ;
          else               
	    TPCROWR[i] = 127.195 + (i-INNER)*2.0 ;
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


  // Parse the mode switch which was received from the Tpt maker
  // To turn on and off individual distortions, set these higher bits
  // Default behavior: no bits set gives you the following defaults

  mDistortionMode |= mode;
  if (mDistortionMode & kDisableTwistClock) {
    mDistortionMode &= ~(mDistortionMode & kTwist);
    mDistortionMode &= ~(mDistortionMode & kClock);
    mDistortionMode &= ~(mDistortionMode & kFast2DBMap);
  }
  if ( !( mode & ( kBMap | kPadrow13 | kTwist | kClock | kMembrane | kEndcap | kIFCShift | kSpaceCharge | kSpaceChargeR2 
                         | kShortedRing | kFast2DBMap | kGridLeak | k3DGridLeak | kGGVoltError | kSectorAlign | kFullGridLeak))) 
    {
       mDistortionMode |= kPadrow13 ;
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
  if ( mDistortionMode & kTwist )         printf (" + Twist") ;
  if ( mDistortionMode & kClock )         printf (" + Clock") ;
  if ( mDistortionMode & kIFCShift )      printf (" + IFCShift") ;
  if ( mDistortionMode & kSpaceCharge )   printf (" + SpaceCharge") ;
  if ( mDistortionMode & kSpaceChargeR2 ) printf (" + SpaceChargeR2") ;
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
  cout << "StMagUtilities::IFCShift      =  " << IFCShift << " cm" << endl ;
  cout << "StMagUtilities::CathodeV      =  " << CathodeV << " volts" << endl ;
  cout << "StMagUtilities::GG            =  " << GG << " volts" << endl ;
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

  if (mDistortionMode & kPadrow13) {
      UndoPad13Distortion    ( Xprime1, Xprime2, Sector ) ;
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


#define  NYARRAY       37               // Dimension of the vector to contain the YArray
#define  NZDRIFT       19               // Dimension of the vector to contain ZDriftArray

/// Pad row 13 distortion
/*!
    Remove the effect of the mechanical imperfections between the inner sectors
    and the outer sectors.  There is a gap between the sectors that allow E field
    lines to leak out of the anode and gated grid region.  HHWieman has modelled this
    effect and his solution is used to remove the distortions.
 */
void StMagUtilities::UndoPad13Distortion( const Float_t x[], Float_t Xprime[] , Int_t Sector )
{

  const Int_t   ORDER    = 2     ;               // ORDER = 1 is linear, ORDER = 2 is quadratice interpolation (Leave at 2 for legacy reasons)
  const Int_t   TERMS    = 400   ;               // Number of terms in the sum
  const Float_t SCALE    = 0.192 ;               // Set the scale for the correction
  const Float_t BOX      = 200.0 - GAPRADIUS ;   // Size of the box in which to work
  const Float_t PI       = TMath::Pi() ;

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
  Polar2Cart(r,phi,Xprime);

  if (usingCartesian) Polar2Cart(r,phi,Xprime);
  else { Xprime[0] = r; Xprime[1] = phi; }
  Xprime[2] = x[2] ;
  
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
      TMatrix  ArrayE(ROWS,COLUMNS), EroverEz(ROWS,COLUMNS) ;
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

  Float_t coef1[ROWS],coef2[ROWS],coef3[ROWS],coef4[ROWS];
  memset(coef1,0,ROWS*sizeof(Float_t));
  memset(coef2,0,ROWS*sizeof(Float_t));
  memset(coef3,0,ROWS*sizeof(Float_t));
  memset(coef4,0,ROWS*sizeof(Float_t));

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
      Float_t OverRelaxcoef4[ROWS] ;
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

      }
    }      

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

  // Return default values if passed a whacko input value (i.e. infinite or NaN)
  if ( finite((double)Charge)*finite(x[0])*finite(x[1])*finite(x[2])*finite(p[0])*finite(p[1])*finite(p[2]) == 0 ) return ;

  const Int_t   ROWS   = TPCROWS ;               // Total number of TPC rows per sector (Inner + Outer)
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

  memcpy(R,TPCROWR,ROWS*sizeof(Double_t));
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

   // Return default values if passed a whacko input value (i.e. infinite or NaN)
   if ( finite((double)Charge)*finite(x[0])*finite(x[1])*finite(x[2])*finite(p[0])*finite(p[1])*finite(p[2]) == 0 ) return ;

   const Float_t InnerOuterRatio = 0.6 ; // Ratio of size of the inner pads to the outer pads (real world == 0.5, GVB likes 0.6)
   const Int_t   ROWS     = TPCROWS  ;        // Total number of TPC rows per sector (Inner + Outer)
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
   StDetectorDbSpaceChargeR2* tempfSpaceChargeR2 = fSpaceChargeR2 ;
   Double_t tempSpaceChargeR2 = SpaceChargeR2 ;
   ManualSpaceChargeR2(sc,SpaceChargeEWRatio); // Set a custom value of the spacecharge parameter but keep previous E/W ratio
   
   BFieldTpc(x,B) ;
   ChargeB  = Charge * TMath::Sign((int)1,(int)(B[2]*1000)) ;
   Pt = TMath::Sqrt( p[0]*p[0] + p[1]*p[1] ) ;
   R0 = TMath::Abs( 1000.0 * Pt / ( 0.299792 * B[2] ) ) ;     // P in GeV, R in cm, B in kGauss
   X0 = x[0] + ChargeB * p[1] * R0 / Pt ;
   Y0 = x[1] - ChargeB * p[0] * R0 / Pt ;

   memcpy(R,TPCROWR,ROWS*sizeof(Double_t));
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
       if ( i < INNER ) { dX[Index] *= InnerOuterRatio ; dY[Index] *= InnerOuterRatio ; } ;  
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
Int_t StMagUtilities::PredictSpaceChargeDistortion (Int_t Charge, Float_t Pt, Float_t VertexZ, Float_t PseudoRapidity, 
	       Float_t DCA,  const unsigned int RowMask1, const unsigned int RowMask2, Float_t &pSpace )
{

   const Int_t   ROWS             =  TPCROWS  ;       // Total number of TPC rows per sector (Inner + Outer)
   const Int_t   RefIndex         =   7  ;       // Refindex 7 (TPCRow 8) is about where 1/R**2 has no effect on points (~97 cm)
   const Int_t   MinInnerTPCHits  =   5  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   MinOuterTPCHits  =  10  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   DEBUG            =   0  ;       // Turn on debugging statements and plots

   unsigned int OneBit = 1 ;
   Int_t InnerTPCHits = 0, OuterTPCHits = 0 ;
   for ( Int_t i = 0 ; i < ROWS ; i++ ) 
     {
       if ( i < INNER )
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
   StDetectorDbSpaceChargeR2* tempfSpaceChargeR2 = fSpaceChargeR2 ;
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
 
   memcpy(R,TPCROWR,ROWS*sizeof(Double_t));
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
       if ( i < INNER ) { dX[Index] *= InnerOuterRatio ; dY[Index] *= InnerOuterRatio ; } ;  
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
Int_t StMagUtilities::PredictSpaceChargeDistortion (Int_t Charge, Float_t Pt, Float_t VertexZ, Float_t PseudoRapidity, Float_t Phi,
						    Float_t DCA,  const unsigned int RowMask1, const unsigned int RowMask2, 
						    Float_t RowMaskErrorR[64], Float_t RowMaskErrorRPhi[64], Float_t &pSpace )
{

   const Int_t   INNERDETECTORS   =   6  ;       // Number of inner detector rows in represented in the bit masks
   const Int_t   SSDLAYERS        =   1  ;       // Number of SSD layers
   const Int_t   MinInnerTPCHits  =   5  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   MinOuterTPCHits  =  10  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   DEBUG            =   0  ;       // Turn on debugging statements and plots

   const Int_t   TPCOFFSET = INNERDETECTORS + SSDLAYERS + 1 ;   // Add one for the vertex in 0th position in RowMasks
   const Int_t   BITS      = INNERDETECTORS + TPCROWS + SSDLAYERS + 1 ;  // Number of bits in the row masks (TPC Rows + etc.)

   unsigned int OneBit = 1 ;
   Int_t InnerTPCHits = 0, OuterTPCHits = 0 ;
   for ( Int_t i = 0 ; i < TPCROWS ; i++ ) 
     {
       if ( i < INNER )
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
   StDetectorDbSpaceChargeR2* tempfSpaceChargeR2 = fSpaceChargeR2 ;
   Double_t tempSpaceChargeR2 = SpaceChargeR2 ;
   if (!useManualSCForPredict) ManualSpaceChargeR2(0.01,SpaceChargeEWRatio); // Set "medium to large" value of the spacecharge parameter for tests, not critical.
                                                   // but keep EWRatio that was previously defined 
   if (DoOnce) {
     xx[0] = TPCROWR[0]; xx[1] = 0; xx[2] = 50;
     DoDistortion ( xx, xxprime ) ;
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
   memcpy(&(R[TPCOFFSET]),TPCROWR,TPCROWS*sizeof(Double_t));
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
      TMatrix  ArrayE(ROWS,COLUMNS), ArrayV(ROWS,COLUMNS), Charge(ROWS,COLUMNS) ;
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
