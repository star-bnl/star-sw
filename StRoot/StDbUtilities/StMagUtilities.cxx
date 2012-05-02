/***********************************************************************
 *
 * $Id: StMagUtilities.cxx,v 1.81 2009/12/11 04:53:57 genevb Exp $
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
 * Blew it again.  I sure wish this wasn't an archive!
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
 * Change Shorted Ring Algorithm over to Wieman's Bessel Function solution.  It is faster.
 * Also Fix Jerome's famous non-ascii typo.
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
 * order. Update D'Oxygen documentation.  Remove 2000/2001 E field switch.
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
 * Jim's modif for FC & SpaceCharge corrections.
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
 * Modifications for larger number of ExB options, forcing different configuration 9EB1 EB2 ...). Added loading of StTableUtilities when 'display' option is required.
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
  kFast2DBMap        = 0x2000,   // bit 14                            <br>
  kGridLeak          = 0x4000    // bit 15                            <br>
  k3DGridLeak        = 0x8000    // bit 16                            <br>
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

#include "StMagUtilities.h"
#include "TFile.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TF1.h"
#include "TH2.h"
#include "StTpcDb/StTpcDb.h"
#include "tables/St_MagFactor_Table.h"
#include "tables/St_tpcFieldCageShort_Table.h"
#include "StDetectorDbMaker/St_tpcFieldCageShortC.h"
  //#include "StDetectorDbMaker/StDetectorDbMagnet.h"

static EBField  gMap  =  kUndefined ;   // Global flag to indicate static arrays are full
static Float_t  gFactor  = 1.0 ;        // Multiplicative factor (allows scaling and sign reversal)
static Float_t  gRescale = 1.0 ;        // Multiplicative factor (allows re-scaling wrt which map read)
StMagUtilities *StMagUtilities::fgInstance = 0;

//________________________________________


ClassImp(StMagUtilities)

/// StMagUtilities default constructor
StMagUtilities::StMagUtilities ()  
{
  cout << "StMagUtilities:: Unfortunately, instantiation with StMagUtilities(<empty>) is obsolete" << endl ;
  cout << "StMagUtilities:: You must specify DataBase pointers or specify the requested Field settings manually" << endl ;
}


/// StMagUtilities constructor using the DataBase
StMagUtilities::StMagUtilities ( StTpcDb* dbin , TDataSet* dbin2, Int_t mode )
{ 
  if (fgInstance) {
    cout << "ReInstate StMagUtilities. Be sure that this is want you want !" << endl;
    SafeDelete(fgInstance);
  }
  fgInstance = this;
  gMap = kMapped        ;    // Select the B field shape (kMapped == mapped field, kConstant == constant field )
  SetDb ( dbin, dbin2 ) ;    // Put DB pointers into private/global space
  GetMagFactor()        ;    // Get the magnetic field scale factor from the DB
  GetTPCParams()        ;    // Get the TPC parameters from the DB
  GetTPCVoltages()      ;    // Get the TPC Voltages from the DB
  GetOmegaTau ()        ;    // Get Omega Tau parameters
  GetSpaceCharge()      ;    // Get the spacecharge variable from the DB
  GetSpaceChargeR2()    ;    // Get the spacecharge variable R2 from the DB and EWRatio
  GetShortedRing()      ;    // Get the parameters that describe the shorted ring on the field cage
  GetGridLeak()         ;    // Get the parameters that describe the gating grid leaks
  CommonStart( mode )   ;    // Read the Magnetic and Electric Field Data Files, set constants
}


/// StMagUtilities constructor not using the DataBase
StMagUtilities::StMagUtilities ( const EBField map, const Float_t factor, Int_t mode )       
{ 
  if (fgInstance) {
    cout << "ReInstate StMagUtilities. Be sure that this is want you want !" << endl;
    SafeDelete(fgInstance);
  }
  fgInstance = this;
  gMap    = map         ;        // Select the type of field (mapped field shape or constant field)
  thedb2  = 0           ;        // Do not get MagFactor from the DB       - use manual selection above
  thedb   = 0           ;        // Do not get TPC parameters from the DB  - use defaults in CommonStart
  gFactor = factor      ;        // Manually selected magnetic field scale factor
  fTpcVolts      =  0   ;        // Do not get TpcVoltages out of the DB   - use defaults in CommonStart
  fOmegaTau      =  0   ;        // Do not get OmegaTau out of the DB      - use defaults in CommonStart
  ManualSpaceCharge(0)  ;        // Do not get SpaceCharge out of the DB   - use defaults inserted here.
  ManualSpaceChargeR2(0,1) ;     // Do not get SpaceChargeR2 out of the DB - use defaults inserted here, SpcChg and EWRatio
  ManualShortedRing(0,0,0,0,0) ; // No shorted rings
  fGridLeak      =  0   ;        // Do not get Grid Leak data from the DB  - use defaults in CommonStart
  CommonStart( mode )   ;        // Read the Magnetic and Electric Field Data Files, set constants
}


//________________________________________


void StMagUtilities::SetDb ( StTpcDb* dbin , TDataSet* dbin2 )
{
  thedb  = dbin  ;
  thedb2 = dbin2 ;
}

void StMagUtilities::GetMagFactor () 
{ 
  St_MagFactor *fMagFactor  =  (St_MagFactor *) thedb2->Find("MagFactor");  assert(fMagFactor) ;
  gFactor        =  (*fMagFactor)[0].ScaleFactor ;        // Set the magnetic field scale factor
  //if (TMath::Abs(gFactor) < 1.e-3) gFactor = 1.e-3;     // JT ... This is incorrect.  Don't do it this way.  The DB returns
                                                          // 1.0 for full field and 0.5 for half field.  These are relative numbers
                                                          // and are not field values in kGauss or Tesla ... so beware.
                                                          // See Interpolate2DBField and Interpolate3DBField for a better way to
                                                          // achieve the same goal of protecting against divide by zero with 0.0 field. 
}

void StMagUtilities::GetTPCParams ()  
{ 
  StarDriftV     =  1e-6*thedb->DriftVelocity() ;        
  TPC_Z0         =  thedb->PadPlaneGeometry()->outerSectorPadPlaneZ() -
                    thedb->WirePlaneGeometry()->outerSectorGatingGridPadPlaneSeparation() ;    
  XTWIST         =  1e3*thedb->GlobalPosition()->TpcEFieldRotationY() ; 
  YTWIST         =  -1e3*thedb->GlobalPosition()->TpcEFieldRotationX() ;            
  IFCShift       =  thedb->FieldCage()->InnerFieldCageShift();
  EASTCLOCKERROR =  1e3*thedb->FieldCage()->EastClockError();
  WESTCLOCKERROR =  1e3*thedb->FieldCage()->WestClockError();
}

void StMagUtilities::GetTPCVoltages ()  
{ 
  fTpcVolts      =  StDetectorDbTpcVoltages::instance() ;  // Initialize the DB for TpcVoltages
  CathodeV       =  fTpcVolts->getCathodeVoltage() * 1000 ; 
  GG             =  fTpcVolts->getGGVoltage() ; 
}

void StMagUtilities::GetSpaceCharge ()  
{ 
  fSpaceCharge   =  StDetectorDbSpaceCharge::instance()  ; 
  SpaceCharge    =  fSpaceCharge->getSpaceChargeCoulombs((double)gFactor) ; 
}

void StMagUtilities::GetSpaceChargeR2 ()  
{ 
  fSpaceChargeR2 =  StDetectorDbSpaceChargeR2::instance() ;  
  SpaceChargeR2  =  fSpaceChargeR2->getSpaceChargeCoulombs((double)gFactor) ;
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
  Bool_t update = (new_shortsTable != shortsTable);
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

void StMagUtilities::GetGridLeak ()
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

  //  These items are not taken from the DB but they should be ... some day.
      IFCRadius   =    47.90 ;     // Radius of the Inner Field Cage
      OFCRadius   =    200.0 ;     // Radius of the Outer Field Cage
      GAPRADIUS   =    121.8 ;     // Radius of gap between rows 13 & 14 at phi = zero degrees (cm)
      GAP13_14    =    1.595 ;     // Width of the gap between the grids at row 13 and row 14 (cm)

  //  End of list of items that might come from the DB

  if ( thedb2 == 0 ) cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected BFIELD setting." << endl ; 
  else  cout << "StMagUtilities::CommonSta  Magnetic Field scale factor is " << gFactor << endl ;

  if ( thedb == 0 )
    {
      cout << "StMagUtilities::CommonSta  ***NO TPC DB, Using default TPC parameters. You sure it is OK??? ***" << endl ; 
      cout << "StMagUtilities::CommonSta  ***NO TPC DB, Using default TPC parameters. You sure it is OK??? ***" << endl ; 
      cout << "StMagUtilities::CommonSta  ***NO TPC DB, Using default TPC parameters. You sure it is OK??? ***" << endl ; 
      cout << "StMagUtilities::CommonSta  ***NO TPC DB, Using default TPC parameters. You sure it is OK??? ***" << endl ; 
      StarDriftV  =     5.54 ;      // Drift Velocity (cm/microSec) Magnitude
      TPC_Z0      =    208.7 ;      // Z location of STAR TPC Gated Grid (cm)
      XTWIST      =   -0.165 ;      // X Displacement of West end of TPC wrt magnet (mRad)
      YTWIST      =    0.219 ;      // Y Displacement of West end of TPC wrt magnet (mRad)
      IFCShift    =   0.0080 ;      // Shift of the IFC towards the West Endcap (cm) (2/1/2002)
      EASTCLOCKERROR =   0.0 ;      // Phi rotation of East end of TPC in milli-radians
      WESTCLOCKERROR = -0.43 ;      // Phi rotation of West end of TPC in milli-radians
      cout << "StMagUtilities::CommonSta  WARNING -- Using hard-wired TPC parameters. " << endl ; 
    }
  else  cout << "StMagUtilities::CommonSta  Using TPC parameters from DataBase. " << endl ; 
  
  if ( fTpcVolts == 0 ) 
    { 
      CathodeV    = -27950.0 ;      // Cathode Voltage (volts)
      GG          =   -115.0 ;      // Gating Grid voltage (volts)
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
      InnerGridLeakStrength  =  0.0   ;  // Relative strength of the Inner grid leak
      InnerGridLeakRadius    =  53.0  ;  // Location (in local Y coordinates) of the Inner grid leak 
      InnerGridLeakWidth     =  0.0   ;  // Half-width of the Inner grid leak.  
      MiddlGridLeakStrength  =  15.0  ;  // Relative strength of the Middle grid leak
      MiddlGridLeakRadius    =  GAPRADIUS ;  // Location (in local Y coordinates) of the Middle grid leak 
      MiddlGridLeakWidth     =  3.0   ;  // Half-width of the Middle grid leak.  Must oversized for numerical reasons.
      OuterGridLeakStrength  =  0.0   ;  // Relative strength of the Outer grid leak
      OuterGridLeakRadius    =  195.0 ;  // Location (in local Y coordinates) of the Outer grid leak 
      OuterGridLeakWidth     =  0.0   ;  // Half-width of the Outer grid leak.  
      // InnerGridLeakStrength  =  1.0   ;  // JT test (Note that GainRatio is taken into account, below.)
      // OuterGridLeakStrength  =  1.0   ;  // JT test (keep these the same unless you really know what you are doing.) 
      cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected GridLeak parameters. " << endl ; 
    }
  else  cout << "StMagUtilities::CommonSta  Using GridLeak parameters from the DB."   << endl ; 
  
  // Parse the mode switch which was received from the Tpt maker
  // To turn on and off individual distortions, set these higher bits
  // Default behavior: no bits set gives you the following defaults

  mDistortionMode = mode;
  if ( !( mode & ( kBMap | kPadrow13 | kTwist | kClock | kMembrane | kEndcap | kIFCShift | kSpaceCharge | kSpaceChargeR2 
                         | kShortedRing | kFast2DBMap | kGridLeak | k3DGridLeak ))) 
    {
       mDistortionMode |= kFast2DBMap ;
       mDistortionMode |= kPadrow13 ;
       mDistortionMode |= kTwist ;
       mDistortionMode |= kClock ;
       mDistortionMode |= kIFCShift ;
       printf("StMagUtilities::CommonSta  Default mode selection\n");
    } 
  else printf("StMagUtilities::CommonSta  Using mode option 0x%X\n",mode);
 
  Float_t  B[3], X[3] = { 0, 0, 0 } ;
  Float_t  OmegaTau ;                       // For an electron, OmegaTau carries the sign opposite of B 

  ReadField() ;                             // Read the Magnetic and Electric Field Data Files
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

  StarMagE   =  TMath::Abs((CathodeV-GG)/TPC_Z0) ;         // STAR Electric Field (V/cm) Magnitude
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

}

//________________________________________


/// B field in Cartesian coordinates - 2D field (ie. Phi symmetric)
void StMagUtilities::BField( const Float_t x[], Float_t B[] )
{                          

  Float_t r, z, Br_value, Bz_value ;

  z  = x[2] ;
  r  = TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  
  if ( r != 0.0 )
    {
      Interpolate2DBfield( r, z, Br_value, Bz_value ) ;
      B[0] = Br_value * (x[0]/r) ;
      B[1] = Br_value * (x[1]/r) ;
      B[2] = Bz_value ; 
    }
  else
    {
      Interpolate2DBfield( r, z, Br_value, Bz_value ) ;
      B[0] = Br_value ;
      B[1] = 0.0 ;
      B[2] = Bz_value ;
    }

}


/// Bfield in Cartesian coordinates - 3D field
void StMagUtilities::B3DField( const Float_t x[], Float_t B[] )
{                          

  Float_t r, z, phi, Br_value, Bz_value, Bphi_value ;

  z  = x[2] ;
  r  = TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  
  if ( r != 0.0 )
    {
      phi = TMath::ATan2( x[1], x[0] ) ;
      if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
      Interpolate3DBfield( r, z, phi, Br_value, Bz_value, Bphi_value ) ;
      B[0] = Br_value * (x[0]/r) - Bphi_value * (x[1]/r) ;
      B[1] = Br_value * (x[1]/r) + Bphi_value * (x[0]/r) ;
      B[2] = Bz_value ; 
    }
  else
    {
      phi = 0 ;
      Interpolate3DBfield( r, z, phi, Br_value, Bz_value, Bphi_value ) ;
      B[0] = Br_value ;
      B[1] = Bphi_value ;
      B[2] = Bz_value ;
    }

  return ;

}


/// B field in Radial coordinates - 2D field (ie Phi symmetric)
void StMagUtilities::BrBzField( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value )
{
  
  Interpolate2DBfield( r, z, Br_value, Bz_value ) ;

}


/// B field in Radial coordinates - 3D field
void StMagUtilities::BrBz3DField( const Float_t r, const Float_t z, const Float_t phi, 
				  Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value )
{

  Float_t phiprime ;
  phiprime = phi ;
  if ( phiprime < 0 ) phiprime += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  Interpolate3DBfield( r, z, phiprime, Br_value, Bz_value, Bphi_value ) ;

}


//________________________________________


/// Main Entry Point for requests to UNDO the E and B field distortions
void StMagUtilities::UndoDistortion( const Float_t x[], Float_t Xprime[] )
{
  // Control by flags JCD Oct 4, 2001
  Float_t Xprime1[3], Xprime2[3] ;

  // Set it up
  for (unsigned int i=0; i<3; ++i) {
      Xprime1[i] = x[i];
  }

  Float_t r2 = x[0]*x[0] + x[1]*x[1] ;   // Point must be inside TPC to be suffer distortions, check this.
  if ( r2 >= OFCRadius*OFCRadius || r2 <= IFCRadius*IFCRadius || x[2] >= TPC_Z0 || x[2] <= -1*TPC_Z0 )
    {
      for (unsigned int i=0; i<3; ++i) { Xprime[i] = x[i] ; }
      return ;
    }
      
  if (mDistortionMode & kBMap) {
      FastUndoBDistortion    ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }
      
  if (mDistortionMode & kFast2DBMap) {
      FastUndo2DBDistortion    ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }

  if ((mDistortionMode & kBMap) && (mDistortionMode & kFast2DBMap)) {
      cout << "StMagUtilities ERROR **** Do not use kBMap and kFast2DBMap at the same time" << endl ;
      cout << "StMagUtilities ERROR **** These routines have duplicate functionality so don't do both." << endl ;
      exit(1) ;
  }


  if (mDistortionMode & kPadrow13) {
      UndoPad13Distortion    ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }
  
  if (mDistortionMode & kTwist) {

      UndoTwistDistortion    ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }

  if (mDistortionMode & kClock) {
      
      UndoClockDistortion    ( Xprime1, Xprime2 ) ; 
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }

  if (mDistortionMode & kMembrane) {
      
      UndoMembraneDistortion ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }

  }

  if (mDistortionMode & kEndcap) 
    { 
      UndoEndcapDistortion ( Xprime1, Xprime2 ) ; 
      for (unsigned int i=0; i<3; ++i) {
	Xprime1[i] = Xprime2[i];
      }
    }
    
  if (mDistortionMode & kIFCShift) { 
      UndoIFCShiftDistortion ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }

  if (mDistortionMode & kSpaceCharge) { 
      UndoSpaceChargeDistortion ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }

  if (mDistortionMode & kSpaceChargeR2) { 
      UndoSpaceChargeR2Distortion ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }

  if ((mDistortionMode & kSpaceCharge) && (mDistortionMode & kSpaceChargeR2)) {
      cout << "StMagUtilities ERROR **** Do not use kSpaceCharge and kspaceChargeR2 at the same time" << endl ;
      cout << "StMagUtilities ERROR **** These routines have overlapping functionality." << endl ;
      exit(1) ;
  }

  if (mDistortionMode & kShortedRing) { 
      UndoShortedRingDistortion ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }

  if (mDistortionMode & kGridLeak) { 
      UndoGridLeakDistortion ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }

  if (mDistortionMode & k3DGridLeak) { 
      Undo3DGridLeakDistortion ( Xprime1, Xprime2 ) ;
      for (unsigned int i=0; i<3; ++i) {
	  Xprime1[i] = Xprime2[i];
      }
  }

  if ((mDistortionMode & kGridLeak) && (mDistortionMode & k3DGridLeak)) {
      cout << "StMagUtilities ERROR **** Do not use kGridLeak and k3DGridLeak at the same time" << endl ;
      cout << "StMagUtilities ERROR **** These routines have overlapping functionality." << endl ;
      exit(1) ;
  }

  // Return it

  for (unsigned int i=0; i<3; ++i) {
      Xprime[i] = Xprime1[i];
  }
  
}


//________________________________________


/// Main Entry Point for requests to DO the E and B field distortions (for simulations)
void StMagUtilities::DoDistortion( const Float_t x[], Float_t Xprime[] )
{

  UndoDistortion ( x, Xprime ) ;

  Xprime[0] = 2*x[0] - Xprime[0] ;
  Xprime[1] = 2*x[1] - Xprime[1] ;
  Xprime[2] = 2*x[2] - Xprime[2] ;

}


//________________________________________


/// B field distortions in 3D ( no Table ) - calculate the distortions due to the shape of the B field
/*! 
    Distortions are calculated point by point and integrated in real time.
    This avoids the time required to set up a table of distorted values but
    is slow for a very large number of points ( > 10,000 ).
*/
void StMagUtilities::UndoBDistortion( const Float_t x[], Float_t Xprime[] )
{

  Double_t ah ;                                        // ah carries the sign opposite of E (for forward integration)
  Float_t  B[3] ; 
  Int_t    sign, index = 1 , NSTEPS ;              
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

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
      B3DField( Xprime, B ) ;                          // Work in kGauss, cm
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
void StMagUtilities::Undo2DBDistortion( const Float_t x[], Float_t Xprime[] )
{

  Double_t ah ;                             // ah carries the sign opposite of E (for forward integration)
  Float_t  B[3] ; 
  Int_t    sign, index = 1 , NSTEPS ;              
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

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
      BField( Xprime, B ) ;                            // Work in kGauss, cm
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
void StMagUtilities::FastUndoBDistortion( const Float_t x[], Float_t Xprime[] )
{

  static  Float_t dx3D[EMap_nPhi][EMap_nR][EMap_nZ], dy3D[EMap_nPhi][EMap_nR][EMap_nZ] ;
  static  Int_t   ilow = 0, jlow = 0, klow = 0 ;
  static  Int_t   DoOnce = 0 ;
  const   Int_t   PHIORDER = 1 ;                    // Linear interpolation = 1, Quadratic = 2 ... PHI Table is crude so use linear interp
  const   Int_t   ORDER    = 1 ;                    // Linear interpolation = 1, Quadratic = 2         

  Int_t   i, j, k ;
  Float_t xx[3]   ;
  Float_t save_x[ORDER+1], saved_x[ORDER+1] ;
  Float_t save_y[ORDER+1], saved_y[ORDER+1] ;

  Float_t r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  Float_t phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  Float_t z      =  x[2] ;
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM

  if ( DoOnce == 0 )
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
		  UndoBDistortion(xx,Xprime) ;
		  dx3D[k][i][j]   = Xprime[0] - xx[0] ;
		  dy3D[k][i][j]   = Xprime[1] - xx[1] ;
		}
	    }
	}
      DoOnce = 1 ;
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
  
  Xprime[0] = Interpolate( &ePhiList[klow], saved_x, PHIORDER, phi ) + x[0] ;
  Xprime[1] = Interpolate( &ePhiList[klow], saved_y, PHIORDER, phi ) + x[1];

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
void StMagUtilities::FastUndo2DBDistortion( const Float_t x[], Float_t Xprime[] )
{

  static  Float_t dR[EMap_nR][EMap_nZ], dRPhi[EMap_nR][EMap_nZ] ;
  static  Int_t   ilow = 0, jlow = 0 ;
  static  Int_t   DoOnce = 0 ;
  const   Int_t   ORDER  = 1 ;                      // Linear interpolation = 1, Quadratic = 2         

  Int_t   i, j ;
  Float_t xx[3] ;
  Float_t save_dR[ORDER+1], saved_dR ;
  Float_t save_dRPhi[ORDER+1], saved_dRPhi ;

  Float_t r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  Float_t phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  Float_t z      =  x[2] ;
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM

  if ( DoOnce == 0 )
    {
      cout << "StMagUtilities::FastUndo2  Please wait for the tables to fill ...  ~5 seconds" << endl ;
      for ( i = 0 ; i < EMap_nR ; i++ )
	{
	  xx[0] = eRList[i] ;
	  xx[1] = 0 ;
	  for ( j = 0 ; j < EMap_nZ ; j++ )
	    {
	      xx[2] = eZList[j] ;
	      Undo2DBDistortion(xx,Xprime) ;
	      dR[i][j] = Xprime[0] ;
	      dRPhi[i][j] = Xprime[1] ;
	    }
	}
      DoOnce = 1 ;
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

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;
  
}


//________________________________________


/// Twist distortion
/*!
    Remove the effects of a simple "twist" of the TPC in the magnet.  If there is
    an angle between the E and B fields, there will be a distortion in the recorded
    tracks.  This routine takes out that distortion.
 */
void StMagUtilities::UndoTwistDistortion( const Float_t x[], Float_t Xprime[] )
{

  Double_t        Zdrift ;
  Int_t           sign ;

  // Work in TPC coordinates but note that XTWIST and YTWIST reported in Magnet coord system 
  // so they have been negated (below)  
  
  if ( x[2] >= 0.0 ) sign =  1 ;                       // (TPC West)
  else               sign = -1 ;                       // (TPC East)  

  Zdrift = sign * ( TPC_Z0 - TMath::Abs(x[2]) ) ;
  Xprime[0] = x[0] - (     Const_1 * YTWIST - Const_2 * XTWIST ) * Zdrift/1000 ;
  Xprime[1] = x[1] - ( -1* Const_1 * XTWIST - Const_2 * YTWIST ) * Zdrift/1000 ;
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
void StMagUtilities::UndoPad13Distortion( const Float_t x[], Float_t Xprime[] )
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
  static Int_t    DoOnce = 0 ;                   // Calculate only once
  static Float_t  SumArray[NZDRIFT][NYARRAY] ;
  static Int_t    ilow = 0, jlow = 0 ;
  
  Float_t  y, Zdrift, save_sum[3] ;
  Double_t r, phi, phi0, sum = 0.0 ;

  if ( DoOnce == 0 ) 
    {                          // Put these coefficients in a table to save time
      cout << "StMagUtilities::PadRow13   Please wait for the tables to fill ...  ~5 seconds" << endl ;
      C[0] = GAP13_14 * GG * SCALE / ( 2 * BOX ) ;   
      for ( Int_t i = 1 ; i < TERMS ; i++ )
	  C[i] = 2 * GG * SCALE * TMath::Sin( GAP13_14*i*PI/( 2*BOX ) ) / ( i * PI ) ;
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
      DoOnce = 1 ;
    }
  
  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;               // Phi ranges from pi to -pi
  phi0   =  ( (Int_t)((TMath::Abs(phi)+PI/12.)/(PI/6.) + 6.0 ) - 6.0 ) * PI/6. ;
  if ( phi < 0 ) phi0 *= -1. ;
  y      =  r * TMath::Cos( phi0 - phi ) ;
  Zdrift =  TPC_Z0 - TMath::Abs(x[2]) ;

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

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
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
void StMagUtilities::UndoClockDistortion( const Float_t x[], Float_t Xprime[] )
{

  Double_t r, phi ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;

  if ( x[2] < 0 )  phi += EASTCLOCKERROR/1000. ;    // Phi rotation error in milli-radians
  if ( x[2] > 0 )  phi += WESTCLOCKERROR/1000. ;    // Phi rotation error in milli-radians
  // Do nothing if x[2] = 0 

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}


//________________________________________


/// Membrane distortion
/*!

 */
void StMagUtilities::UndoMembraneDistortion( const Float_t x[], Float_t Xprime[] )
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
void StMagUtilities::UndoEndcapDistortion( const Float_t x[], Float_t Xprime[] )
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
void StMagUtilities::UndoIFCShiftDistortion( const Float_t x[], Float_t Xprime[] )
{ 

  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  static Int_t DoOnce = 0 ;
  const   Int_t ORDER = 1 ;                         // Linear interpolation = 1, Quadratic = 2         

  if ( DoOnce == 0 )
    {
      cout << "StMagUtilities::IFCShift   Please wait for the tables to fill ...  ~5 seconds" << endl ;
      Int_t Nterms = 100 ;
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
		  Double_t Denominator =
		    TMath::BesselK0( k*OFCRadius ) * TMath::BesselI0( k*IFCRadius ) -
		    TMath::BesselK0( k*IFCRadius ) * TMath::BesselI0( k*OFCRadius ) ;
		  Double_t zterm = 1 + TMath::Cos( k*z ) ;
		  Double_t qwe = Numerator / Denominator ;
		  IntegralOverZ += Cn * zterm * qwe ;
	          if ( n>10 && fabs(IntegralOverZ)*1.e-10 > fabs(qwe) ) break;
		}
	      if  ( eZList[i] < 0 )  IntegralOverZ = -1 * IntegralOverZ ;  // Force AntiSymmetry of solutions in Z
	      shiftEr[i][j] = IntegralOverZ ; 	    }
	}
      DoOnce = 1 ;
    }
  
  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  z      =  x[2] ;
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM

  Interpolate2DEdistortion( ORDER, r, z, shiftEr, Er_integral ) ;
  Ephi_integral = 0.0 ;  // Efield is symmetric in phi

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}


//________________________________________


/// Space Charge Correction 
/*!
    Space Charge distortion assuming a uniform distribution of charge per unit volume
    in the TPC.  We now know that this is not a good assumption but the code is here
    for legacy reasons.  Electrostatic equations solved by Jamie Dunlop  11/01/2001
    Updated to include linear increase of charge from endcap to CM by Jim Thomas 12/18/2001
*/
void StMagUtilities::UndoSpaceChargeDistortion( const Float_t x[], Float_t Xprime[] )
{ 
  
  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  static Int_t DoOnce = 0 ;
  const   Int_t ORDER = 1 ;                         // Linear interpolation = 1, Quadratic = 2         

  if ( DoOnce == 0 )
    {
      Int_t Nterms = 100 ;
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
		  Double_t Denominator =
		    TMath::BesselK0( k*OFCRadius ) * TMath::BesselI0( k*IFCRadius ) -
		    TMath::BesselK0( k*IFCRadius ) * TMath::BesselI0( k*OFCRadius ) ;
		  Double_t qwe = Numerator / Denominator ;
		  IntegralOverZ += Cn * zterm * qwe ;
	          if ( n>10 && fabs(IntegralOverZ)*1.e-10 > fabs(qwe) ) break;
		}
	      spaceEr[i][j] = IntegralOverZ ; 
	    }
	}
      DoOnce = 1 ;
    }
  
  r   =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  z   =  x[2] ;
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM

  Interpolate2DEdistortion( ORDER, r, z, spaceEr, Er_integral ) ;
  Ephi_integral = 0.0 ;  // E field is symmetric in phi

  // Get Space Charge **** Every Event (JCD This is actually per hit)***
  // Need to reset the instance every hit.  May be slow, but there's no per-event hook.  
  if (fSpaceCharge) GetSpaceCharge(); // need to reset it. 

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - SpaceCharge * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - SpaceCharge * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
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
void StMagUtilities::UndoSpaceChargeR2Distortion( const Float_t x[], Float_t Xprime[] )
{ 
  
  const Int_t     ORDER       =    1 ;  // Linear interpolation = 1, Quadratic = 2         
  const Int_t     ROWS        =  257 ;  // (2**n + 1)    
  const Int_t     COLUMNS     =  129 ;  // (2**m + 1) 
  const Int_t     ITERATIONS  =  100 ;  // About 0.05 seconds per iteration
  const Double_t  GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
  const Double_t  GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;

  Float_t   Er_integral, Ephi_integral ;
  Double_t  r, phi, z ;

  static Int_t DoOnce = 0 ;

  if ( DoOnce == 0 )
    {
      cout << "StMagUtilities::UndoSpace  Please wait for the tables to fill ...  ~5 seconds" << endl ;
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
	      Charge(i,j) = zterm * ( 3191/(Radius*Radius) + 122.5/Radius - 0.395 ) / 15823 ;
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

      PoissonRelaxation( ArrayV, Charge, EroverEz, ROWS, COLUMNS, ITERATIONS ) ;

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

      DoOnce = 1 ;      

    }
  
  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  z      =  x[2] ;
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM

  Interpolate2DEdistortion( ORDER, r, z, spaceR2Er, Er_integral ) ;
  Ephi_integral = 0.0 ;  // E field is symmetric in phi

  // Get Space Charge **** Every Event (JCD This is actually per hit)***
  // Need to reset the instance every hit.  May be slow, but there's no per-event hook.
  if (fSpaceChargeR2) GetSpaceChargeR2(); // need to reset it. 

  // Subtract to Undo the distortions and apply the EWRatio on the East end of the TPC 
  if ( r > 0.0 ) 
    {
      if ( x[2] < 0.0 ) 
	{
	  phi =  phi - SpaceChargeR2 * SpaceChargeEWRatio * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
	  r   =  r   - SpaceChargeR2 * SpaceChargeEWRatio * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
	}
      else
	{
	  phi =  phi - SpaceChargeR2 * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
	  r   =  r   - SpaceChargeR2 * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
	}
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
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
void StMagUtilities::UndoShortedRingDistortion( const Float_t x[], Float_t Xprime[] )
{ 
  
  const   Int_t   ORDER     = 1     ;            // Linear interpolation = 1, Quadratic = 2         
  const   Float_t R0        = 2.130 ;            // First resistor (R0) between CM and ring number one (Mohm)
  const   Float_t R182      = 0.310 ;            // Last resistor in the IFC chain
  const   Float_t RStep     = 2.000 ;            // Resistor chain value (except the first one) (Mohm)
  const   Float_t Pitch     = 1.150 ;            // Ring to Ring pitch (cm)
  const   Float_t Z01       = 1.225 ;            // Distance from CM to center of first ring (cm)
  
  static  Bool_t DoOnce = true ;  // Note that this is the reverse logic compared to DoOnce elsewhere in StMagUtilities.
  static  Int_t  NumberOfEastInnerShorts = 0, NumberOfEastOuterShorts = 0 , NumberOfWestInnerShorts = 0, NumberOfWestOuterShorts = 0 ;
 
  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  if (fTpcVolts) DoOnce = UpdateShortedRing() ;

  if ( DoOnce )  // Note reversed logic compared to other methods in StMagUtilities
    {

      cout << "StMagUtilities::UndoShort  Please wait for the tables to fill ...  ~5 seconds" << endl ;

      // Parse the Table and separate out the four different resistor chains
      // Definition: A "missing" resistor is a shorted resistor, an "extra" resistor is a compensating resistor added at the end

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
	    EastInnerShortZ[NumberOfEastInnerShorts-1]  = TPC_Z0 - ( Z01 + (Ring[i]-1)*Pitch) ; }
	  if ( Side[i] == 0 && Cage[i] == 1 ) 
	    { NumberOfEastOuterShorts++ ; EastOuterMissingSum += MissingResistance[i] ; EastOuterExtraSum += Resistor[i] ; 
	    EastOuterMissingOhms[NumberOfEastOuterShorts-1]  = MissingResistance[i] ; 
	    EastOuterShortZ[NumberOfEastOuterShorts-1]  = TPC_Z0 - ( Z01 + (Ring[i]-1)*Pitch) ; }
	  if ( Side[i] == 1 && Cage[i] == 0 ) 
	    { NumberOfWestInnerShorts++ ; WestInnerMissingSum += MissingResistance[i] ; WestInnerExtraSum += Resistor[i] ; 
	    WestInnerMissingOhms[NumberOfWestInnerShorts-1]  = MissingResistance[i] ; 
	    WestInnerShortZ[NumberOfWestInnerShorts-1]  = TPC_Z0 - ( Z01 + (Ring[i]-1)*Pitch) ; }
	  if ( Side[i] == 1 && Cage[i] == 1 ) 
	    { NumberOfWestOuterShorts++ ; WestOuterMissingSum += MissingResistance[i] ; WestOuterExtraSum += Resistor[i] ; 
	    WestOuterMissingOhms[NumberOfWestOuterShorts-1]  = MissingResistance[i] ; 
	    WestOuterShortZ[NumberOfWestOuterShorts-1]  = TPC_Z0 - ( Z01 + (Ring[i]-1)*Pitch) ; }
	}
      
      // Don't fill the tables if there aren't any shorts
      if ( (NumberOfEastInnerShorts + NumberOfEastOuterShorts + NumberOfWestInnerShorts + NumberOfWestOuterShorts) == 0 ) 
	  { Xprime[0] = x[0] ; Xprime[1] = x[1] ; Xprime[2] = x[2] ;  return ; }

      Float_t Rtot   =  R0 + 181*RStep + R182     ;    // Total resistance of the (normal) resistor chain
      Float_t Rfrac  =  RStep*TPC_Z0/(Rtot*Pitch) ;    // Fraction of full resistor chain inside TPC drift volume (~1.0)
      Float_t EastInnerRtot = Rtot + EastInnerExtraSum - EastInnerMissingSum ;  // Total resistance of the real resistor chain
      Float_t EastOuterRtot = Rtot + EastOuterExtraSum - EastOuterMissingSum ;  // Total resistance of the real resistor chain
      Float_t WestInnerRtot = Rtot + WestInnerExtraSum - WestInnerMissingSum ;  // Total resistance of the real resistor chain
      Float_t WestOuterRtot = Rtot + WestOuterExtraSum - WestOuterMissingSum ;  // Total resistance of the real resistor chain
      
      //Float_t deltaV    = GG*0.99 - CathodeV * (1.0-TPC_Z0*RStep/(Pitch*Rtot)) ;    // (test) Error on GG voltage from nominal (99% effective)
      
      Int_t Nterms = 100 ;
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
		      Ein  = 2 * ( Rfrac*EastInnerExtraSum + sum ) / (k*EastInnerRtot*TPC_Z0) ;		
		      sum  = 0.0 ;
		      for  ( Int_t m = 0 ; m < NumberOfEastOuterShorts ; m++ ) 
			sum += ( 1 - Rfrac - TMath::Cos(k*EastOuterShortZ[m]) ) * EastOuterMissingOhms[m] ; 
		      Eout = 2 * ( Rfrac*EastOuterExtraSum + sum ) / (k*EastOuterRtot*TPC_Z0) ;		
		    }
		  if ( z == 0 ) continue ;
		  if ( z > 0 ) 
		    {
		      sum  = 0.0 ;
		      for  ( Int_t m = 0 ; m < NumberOfWestInnerShorts ; m++ ) 
			sum += ( 1 - Rfrac - TMath::Cos(k*WestInnerShortZ[m]) ) * WestInnerMissingOhms[m] ; 
		      Ein  = 2 * ( Rfrac*WestInnerExtraSum + sum ) / (k*WestInnerRtot*TPC_Z0) ;		
		      sum  = 0.0 ;
		      for  ( Int_t m = 0 ; m < NumberOfWestOuterShorts ; m++ ) 
			sum += ( 1 - Rfrac - TMath::Cos(k*WestOuterShortZ[m]) ) * WestOuterMissingOhms[m] ; 
		      Eout = 2 * ( Rfrac*WestOuterExtraSum + sum ) / (k*WestOuterRtot*TPC_Z0) ;		
		    }
		  //Ein   =  2 * RStep * -1*deltaV / ( k * Pitch * Rtot * CathodeV ) ;        // (test) Gating Grid studies (note -1)
		  //Eout  =  2 * RStep * -1*deltaV / ( k * Pitch * Rtot * CathodeV ) ;        // (test) Gating Grid studies (note -1)
		  Double_t An   =  Ein  * TMath::BesselK0( k*OFCRadius ) - Eout * TMath::BesselK0( k*IFCRadius ) ;
		  Double_t Bn   =  Eout * TMath::BesselI0( k*IFCRadius ) - Ein  * TMath::BesselI0( k*OFCRadius ) ;
		  Double_t Numerator =
		    An * TMath::BesselI1( k*r ) - Bn * TMath::BesselK1( k*r ) ;
		  Double_t Denominator =
		    TMath::BesselK0( k*OFCRadius ) * TMath::BesselI0( k*IFCRadius ) -
		    TMath::BesselK0( k*IFCRadius ) * TMath::BesselI0( k*OFCRadius ) ;
		  Double_t zterm = TMath::Cos( k*(TPC_Z0-TMath::Abs(z)) ) - 1 ;
		  Double_t qwe = Numerator / Denominator ;
		  IntegralOverZ += TPC_Z0 * zterm * qwe ;
	          if ( n>10 && fabs(IntegralOverZ)*1.e-10 > fabs(qwe) ) break;   // Assume series converges, break if small terms
		}
	      shortEr[i][j] = IntegralOverZ ;
 	    }
	}
      DoOnce = false ;     // Note reversed logic compared to other methods in StMagUtilies
    }
  
  if ( (NumberOfEastInnerShorts + NumberOfEastOuterShorts + NumberOfWestInnerShorts + NumberOfWestOuterShorts) == 0 ) 
       { Xprime[0] = x[0] ; Xprime[1] = x[1] ; Xprime[2] = x[2] ; return ; }
  
  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  z      =  x[2] ;
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM

  Interpolate2DEdistortion( ORDER, r, z, shortEr, Er_integral ) ;
  Ephi_integral = 0.0 ;  // Efield is symmetric in phi

  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}

  
//________________________________________


/// Read the electric and magnetic field maps stored on disk
void StMagUtilities::ReadField( )
{

  FILE    *magfile, *b3Dfile ;
  TString comment, filename, filename3D ;
  TString MapLocation ;
  TString BaseLocation = "$STAR/StarDb/StMagF/" ;     // Base Directory for Maps

  if ( gMap == kMapped )                    // Mapped field values
    {
      if ( TMath::Abs(gFactor) > 0.8 )      // Scale from full field data 
	{
	  if ( gFactor > 0 )
	    {
	      filename   = "bfield_full_positive_2D.dat" ;
	      filename3D = "bfield_full_positive_3D.dat" ;
	      comment    = "Measured Full Field" ;
	      gRescale   = 1 ;                // Normal field 
	    }
	  else
	    {
	      filename   = "bfield_full_negative_2D.dat" ;
	      filename3D = "bfield_full_negative_3D.dat" ;
	      comment    = "Measured Full Field Reversed" ;
	      gRescale   = -1 ;               // Reversed field
	    }
	}
      else                                  // Scale from half field data             
	{
	  filename   = "bfield_half_positive_2D.dat" ;
	  filename3D = "bfield_half_positive_3D.dat" ;
          comment    = "Measured Half Field" ;
	  gRescale   = 2 ;                    // Adjust scale factor to use half field data
	}
    }
  else if ( gMap == kConstant )             // Constant field values
    {
      filename = "const_full_positive_2D.dat" ;
      comment  = "Constant Full Field" ;
      gRescale = 1 ;                        // Normal field
    }
  else
    {
      fprintf(stderr,"StMagUtilities::ReadField  No map available - you must choose a mapped field or a constant field\n");
      exit(1) ;
    }
      
  printf("StMagUtilities::ReadField  Reading  Magnetic Field  %s,  Scale factor = %f \n",comment.Data(),gFactor);
  printf("StMagUtilities::ReadField  Filename is %s, Adjusted Scale factor = %f \n",filename.Data(),gFactor*gRescale);
  printf("StMagUtilities::ReadField  Version  ") ;
  
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

  printf("\n");
  
  MapLocation = BaseLocation + filename ;
  gSystem->ExpandPathName(MapLocation) ;
  magfile = fopen(MapLocation.Data(),"r") ;
  printf("StMagUtilities::ReadField  Reading  2D Magnetic Field file: %s \n",filename.Data());

  if (magfile) 

    {
      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , magfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , magfile ) ;
      fgets  ( cname, sizeof(cname) , magfile ) ;
      fgets  ( cname, sizeof(cname) , magfile ) ;
      fgets  ( cname, sizeof(cname) , magfile ) ;

      for ( Int_t j=0 ; j < BMap_nZ ; j++ ) 
	{
	  for ( Int_t k=0 ; k < BMap_nR ; k++ )
	    {
	      fgets  ( cname, sizeof(cname) , magfile ) ; 
	      sscanf ( cname, " %f %f %f %f ", &Radius[k], &ZList[j], &Br[j][k], &Bz[j][k] ) ;  
	    }
	}
    }

  else 

    { 
      fprintf(stderr,"StMagUtilities::ReadField  File %s not found !\n",MapLocation.Data());
      exit(1);
    }

  fclose(magfile) ;
      
  MapLocation = BaseLocation + filename3D ;
  gSystem->ExpandPathName(MapLocation) ;
  b3Dfile = fopen(MapLocation.Data(),"r") ;
  printf("StMagUtilities::ReadField  Reading  3D Magnetic Field file: %s \n",filename3D.Data());

  if (b3Dfile) 

    {
      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      
      for ( Int_t i=0 ; i < BMap_nPhi ; i++ ) 
	{
	  for ( Int_t j=0 ; j < BMap_nZ ; j++ ) 
	    {
	      for ( Int_t k=0 ; k < BMap_nR ; k++ )
		{
		  fgets  ( cname, sizeof(cname) , b3Dfile ) ; 
		  sscanf ( cname, " %f %f %f %f %f %f ",
			   &R3D[k], &Z3D[j], &Phi3D[i], &Br3D[i][j][k], &Bz3D[i][j][k], &Bphi3D[i][j][k] ) ;
		  Phi3D[i] *= TMath::Pi() / 180. ;   // Convert to Radians  phi = 0 to 2*Pi
		}
	    }
	}
    }

  else if ( gMap == kConstant )             // Constant field values

    {
      for ( Int_t i=0 ; i < BMap_nPhi ; i++ ) 
	{
	  for ( Int_t j=0 ; j < BMap_nZ ; j++ ) 
	    {
	      for ( Int_t k=0 ; k < BMap_nR ; k++ )
		{
		  Br3D[i][j][k] = Br[j][k] ;
		  Bz3D[i][j][k] = Bz[j][k] ;
		  Bphi3D[i][j][k] = 0 ;
		}
	    }
	}
    }

  else

    { 
      fprintf(stderr,"StMagUtilities::ReadField  File %s not found !\n",MapLocation.Data());
      exit(1);
    }

  fclose(b3Dfile) ;

  // Membrane correction is Obsolete (JT 2009).  It is now disabled and standard E field maps are now defined in the .h file
  /*
  FILE *efile ;
  filename = "membrane_efield.dat" ;
  MapLocation = BaseLocation + filename ;
  gSystem->ExpandPathName(MapLocation) ;
  efile = fopen(MapLocation.Data(),"r") ;
  printf("StMagUtilities::ReadField  Reading  CM Electric Field Distortion File: %s \n",filename.Data());

  if (efile) 
    {

      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , efile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      
      for ( Int_t i=0 ; i < EMap_nZ ; i++ ) 
	{
	  for ( Int_t j=0 ; j < EMap_nPhi ; j++ )
	    {
	      for ( Int_t k=0 ; k < EMap_nR ; k++ )
		{
		  if ( j == EMap_nPhi-1 )
		    {
		      ePhiList[j] = 6.2832 ;    // Repeat phi = 0 column in phi == 2 PI column
		      cmEr[i][j][k] = cmEr[i][0][k] ;
		      cmEphi[i][j][k] = cmEphi[i][0][k] ;
		    }
		  else
		    {
		      fgets  ( cname, sizeof(cname) , efile ) ; 
		      sscanf ( cname, " %f %f %f %f %f ", &eRList[k], &ePhiList[j], 
			       &eZList[i], &cmEr[i][j][k], &cmEphi[i][j][k] ) ; 
		      //ePhiList[j] *= TMath::Pi() / 180. ;  // Assume table uses  phi = 0 to 2*Pi
		    }
		}
	    }
	}
    }      

  else 
    { 
      fprintf(stderr,"StMagUtilities::ReadField  File %s not found !\n",MapLocation.Data());
      exit(1);
    }

  fclose(efile) ;
  */
  // End of Obsolete section
 

  // EndCap correction is Obsolete (JT 2009).  It is now disabled and standard E field maps are now defined in the .h file
  /*
  FILE *eefile ;
  filename = "endcap_efield.dat" ;
  MapLocation = BaseLocation + filename ;
  gSystem->ExpandPathName(MapLocation) ;
  eefile = fopen(MapLocation.Data(),"r") ;
  printf("StMagUtilities::ReadField  Reading  Endcap Electric Field Distortion File: %s \n",filename.Data());

  if (eefile) 
    {

      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , eefile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      
      for ( Int_t i=0 ; i < EMap_nZ ; i++ ) 
	{
	  for ( Int_t j=0 ; j < EMap_nPhi ; j++ )
	    {
	      for ( Int_t k=0 ; k < EMap_nR ; k++ )
		{
		  if ( j == EMap_nPhi-1 )
		    {
		      ePhiList[j] = 6.2832 ;    // Repeat phi = 0 column in phi == 2 PI column
		      endEr[i][j][k] = endEr[i][0][k] ;
		      endEphi[i][j][k] = endEphi[i][0][k] ;
		    }
		  else
		    {
		      fgets  ( cname, sizeof(cname) , eefile ) ; 
		      sscanf ( cname, " %f %f %f %f %f ", &eRList[k], &ePhiList[j], 
			       &eZList[i], &endEr[i][j][k], &endEphi[i][j][k] ) ;  
		      //eePhiList[j] *= TMath::Pi() / 180. ;  // Assume table uses  phi = 0 to 2*Pi
		    }
		}
	    }
	}
    }      

  else 
    { 
      fprintf(stderr,"StMagUtilities::ReadField  File %s not found !\n",MapLocation.Data());
      exit(1);
    }

  fclose(eefile) ;
  */
  // End of Obsolete section

  return ;

}


//________________________________________

/// Interpolate the B field map - 2D interpolation
void StMagUtilities::Interpolate2DBfield( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value )
{

  Float_t fscale ;

  fscale = 0.001*gFactor*gRescale ;                 // Scale STAR maps to work in kGauss, cm
  if ( TMath::Abs(fscale) < 4e-7 ) fscale = 4e-7 ;  // Zero field is unphysical, so set it to Earths Field (~1 gauss)
                                                    // Note that this assumes that the 1/2 field (0.25 Tesla) map
                                                    // is the reference map for the scaledown.  See ReadField().

  const   Int_t ORDER = 1  ;                        // Linear interpolation = 1, Quadratic = 2        
  static  Int_t jlow=0, klow=0 ;                            
  Float_t save_Br[ORDER+1] ;
  Float_t save_Bz[ORDER+1] ;

  Search ( BMap_nZ, ZList,  z, jlow ) ;
  Search ( BMap_nR, Radius, r, klow ) ;
  if ( jlow < 0 ) jlow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( klow < 0 ) klow = 0 ;
  if ( jlow + ORDER  >=    BMap_nZ - 1 ) jlow =   BMap_nZ - 1 - ORDER ;
  if ( klow + ORDER  >=    BMap_nR - 1 ) klow =   BMap_nR - 1 - ORDER ;

  for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
    {
      save_Br[j-jlow]   = Interpolate( &Radius[klow], &Br[j][klow], ORDER, r )   ;
      save_Bz[j-jlow]   = Interpolate( &Radius[klow], &Bz[j][klow], ORDER, r )   ;
    }
  Br_value  = fscale * Interpolate( &ZList[jlow], save_Br, ORDER, z )   ; 
  Bz_value  = fscale * Interpolate( &ZList[jlow], save_Bz, ORDER, z )   ; 

}

/// Interpolate the B field map - 3D interpolation
void StMagUtilities::Interpolate3DBfield( const Float_t r, const Float_t z, const Float_t phi, 
			 Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value )
{

  Float_t fscale ;

  fscale = 0.001*gFactor*gRescale ;                 // Scale STAR maps to work in kGauss, cm
  if ( TMath::Abs(fscale) < 4e-7 ) fscale = 4e-7 ;  // Zero field is unphysical, set it to Earths Field (~1 gauss)

  const   Int_t ORDER = 1 ;                         // Linear interpolation = 1, Quadratic = 2   
  const   Int_t PHIORDER = 1 ;                      // Linear interpolation = 1, Quadratic = 2 ... PHI table is crude so do linear interp   
  static  Int_t ilow=0, jlow=0, klow=0 ;
  Float_t save_Br[ORDER+1],   saved_Br[ORDER+1] ;
  Float_t save_Bz[ORDER+1],   saved_Bz[ORDER+1] ;
  Float_t save_Bphi[ORDER+1], saved_Bphi[ORDER+1] ;

  Search( BMap_nPhi, Phi3D, phi, ilow ) ;
  Search( BMap_nZ,   Z3D,   z,   jlow ) ;
  Search( BMap_nR,   R3D,   r,   klow ) ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow < 0 ) klow = 0 ;

  if ( ilow + ORDER  >=  BMap_nPhi - 1 ) ilow = BMap_nPhi - 1 - ORDER ;
  if ( jlow + ORDER  >=    BMap_nZ - 1 ) jlow =   BMap_nZ - 1 - ORDER ;
  if ( klow + ORDER  >=    BMap_nR - 1 ) klow =   BMap_nR - 1 - ORDER ;

  for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
      for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
	{
	  save_Br[j-jlow]   = Interpolate( &R3D[klow], &Br3D[i][j][klow], ORDER, r )   ;
	  save_Bz[j-jlow]   = Interpolate( &R3D[klow], &Bz3D[i][j][klow], ORDER, r )   ;
	  save_Bphi[j-jlow] = Interpolate( &R3D[klow], &Bphi3D[i][j][klow], ORDER, r ) ; 
	}
      saved_Br[i-ilow]   = Interpolate( &Z3D[jlow], save_Br, ORDER, z )   ; 
      saved_Bz[i-ilow]   = Interpolate( &Z3D[jlow], save_Bz, ORDER, z )   ; 
      saved_Bphi[i-ilow] = Interpolate( &Z3D[jlow], save_Bphi, ORDER, z ) ; 
    }
  Br_value   = fscale * Interpolate( &Phi3D[ilow], saved_Br, PHIORDER, phi )   ;
  Bz_value   = fscale * Interpolate( &Phi3D[ilow], saved_Bz, PHIORDER, phi )   ;
  Bphi_value = fscale * Interpolate( &Phi3D[ilow], saved_Bphi, PHIORDER, phi ) ; 

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


/// Interpolate function Y(x) using linear (ORDER=1) or quadratic (ORDER=2) interpolation.
Float_t StMagUtilities::Interpolate( const Float_t Xarray[], const Float_t Yarray[], 
				     const Int_t ORDER, const Float_t x )
{

  Float_t y ;

  if ( ORDER == 2 )                // Quadratic Interpolation = 2 

    {
      y  = (x-Xarray[1]) * (x-Xarray[2]) * Yarray[0] / ( (Xarray[0]-Xarray[1]) * (Xarray[0]-Xarray[2]) ) ; 
      y += (x-Xarray[2]) * (x-Xarray[0]) * Yarray[1] / ( (Xarray[1]-Xarray[2]) * (Xarray[1]-Xarray[0]) ) ; 
      y += (x-Xarray[0]) * (x-Xarray[1]) * Yarray[2] / ( (Xarray[2]-Xarray[0]) * (Xarray[2]-Xarray[1]) ) ; 
      
    }

  else                             // Linear Interpolation = 1

    {
      y  = Yarray[0] + ( Yarray[1]-Yarray[0] ) * ( x-Xarray[0] ) / ( Xarray[1] - Xarray[0] ) ;
    }

  return (y) ;

}


//________________________________________


/// Search an ordered table by starting at the most recently used point
void StMagUtilities::Search( const Int_t N, const Float_t Xarray[], const Float_t x, Int_t &low )
{

  Long_t middle, high ;
  Int_t  ascend = 0, increment = 1 ;

  if ( Xarray[N-1] >= Xarray[0] ) ascend = 1 ;  // Ascending ordered table if true
  
  if ( low < 0 || low > N-1 ) { low = -1 ; high = N ; }

  else                                            // Ordered Search phase
    {
      if ( (Int_t)( x >= Xarray[low] ) == ascend ) 
	{
	  if ( low == N-1 ) return ;          
	  high = low + 1 ;
	  while ( (Int_t)( x >= Xarray[high] ) == ascend )  
	    {
	      low = high ;
	      increment *= 2 ;
	      high = low + increment ;
	      if ( high > N-1 )  {  high = N ; break ;  }
	    }
	}
      else
	{
	  if ( low == 0 )  {  low = -1 ;  return ;  }
	  high = low - 1 ;
	  while ( (Int_t)( x < Xarray[low] ) == ascend )
	    {
	      high = low ;
	      increment *= 2 ;
	      if ( increment >= high )  {  low = -1 ;  break ;  }
	      else  low = high - increment ;
	    }
	}
    }

  while ( (high-low) != 1 )                      // Binary Search Phase
    {
      middle = ( high + low ) / 2 ;
      if ( (Int_t)( x >= Xarray[middle] ) == ascend )
	low = middle ;
      else
	high = middle ;
    }

  if ( x == Xarray[N-1] ) low = N-2 ;
  if ( x == Xarray[0]   ) low = 0 ;

}


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
void StMagUtilities::PoissonRelaxation( TMatrix &ArrayV, const TMatrix &Charge, TMatrix &EroverEz,
					const Int_t ROWS, const Int_t COLUMNS, const Int_t ITERATIONS )
{

  const Float_t  GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
  const Float_t  GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;
  const Float_t  Ratio       =  GRIDSIZER*GRIDSIZER / (GRIDSIZEZ*GRIDSIZEZ) ;
  //const Float_t  Four        =  2.0 + 2.0*Ratio ;

  TMatrix  ArrayE(ROWS,COLUMNS) ;

  //Check that number of ROWS and COLUMNS is suitable for a binary expansion

  if ( !IsPowerOfTwo(ROWS-1) )
    { cout << "StMagUtilities::PoissonRelaxation - Error in the number of ROWS.  Must be 2**M - 1" << endl ; exit(1) ; }
  if ( !IsPowerOfTwo(COLUMNS-1) )
    { cout << "StMagUtilities::PoissonRelaxation - Error in the number of COLUMNS.  Must be 2**N - 1" << endl ; exit(1) ; }
  
  //Solve Poisson's equation in cylindrical coordinates by relaxation technique
  //Allow for different size grid spacing in R and Z directions
  //Use a binary expansion of the matrix to speed up the solution of the problem

  Int_t i_one = (ROWS-1)/4 ;
  Int_t j_one = (COLUMNS-1)/4 ;
  Int_t loops = 1 + (int) ( 0.5 + TMath::Log2( (double) TMath::Max(i_one,j_one) ) ) ;  // Solve for N in 2**N and add one

  for ( Int_t count = 0 ; count < loops ; count++ ) {  // Do several loops as the matrix expands and the resolution increases

    Float_t tempGRIDSIZER = GRIDSIZER * i_one ;
    Float_t tempRatio     = Ratio * i_one * i_one / ( j_one * j_one ) ;
    Float_t tempFourth    = 1.0 / (2.0 + 2.0*tempRatio) ;

    Float_t coef1[ROWS],coef2[ROWS];
    for ( Int_t i = i_one ; i < ROWS-1 ; i+=i_one )  {
      Float_t Radius = IFCRadius + i*GRIDSIZER ;
      coef1[i] = 1.0 + tempGRIDSIZER/(2*Radius);
      coef2[i] = 1.0 - tempGRIDSIZER/(2*Radius);
    }

    TMatrix SumCharge(ROWS,COLUMNS) ;

    for ( Int_t i = i_one ; i < ROWS-1 ; i += i_one ) {
      Float_t Radius = IFCRadius + i_one*GRIDSIZER ;
      for ( Int_t j = j_one ; j < COLUMNS-1 ; j += j_one ) {
	if ( i_one == 1 && j_one == 1 ) SumCharge(i,j) = Charge(i,j) ;
	else {           // Add up all enclosed charge within 1/2 unit in all directions
	  Float_t weight = 0.0 ;
	  Float_t sum    = 0.0 ;
	  SumCharge(i,j) = 0.0 ;
	  for ( Int_t ii = i-i_one/2 ; ii <= i+i_one/2 ; ii++ ) {
	    for ( Int_t jj = j-j_one/2 ; jj <= j+j_one/2 ; jj++ ) {
	      if ( ii == i-i_one/2 || ii == i+i_one/2 || jj == j-j_one/2 || jj == j+j_one/2 ) weight = 0.5 ;
	      else
		weight = 1.0 ;
	      SumCharge(i,j) += Charge(ii,jj)*weight*Radius ;   // Note that this is cylindrical geometry
	      sum += weight*Radius ;
	    }
	  }
	  SumCharge(i,j) /= sum ;
	}
        SumCharge(i,j) *= tempGRIDSIZER*tempGRIDSIZER; // just saving a step later on
       }
    }

    for ( Int_t k = 1 ; k <= ITERATIONS; k++ ) {               // Solve Poisson's Equation

      Float_t OverRelax   = 1.0 + TMath::Sqrt( TMath::Cos( (k*TMath::PiOver2())/ITERATIONS ) ) ; // Over-relaxation index, >= 1 but < 2
      Float_t OverRelaxM1 = OverRelax - 1.0 ;
      Float_t OverRelaxtempFourth, OverRelaxcoef5 ;
      OverRelaxtempFourth = OverRelax * tempFourth ;
      OverRelaxcoef5 = OverRelaxM1 / OverRelaxtempFourth ; 

      for ( Int_t i = i_one ; i < ROWS-1 ; i += i_one ) {
	for ( Int_t j = j_one ; j < COLUMNS-1 ; j += j_one ) {

	  ArrayV(i,j) = (   coef2[i]       *   ArrayV(i-i_one,j)
			  + tempRatio      * ( ArrayV(i,j-j_one) + ArrayV(i,j+j_one) )
			  - OverRelaxcoef5 *   ArrayV(i,j) 
			  + coef1[i]       *   ArrayV(i+i_one,j) 
			  + SumCharge(i,j) 
			) * OverRelaxtempFourth;

	}
      }

      if ( k == ITERATIONS ) {       // After full solution is achieved, copy low resolution solution into higher res array
	for ( Int_t i = i_one ; i < ROWS-1 ; i += i_one ) {
	  for ( Int_t j = j_one ; j < COLUMNS-1 ; j += j_one ) {

	    if ( i_one > 1 ) {              
	      ArrayV(i+i_one/2,j)                    =  ( ArrayV(i+i_one,j) + ArrayV(i,j)     ) / 2 ;
	      if ( i == i_one )  ArrayV(i-i_one/2,j) =  ( ArrayV(0,j)       + ArrayV(i_one,j) ) / 2 ;
	    }
	    if ( j_one > 1 ) {
	      ArrayV(i,j+j_one/2)                    =  ( ArrayV(i,j+j_one) + ArrayV(i,j) )     / 2 ;
	      if ( j == j_one )  ArrayV(i,j-j_one/2) =  ( ArrayV(i,0)       + ArrayV(i,j_one) ) / 2 ;
	    }
	    if ( i_one > 1 && j_one > 1 ) {
	      ArrayV(i+i_one/2,j+j_one/2) =  ( ArrayV(i+i_one,j+j_one) + ArrayV(i,j) ) / 2 ;
	      if ( i == i_one ) ArrayV(i-i_one/2,j-j_one/2) =   ( ArrayV(0,j-j_one) + ArrayV(i_one,j) ) / 2 ;
	      if ( j == j_one ) ArrayV(i-i_one/2,j-j_one/2) =   ( ArrayV(i-i_one,0) + ArrayV(i,j_one) ) / 2 ;
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
       TH2F* h1  = new TH2F(ArrayV)  ;  // JT test  
       h1 -> DrawCopy("lego") ;      // JT test
       ArrayV -> Draw("lego") ; // JT test
       c1 -> Update() ;
       cout << "Hit any key to continue" << endl ;  // JT test
       Char_t anychar ;    // JT test
       cin  >> anychar ;   // JT test
    */ //End JT test block

    i_one = i_one / 2 ; if ( i_one < 1 ) i_one = 1 ;
    j_one = j_one / 2 ; if ( j_one < 1 ) j_one = 1 ;

  }      

  //Differentiate V(r) and solve for E(r) using special equations for the first and last row
  //Integrate E(r)/E(z) from point of origin to pad plane

  for ( Int_t j = COLUMNS-1 ; j >= 0 ; j-- )  // Count backwards to facilitate integration over Z
    {	  
      // Differentiate in R
      for ( Int_t i = 1 ; i < ROWS-1 ; i++ )  ArrayE(i,j) = -1 * ( ArrayV(i+1,j) - ArrayV(i-1,j) ) / (2*GRIDSIZER) ;
      ArrayE(0,j)      =  -1 * ( -0.5*ArrayV(2,j) + 2.0*ArrayV(1,j) - 1.5*ArrayV(0,j) ) / GRIDSIZER ;  
      ArrayE(ROWS-1,j) =  -1 * ( 1.5*ArrayV(ROWS-1,j) - 2.0*ArrayV(ROWS-2,j) + 0.5*ArrayV(ROWS-3,j) ) / GRIDSIZER ; 
      // Integrate over Z
      for ( Int_t i = 0 ; i < ROWS ; i++ ) 
	{
	  Int_t Index = 1 ;   // Simpsons rule if N=odd.  If N!=odd then add extra point by trapezoidal rule.  
	  EroverEz(i,j) = 0.0 ;
	  for ( Int_t k = j ; k < COLUMNS ; k++ ) 
	    { 
	      EroverEz(i,j)  +=  Index*(GRIDSIZEZ/3.0)*ArrayE(i,k)/(-1*StarMagE) ;
	      if ( Index != 4 )  Index = 4; else Index = 2 ;
	    }
	  if ( Index == 4 ) EroverEz(i,j)  -=  (GRIDSIZEZ/3.0)*ArrayE(i,COLUMNS-1)/ (-1*StarMagE) ;
	  if ( Index == 2 ) EroverEz(i,j)  +=  
			      (GRIDSIZEZ/3.0)*(0.5*ArrayE(i,COLUMNS-2)-2.5*ArrayE(i,COLUMNS-1))/(-1*StarMagE) ;
	  if ( j == COLUMNS-2 ) EroverEz(i,j) =  
				  (GRIDSIZEZ/3.0)*(1.5*ArrayE(i,COLUMNS-2)+1.5*ArrayE(i,COLUMNS-1))/(-1*StarMagE) ;
	  if ( j == COLUMNS-1 ) EroverEz(i,j) =  0.0 ;
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
					  const Int_t ROWS, const Int_t COLUMNS,  const Int_t PHISLICES, const Float_t DELTAPHI, 
					  const Int_t ITERATIONS, const Int_t SYMMETRY )
{

  const Float_t  GRIDSIZEPHI =  DELTAPHI ;
  const Float_t  GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
  const Float_t  GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;
  const Float_t  RatioPhi    =  GRIDSIZER*GRIDSIZER / (GRIDSIZEPHI*GRIDSIZEPHI) ;
  const Float_t  RatioZ      =  GRIDSIZER*GRIDSIZER / (GRIDSIZEZ*GRIDSIZEZ) ;

  TMatrix ArrayE(ROWS,COLUMNS) ;

  //Check that the number of ROWS and COLUMNS is suitable for a binary expansion
  if ( !IsPowerOfTwo((ROWS-1))    )
    { cout << "StMagUtilities::Poisson3DRelaxation - Error in the number of ROWS.  Must be 2**M - 1" << endl ; exit(1) ; }
  if ( !IsPowerOfTwo((COLUMNS-1)) )
    { cout << "StMagUtilities::Poisson3DRelaxation - Error in the number of COLUMNS.  Must be 2**N - 1" << endl ; exit(1) ; }
  if ( PHISLICES <= 3   )
    { cout << "StMagUtilities::Poisson3DRelaxation - Error in the number of PHISLICES.  Must be larger than 3" << endl ; exit(1) ; }
  
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

  for ( Int_t count = 0 ; count < loops ; count++ ) {  // START the master loop and do the binary expansion
   
    Float_t  tempGRIDSIZER   =  GRIDSIZER  * i_one ;
    Float_t  tempRatioPhi    =  RatioPhi * i_one * i_one ; // Used to be divided by ( m_one * m_one ) when m_one was != 1
    Float_t  tempRatioZ      =  RatioZ   * i_one * i_one / ( j_one * j_one ) ;

    Float_t coef1[ROWS],coef2[ROWS],coef3[ROWS],coef4[ROWS];
    for ( Int_t i = i_one ; i < ROWS-1 ; i+=i_one )  {
      Float_t Radius = IFCRadius + i*GRIDSIZER ;
      coef1[i] = 1.0 + tempGRIDSIZER/(2*Radius);
      coef2[i] = 1.0 - tempGRIDSIZER/(2*Radius);
      coef3[i] = tempRatioPhi/(Radius*Radius);
      coef4[i] = 0.5 / (1.0 + tempRatioZ + coef3[i]);
    }

    for ( Int_t m = 0 ; m < PHISLICES ; m++ ) {
      TMatrix &Charge    = *ArrayofCharge[m] ;
      TMatrix &SumCharge = *ArrayofSumCharge[m] ;
      for ( Int_t i = i_one ; i < ROWS-1 ; i += i_one ) {
	Float_t Radius = IFCRadius + i*GRIDSIZER ;
	for ( Int_t j = j_one ; j < COLUMNS-1 ; j += j_one ) {
	  if ( i_one == 1 && j_one == 1 ) SumCharge(i,j) = Charge(i,j) ;
	  else {           // Add up all enclosed charge within 1/2 unit in all directions
	    Float_t weight = 0.0 ;
	    Float_t sum    = 0.0 ;
	    SumCharge(i,j) = 0.0 ;
	    for ( Int_t ii = i-i_one/2 ; ii <= i+i_one/2 ; ii++ ) {
	      for ( Int_t jj = j-j_one/2 ; jj <= j+j_one/2 ; jj++ ) {
		if ( ii == i-i_one/2 || ii == i+i_one/2 || jj == j-j_one/2 || jj == j+j_one/2 ) weight = 0.5 ;
		else
		  weight = 1.0 ; 
		SumCharge(i,j) += Charge(ii,jj)*weight*Radius ;  
		sum += weight*Radius ;
	      }
	    }
	    SumCharge(i,j) /= sum ;
	  }
          SumCharge(i,j) *= tempGRIDSIZER*tempGRIDSIZER; // just saving a step later on
	}
      }
    }

    for ( Int_t k = 1 ; k <= ITERATIONS; k++ ) {

      Float_t OverRelax   = 1.0 + TMath::Sqrt( TMath::Cos( (k*TMath::PiOver2())/ITERATIONS ) ) ; // Over-relaxation index, >= 1 but < 2
      Float_t OverRelaxM1 = OverRelax - 1.0 ;
      Float_t OverRelaxcoef4[ROWS], OverRelaxcoef5[ROWS] ;
      for ( Int_t i = i_one ; i < ROWS-1 ; i+=i_one ) { 
	OverRelaxcoef4[i] = OverRelax * coef4[i] ;
	OverRelaxcoef5[i] = OverRelaxM1 / OverRelaxcoef4[i] ; 
      }

      for ( Int_t m = 0 ; m < PHISLICES ; m++ ) {

	m_plus  = m + 1;
	m_minus = m - 1 ; 
	if (SYMMETRY==1) {  // Reflection symmetry in phi (e.g. symmetry at sector boundaries, or half sectors, etc.)
	  if ( m_plus  > PHISLICES-1 ) m_plus  = PHISLICES - 2 ;
	  if ( m_minus < 0 )           m_minus = 1 ;
	}
	else { // No Symmetries in phi, no boundaries, the calculation is continuous across all phi
	  if ( m_plus  > PHISLICES-1 ) m_plus  = m + 1 - PHISLICES ;
	  if ( m_minus < 0 )           m_minus = m - 1 + PHISLICES ;
	}
	TMatrix &ArrayV    =  *ArrayofArrayV[m] ;
	TMatrix &ArrayVP   =  *ArrayofArrayV[m_plus] ;
	TMatrix &ArrayVM   =  *ArrayofArrayV[m_minus] ;
	TMatrix &SumCharge =  *ArrayofSumCharge[m] ;

	for ( Int_t i = i_one ; i < ROWS-1 ; i+=i_one )  {
	  for ( Int_t j = j_one ; j < COLUMNS-1 ; j+=j_one ) {

            ArrayV(i,j) = (   coef2[i]          *   ArrayV(i-i_one,j)
			    + tempRatioZ        * ( ArrayV(i,j-j_one)  +  ArrayV(i,j+j_one) )
			    - OverRelaxcoef5[i] *   ArrayV(i,j) 
			    + coef1[i]          *   ArrayV(i+i_one,j)  
			    + coef3[i]          * ( ArrayVP(i,j)       +  ArrayVM(i,j) )
			    + SumCharge(i,j) 
			  ) * OverRelaxcoef4[i] ;     // Note: over-relax the solution at each step.  This speeds up the convergance.

	  }
	}

	if ( k == ITERATIONS ) {       // After full solution is achieved, copy low resolution solution into higher res array
	  for ( Int_t i = i_one ; i < ROWS-1 ; i+=i_one )  {
	    for ( Int_t j = j_one ; j < COLUMNS-1 ; j+=j_one ) {
	      
	      if ( i_one > 1 ) {              
		ArrayV(i+i_one/2,j)                    =  ( ArrayV(i+i_one,j) + ArrayV(i,j)     ) / 2 ;
		if ( i == i_one )  ArrayV(i-i_one/2,j) =  ( ArrayV(0,j)       + ArrayV(i_one,j) ) / 2 ;
	      }
	      if ( j_one > 1 ) {
		ArrayV(i,j+j_one/2)                    =  ( ArrayV(i,j+j_one) + ArrayV(i,j) )     / 2 ;
		if ( j == j_one )  ArrayV(i,j-j_one/2) =  ( ArrayV(i,0)       + ArrayV(i,j_one) ) / 2 ;
	      }
	      if ( i_one > 1 && j_one > 1 ) {
		ArrayV(i+i_one/2,j+j_one/2) =  ( ArrayV(i+i_one,j+j_one) + ArrayV(i,j) ) / 2 ;
		if ( i == i_one ) ArrayV(i-i_one/2,j-j_one/2) =   ( ArrayV(0,j-j_one) + ArrayV(i_one,j) ) / 2 ;
		if ( j == j_one ) ArrayV(i-i_one/2,j-j_one/2) =   ( ArrayV(i-i_one,0) + ArrayV(i,j_one) ) / 2 ;
		// Note that this leaves a point at the upper left and lower right corners uninitialized.  Not a big deal.
	      }
	    }	    
	  }
	}

      }
    }      

    i_one = i_one / 2 ; if ( i_one < 1 ) i_one = 1 ;
    j_one = j_one / 2 ; if ( j_one < 1 ) j_one = 1 ;

  }
  
  //Differentiate V(r) and solve for E(r) using special equations for the first and last row
  //Integrate E(r)/E(z) from point of origin to pad plane

  for ( Int_t m = 0 ; m < PHISLICES ; m++ )
    {
      TMatrix &ArrayV    =  *ArrayofArrayV[m] ;
      TMatrix &EroverEz  =  *ArrayofEroverEz[m] ;
      
      for ( Int_t j = COLUMNS-1 ; j >= 0 ; j-- )  // Count backwards to facilitate integration over Z
	{	  
	  // Differentiate in R
	  for ( Int_t i = 1 ; i < ROWS-1 ; i++ )  ArrayE(i,j) = -1 * ( ArrayV(i+1,j) - ArrayV(i-1,j) ) / (2*GRIDSIZER) ;
	  ArrayE(0,j)      =  -1 * ( -0.5*ArrayV(2,j) + 2.0*ArrayV(1,j) - 1.5*ArrayV(0,j) ) / GRIDSIZER ;  
	  ArrayE(ROWS-1,j) =  -1 * ( 1.5*ArrayV(ROWS-1,j) - 2.0*ArrayV(ROWS-2,j) + 0.5*ArrayV(ROWS-3,j) ) / GRIDSIZER ; 
	  // Integrate over Z
	  for ( Int_t i = 0 ; i < ROWS ; i++ ) 
	    {
	      Int_t Index = 1 ;   // Simpsons rule if N=odd.  If N!=odd then add extra point by trapezoidal rule.  
	      EroverEz(i,j) = 0.0 ;
	      for ( Int_t k = j ; k < COLUMNS ; k++ ) 
		{ 
		  EroverEz(i,j)  +=  Index*(GRIDSIZEZ/3.0)*ArrayE(i,k)/(-1*StarMagE) ;
		  if ( Index != 4 )  Index = 4; else Index = 2 ;
		}
	      if ( Index == 4 ) EroverEz(i,j)  -=  (GRIDSIZEZ/3.0)*ArrayE(i,COLUMNS-1)/ (-1*StarMagE) ;
	      if ( Index == 2 ) EroverEz(i,j)  +=  
				(GRIDSIZEZ/3.0)*(0.5*ArrayE(i,COLUMNS-2)-2.5*ArrayE(i,COLUMNS-1))/(-1*StarMagE) ;
	      if ( j == COLUMNS-2 ) EroverEz(i,j) =  
				    (GRIDSIZEZ/3.0)*(1.5*ArrayE(i,COLUMNS-2)+1.5*ArrayE(i,COLUMNS-1))/(-1*StarMagE) ;
	      if ( j == COLUMNS-1 ) EroverEz(i,j) =  0.0 ;
	    }
	}
      // if ( m == 0 ) { TCanvas*  c1 =  new TCanvas("ErOverEz","ErOverEz",50,50,840,600) ;  c1 -> cd() ;
      // EroverEz.Draw("surf") ; } // JT test
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
	  if ( m_plus  > PHISLICES-1 ) m_plus  = m + 1 - PHISLICES ;
	  if ( m_minus < 0 )           m_minus = m - 1 + PHISLICES ;
	}
      TMatrix &ArrayVP     =  *ArrayofArrayV[m_plus] ;
      TMatrix &ArrayVM     =  *ArrayofArrayV[m_minus] ;
      TMatrix &EPhioverEz  =  *ArrayofEPhioverEz[m] ;
      for ( Int_t j = COLUMNS-1 ; j >= 0 ; j-- )  // Count backwards to facilitate integration over Z
	{	  
	  // Differentiate in Phi
	  for ( Int_t i = 0 ; i < ROWS ; i++ )  
	    {
	      Float_t Radius = IFCRadius + i*GRIDSIZER ;
	      ArrayE(i,j) = -1 * ( ArrayVP(i,j) - ArrayVM(i,j) ) / (2*Radius*GRIDSIZEPHI) ;
	    }
	  // Integrate over Z
	  for ( Int_t i = 0 ; i < ROWS ; i++ ) 
	    {
	      Int_t Index = 1 ;   // Simpsons rule if N=odd.  If N!=odd then add extra point by trapezoidal rule.  
	      EPhioverEz(i,j) = 0.0 ;
	      for ( Int_t k = j ; k < COLUMNS ; k++ ) 
		{ 
		  EPhioverEz(i,j)  +=  Index*(GRIDSIZEZ/3.0)*ArrayE(i,k)/(-1*StarMagE) ;
		  if ( Index != 4 )  Index = 4; else Index = 2 ;
		}
	      if ( Index == 4 ) EPhioverEz(i,j)  -=  (GRIDSIZEZ/3.0)*ArrayE(i,COLUMNS-1)/ (-1*StarMagE) ;
	      if ( Index == 2 ) EPhioverEz(i,j)  +=  
				(GRIDSIZEZ/3.0)*(0.5*ArrayE(i,COLUMNS-2)-2.5*ArrayE(i,COLUMNS-1))/(-1*StarMagE) ;
	      if ( j == COLUMNS-2 ) EPhioverEz(i,j) =  
				    (GRIDSIZEZ/3.0)*(1.5*ArrayE(i,COLUMNS-2)+1.5*ArrayE(i,COLUMNS-1))/(-1*StarMagE) ;
	      if ( j == COLUMNS-1 ) EPhioverEz(i,j) =  0.0 ;
	    }
	}
      // if ( m == 5 ) { TCanvas* c2 =  new TCanvas("ArrayE","ArrayE",50,50,840,600) ;  c2 -> cd() ;
      // ArrayE.Draw("surf") ; } // JT test
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
  0xFFFFFF00, 0x1FFFFF represent all 45 rows of the TPC.
  
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

  const Int_t   INNER8 =  8 ;               // Number of TPC rows in the inner sector with 4.8 cm spacing
  const Int_t   INNER  = 13 ;               // Number of TPC rows in the inner sectors 
  const Int_t   ROWS   = 45 ;               // Total number of TPC rows per sector (Inner + Outer)
  const Float_t TestRadius =  77.00 ;      // A random test radius inside the TPC to compare which way the track is going

  Int_t    ChargeB ;
  Float_t  B[3], Rotation, Direction, xx[3], xxprime[3] ;
  Double_t Xtrack[ROWS], Ytrack[ROWS], Ztrack[ROWS] ;
  Double_t Xtrack1[ROWS], Ytrack1[ROWS], Ztrack1[ROWS] ;
  Double_t R[ROWS], dX[ROWS], dY[ROWS], C0, X0, Y0, R0, Pt, R2, theta, theta0, DeltaTheta ;
  Double_t Xprime[ROWS+1], Yprime[ROWS+1], eX[ROWS+1], eY[ROWS+1] ;  // Extra index is to accomodate the vertex in the fit for primaries
 
  BField(x,B) ;
  ChargeB  = Charge * TMath::Sign((int)1,(int)(B[2]*1000)) ;
  Pt = TMath::Sqrt( p[0]*p[0] + p[1]*p[1] ) ;
  R0 = TMath::Abs( 1000.0 * Pt / ( 0.299792 * B[2] ) ) ;     // P in GeV, R in cm, B in kGauss
  X0 = x[0] + ChargeB * p[1] * R0 / Pt ;
  Y0 = x[1] - ChargeB * p[0] * R0 / Pt ; 
  Rotation = TMath::Sign( (double)1.0, (x[0]-X0)*p[1] - (x[1]-Y0)*p[0] ) ; 

  for ( Int_t i = 0 ; i < ROWS ; i++ )
    {
      if ( i < INNER8 ) 
	R[i] = 60.0 + i*4.8 ;                // Not correct because TPC rows aren't circles ... but we dont' care
      else if ( i < INNER ) 
	R[i] = 93.6 + (i-INNER8+1)*5.2 ;
      else               
	R[i] = 127.195 + (i-INNER)*2.0 ;
    }

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
      UndoSpaceChargeDistortion(xx,xxprime) ;
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
      UndoSpaceChargeDistortion(x,xxprime) ;
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
  0xFFFFFF00, 0x1FFFFF represent all 45 rows of the TPC.
  
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
   const Int_t   INNER8   =  8  ;        // Number of TPC rows in the inner sector with 4.8 cm spacing
   const Int_t   INNER    = 13  ;        // Number of TPC rows in the inner sectors
   const Int_t   ROWS     = 45  ;        // Total number of TPC rows per sector (Inner + Outer)
   const Int_t   RefIndex =  7  ;        // Refindex 7 (TPCRow 8) is about where 1/R**2 has no effect on points (~97 cm radius).
   const Int_t   MinHits  = 15  ;        // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   DEBUG    =  0  ;        // Turn on debugging statements and plots

   Int_t    ChargeB ;
   Float_t  B[3], Direction, xx[3], xxprime[3] ;
   Double_t Xreference, Yreference ;
   Double_t Xtrack[ROWS], Ytrack[ROWS], Ztrack[ROWS] ;
   Double_t R[ROWS], C0, X0, Y0, R0, Pt, R2, DeltaTheta, DCA ;
   Double_t Xprime[ROWS+1], Yprime[ROWS+1], Zprime[ROWS+1], dX[ROWS+1], dY[ROWS+1] ;  
   Double_t U[ROWS+1], V[ROWS+1], eU[ROWS+1], eV[ROWS+1] ;  
   // Extra index is to accomodate the vertex in the fit for primaries

   // Temporarily overide settings for space charge data (only)
   StDetectorDbSpaceChargeR2* tempfSpaceChargeR2 = fSpaceChargeR2 ;
   Double_t tempSpaceChargeR2 = SpaceChargeR2 ;
   ManualSpaceChargeR2(sc,SpaceChargeEWRatio); // Set a custom value of the spacecharge parameter but keep previous E/W ratio
   
   BField(x,B) ;
   ChargeB  = Charge * TMath::Sign((int)1,(int)(B[2]*1000)) ;
   Pt = TMath::Sqrt( p[0]*p[0] + p[1]*p[1] ) ;
   R0 = TMath::Abs( 1000.0 * Pt / ( 0.299792 * B[2] ) ) ;     // P in GeV, R in cm, B in kGauss
   X0 = x[0] + ChargeB * p[1] * R0 / Pt ;
   Y0 = x[1] - ChargeB * p[0] * R0 / Pt ;
   DCA = TMath::Sqrt( X0*X0 + Y0*Y0 ) - R0 ;  // Negative means (0,0) is inside the circle

   for ( Int_t i = 0 ; i < ROWS ; i++ )       // Note that i starts at zero
     {
      if ( i < INNER8 ) 
	R[i] = 60.0 + i*4.8 ;                 // Not correct because TPC rows aren't circles ... but we dont' care
      else if ( i < INNER ) 
	R[i] = 93.6 + (i-INNER8+1)*5.2 ;
      else              
	R[i] = 127.195 + (i-INNER)*2.0 ;
     }

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

   const Int_t   INNER8           =   8  ;       // Number of TPC rows in the inner sector with 4.8 cm spacing
   const Int_t   INNER            =  13  ;       // Number of TPC rows in the inner sector
   const Int_t   ROWS             =  45  ;       // Total number of TPC rows per sector (Inner + Outer)
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
	 }
       else
	 {
	   if ( ( i < 24  ) && (( RowMask1 & OneBit<<(i+8)  ) != 0 )) OuterTPCHits++ ;  
	   if ( ( i >= 24 ) && (( RowMask2 & OneBit<<(i-24) ) != 0 )) OuterTPCHits++ ;  
	 }
     }

   pSpace  = 0 ;

   if ( (Pt < 0.3) || (Pt > 2.0) )                           return(0) ; // Fail
   if ( (VertexZ < -50) || (VertexZ > 50) )                  return(0) ; // Fail
   if ( (PseudoRapidity < -1.0) || (PseudoRapidity > 1.0) )  return(0) ; // Fail
   if ( (Charge != 1) && (Charge != -1) )                    return(0) ; // Fail
   if ( (DCA < -4.0) || (DCA > 4.0) )                        return(0) ; // Fail
   if ( InnerTPCHits < MinInnerTPCHits )                     return(0) ; // No action if too few hits in the TPC   
   if ( OuterTPCHits < MinOuterTPCHits )                     return(0) ; // No action if too few hits in the TPC   

   Int_t    ChargeB ;
   Float_t  B[3], Direction, xx[3], xxprime[3] ;
   Double_t Xreference, Yreference ;
   Double_t Xtrack[ROWS], Ytrack[ROWS], Ztrack[ROWS] ;
   Double_t R[ROWS], C0, X0, Y0, R0, Pz, DeltaTheta ;
   Double_t Xprime[ROWS+1], Yprime[ROWS+1], Zprime[ROWS+1], dX[ROWS+1], dY[ROWS+1] ;  
   Double_t U[ROWS+1], V[ROWS+1], eU[ROWS+1], eV[ROWS+1] ;  

   // Temporarily overide settings for space charge data (only)
   StDetectorDbSpaceChargeR2* tempfSpaceChargeR2 = fSpaceChargeR2 ;
   Double_t tempSpaceChargeR2 = SpaceChargeR2 ;
   ManualSpaceChargeR2(0.01,SpaceChargeEWRatio); // Set "medium to large" value of the spacecharge parameter for tests, not critical.
                                                 // but keep EWRatio that was previously defined
   Float_t x[3] = { 0, 0, 0 } ;
   BField(x,B) ;
   ChargeB  = Charge * TMath::Sign((int)1,(int)(B[2]*1000)) ;
   R0 = TMath::Abs( 1000.0 * Pt / ( 0.299792 * B[2] ) ) ;     // P in GeV, R in cm, B in kGauss
   X0 = ChargeB *  0.707107 * R0  ;   // Assume a test particle that shoots out at 45 degrees
   Y0 = ChargeB * -0.707107 * R0  ;
   Pz = Pt * TMath::SinH(PseudoRapidity) ;
 
   for ( Int_t i = 0 ; i < ROWS ; i++ )       // Note that i starts at zero
     {
      if ( i < INNER8 ) 
	R[i] = 60.0 + i*4.8 ;                 // Not correct because TPC rows aren't circles ... but we dont' care
      else if ( i < INNER ) 
	R[i] = 93.6 + (i-INNER8+1)*5.2 ;
      else              
	R[i] = 127.195 + (i-INNER)*2.0 ;
     }

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
       Ztrack[i]  =   VertexZ + ChargeB*DeltaTheta*R0*Pz / Pt ;
       xx[0] = Xtrack[i] ; xx[1] = Ytrack[i] ; xx[2] = Ztrack[i] ;

       if (mDistortionMode & kSpaceChargeR2) {    // Daisy Chain all possible distortions and sort on flags
	 UndoSpaceChargeR2Distortion ( xx, xxprime ) ;
	 InnerOuterRatio = 1.3 ;  // JT test.  Without the GridLeak, GVB prefers 1.3  (real world == 0.5)  
	 for ( unsigned int j = 0 ; j < 3; ++j ) 
	   {
	     xx[j] = xxprime[j];
	   }
       }
       if (mDistortionMode & kGridLeak) { 
	 UndoGridLeakDistortion ( xx, xxprime ) ;
	 InnerOuterRatio = 0.6 ; // JT test.  With the GridLeak, GVB prefers 0.6  (note that order is important in this loop).
	 for ( unsigned int j = 0 ; j < 3 ; ++j ) 
	   {
	     xx[j] = xxprime[j];
	 }
       }
       if (mDistortionMode & k3DGridLeak) { 
	 Undo3DGridLeakDistortion ( xx, xxprime ) ;
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
       Xprime[Index] = Xtrack[i] ; Yprime[Index] = Ytrack[i] ; Zprime[Index] = Ztrack[i] ;
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
   
   return(1) ; // Success 

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
  0xFFFFFF00, 0x1FFFFF represent all 45 rows of the TPC and no SVT or SSD hits.

  You must provide the hit errors by instantiating with two vectors that include the track errors in both X and Y.  
  RowMaskErrorR[]    is the array of hit errors in the radial direction (ie in the direction of a high pt track)
  RowMaskErrorRPhi[] is the array of hit errors in the r-phi direction  (ie tranvsverse to the direction of a high pt track)
  
  pSpace contains the estimate of the space charge in the TPC that caused the distortion on the original track.

  The function returns one if it succeeds and pSpace will be finite.
  The function returns zero if it fails and pSpace will be zero.  
  The function returns zero if there are too few hits in the TPC inner and outer sectors.  
  There are also cuts on Pt and rapdity, etc, that can cause the funtion to return zero.

*/
Int_t StMagUtilities::PredictSpaceChargeDistortion (Int_t Charge, Float_t Pt, Float_t VertexZ, Float_t PseudoRapidity, 
						    Float_t DCA,  const unsigned int RowMask1, const unsigned int RowMask2, 
						    Float_t RowMaskErrorR[64], Float_t RowMaskErrorRPhi[64], Float_t &pSpace )
{

   const Int_t   INNERDETECTORS   =   6  ;       // Number of inner detector rows in represented in the bit masks
   const Int_t   SSDLAYERS        =   1  ;       // Number of SSD layers
   const Int_t   INNER8           =   8  ;       // Number of TPC rows in the inner sector with 4.8 cm spacing
   const Int_t   INNER            =  13  ;       // Number of TPC rows in the inner sector
   const Int_t   TPCROWS          =  45  ;       // Total number of TPC rows per sector (Inner + Outer)
   const Int_t   MinInnerTPCHits  =   5  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   MinOuterTPCHits  =  10  ;       // Minimum number of hits on a track.  If less than this, then no action taken.
   const Int_t   DEBUG            =   0  ;       // Turn on debugging statements and plots

   const Int_t   TPCOFFSET = INNERDETECTORS + SSDLAYERS + 1 ;   // Add one for the vertex in 0th position in RowMasks
   const Int_t   BITS      = INNERDETECTORS + TPCROWS + SSDLAYERS + 1 ;  // Number of bits in the row masks (45 TPC Rows + etc.)

   unsigned int OneBit = 1 ;
   Int_t InnerTPCHits = 0, OuterTPCHits = 0 ;
   for ( Int_t i = 0 ; i < TPCROWS ; i++ ) 
     {
       if ( i < INNER )
	 {
	   if ( ( i < 24  ) && (( RowMask1 & OneBit<<(i+8)  ) != 0 )) InnerTPCHits++ ;  
	 }
       else
	 {
	   if ( ( i < 24  ) && (( RowMask1 & OneBit<<(i+8)  ) != 0 )) OuterTPCHits++ ;  
	   if ( ( i >= 24 ) && (( RowMask2 & OneBit<<(i-24) ) != 0 )) OuterTPCHits++ ;  
	 }
     }

   pSpace  = 0 ;

   if ( (Pt < 0.3) || (Pt > 2.0) )                           return(0) ; // Fail
   if ( (VertexZ < -50) || (VertexZ > 50) )                  return(0) ; // Fail
   if ( (PseudoRapidity < -1.0) || (PseudoRapidity > 1.0) )  return(0) ; // Fail
   if ( (Charge != 1) && (Charge != -1) )                    return(0) ; // Fail
   if ( (DCA < -4.0) || (DCA > 4.0) )                        return(0) ; // Fail
   if ( InnerTPCHits < MinInnerTPCHits )                     return(0) ; // No action if too few hits in the TPC   
   if ( OuterTPCHits < MinOuterTPCHits )                     return(0) ; // No action if too few hits in the TPC   

   Int_t    ChargeB ;
   Float_t  B[3], xx[3], xxprime[3] ;
   Double_t Xtrack[BITS], Ytrack[BITS], Ztrack[BITS] ;
   Double_t R[BITS], X0, Y0, R0, Pz, DeltaTheta ;
   Double_t Xprime[BITS+1], Yprime[BITS+1], Zprime[BITS+1], dX[BITS+1], dY[BITS+1] ;  

   // Temporarily overide settings for space charge data (only)
   StDetectorDbSpaceChargeR2* tempfSpaceChargeR2 = fSpaceChargeR2 ;
   Double_t tempSpaceChargeR2 = SpaceChargeR2 ;
   ManualSpaceChargeR2(0.01,SpaceChargeEWRatio);   // Set "medium to large" value of the spacecharge parameter for tests, not critical.
                                                   // but keep EWRatio that was previously defined 
   Float_t x[3] = { 0, 0, 0 } ;  // Get the B field at the vertex 
   BField(x,B) ;
   ChargeB = Charge * TMath::Sign((int)1,(int)(B[2]*1000)) ;
   R0 = TMath::Abs( 1000.0 * Pt / ( 0.299792 * B[2] ) ) ;     // P in GeV, R in cm, B in kGauss
   X0 = ChargeB *  0.0 * R0  ;   // Assume a test particle that shoots out at 0 degrees
   Y0 = ChargeB * -1.0 * R0  ;
   Pz = Pt * TMath::SinH(PseudoRapidity) ;
   
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
   for ( Int_t i = TPCOFFSET ; i < TPCROWS + TPCOFFSET ; i++ )
     {
       if ( (i-TPCOFFSET) < INNER8 )  
	 R[i] = 60.0 + (i-TPCOFFSET)*4.8 ; // Not completly correct because TPC rows are flat.
       else if ( (i-TPCOFFSET) < INNER )  
	 R[i] = 93.6 + (i-TPCOFFSET-INNER8+1)*5.2 ; // Not completly correct because TPC rows are flat.
       else                          
	 R[i] = 127.195 + (i-TPCOFFSET-INNER)*2.0 ;
     }

   for ( Int_t i = 0 ; i < BITS ; i++ )
     {

       Ytrack[i] = -1 * ChargeB * ( R[i]*R[i]/(2*R0) ) ;
       Xtrack[i] = TMath::Sqrt( R[i]*R[i] - Ytrack[i]*Ytrack[i] ) ;
       DeltaTheta  =  TMath::ATan2(-1*Y0,-1*X0) - TMath::ATan2(Ytrack[i]-Y0,Xtrack[i]-X0) ;
       while ( DeltaTheta < -1*TMath::Pi() ) DeltaTheta += TMath::TwoPi() ;
       while ( DeltaTheta >=   TMath::Pi() ) DeltaTheta -= TMath::TwoPi() ;
       Ztrack[i]  =   VertexZ + ChargeB*DeltaTheta*R0*Pz / Pt ;
       
       if ( R[i] < IFCRadius || R[i] > OFCRadius ) continue ;   // Check if point is inside the TPC.  Do nothing if it is not.

       xx[0] = Xtrack[i] ; xx[1] = Ytrack[i] ; xx[2] = Ztrack[i] ;

       if (mDistortionMode & kSpaceChargeR2) {    // Daisy Chain all possible distortions and sort on flags
	 UndoSpaceChargeR2Distortion ( xx, xxprime ) ;
	 for ( unsigned int j = 0 ; j < 3; ++j ) 
	   {
	     xx[j] = xxprime[j];
	   }
       }
       if (mDistortionMode & kGridLeak) { 
	 UndoGridLeakDistortion ( xx, xxprime ) ;
	 for ( unsigned int j = 0 ; j < 3 ; ++j ) 
	   {
	     xx[j] = xxprime[j];
	 }
       }
       if (mDistortionMode & k3DGridLeak) { 
	 Undo3DGridLeakDistortion ( xx, xxprime ) ;
	 for ( unsigned int j = 0 ; j < 3 ; ++j ) 
	   {
	     xx[j] = xxprime[j];
	 }
       }

       Xtrack[i] = 2*Xtrack[i] - xx[0] ;   // Distort the tracks
       Ytrack[i] = 2*Ytrack[i] - xx[1] ;  
       Ztrack[i] = 2*Ztrack[i] - xx[2] ; 
       
     }
   
   Int_t Index = -1 ;                    // Count number of 'hits' in the bit mask
   Int_t TPCIndex = -1 ;                 // Count the number of these hits in the TPC
   for ( Int_t i = 1 ; i < BITS ; i++ )  // Delete rows not in the bit masks.  Note i=1 to skip the vertex.
     {
       if ( ( i <  32 ) && (( RowMask1 & OneBit<<(i)    ) == 0 )) continue ;  // Skip this row if not in bit mask
       if ( ( i >= 32 ) && (( RowMask2 & OneBit<<(i-32) ) == 0 )) continue ;  // Skip this row if not in bit mask
       Index++ ;   if ( i >= TPCOFFSET ) TPCIndex++ ;
       Xprime[Index] = Xtrack[i] ;        Yprime[Index] = Ytrack[i] ;         Zprime[Index] = Ztrack[i] ;
       dX[Index]     = RowMaskErrorR[i] ;     dY[Index] = RowMaskErrorRPhi[i] ;   
       // printf("%9.2f  %9.2f  %9.2f  %9.2f  %9.2f \n", Xprime[Index], Yprime[Index], Zprime[Index], dX[Index], dY[Index] ) ; // JT Test
     }

   TGraphErrors gre(Index+1,Xprime,Yprime,dX,dY) ;  

   // Note that circle fitting has ambiguities.  The "+" solution is for Y points greater than Y0.  "-" for Y < Y0.
   Double_t DCA_new ;
   TF1 newDCAplus ("newDCAplus" , "( [1] + sqrt( [1]**2 - x**2 + 2*x*[0] + [2]*[2] - [2]*(2*sqrt([0]**2+[1]**2))) )" );
   TF1 newDCAminus("newDCAminus", "( [1] - sqrt( [1]**2 - x**2 + 2*x*[0] + [2]*[2] - [2]*(2*sqrt([0]**2+[1]**2))) )" );

   if (ChargeB > 0 ) 
     {
       newDCAplus.SetParameter( 0,  X0 );  
       newDCAplus.SetParameter( 1,  Y0 );  
       newDCAplus.SetParameter( 2, 0.0 );  
       gre.Fit("newDCAplus","Q") ;
       DCA_new = newDCAplus.GetParameter( 2 ) ;  // Negative means that (0,0) is inside the circle
     }
   else
     {
       newDCAminus.SetParameter( 0,  X0 );  
       newDCAminus.SetParameter( 1,  Y0 );  
       newDCAminus.SetParameter( 2, 0.0 );  
       gre.Fit("newDCAminus","Q") ;
       DCA_new = newDCAminus.GetParameter( 2 ) ;  // Negative means that (0,0) is inside the circle
     }
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
   
   return(1) ; // Success 

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


/// Grid Leakage Calculation
/*!
  Calculate the distortions due to charge leaking out of the gap between the inner and outer sectors
  as well as the gap between the IFC and the innersector, as well as outersector and OFC.
  Original work by Gene VanBuren, and J. Thomas 
*/
void StMagUtilities::UndoGridLeakDistortion( const Float_t x[], Float_t Xprime[] )
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
    { Xprime[0] = x[0] ; Xprime[1] = x[1] ; Xprime[2] = x[2] ; return ; }

  static Int_t DoOnce = 0 ;

  if ( DoOnce == 0 )
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

      PoissonRelaxation( ArrayV, Charge, EroverEz, ROWS, COLUMNS, ITERATIONS ) ;

      DoOnce = 1 ;      

    }
  
  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;              // Table uses phi from 0 to 2*Pi
  z      =  TMath::Abs(x[2]) ;                       // Force symmetry in Z 
  if ( z > 0 && z <  0.2 ) z =  0.2 ;                // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;                // Protect against discontinuity at CM
  
  Float_t phi0     =  TMath::Pi() * ((Int_t)(0.499 + phi*6/TMath::Pi())) / 6.0 ;  
  Float_t local_y  =  r * TMath::Cos( phi - phi0 ) ; // Cheat! Cheat! Use local y because charge is a flat sheet.

  Er_integral   =  Interpolate2DTable( ORDER, local_y, z, ROWS, COLUMNS, Rlist, Zedlist, EroverEz ) ;
  Ephi_integral =  0.0 ;                             // E field is symmetric in phi

  // Get Space Charge 
  if (fSpaceChargeR2) GetSpaceChargeR2();            // need to reset it each event 

  // Subtract to Undo the distortions and apply the EWRatio factor to the data on the East end of the TPC
  if ( r > 0.0 ) 
    {
      if ( x[2] < 0.0 )
	{
	  phi =  phi - SpaceChargeR2 * SpaceChargeEWRatio * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
	  r   =  r   - SpaceChargeR2 * SpaceChargeEWRatio * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
	}
      else
	{
	  phi =  phi - SpaceChargeR2 * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
	  r   =  r   - SpaceChargeR2 * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
	}
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}

  
//________________________________________

  
/// 3D GridLeak Distortion Calculation
/*!
  Calculate the 3D distortions due to charge leaking out of the gap between the inner and outer sectors.
  Original work by Gene VanBuren, and J. Thomas 
*/
void StMagUtilities::Undo3DGridLeakDistortion( const Float_t x[], Float_t Xprime[] )
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

  static Int_t DoOnce = 0 ;

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

  Xprime[0] = x[0] ;  
  Xprime[1] = x[1] ; 
  Xprime[2] = x[2] ; 

  if ( MiddlGridLeakStrength == 0 ) return ; 

  if ( DoOnce == 0 )
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
	      for ( Int_t j = 1 ; j < COLUMNS-1 ; j++ )    
		{

		  Float_t top = 0, bottom = 0 ;

		  Float_t local_y_hi  = (Radius+GRIDSIZER/2.0) * TMath::Cos(Philist[k]) ;
		  Float_t local_y_lo  = (Radius-GRIDSIZER/2.0) * TMath::Cos(Philist[k]) ;
		  Float_t charge_y_hi =  GAPRADIUS + GAP13_14/2.0 ;   // Use physical Gap radius and width
		  Float_t charge_y_lo =  GAPRADIUS - GAP13_14/2.0 ;   // Use physical Gap radius and width

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

      Poisson3DRelaxation( ArrayofArrayV, ArrayofCharge, ArrayofEroverEz, ArrayofEPhioverEz, ROWS, COLUMNS, PHISLICES, 
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

      DoOnce = 1 ;      

    }
  
  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  z      =  TMath::Abs(x[2]) ;                      // Assume a solution that is symmetric in Z !!
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM

  Float_t phi_prime, local_y, r_eff, FLIP = 1.0 ;
  phi_prime = phi ;
  if ( SYMMETRY == 1 ) 
    {
      Int_t   N = (int)(phi/(TMath::Pi()/12.0))  ;
      phi_prime = phi - N * TMath::Pi() / 12.0 ;
      if ( TMath::Power(-1,N) < 0 ) phi_prime = TMath::Pi() / 12.0 - phi_prime ; // Note that 
      if ( TMath::Power(-1,N) < 0 ) FLIP = -1 ;     // Note change of sign.  Assume reflection symmetry!!
    }

  r_eff   = r ;                                     // Do not allow calculation to go too near the gap
  local_y = r * TMath::Cos(phi_prime) ;             
  if ( local_y > GAPRADIUS - GAP13_14 && local_y < GAPRADIUS ) r_eff = (GAPRADIUS - GAP13_14) / TMath::Cos(phi_prime) ;
  if ( local_y < GAPRADIUS + GAP13_14 && local_y > GAPRADIUS ) r_eff = (GAPRADIUS + GAP13_14) / TMath::Cos(phi_prime) ;

  Er_integral   = Interpolate3DTable( ORDER, r_eff, z, phi_prime, neR3D, EMap_nZ, PHISLICES, eRadius, eZList, Philist, ArrayoftiltEr )   ;
  Ephi_integral = Interpolate3DTable( ORDER, r_eff, z, phi_prime, neR3D, EMap_nZ, PHISLICES, eRadius, eZList, Philist, ArrayoftiltEphi ) ;
  Ephi_integral *= FLIP ;                           // Note possible change of sign if we have reflection symmetry!!

  if (fSpaceChargeR2) GetSpaceChargeR2();           // Get latest spacecharge values from DB 

  // Subtract to Undo the distortions and apply the EWRatio factor to the data on the East end of the TPC

  if ( r > 0.0 ) 
    {
      if ( x[2] < 0.0 ) 
	{
	  phi =  phi - SpaceChargeR2 * SpaceChargeEWRatio * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
	  r   =  r   - SpaceChargeR2 * SpaceChargeEWRatio * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
	}
      else
	{
	  phi =  phi - SpaceChargeR2 * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
	  r   =  r   - SpaceChargeR2 * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
	}
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}


//________________________________________
