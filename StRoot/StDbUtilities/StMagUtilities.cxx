/***********************************************************************
 *
 * $Id: StMagUtilities.cxx,v 1.35 2003/06/27 18:41:14 jhthomas Exp $
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
  kSpaceCharge       = 0x400     // Bit 11                            <br>
  kSpaceChargeR2     = 0x800     // Bit 12                            <br>
} ;                                                                   <br>

Note that the option flag used in the chain is 2x larger 
than shown here in order to allow the first bit to be used 
as an on/off flag and then it is shifted away before entering 
StMagUtilities.  This can be summarized by saying:

<br> 

Bit counting starts at 0 for the chain option flag (...,3,2,1,0) <br>

<p>

To do:  <br>
- Finish pulling parameters out of DB rather than from #define. 
- Use Magnet current rather than MagFactor
- Add a routine to distort the track if we are given a Geant Vector full of points == a track
- Add simulated B field map in the regions where the field is not mapped.

*/

#include "TFile.h"
#include "TCanvas.h"
#include "TMatrixD.h"
#include "TGraphErrors.h"
#include "TF1.h"
#include "StMagUtilities.h"
#include "StTpcDb/StTpcDb.h"
#include "tables/St_MagFactor_Table.h"
#include "StDetectorDbMaker/StDetectorDbSpaceCharge.h"
#include "StDetectorDbMaker/StDetectorDbMagnet.h"
#include "StDetectorDbMaker/StDetectorDbTpcVoltages.h"

static EBField  gMap  =  kUndefined ;   // Global flag to indicate static arrays are full
static Float_t  gFactor  = 1.0 ;        // Multiplicative factor (allows scaling and sign reversal)
static Float_t  gRescale = 1.0 ;        // Multiplicative factor (allows re-scaling wrt which map read)

//________________________________________


ClassImp(StMagUtilities)

/// StMagUtilities default constructor

StMagUtilities::StMagUtilities ()
  
{
  cout << "StMagUtilities:: Unfortunately, instantiation with StMagUtilities(<empty>) is obsolete" << endl ;
  cout << "StMagUtilities:: You must specify DataBase pointers or specify the requested Field settings manually" << endl ;
}


/// StMagUtilities constructor using the DataBase

StMagUtilities::StMagUtilities ( StTpcDb* dbin , TDataSet* dbin2, Int_t mode = 0 )

{ 
  gMap = kMapped ;                    // Do once & Select the B field map (mapped field or constant)
  fSpaceCharge = StDetectorDbSpaceCharge::instance() ;  // Initialize the DB for SpaceCharge (EbyE)
  fSpaceChargeR2 = fSpaceCharge ;     // Temporary until DB has a new entry !!!!!
  fTpcVolts    = StDetectorDbTpcVoltages::instance() ;  // Initialize the DB for TpcVoltages
  CommonStart( mode, dbin , dbin2 ) ; // Read the Magnetic and Electric Field Data Files, set constants
}


/// StMagUtilities constructor not using the DataBase

StMagUtilities::StMagUtilities ( const EBField map, const Float_t factor, Int_t mode = 0 )       

{ 
  gFactor = factor ;
  gMap = map ;                        // Do once & select the requested map (mapped or constant)
  fSpaceCharge = 0 ;                  // Do not get SpaceCharge out of the DB - use default in CommonStart
  fSpaceChargeR2 = 0 ;                // Do not get SpaceChargeR2 out of the DB - use default in CommonStart
  fTpcVolts = 0 ;                     // Do not get TpcVoltages out of the DB - use default in CommonStart
  CommonStart( mode, 0 , 0 ) ;        // Read the Magnetic and Electric Field Data Files, set constants
}

//________________________________________


void StMagUtilities::SetDb ( StTpcDb* dbin , TDataSet* dbin2 )
 
{
  thedb = dbin ;
  thedb2 = dbin2 ;
}


//________________________________________

/// Initialization method.  This will also sort the options received by the tpt Maker

void StMagUtilities::CommonStart ( Int_t mode, StTpcDb* dbin, TDataSet* dbin2 )

{

  Float_t  B[3], X[3] = { 0, 0, 0 } ;
  Float_t  OmegaTau ;           // For an electron, OmegaTau carries the sign opposite of B 

  // These items are available in the DB
  StarDriftV  =     5.45 ;      // Drift Velocity (cm/microSec) Magnitude
  TPC_Z0      =    209.3 ;      // Z location of STAR TPC Ground wire Plane (cm)
  XTWIST      =   -0.165 ;      // X Displacement of West end of TPC wrt magnet (mRad)
  YTWIST      =    0.219 ;      // Y Displacement of West end of TPC wrt magnet (mRad)
  SpaceCharge =      0.0 ;      // Space Charge parameter (uniform in the TPC, Coulombs/Epsilon-nought)
  SpaceChargeR2 =    0.0 ;      // Space Charge parameter (space charge from event ~1/R**2, Coulombs/Epsilon-nought)
  IFCShift    =   0.0080 ;      // Shift of the IFC towards the West Endcap (cm) (2/1/2002)
  CathodeV    = -31000.0 ;      // Cathode Voltage (volts)
  GG          =   -127.5 ;      // Gating Grid voltage (volts)
  EASTCLOCKERROR =   0.0 ;      // Phi rotation of East end of TPC in milli-radians
  WESTCLOCKERROR = -0.43 ;      // Phi rotation of West end of TPC in milli-radians
  // These items are not taken from the DB but they should be ... some day.
  IFCRadius   =    47.45 ;      // Radius of the Inner Field Cage
  OFCRadius   =    200.0 ;      // Radius of the Outer Field Cage
  // End of list of items that might come from the DB

  SetDb( dbin, dbin2 ) ;

  if ( dbin != 0 )  // Initialize parameters to database values, if requested and where available
    { 
      StarDriftV  =  1e-6*thedb->DriftVelocity() ;        
      TPC_Z0      =  thedb->PadPlaneGeometry()->outerSectorPadPlaneZ()-thedb->WirePlaneGeometry()
	             ->outerSectorFrischGridPadPlaneSeparation() ;    
      XTWIST      =  1e3*thedb->GlobalPosition()->TpcEFieldRotationY() ; 
      YTWIST      =  -1e3*thedb->GlobalPosition()->TpcEFieldRotationX() ;            
      IFCShift    =  thedb->FieldCage()->InnerFieldCageShift();
      EASTCLOCKERROR = 1e3*thedb->FieldCage()->EastClockError();
      WESTCLOCKERROR = 1e3*thedb->FieldCage()->WestClockError();
      cout << "StMagUtilities::CommonSta  Using TPC parameters from DataBase. " << endl ; 
    }
  else
    {
      cout << "StMagUtilities::CommonSta  WARNING -- Using hard-wired TPC parameters. " << endl ; 
    }

  if ( fTpcVolts != 0 )     // Get TpcVoltages
    {
      CathodeV = fTpcVolts->getCathodeVoltage() * 1000 ; 
      GG       = fTpcVolts->getGGVoltage() ; 
    }
  else
    {
      cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected TpcVolages setting. " << endl ; 
    }

  if ( fSpaceCharge != 0  )  // Get SpaceCharge so it can be printed, below.
    {
      SpaceCharge   = fSpaceCharge->getSpaceChargeCoulombs((double)gFactor) ; 
    }
  else
    {
      cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected SpaceCharge settings. " << endl ; 
    }

  if ( fSpaceChargeR2 != 0 )  // Get SpaceCharge so it can be printed, below.
    {
      SpaceChargeR2 = fSpaceChargeR2->getSpaceChargeCoulombs((double)gFactor) ; 
      SpaceChargeR2 *= 0.525 ; // Temporary until the DB has a new entry to cover this case
    }
  else
    {
      cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected SpaceChargeR2 settings. " << endl ; 
    }

  if ( dbin2 != 0 )  // Initialize the DB for the Magnetic Field and set the scale factor
    {
      St_MagFactor *fMagFactor = (St_MagFactor *) thedb2->Find("MagFactor"); assert(fMagFactor);
      gFactor = (*fMagFactor)[0].ScaleFactor;  // Set the magnetic field scale factor
    }
  else
    {
      cout << "StMagUtilities::CommonSta  WARNING -- Using manually selected BFIELD setting. " << endl ; 
    }

  // Default behavior: no bits set gives you this default
  // To turn on and off individual distortions, set these higher bits

  mDistortionMode = mode;
  
  if ( !( mode & ( kBMap | kPadrow13 | kTwist | kClock | kMembrane | kEndcap | kIFCShift | kSpaceCharge | kSpaceChargeR2 ))) 
    {
      mDistortionMode |= kBMap ;
      mDistortionMode |= kPadrow13 ;
      mDistortionMode |= kTwist ;
      mDistortionMode |= kClock ;
      mDistortionMode |= kIFCShift ;
      printf("StMagUtilities::CommonSta  Default mode selection\n");
    } 
  else 
    {
      printf("StMagUtilities::CommonSta  Using mode option 0x%X\n",mode);
    }
 
  ReadField() ;                             // Read the Magnetic and Electric Field Data Files
  BField(X,B) ;                             // Work in kGauss, cm and assume Bz dominates

  // Theoretically, OmegaTau is defined as shown in the next line.  
  // OmegaTau   =  -10. * B[2] * StarDriftV / StarMagE ;  // cm/microsec, Volts/cm
  // Instead, we will use scaled values from the Aleph collaboration

  StarMagE   =  TMath::Abs(CathodeV/TPC_Z0) ;           // STAR Electric Field (V/cm) Magnitude
  OmegaTau   =  -11.0 * B[2] * StarDriftV / StarMagE ;  // B in kGauss, note the sign of B is important 

  Const_0    =  1. / ( 1. + pow( OmegaTau, 2 ) ) ;
  Const_1    =  OmegaTau / ( 1. + pow( OmegaTau, 2 ) ) ;
  Const_2    =  pow( OmegaTau, 2 ) / ( 1. + pow( OmegaTau, 2 ) ) ;

  cout << "StMagUtilities::DriftVel     =  " << StarDriftV << " cm/microsec" <<  endl ; 
  cout << "StMagUtilities::TPC_Z0       =  " << TPC_Z0 << " cm" << endl ; 
  cout << "StMagUtilities::OmegaTau     =  " << OmegaTau << endl ; 
  cout << "StMagUtilities::XTWIST       =  " << XTWIST << " mrad" << endl ;
  cout << "StMagUtilities::YTWIST       =  " << YTWIST << " mrad" << endl ;
  cout << "StMagUtilities::SpaceCharge  =  " << SpaceCharge << " Coulombs/epsilon-nought" << endl ;
  cout << "StMagUtilities::SpaceChargeR2=  " << SpaceChargeR2 << " Coulombs/epsilon-nought" << endl ;
  cout << "StMagUtilities::IFCShift     =  " << IFCShift << " cm" << endl ;
  cout << "StMagUtilities::CathodeV     =  " << CathodeV << " volts" << endl ;
  cout << "StMagUtilities::GG           =  " << GG << " volts" << endl ;
  cout << "StMagUtilities::EastClock    =  " << EASTCLOCKERROR << " mrad" << endl;
  cout << "StMagUtilities::WestClock    =  " << WESTCLOCKERROR << " mrad" << endl;

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

  if (mDistortionMode & kEndcap) { 
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

/// B field distortions ( no Table ) - calculate the distortions due to the shape of the B field

/*! 
    Distortions are calculated point by point and integrated in real time.
    This avoids the time required to set up a table of distorted values but
    is slow for a very large number of points ( > 10,000 ).
*/

void StMagUtilities::UndoBDistortion( const Float_t x[], Float_t Xprime[] )

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
      B3DField( Xprime, B ) ;                          // Work in kGauss, cm
      if ( TMath::Abs(B[2]) > 0.001 )                  // Protect From Divide by Zero Faults
	{
	  Xprime[0] +=  index*(ah/3)*( Const_2*B[0] - Const_1*B[1] ) / B[2] ;
	  Xprime[1] +=  index*(ah/3)*( Const_2*B[1] + Const_1*B[0] ) / B[2] ;
	}
      if ( index != 4 ) index = 4; else index = 2 ;
    }    

}


/// B field distortions (Table) - calculate the distortions due to the shape of the B field

/*! 
    Distortions are calculated and then stored in a table.  This method requires
    about 1 minute of CPU time to generate the table but it is very fast after the
    table has been created.  Use it when you have a large number of points ( > 10,000 ).
*/

void StMagUtilities::FastUndoBDistortion( const Float_t x[], Float_t Xprime[] )

{

#define NPOINTS 22                                  // Number of points on the Z interpolation grid

  static  Int_t   DoOnce = 0 ;
  static  Float_t xarray[2*NPOINTS-1], yarray[2*NPOINTS-1], zarray[NPOINTS] ;
  static  Float_t dXplus[2*NPOINTS-1][2*NPOINTS-1][NPOINTS], dYplus[2*NPOINTS-1][2*NPOINTS-1][NPOINTS] ;
  static  Float_t dXminus[2*NPOINTS-1][2*NPOINTS-1][NPOINTS], dYminus[2*NPOINTS-1][2*NPOINTS-1][NPOINTS] ;

  const   Int_t ORDER = 2 ;                         // Linear interpolation = 1, Quadratic = 2         
  Int_t   i, j, k ;
  Int_t   ilow, jlow, klow ;
  Float_t xx[3] ;
  Float_t save_dX[ORDER+1], saved_dX[ORDER+1] ;
  Float_t save_dY[ORDER+1], saved_dY[ORDER+1] ;

  if ( DoOnce == 0 )
    {
      cout << "StMagUtilities::FastUndoD  Please wait for the tables to fill ... ~60 seconds" << endl ;
      for ( i = 0 ; i < 2*NPOINTS-1 ; i++ )
	{
	  xarray[i] = -1*TPC_Z0 + i*TPC_Z0/(NPOINTS-1) ;
	  xx[0] = xarray[i] ;
	  for ( j = 0 ; j < 2*NPOINTS-1 ; j++ )
	    {
	      yarray[j] = -1*TPC_Z0 + j*TPC_Z0/(NPOINTS-1) ;
	      xx[1] = yarray[j] ;
	      for ( k = 0 ; k < NPOINTS ; k++ )
		{
		  zarray[k] = k * TPC_Z0/(NPOINTS-1) ;
		  xx[2] = zarray[k] ;
		  if ( k == 0 ) xx[2] = 0.1 ;       // Stay off central membrane by a tiny bit
		  UndoBDistortion(xx,Xprime) ;
		  dXplus[i][j][k] = Xprime[0] ;
		  dYplus[i][j][k] = Xprime[1] ;
		  xx[2] = -1*zarray[k] ;            // Note sign flip for Z < 0
		  if ( k == 0 ) xx[2] = -0.1 ;      // Stay off central membrane by a tiny bit
		  UndoBDistortion(xx,Xprime) ;
		  dXminus[i][j][k] = Xprime[0] ;
		  dYminus[i][j][k] = Xprime[1] ;
		}
	    }
	}
      DoOnce = 1 ;
    }

  Search( 2*NPOINTS-1, xarray, x[0], ilow ) ;
  Search( 2*NPOINTS-1, yarray, x[1], jlow ) ;
  Search( NPOINTS,     zarray, TMath::Abs(x[2]), klow ) ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow < 0 ) klow = 0 ;
  if ( ilow + ORDER  >=  2*NPOINTS - 2 ) ilow =  2*NPOINTS - 2 - ORDER ;
  if ( jlow + ORDER  >=  2*NPOINTS - 2 ) jlow =  2*NPOINTS - 2 - ORDER ;
  if ( klow + ORDER  >=  NPOINTS - 1 )   klow =  NPOINTS - 1 - ORDER ;
  
  for ( i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
      for ( j = jlow ; j < jlow + ORDER + 1 ; j++ )
	{
	  if ( x[2] >= 0 )
	    {
	      save_dX[j-jlow] = Interpolate( &zarray[klow], &dXplus[i][j][klow], ORDER, x[2] )   ;
	      save_dY[j-jlow] = Interpolate( &zarray[klow], &dYplus[i][j][klow], ORDER, x[2] )   ;
	    }
	  else
	    {
	      save_dX[j-jlow] = Interpolate( &zarray[klow], &dXminus[i][j][klow], ORDER, -1*x[2] )   ;
	      save_dY[j-jlow] = Interpolate( &zarray[klow], &dYminus[i][j][klow], ORDER, -1*x[2] )   ;
	    }
	}
      saved_dX[i-ilow] = Interpolate( &yarray[jlow], save_dX, ORDER, x[1] )   ; 
      saved_dY[i-ilow] = Interpolate( &yarray[jlow], save_dY, ORDER, x[1] )   ; 
    }
  Xprime[0] = Interpolate( &xarray[ilow], saved_dX, ORDER, x[0] )   ;
  Xprime[1] = Interpolate( &xarray[ilow], saved_dY, ORDER, x[0] )   ;
  Xprime[2] = x[2] ;
  
}


//________________________________________

/// Twist distortion

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

#define  GAP13_14      1.595            // Width of the gap between the grids at row 13 and row 14 (cm)
#define  GAPRADIUS     121.8            // Radius of gap between rows 13 & 14 at phi = zero degrees (cm)
#define  NYARRAY       33               // Dimension of the vector to contain the YArray
#define  NZDRIFT       15               // Dimension of the vector to contain ZDriftArray

/// Pad row 13 distortion

void StMagUtilities::UndoPad13Distortion( const Float_t x[], Float_t Xprime[] )

{

  const Int_t   TERMS    = 400 ;                 // Number of terms in the sum
  const Float_t SCALE    = 0.192 ;               // Set the scale for the correction
  const Float_t BOX      = 200.0 - GAPRADIUS ;   // Size of the box in which to work
  const Float_t PI       = TMath::Pi() ;

  static Float_t ZDriftArray[NZDRIFT] = {0,1,2,3,4,5,7.5,10,15,20,30,50,75,100,220} ;
  static Float_t YArray[NYARRAY] = { 50.0, 75.0,  100.0,
				     103.5, 104.0, 104.5, 
				     108.7, 109.2, 109.7,
				     113.9, 114.4, 114.9,
				     118.9, 119.6, 119.8, 
				     120.1, 120.5, 121.0, 121.5, 122.1, 122.6, 125.2, 
				     126.2, 127.195, 
				     128.2, 129.195,
				     130.2, 131.195,
				     132.2, 133.195, 137.195, 150., 200. } ;

  static Double_t C[TERMS] ;                     // Coefficients for series
  static Int_t    DoOnce = 0 ;                   // Calculate only once
  static Float_t  SumArray[NZDRIFT][NYARRAY] ;
  static Int_t    ilow, jlow, ORDER ;

  Float_t  y, Zdrift, save_sum[3] ;
  Double_t r, phi, phi0, sum = 0.0 ;

  if ( DoOnce == 0 ) 
    {                          // Put these coefficients in a table to save time
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

  ORDER = 2 ;                                       // Quadratic Interpolation of the table
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

void StMagUtilities::UndoClockDistortion( const Float_t x[], Float_t Xprime[] )

{

  Double_t r, phi ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;

  if ( x[2] < 0 )  phi += EASTCLOCKERROR/1000. ;    // Phi rotation error in milli-radians
  if ( x[2] >= 0 ) phi += WESTCLOCKERROR/1000. ;    // Phi rotation error in milli-radians

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}


//________________________________________

/// Membrane distortion
 
void StMagUtilities::UndoMembraneDistortion( const Float_t x[], Float_t Xprime[] )

{

  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  z      =  x[2] ;
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM
 
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

}


//________________________________________

/// Endcap distortion

void StMagUtilities::UndoEndcapDistortion( const Float_t x[], Float_t Xprime[] )

{

  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  r      =  TMath::Sqrt( x[0]*x[0] + x[1]*x[1] ) ;
  phi    =  TMath::ATan2(x[1],x[0]) ;
  if ( phi < 0 ) phi += 2*TMath::Pi() ;             // Table uses phi from 0 to 2*Pi
  z      =  x[2] ;
  if ( z > 0 && z <  0.2 ) z =  0.2 ;               // Protect against discontinuity at CM
  if ( z < 0 && z > -0.2 ) z = -0.2 ;               // Protect against discontinuity at CM

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

}


//________________________________________

/// IFC distortion

void StMagUtilities::UndoIFCShiftDistortion( const Float_t x[], Float_t Xprime[] )
// Electrostatic equations solved in Rectangular Coodinates by Jim Thomas
// Updated to work in cylindrical coordinates by Jamie Dunlop  11/01/2001
{ 

  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  static Int_t DoOnce = 0 ;

  if ( DoOnce == 0 )
    {
      Int_t Nterms = 100 ;
      for ( Int_t i = 0 ; i < neZ ; ++i ) 
	{
	  z = TMath::Abs( eZList[i] ) ;
	  for ( Int_t j = 0 ; j < neR ; ++j ) 
	    {
	      r = eRadius[j] ;
	      Double_t IntegralOverZ = 0 ;
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
		  IntegralOverZ += Cn * zterm * Numerator / Denominator ;
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

  Interpolate2DEdistortion( r, z, shiftEr, Er_integral ) ;
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

/// Space Charge distortion using a uniform distribution of charge per unit volume

void StMagUtilities::UndoSpaceChargeDistortion( const Float_t x[], Float_t Xprime[] )
// Electrostatic equations solved by Jamie Dunlop  11/01/2001
// Updated to include linear increase of charge from endcap to CM by Jim Thomas 12/18/2001

{ 
  

  Float_t  Er_integral, Ephi_integral ;
  Double_t r, phi, z ;

  static Int_t DoOnce = 0 ;

  if ( DoOnce == 0 )
    {
      Int_t Nterms = 100 ;
      for ( Int_t i = 0 ; i < neZ ; ++i ) 
	{
	  z = TMath::Abs( eZList[i] ) ;
	  for ( Int_t j = 0 ; j < neR ; ++j ) 
	    {
	      r = eRadius[j] ;
	      Double_t IntegralOverZ = 0 ;
	      for ( Int_t n = 1 ; n < Nterms ; ++n ) 
		{
		  Double_t k  = n * TMath::Pi() / TPC_Z0 ;  // Integrated Charge Density
		  Double_t Zterm = pow(-1,(n+1)) * ( 1.0 - TMath::Cos( k * ( TPC_Z0 - z ) ) ) ;
		  //Double_t k  = (2*n-1) * TMath::Pi() / TPC_Z0 ;  // Uniform Charge Density
		  //Double_t Zterm = 1.0 + TMath::Cos( k *  z ) ;   // Uniform Charge Density
		  Double_t Cn = -4.0 / ( k*k*k * TPC_Z0 * StarMagE ) ;
		  Double_t Numerator =
		    TMath::BesselI1( k*r )         * TMath::BesselK0( k*OFCRadius ) -
		    TMath::BesselI1( k*r )         * TMath::BesselK0( k*IFCRadius ) +
		    TMath::BesselK1( k*r )         * TMath::BesselI0( k*OFCRadius ) -
		    TMath::BesselK1( k*r )         * TMath::BesselI0( k*IFCRadius ) ;
		  Double_t Denominator =
		    TMath::BesselK0( k*OFCRadius ) * TMath::BesselI0( k*IFCRadius ) -
		    TMath::BesselK0( k*IFCRadius ) * TMath::BesselI0( k*OFCRadius ) ;
		  IntegralOverZ += Cn * Zterm * Numerator / Denominator ;
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

  Interpolate2DEdistortion( r, z, spaceEr, Er_integral ) ;
  Ephi_integral = 0.0 ;  // E field is symmetric in phi

  // Get Space Charge **** Every Event (JCD This is actually per hit)***
  // Need to reset the instance every hit.  May be slow, but there's no per-event hook.  
  if ( fSpaceCharge !=0 )  // need to reset it. 
    {
      fSpaceCharge =  StDetectorDbSpaceCharge::instance();
      SpaceCharge  =  fSpaceCharge->getSpaceChargeCoulombs((double)gFactor) ;
    }
  
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

/// Space Charge distortion using space charge from a real event (~1/R**2 distribution)

void StMagUtilities::UndoSpaceChargeR2Distortion( const Float_t x[], Float_t Xprime[] )
// Electrostatic equations solved by relaxtion.  Original work by H. H. Wieman, N. Smirnov, and J. Thomas 
// Charge density has 1/R**2 distribution but also includes linear increase of charge from endcap to CM 

{ 
  
  const Int_t     ROWS        =  31 ;
  const Int_t     COLUMNS     =  43 ;
  const Int_t     ITERATIONS  =  1000 ;
  const Double_t  GRIDSIZER   =  (OFCRadius-IFCRadius) / (ROWS-1) ;
  const Double_t  GRIDSIZEZ   =  TPC_Z0 / (COLUMNS-1) ;
  const Double_t  Ratio       =  GRIDSIZER*GRIDSIZER / (GRIDSIZEZ*GRIDSIZEZ) ;
  const Double_t  Four        =  2.0 + 2.0*Ratio ;

  Float_t   Er_integral, Ephi_integral ;
  Double_t  r, phi, z ;

  static Int_t DoOnce = 0 ;

  if ( DoOnce == 0 )
    {
      TMatrixD  ArrayV(ROWS,COLUMNS), Charge(ROWS,COLUMNS) ;
      TMatrixD  ArrayE(ROWS,COLUMNS), EroverEz(ROWS,COLUMNS) ;
      Float_t   Rlist[ROWS], Zedlist[COLUMNS] ;
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
	      // Next line is for 1/R**2 charge deposition in the TPC; then integrated in Z due to drifting iones
	      //Charge(i,j) = zterm / ( TMath::Log(OFCRadius/IFCRadius) * ( Radius*Radius ) ) ; 
	      // Next line is for 1/R**3 charge deposition in the TPC; then integrated in Z due to drifting iones
	      // Charge(i,j) = zterm / ( ( 1/IFCRadius - 1/OFCRadius) * ( Radius*Radius*Radius ) ) ; 
	    } // All cases normalized to have same total charge as the Uniform Charge case == 1.0
	}
      //Solve Poisson's equation in cylindrical coordinates by relaxation technique
      //Allow for different size grid spacing in R and Z directions
      for ( Int_t k = 1 ; k <= ITERATIONS; k++ )
	{
	  for ( Int_t j = 1 ; j < COLUMNS-1 ; j++ )  
	    {
	      for ( Int_t i = 1 ; i < ROWS-1 ; i++ )  
		{
		  Double_t Radius = IFCRadius + i*GRIDSIZER ;
		  ArrayV(i,j) = ( ArrayV(i+1,j) + ArrayV(i-1,j) + Ratio*ArrayV(i,j+1) + Ratio*ArrayV(i,j-1) ) 
		    + ArrayV(i+1,j)*GRIDSIZER/(2*Radius) 
		    - ArrayV(i-1,j)*GRIDSIZER/(2*Radius)
		    + Charge(i,j)*GRIDSIZER*GRIDSIZER  ;
		  ArrayV(i,j) *=  1.0/Four ;
		}
	    }
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
      //Interpolate results onto standard grid for Electric Fields
      Int_t ilow, jlow ;
      Float_t save_Er[2] ;	      
      for ( Int_t i = 0 ; i < neZ ; ++i ) 
	{
	  z = TMath::Abs( eZList[i] ) ;
	  for ( Int_t j = 0 ; j < neR ; ++j ) 
	    { // Linear interpolation
	      r = eRadius[j] ;
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

  Interpolate2DEdistortion( r, z, spaceR2Er, Er_integral ) ;
  Ephi_integral = 0.0 ;  // E field is symmetric in phi

  // Get Space Charge **** Every Event (JCD This is actually per hit)***
  // Need to reset the instance every hit.  May be slow, but there's no per-event hook.
  if ( fSpaceChargeR2 !=0 )   // need to reset it. 
    {
      fSpaceChargeR2 =  StDetectorDbSpaceCharge::instance();
      SpaceChargeR2  =  fSpaceChargeR2->getSpaceChargeCoulombs((double)gFactor) ;
      SpaceChargeR2 *= 0.525 ; // Temporary until the DB has a new entry to cover this case
    }
  
  // Subtract to Undo the distortions
  if ( r > 0.0 ) 
    {
      phi =  phi - SpaceChargeR2 * ( Const_0*Ephi_integral - Const_1*Er_integral ) / r ;      
      r   =  r   - SpaceChargeR2 * ( Const_0*Er_integral   + Const_1*Ephi_integral ) ;  
    }

  Xprime[0] = r * TMath::Cos(phi) ;
  Xprime[1] = r * TMath::Sin(phi) ;
  Xprime[2] = x[2] ;

}

  
//________________________________________

/// Read the electric and magnetic field maps stored on disk

void StMagUtilities::ReadField( )

{

  FILE    *magfile, *efile, *eefile, *b3Dfile ;
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
      
  printf("StMagUtilities::ReadField  Reading Magnetic Field:  %s,  Scale factor = %f \n",comment.Data(),gFactor);
  printf("StMagUtilities::ReadField  Filename is %s, Adjusted Scale factor = %f \n",filename.Data(),gFactor*gRescale);

  
  printf("StMagUtilities::ReadField  Version: ") ;
  
  if ( mDistortionMode & kBMap )          printf ("3D Mag Field Distortions") ;
  if ( mDistortionMode & kPadrow13 )      printf (" + Padrow 13") ;
  if ( mDistortionMode & kTwist )         printf (" + Twist") ;
  if ( mDistortionMode & kClock )         printf (" + Clock") ;
  if ( mDistortionMode & kIFCShift )      printf (" + IFCShift") ;
  if ( mDistortionMode & kSpaceCharge )   printf (" + SpaceCharge") ;
  if ( mDistortionMode & kSpaceChargeR2 ) printf (" + SpaceChargeR2") ;
  if ( mDistortionMode & kMembrane )      printf (" + Central Membrane") ;
  if ( mDistortionMode & kEndcap )        printf (" + Endcap") ;

  printf("\n");
  
  MapLocation = BaseLocation + filename ;
  gSystem->ExpandPathName(MapLocation) ;
  magfile = fopen(MapLocation.Data(),"r") ;

  if (magfile) 

    {
      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , magfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , magfile ) ;
      fgets  ( cname, sizeof(cname) , magfile ) ;
      fgets  ( cname, sizeof(cname) , magfile ) ;
      fgets  ( cname, sizeof(cname) , magfile ) ;

      for ( Int_t j=0 ; j < nZ ; j++ ) 
	{
	  for ( Int_t k=0 ; k < nR ; k++ )
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
  printf("StMagUtilities::ReadField  Reading 3D Magnetic Field file: %s \n",filename3D.Data());

  if (b3Dfile) 

    {
      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , b3Dfile ) ;    // Read comment lines at begining of file
      
      for ( Int_t i=0 ; i < nPhi ; i++ ) 
	{
	  for ( Int_t j=0 ; j < nZ ; j++ ) 
	    {
	      for ( Int_t k=0 ; k < nR ; k++ )
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
      for ( Int_t i=0 ; i < nPhi ; i++ ) 
	{
	  for ( Int_t j=0 ; j < nZ ; j++ ) 
	    {
	      for ( Int_t k=0 ; k < nR ; k++ )
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

  filename = "membrane_efield.dat" ;
  MapLocation = BaseLocation + filename ;
  gSystem->ExpandPathName(MapLocation) ;
  efile = fopen(MapLocation.Data(),"r") ;
  printf("StMagUtilities::ReadField  Reading CM Electric Field Distortion File: %s \n",filename.Data());

  if (efile) 
    {

      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , efile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      fgets  ( cname, sizeof(cname) , efile ) ;
      
      for ( Int_t i=0 ; i < neZ ; i++ ) 
	{
	  for ( Int_t j=0 ; j < nePhi ; j++ )
	    {
	      for ( Int_t k=0 ; k < neR ; k++ )
		{
		  if ( j == nePhi-1 )
		    {
		      ePhiList[j] = 6.2832 ;    // Repeat phi = 0 column in phi == 2 PI column
		      cmEr[i][j][k] = cmEr[i][0][k] ;
		      cmEphi[i][j][k] = cmEphi[i][0][k] ;
		    }
		  else
		    {
		      fgets  ( cname, sizeof(cname) , efile ) ; 
		      sscanf ( cname, " %f %f %f %f %f", &eRadius[k], &ePhiList[j], 
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

  filename = "endcap_efield.dat" ;
  MapLocation = BaseLocation + filename ;
  gSystem->ExpandPathName(MapLocation) ;
  eefile = fopen(MapLocation.Data(),"r") ;
  printf("StMagUtilities::ReadField  Reading Endcap Electric Field Distortion File: %s \n",filename.Data());

  if (eefile) 
    {

      Char_t cname[128] ;
      fgets  ( cname, sizeof(cname) , eefile ) ;    // Read comment lines at begining of file
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      fgets  ( cname, sizeof(cname) , eefile ) ;
      
      for ( Int_t i=0 ; i < neZ ; i++ ) 
	{
	  for ( Int_t j=0 ; j < nePhi ; j++ )
	    {
	      for ( Int_t k=0 ; k < neR ; k++ )
		{
		  if ( j == nePhi-1 )
		    {
		      ePhiList[j] = 6.2832 ;    // Repeat phi = 0 column in phi == 2 PI column
		      endEr[i][j][k] = endEr[i][0][k] ;
		      endEphi[i][j][k] = endEphi[i][0][k] ;
		    }
		  else
		    {
		      fgets  ( cname, sizeof(cname) , eefile ) ; 
		      sscanf ( cname, " %f %f %f %f %f", &eRadius[k], &ePhiList[j], 
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

  return ;

}


//________________________________________

/// Interpolate the B field map - 2D interpolation

void StMagUtilities::Interpolate2DBfield( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value )

{

  Float_t fscale ;

  fscale = 0.001*gFactor*gRescale ;               // Scale STAR maps to work in kGauss, cm

  const   Int_t ORDER = 1  ;                      // Linear interpolation = 1, Quadratic = 2        
  static  Int_t jlow, klow ;                            
  Float_t save_Br[ORDER+1] ;
  Float_t save_Bz[ORDER+1] ;

  Search ( nZ, ZList,  z, jlow ) ;
  Search ( nR, Radius, r, klow ) ;
  if ( jlow < 0 ) jlow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( klow < 0 ) klow = 0 ;
  if ( jlow + ORDER  >=    nZ - 1 ) jlow =   nZ - 1 - ORDER ;
  if ( klow + ORDER  >=    nR - 1 ) klow =   nR - 1 - ORDER ;

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

  fscale = 0.001*gFactor*gRescale ;               // Scale STAR maps to work in kGauss, cm

  const   Int_t ORDER = 1 ;                       // Linear interpolation = 1, Quadratic = 2   
  static  Int_t ilow, jlow, klow ;
  Float_t save_Br[ORDER+1],   saved_Br[ORDER+1] ;
  Float_t save_Bz[ORDER+1],   saved_Bz[ORDER+1] ;
  Float_t save_Bphi[ORDER+1], saved_Bphi[ORDER+1] ;

  Search( nPhi, Phi3D, phi, ilow ) ;
  Search( nZ,   Z3D,   z,   jlow ) ;
  Search( nR,   R3D,   r,   klow ) ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow < 0 ) klow = 0 ;

  if ( ilow + ORDER  >=  nPhi - 1 ) ilow = nPhi - 1 - ORDER ;
  if ( jlow + ORDER  >=    nZ - 1 ) jlow =   nZ - 1 - ORDER ;
  if ( klow + ORDER  >=    nR - 1 ) klow =   nR - 1 - ORDER ;

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
  Br_value   = fscale * Interpolate( &Phi3D[ilow], saved_Br, ORDER, phi )   ;
  Bz_value   = fscale * Interpolate( &Phi3D[ilow], saved_Bz, ORDER, phi )   ;
  Bphi_value = fscale * Interpolate( &Phi3D[ilow], saved_Bphi, ORDER, phi ) ; 

}


//________________________________________

/// Interpolate the E field map - 2D interpolation

void StMagUtilities::Interpolate2DEdistortion( const Float_t r, const Float_t z, 
 					       const Float_t Er[neZ][neR], Float_t &Er_value )

{

  const   Int_t ORDER = 1 ;                      // Linear interpolation = 1, Quadratic = 2         
  static  Int_t jlow, klow ;
  Float_t save_Er[ORDER+1] ;

  Search( neZ,   eZList,   z,   jlow   ) ;
  Search( neR,   eRadius,  r,   klow   ) ;
  if ( jlow < 0 ) jlow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( klow < 0 ) klow = 0 ;
  if ( jlow + ORDER  >=    neZ - 1 ) jlow =   neZ - 1 - ORDER ;
  if ( klow + ORDER  >=    neR - 1 ) klow =   neR - 1 - ORDER ;

  for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
    {
      save_Er[j-jlow]     = Interpolate( &eRadius[klow], &Er[j][klow], ORDER, r )   ;
    }
  Er_value = Interpolate( &eZList[jlow], save_Er, ORDER, z )   ;

}

/// Interpolate the E field map - 3D interpolation

void StMagUtilities::Interpolate3DEdistortion( const Float_t r, const Float_t phi, const Float_t z, 
					     const Float_t Er[neZ][nePhi][neR], const Float_t Ephi[neZ][nePhi][neR], 
                                             Float_t &Er_value, Float_t &Ephi_value )

{

  const   Int_t ORDER = 1 ;                      // Linear interpolation = 1, Quadratic = 2         
  static  Int_t ilow, jlow, klow ;
  Float_t save_Er[ORDER+1],   saved_Er[ORDER+1] ;
  Float_t save_Ephi[ORDER+1], saved_Ephi[ORDER+1] ;

  Search( neZ,   eZList,   z,   ilow   ) ;
  Search( nePhi, ePhiList, phi, jlow   ) ;
  Search( neR,   eRadius,  r,   klow   ) ;
  if ( ilow < 0 ) ilow = 0 ;   // artifact of Root's binsearch, returns -1 if out of range
  if ( jlow < 0 ) jlow = 0 ;
  if ( klow < 0 ) klow = 0 ;

  if ( ilow + ORDER  >=    neZ - 1 ) ilow =   neZ - 1 - ORDER ;
  if ( jlow + ORDER  >=  nePhi - 1 ) jlow = nePhi - 1 - ORDER ;
  if ( klow + ORDER  >=    neR - 1 ) klow =   neR - 1 - ORDER ;

  for ( Int_t i = ilow ; i < ilow + ORDER + 1 ; i++ )
    {
      for ( Int_t j = jlow ; j < jlow + ORDER + 1 ; j++ )
	{
	  save_Er[j-jlow]     = Interpolate( &eRadius[klow], &Er[i][j][klow], ORDER, r )   ;
	  save_Ephi[j-jlow]   = Interpolate( &eRadius[klow], &Ephi[i][j][klow], ORDER, r )   ;
	}
      saved_Er[i-ilow]     = Interpolate( &ePhiList[jlow], save_Er, ORDER, phi )   ; 
      saved_Ephi[i-ilow]   = Interpolate( &ePhiList[jlow], save_Ephi, ORDER, phi )   ; 
    }
  Er_value     = Interpolate( &eZList[ilow], saved_Er, ORDER, z )    ;
  Ephi_value   = Interpolate( &eZList[ilow], saved_Ephi, ORDER, z )  ;
 
}


//________________________________________

/// Interpolate a 3x2 table (quadratic) or a 2x2 table (linear)

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

void StMagUtilities::Search( Int_t N, Float_t Xarray[], Float_t x, Int_t &low )

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

/// Convert from the old (Uniform) space charge correction to the new (1/R**2) space charge correction. 

void StMagUtilities::FixSpaceChargeDistortion ( const Int_t Charge, const Float_t x[3], const Float_t p[3], 
					   const Prime PrimaryOrGlobal, Float_t x_new[3], Float_t p_new[3],
		         const unsigned int RowMask1 = 0xFFFFFF00 , const unsigned int RowMask2 = 0x1FFFFF,
                                                                        const Float_t VertexError = 0.0200 )
// Applicable to 200 GeV Au+Au data that is on the P02ge (and other) microDSTs.
// Given the charge and momentum of a particle and a point on the circular path described by the particle , 
// this function returns the new position of the point (cm) and the new momentum of the particle (GeV).  This 
// is done by undoing the old space charge corrections and then applying the new space charge corrections.
// 
// Input x[3], p[3] and return x_new[3], p_new[3].        x[3] in cm and p[3] in GeV.
//   
// The program works by calculating the hits on the TPC rows for
// the input track, distorts the hits according to the new presciption, and then refits the new hits to find
// the new track parameters.  If the track is a primary track (PrimaryOrGlobal == 0) then x[3] is assumed to
// be the vertex and it is included in the refit.  If the track is a global track (PrimaryOrGlobal == 1) then
// x[3] is assumed to lie somewhere (anywhere) on the track but it is not included in the fit.  For a global
// track, x[3] must lie on the track because it is used to determine where the track flies (ie. angle phi).
//
// PrimaryOrGlobal = 0   for a primary track.
// PrimaryOrGlobal = 1   for a global track.  You can also use the "Prime" enumeration in the .h file.
//
// The code attempts to be as realistic as possible when it does the refit.  Therefore, it asks you for
// the hit masks from the microDSTs.  These masks tell you which TPC rows were used in the original track fit.
// For future reference, the masks come in two words.  The first word covers TPC rows 1-24 and the second 
// word covers rows 25-45.  The first 8 bits of the first word are reserved for the FTPC and therefore
// 0xFFFFFF00, 0x1FFFFF represent all 45 rows of the TPC.
//
// VertexError is quoted in cm (RMS). It is for experts.  If you are working with primary tracks, the vertex
// is included in the fit.  The true error bar is multiplcity dependent.  (sigma**2 increase linearly with mult).
// So you can calculate this, external to the function, and then work with a realistic vertex error bar if
// you wish to do it.  200 microns error is a good average value for central Au-Au events.

{

  x_new[0] = x[0] ; x_new[1] = x[1] ; x_new[2] = x[2] ;          // Default is to do nothing
  p_new[0] = p[0] ; p_new[1] = p[1] ; p_new[2] = p[2] ;

  // Return default values if passed a whacko input value (i.e. infinite or NaN)
  if ( finite((double)Charge)*finite(x[0])*finite(x[1])*finite(x[2])*finite(p[0])*finite(p[1])*finite(p[2]) == 0 ) return ;

  const Int_t   INNER = 13 ;               // Number of TPC rows in the inner sectors 
  const Int_t   ROWS  = 45 ;               // Total number of TPC rows per sector (Inner + Outer)
  const Float_t IFCRadius  =  47.45 ;      // Radius of the Inner Field Cage
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
  Rotation = TMath::Sign( 1.0, (x[0]-X0)*p[1] - (x[1]-Y0)*p[0] ) ; 

  for ( Int_t i = 0 ; i < ROWS ; i++ )
    {
      if ( i < INNER )  R[i] = 60.0 + i*4.96666 ;           // Not correct because TPC rows aren't circles ... but we dont' care
      else              R[i] = 127.195 + (i-INNER)*2.0 ;
    }

  if (Y0 == 0.0)  Direction = TMath::Sign(1.0,p[1]) ;
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









