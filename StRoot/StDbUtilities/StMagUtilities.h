/***********************************************************************
 *
 * $Id: StMagUtilities.h,v 1.20 2002/09/18 22:21:35 jhthomas Exp $
 *
 * Author: Jim Thomas   11/1/2000
 *
 ***********************************************************************
 *
 * Description: Utilities for the Magnetic Field
 *
 ***********************************************************************
 *
 * $Log: StMagUtilities.h,v $
 * Revision 1.20  2002/09/18 22:21:35  jhthomas
 * Add new option for 1/R**2 space charge density distribution.  Flag = 0x800
 *
 * Revision 1.19  2002/02/22 17:44:19  jhthomas
 * Get CathodeV and GG from DB. Change Defaults.  Change Instantiation argument
 * order. Update D'Oxygen documentation.  Remove 2000/2001 E field switch.
 *
 * Revision 1.18  2002/02/02 01:01:09  jeromel
 * Jim's modif for FC & SpaceCharge corrections.
 *
 * Revision 1.17  2001/10/25 23:00:24  hardtke
 * Use database to get a few parameters in StMagUtilities (including twist)
 *
 * Revision 1.16  2001/10/05 21:27:35  jeromel
 * Small comment addition for historical purposes.
 *
 * Revision 1.15  2001/10/05 20:18:02  dunlop
 * Tweaked enumeration of distortion selection to respect first three bits for
 * year flag
 *
 * Revision 1.14  2001/10/05 03:44:25  jeromel
 * Modifications by Jamie so we can turn on/off every corrections.
 *
 * Revision 1.13  2001/08/01 18:34:40  jhthomas
 * Add temporary mode flag for year 2 running (different cathode potentials)
 *
 * Revision 1.12  2001/06/14 22:12:11  jhthomas
 * Speedup UndoBDistorion by adding table lookups
 *
 * Revision 1.11  2001/06/13 16:24:43  jhthomas
 * Speed up PadRow13 Corrections
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
#ifndef StMagUtilities_H
#define StMagUtilities_H
 
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream.h>
#include <fstream.h>
#include <iomanip.h>
#include "TSystem.h"
#include "TROOT.h"        // Stop at this point and put further includes in .cxx file

#define  nZ               57            // Standard STAR B field Map. Number of Z points in table
#define  nR               28            // Number of R points in table
#define  nPhi             37            // Number of Phi points in table
#define  neZ              69            // Standard STAR E field Map. Number of Z points in table
#define  neR              33            // Number of R points in table
#define  nePhi            13            // Number of Phi points in table ( add one for 360 == 0 )

enum   EBField  { kUndefined = 0, kConstant = 1, kMapped = 2, kChain = 3 } ;

// Bit counting starts at 1 for the mode switch (...,3,2,1)

enum   DistortSelect 
{ 
  kBMap              = 0x08,     // Bit 4
  kPadrow13          = 0x10,     // Bit 5
  kTwist             = 0x20,     // Bit 6
  kClock             = 0x40,     // Bit 7
  kMembrane          = 0x80,     // Bit 8
  kEndcap            = 0x100,    // Bit 9
  kIFCShift          = 0x200,    // Bit 10
  kSpaceCharge       = 0x400,    // Bit 11
  kSpaceChargeR2     = 0x800     // Bit 12
} ;

// DO NOT change the numbering of these constants. StBFChain depends
// on these values to build an option flag. The option flag used in 
// the chain is 2x larger than shown here in order to allow the first 
// bit to be used as an on/off flag.  It is shifted away before entering 
// StMagUtilities.  So, this can be summarized by saying: 
// Bit counting starts at 0 for the chain option flag (...,3,2,1,0) 

class StTpcDb ;
class TDataSet ;
class StDetectorDbSpaceCharge ;
class StDetectorDbTpcVoltages ;

class StMagUtilities {


 private:
  
  StTpcDb*  thedb ;  
  TDataSet* thedb2 ;
  StDetectorDbSpaceCharge* fSpaceCharge ;
  StDetectorDbSpaceCharge* fSpaceChargeR2 ;  // Check that this is correct after updating DB !!!
  StDetectorDbTpcVoltages* fTpcVolts ;

  virtual void    CommonStart ( Int_t mode, StTpcDb* dbin , TDataSet* dbin2 ) ;
  virtual void    ReadField ( ) ;
  virtual void    Search ( Int_t N, Float_t Xarray[], Float_t x, Int_t &low ) ;
  virtual Float_t Interpolate ( const Float_t Xarray[], const Float_t Yarray[], 
				const Int_t ORDER, const Float_t x ) ;
  virtual void    Interpolate2DBfield ( const Float_t r, const Float_t z, 
					Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    Interpolate3DBfield ( const Float_t r, const Float_t z, const Float_t phi, 
					Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value ) ;
  virtual void    Interpolate2DEdistortion ( const Float_t r, const Float_t z, 
					     const Float_t Er[neZ][neR], Float_t &Er_value ) ;
  virtual void    Interpolate3DEdistortion ( const Float_t r, const Float_t phi, const Float_t z, 
					     const Float_t Er[neZ][nePhi][neR], const Float_t Ephi[neZ][nePhi][neR], 
					     Float_t &Er_value, Float_t &Ephi_value ) ;

  Int_t    mDistortionMode;             // Distortion mode - determines which corrections are run

  Float_t  StarDriftV ;                 // Drift Velocity (cm/microSec) Magnitude
  Float_t  TPC_Z0 ;                     // Z location of STAR TPC Ground Wire Plane (cm) Magnitude
  Float_t  XTWIST ;                     // X Displacement of West end of TPC wrt magnet (mRad)
  Float_t  YTWIST ;                     // Y Displacement of West end of TPC wrt magnet (mRad)
  Double_t CathodeV ;                   // Cathode Potential (volts)
  Double_t GG ;                         // Gating Grid voltage (volts)
  Float_t  EASTCLOCKERROR ;             // Phi rotation of East end of TPC in milli-radians
  Float_t  WESTCLOCKERROR ;             // Phi rotation of West end of TPC in milli-radians
  Float_t  IFCRadius ;                  // Radius of the Inner Field Cage
  Float_t  OFCRadius ;                  // Radius of the Outer Field Cage
  Float_t  StarMagE ;                   // STAR Electric Field (V/cm) Magnitude
  Float_t  IFCShift ;                   // Shift of the IFC towards the West Endcap (cm)
  Float_t  Const_0, Const_1, Const_2 ;  // OmegaTau parameters
  Double_t SpaceCharge, SpaceChargeR2 ; // Space Charge parameters (uniform or 1/R**2 in the TPC - arbitrary units)

  Float_t  Bz[nZ][nR], Br[nZ][nR] ;         
  Float_t  Radius[nR], ZList[nZ] ;         
  Float_t  Bz3D[nPhi][nZ][nR], Br3D[nPhi][nZ][nR], Bphi3D[nPhi][nZ][nR] ;         
  Float_t  R3D[nR], Z3D[nZ], Phi3D[nPhi] ;         
  Float_t  cmEr[neZ][nePhi][neR],    cmEphi[neZ][nePhi][neR] ;
  Float_t  endEr[neZ][nePhi][neR],   endEphi[neZ][nePhi][neR] ;
  Float_t  shiftEr[neZ][neR] ;
  Float_t  spaceEr[neZ][neR] ;
  Float_t  spaceR2Er[neZ][neR] ;
  Float_t  eRadius[neR], ePhiList[nePhi], eZList[neZ]  ;         
  
 public:

  StMagUtilities () ;
  StMagUtilities ( StTpcDb* dbin,  TDataSet* dbin2, Int_t mode = 0 ) ;
  StMagUtilities ( const EBField map, const Float_t factor, Int_t mode = 0 ) ;
  virtual ~StMagUtilities () {}

  virtual void    SetDb( StTpcDb* dbin , TDataSet* dbin2 ) ;
  virtual void    BField ( const Float_t x[], Float_t B[] ) ;
  virtual void    BrBzField( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    B3DField ( const Float_t x[], Float_t B[] ) ;
  virtual void    BrBz3DField ( const Float_t r, const Float_t z, const Float_t phi,
				Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value ) ;
  virtual void    DoDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoBDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    FastUndoBDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoPad13Distortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoTwistDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoClockDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoMembraneDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoEndcapDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoSpaceChargeDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoSpaceChargeR2Distortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoIFCShiftDistortion ( const Float_t x[], Float_t Xprime[] ) ;

  ClassDef(StMagUtilities,1)    // Base class for all STAR MagField

};

#endif








