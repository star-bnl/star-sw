/***********************************************************************
 *
 * $Id: StMagUtilities.h,v 1.3 2000/12/15 16:10:45 jhthomas Exp $
 *
 * Author: Jim Thomas   11/1/2000
 *
 ***********************************************************************
 *
 * Description: Utilities for the Magnetid Field
 *
 ***********************************************************************
 *
 * $Log: StMagUtilities.h,v $
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
#include "TROOT.h"
#include "TFile.h"

#define  nZ               57            // Standard STAR Map grid # of Z points in table
#define  nR               28            // Number of R points in table
#define  StarDriftV     5.45            // STAR Drift Velocity (cm/microSec)
#define  StarMagE      148.0            // STAR Electric Field (V/cm) Magnitude
#define  TPC_Z0        208.7            // Z location of STAR TPC Gating Grid
#define  GAP13         1.595            // Width of the gap between the grids at row 13 and row 14 (cm)
#define  GAPRADIUS     121.8            // Radius of row 13 at phi = zero degrees (cm)
#define  GG           -127.5            // Gating Grid voltage (volts)
#define  XTWIST       -0.075           // X Displacement of West end of TPC (cm) [Survey say -0.08]
#define  YTWIST        0.02            // Y Displacement of West end of TPC (cm)
#define  TPCLENGTH     220.0            // Overall Length of half the TPC == Z location of survey points
#define  EASTCLOCKERROR  0.0            // Phi rotation of East end of TPC in milli-radians
#define  WESTCLOCKERROR -0.43           // Phi rotation of West end of TPC in milli-radians

enum EBField   { kUndefined=0, kConstant=1, kMapped=2, kChain=3 } ;

class StMagUtilities {
  

 private:

  Float_t Bz[nZ][nR], Br[nZ][nR] ;         
  Float_t Radius[nR], ZList[nZ]  ;         

  virtual void    ReadField ( ) ;
  virtual Float_t QuadInterp ( const Float_t Xarray[], const Float_t Yarray[], 
			       const Float_t x ) ;
  virtual void    InterpolateBfield ( const Float_t r, const Float_t z, 
				      Float_t &Br_value, Float_t &Bz_value ) ;

 protected:
  
  EBField   fMap ;      // Field Map identifier
  Float_t   fFactor ;   // Multiplicative factor (allows scaling and sign reversal)
  Float_t   fRescale ;  // Multiplicative factor (allows re-scaling according to which map read)

 public:

  StMagUtilities () ;
  StMagUtilities ( const EBField map, const Float_t factor ) ;
  virtual ~StMagUtilities() {}

  virtual void    BField ( const Float_t x[], Float_t B[] ) ;
  virtual void    BrBzField( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    EField ( const Float_t x[], Float_t E[] ) ;
  virtual void    DoDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoBDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoEDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoPad13Distortion( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoTwistDistortion( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoClockDistortion( const Float_t x[], Float_t Xprime[] ) ;
  virtual Float_t GetFactor() { return fFactor ; }
  virtual void    SetFactor ( Float_t value ) { fFactor = value ; }
  virtual Int_t   Search( Int_t N, Float_t Xarray[], Float_t x ) ;
  virtual EBField GetMap() { return fMap ; }

  ClassDef(StMagUtilities,1)    // Base class for all STAR MagField

};

#endif



