/***********************************************************************
 *
 * $Id: StMagUtilities.h,v 1.2 2000/11/03 02:41:58 jhthomas Exp $
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

#define  nZ              57            // Standard STAR Map grid # of Z points in table
#define  nR              28            // Number of R points in table
#define  StarDriftV    5.45            // STAR Drift Velocity (cm/microSec)
#define  StarMagE     148.0            // STAR Electric Field (V/cm) Magnitude
#define  NSTEPS         101            // For Simpson's Integration, must be Odd 
#define  TPC_Z0       209.3            // Z location of STAR TPC pad plane

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
  virtual void    DoDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual Float_t GetFactor() { return fFactor ; }
  virtual void    SetFactor ( Float_t value ) { fFactor = value ; }
  virtual EBField GetMap() { return fMap ; }

  ClassDef(StMagUtilities,1)    // Base class for all STAR MagField

};

#endif



