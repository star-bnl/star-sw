/***********************************************************************
 *
 * $Id: StMagUtilities.h,v 1.16 2001/10/05 21:27:35 jeromel Exp $
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
#include "TROOT.h"
#include "TFile.h"

enum   EBField  { kUndefined=0, kConstant=1, kMapped=2, kChain=3 } ;
// DO NOT change the numbering of those constants. StBFChain depends
// on those values to build an option mask. In StBFChain, the options
// are set as those values x2 since the mask is right shifted first.
enum   DistortSelect { kElectricField2001 = 1, 
		       kBMap = 0x08,
		       kPadrow13 = 0x10,
		       kTwist = 0x20,
		       kClock = 0x40,
		       kMembrane = 0x80,
		       kEndcap = 0x100 
};


		       
class StMagUtilities {


 private:
    Int_t mDistortionMode;

  virtual void    Init ( Int_t mode ) ;
  virtual void    ReadField ( ) ;
  virtual void    Search ( Int_t N, Float_t Xarray[], Float_t x, Int_t &low ) ;
  virtual Float_t Interpolate ( const Float_t Xarray[], const Float_t Yarray[], 
				const Int_t ORDER, const Float_t x ) ;
  virtual void    InterpolateBfield  ( const Float_t r, const Float_t z, 
				       Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    Interpolate3Dfield ( const Float_t r, const Float_t z, const Float_t phi, 
				       Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value ) ;
  virtual void    InterpolateEdistortion ( const Float_t r, const Float_t phi, const Float_t z, 
                                           Float_t &Er_value, Float_t &Ephi_value ) ;
  virtual void    InterpolateEEdistortion ( const Float_t r, const Float_t phi, const Float_t z, 
					    Float_t &Er_value, Float_t &Ephi_value ) ;
    
 public:

  StMagUtilities () ;
  StMagUtilities ( Int_t mode ) ;
  StMagUtilities ( const EBField map, const Float_t factor=1.0, Int_t mode = 0) ;
  virtual ~StMagUtilities() {}

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
  virtual void    UndoMembraneDistortion ( const Float_t x[3], Float_t Xprime[3] ) ;
  virtual void    UndoEndcapDistortion ( const Float_t x[3], Float_t Xprime[3] ) ;

  ClassDef(StMagUtilities,1)    // Base class for all STAR MagField

};

#endif








