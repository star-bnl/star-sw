/***********************************************************************
 *
 * $Id: StMagUtilities.h,v 1.6 2001/04/23 17:06:04 didenko Exp $
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
 * Revision 1.6  2001/04/23 17:06:04  didenko
 * restore right version
 *
 * Revision 1.4  2001/02/08 22:26:20  jhthomas
 * Added corrections for CM electrostatic distortions
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

class StMagUtilities {

 private:

  virtual void    ReadField ( ) ;
  virtual Int_t   Search ( Int_t N, Float_t Xarray[], Float_t x ) ;
  virtual Float_t QuadInterp ( const Float_t Xarray[], const Float_t Yarray[], 
			       const Float_t x ) ;
  virtual void    InterpolateBfield ( const Float_t r, const Float_t z, 
				      Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    InterpolateEdistortion ( const Float_t r, const Float_t phi, const Float_t z, 
                                           Float_t &Er_value, Float_t &Ephi_value ) ;

 public:

  StMagUtilities () ;
  StMagUtilities ( const EBField map, const Float_t factor ) ;
  virtual ~StMagUtilities() {}

  virtual void    BField ( const Float_t x[], Float_t B[] ) ;
  virtual void    BrBzField( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    DoDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoBDistortion ( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoPad13Distortion( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoTwistDistortion( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoClockDistortion( const Float_t x[], Float_t Xprime[] ) ;
  virtual void    UndoMembraneDistortion( const Float_t x[3], Float_t Xprime[3] ) ;

  ClassDef(StMagUtilities,1)    // Base class for all STAR MagField

};

#endif





