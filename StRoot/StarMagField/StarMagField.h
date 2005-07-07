/***********************************************************************
 *
 * $Id: StarMagField.h,v 1.1.1.1 2005/07/07 14:13:55 fisyak Exp $
 *
 * Author: Jim Thomas   11/1/2000
 *
 ***********************************************************************
 *
 * Description: the STAR Magnetic Field
 *
 ***********************************************************************
 *
 * $Log: StarMagField.h,v $
 * Revision 1.1.1.1  2005/07/07 14:13:55  fisyak
 * The version of STAR mag. field extracted from StDbUtilities/StMagUtilities to be used in Simulation and Reconstruction instead of agufld
 *
 * Revision 1.2  2005/07/07 14:07:55  fisyak
 * Final version before moving to official repository
 *
 * Revision 1.1  2004/03/12 13:26:24  fisyak
 * Singleton for STAR magnetic field
 *
  ***********************************************************************/
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StarMagField Class                                                 //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#ifndef StarMagField_H
#define StarMagField_H
 
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <Stiostream.h>
#include <Rtypes.h>

#define  nZ               57            // Standard STAR B field Map. Number of Z points in table
#define  nR               28            // Number of R points in table
#define  nPhi             37            // Number of Phi points in table
#define  neZ              69            // Standard STAR E field Map. Number of Z points in table
#define  neR              33            // Number of R points in table
#define  nePhi            13            // Number of Phi points in table ( add one for 360 == 0 )


class StarMagField {
 public:
  enum   EBField  { kUndefined = 0, kConstant = 1, kMapped = 2, kChain = 3 } ;
 private:

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

  static StarMagField *fgInstance;
  static EBField  gMap;
  static Float_t  gFactor;
  static Float_t  gRescale;
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
  static Float_t  BDipole;    //  field value (kG)
  static Float_t  RmaxDip;    //  Inner field volume radius
  static Float_t  ZminDip;    //  StArt of the DX mAgnet in Z
  static Float_t  ZmaxDip;    //  End of the DX mAgnet in Z
  

 public:

  StarMagField ( EBField map = kUndefined, Float_t factor = 1) ;
  static StarMagField *Instance() {return fgInstance;}
  virtual ~StarMagField () { fgInstance = 0; }

  virtual void    BField   ( const Float_t x[], Float_t B[] ) ;
  virtual void    BField   ( const Double_t x[], Double_t B[] ) ;
  virtual void    BrBzField( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    B3DField ( const Float_t x[], Float_t B[] ) ;
  virtual void    BrBz3DField ( const Float_t r, const Float_t z, const Float_t phi,
				Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value ) ;
  static  void    SetFactor (Float_t factor) {gFactor  = factor;}
  static  void    SetRescale(Float_t factor) {gRescale = factor;}
  static  void    SetBDipole(Float_t m = -42.67) { BDipole = m;}
  static  void    SetRmaxDip(Float_t m =   15.3) { RmaxDip = m;}
  static  void    SetZminDip(Float_t m =  980.0) { ZminDip = m;}
  static  void    SetZmaxDip(Float_t m = 1350.0) { ZmaxDip = m;}
  static  EBField GetMap()     {return gMap;}
  static  Float_t GetFactor()  {return gFactor;}
  static  Float_t GetRescale() {return gRescale;}
    
  ClassDef(StarMagField,1)    // Base class for all STAR MagField

};

#endif








