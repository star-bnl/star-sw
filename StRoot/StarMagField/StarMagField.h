/***********************************************************************
 *
 * $Id: StarMagField.h,v 1.3 2007/09/13 00:00:27 fisyak Exp $
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
 * Revision 1.3  2007/09/13 00:00:27  fisyak
 * add mag.field in steel, from Lijuan Ruan
 *
 * Revision 1.2  2005/07/28 19:46:01  fisyak
 * Add:
 * -  frindge magnetic field from P.Nevski extrapolation,
 * -  lock mechanism for Magnetic Field parameters
 * Remove:
 * -  Electric Field (Still part of StDbUtilitie/StMagUtilities)
 *
 * Comparision between mfldgeo and StarMagField calculation for Z and R components of mag.field
 * is at http://www.star.bnl.gov/~fisyak/star/MagField/
 *
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
#define  nZSteel          16
#define  nRSteel         115
#define  nPhiSteel        25
#if 0
#define  neZ              69            // Standard STAR E field Map. Number of Z points in table
#define  neR              33            // Number of R points in table
#define  nePhi            13            // Number of Phi points in table ( add one for 360 == 0 )
#endif

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
  virtual void    Interpolate2ExtDBfield ( const Float_t r, const Float_t z, 
					Float_t &Br_value, Float_t &Bz_value ) ;
   virtual void    Interpolate3DBfield ( const Float_t r, const Float_t z, const Float_t phi,
  				Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value ) ;

  //added by Lijuan
  
  virtual void    Interpolate3DBSteelfield ( const Float_t r, const Float_t z, const Float_t phi,
					     Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value );
  //end added by Lijuan


#if 0
  virtual void    Interpolate2DEdistortion ( const Float_t r, const Float_t z, 
					     const Float_t Er[neZ][neR], Float_t &Er_value ) ;
  virtual void    Interpolate3DEdistortion ( const Float_t r, const Float_t phi, const Float_t z, 
					     const Float_t Er[neZ][nePhi][neR], const Float_t Ephi[neZ][nePhi][neR], 
					     Float_t &Er_value, Float_t &Ephi_value ) ;
#endif
  static StarMagField *fgInstance;

  EBField  fMap;       // (D) = kMapped; Global flag to indicate static arrays are full
  Float_t  fFactor;    // (D) = 1.0    ; Multiplicative factor (allows scaling and sign reversal)
  Float_t  fRescale;   // (D) = 1.0    ; Multiplicative factor (allows re-scaling wrt which map read)
  Float_t  fBDipole;   // (D) = -42.67 ; field value (kG)
  Float_t  fRmaxDip;   // (D) =  15.34 ; Inner field volume radius
  Float_t  fZminDip;   // (D) =  980.0 ; StArt of the DX mAgnet in Z
  Float_t  fZmaxDip;   // (D) = 1350.0 ; End of the DX mAgnet in Z
  Bool_t   fLock;      // (D) = kFALSE ; Set kTRUE if lock above values 

  Float_t  Bz[nZ][nR], Br[nZ][nR] ;         
  Float_t  Radius[nR], ZList[nZ] ;         
  Float_t  Bz3D[nPhi][nZ][nR], Br3D[nPhi][nZ][nR], Bphi3D[nPhi][nZ][nR] ;         
  Float_t  R3D[nR], Z3D[nZ], Phi3D[nPhi] ;         
  Float_t  R3DSteel[nRSteel], Z3DSteel[nZSteel], Phi3DSteel[nPhiSteel] ;         
  Float_t  Bz3DSteel[nPhiSteel][nZSteel][nRSteel];
  Float_t  Bx3DSteel[nPhiSteel][nZSteel][nRSteel], By3DSteel[nPhiSteel][nZSteel][nRSteel] ;        

  //added by Lijuan
  Float_t  Br3DSteel[nPhiSteel][nZSteel][nRSteel], Bphi3DSteel[nPhiSteel][nZSteel][nRSteel] ;        
  //end added by Lijuan
 
#if 0
  Float_t  cmEr[neZ][nePhi][neR],    cmEphi[neZ][nePhi][neR] ;
  Float_t  endEr[neZ][nePhi][neR],   endEphi[neZ][nePhi][neR] ;
  Float_t  shiftEr[neZ][neR] ;
  Float_t  spaceEr[neZ][neR] ;
  Float_t  spaceR2Er[neZ][neR] ;
  Float_t  eRadius[neR], ePhiList[nePhi], eZList[neZ]  ;         
#endif
 public:

  StarMagField ( EBField map     = kMapped, Float_t Factor  =      1, 
		 Bool_t  Lock    =  kFALSE, Float_t Rescale =      1, 
		 Float_t Bdipole =  -42.67, Float_t Rmaxdip =  15.34,
		 Float_t Zmindip =   980.0, Float_t Zmaxdip = 1350.0) ;
  static StarMagField *Instance() {return fgInstance;}
  virtual ~StarMagField () { fgInstance = 0; }


  virtual void    BField   ( const Float_t x[], Float_t B[] ) ;
  virtual void    BField   ( const Double_t x[], Double_t B[] ) ;
  virtual void    BrBzField( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    B3DField ( const Float_t x[], Float_t B[] ) ;
  virtual void    BrBz3DField ( const Float_t r, const Float_t z, const Float_t phi,
  				Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value ) ;
  virtual void    SetFactor (Float_t factor = 1);
  virtual void    SetRescale(Float_t factor = 1);
  virtual void    SetBDipole(Float_t m = -42.67);
  virtual void    SetRmaxDip(Float_t m =   15.3);
  virtual void    SetZminDip(Float_t m =  980.0);
  virtual void    SetZmaxDip(Float_t m = 1350.0);
  virtual void    SetLock();
  virtual EBField GetMap()     {return fMap;}
  virtual Float_t GetFactor()  {return fFactor;}
  virtual Float_t GetRescale() {return fRescale;}
  virtual Bool_t  IsLocked()   {return fLock;}
  virtual void    Print();
  ClassDef(StarMagField,1)    // Base class for all STAR MagField

};

#endif
