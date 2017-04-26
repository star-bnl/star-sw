/***********************************************************************
 *
 * $Id: StarMagField.h,v 1.17 2017/04/26 21:11:25 perev Exp $
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
 * Revision 1.17  2017/04/26 21:11:25  perev
 * Add setConstBz
 *
 * Revision 1.16  2014/06/26 21:50:17  fisyak
 * New Tpc Alignment, v632
 *
 * Revision 1.15  2013/03/26 13:38:18  fisyak
 * restore back modififcations as not related to drop in no. of reconstructed tracks
 *
 * Revision 1.13  2013/01/17 15:11:33  fisyak
 * More clear handling ROOT and non ROOT versions
 *
 * Revision 1.11  2013/01/16 00:05:27  fisyak
 * Add TObject
 *
 * Revision 1.10  2013/01/15 23:45:02  fisyak
 * Account ROOT version with TVirtualMagField
 *
 * Revision 1.9  2013/01/15 17:35:23  fisyak
 * Create clean versions of ROOT and non ROOT StarMagField
 *
 * Revision 1.8  2009/12/11 14:19:07  fisyak
 * switch from define to enum
 *
 * Revision 1.7  2009/12/08 15:33:57  fisyak
 * Hold replacement defines via enum till StMagUtilities will be updated
 *
 * Revision 1.6  2009/12/07 23:38:15  fisyak
 * Move size definition from #define  to enumerations
 *
 * Revision 1.5  2009/01/13 03:19:44  perev
 * Mag field nou controlled from starsim. BugFix
 *
 * Revision 1.4  2007/09/21 21:07:08  fisyak
 * Remove ClassDef and ClassImp
 *
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
#if defined (__ROOT__)
#include "TH2.h"
#include "TGeoMatrix.h"
#if ROOT_VERSION_CODE >= 335360 /* ROOT_VERSION(5,30,0) */
#include "TVirtualMagField.h"
class StarMagField : public TVirtualMagField 
#else
#include "TObject.h"
class StarMagField : public TObject
#endif
#else
class StarMagField 
#endif
{
 public:
  enum   EBField  { kUndefined = 0, kConstant = 1, kMapped = 2, kChain = 3 } ;
  enum   ESmFSizes {nZ = 57, nR = 28, nPhi = 37, nZSteel = 16, nRSteel = 115, nPhiSteel = 25};
  static  void    Search ( Int_t N, const Float_t Xarray[], Float_t x, Int_t &low ) ;
  virtual Float_t Interpolate ( const Float_t Xarray[], const Float_t Yarray[], 
				const Int_t ORDER, const Float_t x ) ;
  virtual void    Interpolate2DBfield ( const Float_t r, const Float_t z, 
					Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    Interpolate2ExtDBfield ( const Float_t r, const Float_t z, 
					Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    Interpolate3DBfield ( const Float_t r, const Float_t z, const Float_t phi,
  				Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value ) ;

 private:

  virtual void    ReadField ( ) ;
  static StarMagField *fgInstance;
#if defined (__ROOT__)
  TGeoRotation fStarMagFieldRotation;
  TH2F *fBzdZCorrection; // correction due to endcap calomiter
  TH2F *fBrdZCorrection; // correction due to endcap calomiter
#endif 
 public:
  //added by Lijuan
  
  virtual void    Interpolate3DBSteelfield ( const Float_t r, const Float_t z, const Float_t phi,
					     Float_t &Br_value, Float_t &Bz_value, Float_t &Bphi_value );
  //end added by Lijuan



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
  static bool     mConstBz;

  //added by Lijuan
  Float_t  Br3DSteel[nPhiSteel][nZSteel][nRSteel], Bphi3DSteel[nPhiSteel][nZSteel][nRSteel] ;        
  //end added by Lijuan
 
 public:

  StarMagField ( EBField map     = kMapped, Float_t Factor  =      1, 
		 Bool_t  Lock    =  kFALSE, Float_t Rescale =      1, 
		 Float_t Bdipole =  -42.67, Float_t Rmaxdip =  15.34,
		 Float_t Zmindip =   980.0, Float_t Zmaxdip = 1350.0) ;
  virtual ~StarMagField () { 
    fgInstance = 0; 
#if defined (__ROOT__)
    SafeDelete(fBzdZCorrection);
    SafeDelete(fBrdZCorrection);
#endif
  }
  static StarMagField *Instance();

  static void setConstBz( bool state ){ mConstBz = state; }

  virtual void    BField   ( const Float_t x[], Float_t B[] ) ;
  virtual void    BField   ( const Double_t x[], Double_t B[] ) ;
  virtual void    Field    ( const Float_t x[], Float_t B[] ) {BField(x,B);}
  virtual void    Field    ( const Double_t x[], Double_t B[] ) {BField(x,B);}
  virtual void    BrBzField( const Float_t r, const Float_t z, Float_t &Br_value, Float_t &Bz_value ) ;
  virtual void    B3DField ( const Float_t x[], Float_t B[] ) ;
  virtual void    B3DField ( const Double_t x[], Double_t B[] ) ;
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
  virtual void    Print(Option_t* opt="") const;
#ifdef __ROOT__
  void  SetStarMagFieldRotation(TGeoRotation &rot);
  void  SetStarMagFieldRotation(Double_t *rot);
  const TGeoRotation &StarMagFieldRotation() {return *&fStarMagFieldRotation;}
  ClassDef(StarMagField,1)    // Base class for all STAR MagField
#endif
};

#endif
