// @(#)root/g3d:$Name:  $:$Id: TEmcTower.h,v 1.1 2006/08/23 21:24:47 fine Exp $
// Author: Valeri Fine   21/12/04

#ifndef ROOT_TEmcTower
#define ROOT_TEmcTower


////////////////////////////////////////////////////////////////////////////
//                                                                        //
// TEmcTower                                                              //
//                                                                        //
// TEmcTower is a phi segment of a tube. It has 5 parameters, the same 3  //
// TUBE plus the phi limits. The segment start at first limit and         //
// includes increasing phi value up to the second limit or that plus      //
// 360 degrees.                                                           //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#ifndef ROOT_TTUBS
#include "TTUBS.h"
#endif
#include "TVolume.h"

class TDataProvider;
class TRotMatrix;
class TVolumePosition;

class TEmcTower : public TTUBS {

  protected:
       Float_t      fRadius;   // The middle radius of teh shapes

    // Data providers
       TDataProvider  *fColorDataProvider; //!
       TDataProvider  *fSizeDataProvider;  //!
       TDataProvider  *fStyleDataProvider; //!

  protected:
      Bool_t  CheckTowerParameters();
    public:
        TEmcTower();
        TEmcTower(const char *name, const char *title, const char *material, Float_t radius,
              Float_t dz, Float_t phi1=0, Float_t phi2=360);

        virtual ~TEmcTower();

        const TDataProvider  *GetColorProvider() const {  return fColorDataProvider;}
        const TDataProvider  *GetSizeProvider () const {  return fSizeDataProvider;}
        const TDataProvider  *GetStyleProvider() const {  return fStyleDataProvider;}
        virtual void    Paint(Option_t *option);
        virtual const TBuffer3D &GetBuffer3D(Int_t reqSections) const;
        virtual void    ResetProviders();
        virtual void    SetColorProvider(TDataProvider *provider){  fColorDataProvider = provider;}
        virtual void    SetSizeProvider (TDataProvider *provider){  fSizeDataProvider  = provider;}
        virtual void    SetStyleProvider(TDataProvider *provider){  fStyleDataProvider = provider;}
    
       // ClassDef(TEmcTower,1)  //TUBS shape
};


class TEmcTowers : public  TVolume {
protected:
   Int_t        fSegments; // the number of segments alone of Z-axis
   Int_t        fSectors;  // the number of sectors around of Z-axis  
   TVolume     *fBaseEmcTower;
   // temporary variables
   TRotMatrix      *fMatrix;          //!
   TRotMatrix      *fInitMatrix;      //!
   TRotMatrix      *fInvMatrix;       //!
   TVolumePosition *fInitialPosition; //!
   TVolumePosition *fZStep;           //! 
   TVolumePosition *fAngleStep;       //! 

   TVolumePosition *fFullZStep; //!
   TVolumePosition *fEZStep;           //! 
   TVolumePosition *fFullEZStep;       //! 

   TEmcTower       *fThisTower;       //! 
public:
   TEmcTowers(const Text_t *name, const Text_t *title, Float_t radius,
      Float_t dz, Int_t  nSegments, Int_t  nSectors, Float_t phi1=0, Float_t phi2=360);
   ~TEmcTowers();
   TEmcTower *GetTower() const ;
   const TDataProvider *GetColorProvider() const { return GetTower() ? GetTower()->GetColorProvider() : 0; }
   const TDataProvider *GetSizeProvider () const { return GetTower() ? GetTower()->GetSizeProvider()  : 0; }
   const TDataProvider *GetStyleProvider() const { return GetTower() ? GetTower()->GetStyleProvider() : 0; }
   virtual void    Paint(Option_t *option);
   virtual void    ResetProviders()  { if (GetTower() ) GetTower()->ResetProviders(); }
   virtual void    SetColorProvider(TDataProvider *provider){ if (GetTower() ) GetTower()->SetColorProvider(provider); }
   virtual void    SetSizeProvider (TDataProvider *provider){ if (GetTower() ) GetTower()->SetSizeProvider(provider);  }
   virtual void    SetStyleProvider(TDataProvider *provider){ if (GetTower() ) GetTower()->SetStyleProvider(provider); }
   virtual void    Touchup(const TObject *);
   ClassDef(TEmcTowers,1)    //TEmcTowers Volume
};
inline TEmcTower *TEmcTowers::GetTower() const {return fThisTower;}

#endif
