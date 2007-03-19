// @(#)root/g3d:$Name:  $:$Id: TCoinEmcTower.h,v 1.1 2007/03/19 00:40:36 fine Exp $
// Author: Valeri Fine   21/12/04

#ifndef ROOT_TCoinEmcTower
#define ROOT_TCoinEmcTower


////////////////////////////////////////////////////////////////////////////
//                                                                        //
// TCoinEmcTower                                                              //
//                                                                        //
// TCoinEmcTower is a phi segment of a tube. It has 5 parameters, the same 3  //
// TUBE plus the phi limits. The segment start at first limit and         //
// includes increasing phi value up to the second limit or that plus      //
// 360 degrees.                                                           //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "TDataSet.h"
// #define STAR_COIN
#ifdef STAR_COIN
#include <Inventor/fields/SoMFVec3f.h>
#else
 typedef  float SbVec3f;
#endif

class TDataProvider;
class SoSeparator;
class SoGroup;
class SoMaterial;
class SoCoordinate3;
class TCoinEmcTowers;
class SoDrawStyle;
class SoIndexedFaceSet;
class SoNode;

class TCoinEmcTower : public TObject, public TAttFill {

  protected:
       Int_t          fTowerId;          // TowerId
       Float_t        fRadiusPosition;   // The radial postion of the shapes
       Float_t        fRadiusSize;       // The radial size

       Float_t        fThetaPosition;
       Float_t        fThetaSize;

       Float_t        fPhiPosition;
       Float_t        fPhiSize;

       SoGroup          *fTowerSeparator;
       SoMaterial       *fMaterial;
       SoCoordinate3    *fVertices;
       SoDrawStyle      *fStyle;
       TCoinEmcTowers   *fMotherNode;
       SoIndexedFaceSet *fFaceset;
       
       SbVec3f        fTowerVertices[8];
       
  protected:
      Bool_t  CheckTowerParameters();
      SoNode *SetCurrentColor(int rootColorIndx, float translucent);
      SoNode *SetCurrentColor(const Float_t *rgba);
      SoNode *SetStyle(bool visible=true);
      SoNode *MakeTowerNode();
      void    ResetSize(float newSize);
  public:
        TCoinEmcTower();
        TCoinEmcTower(Float_t thetapos,  Float_t phipos,Float_t radpos=251.0f
             ,Float_t thetasize=60./20, Float_t phisize=180./60, Float_t radsize=30.0f);

        virtual ~TCoinEmcTower();

        SoGroup        *GetTower();
        virtual void    SetMotherNode(TCoinEmcTowers *mother)    { fMotherNode=mother; }
        virtual void    SetTowerId(Int_t id);
        virtual void    UpdateShape(const char *o="");
       // ClassDef(TCoinEmcTower,1)  //TUBS shape
};


class TCoinEmcTowers : public  TNamed, public TAttFill  {
protected:
   Int_t        fSegments; // the number of segments alone of Z-axis
   Int_t        fSectors;  // the number of sectors around of Z-axis  
   Float_t      fTheta;    // The full  EMC angle size. (each tower tTheta/fSegments ) 
   SoSeparator  *fEmcSeparator;
      
   TList       *fThisTowers;       //! 

   // Data providers
   TDataProvider  *fColorDataProvider; //!
   TDataProvider  *fSizeDataProvider;  //!
   TDataProvider  *fStyleDataProvider; //!

public:
 //  "emchits","emchits",251,  292.1,    20,   60
   TCoinEmcTowers(const Text_t *name="emchits", const Text_t *title="emchits"
             , Float_t radius=251.0f
             , Float_t dz=292.1
             , Int_t  nSegments=20, Int_t  nSectors=60, Float_t theta = 3.14/3, Float_t phi1=0, Float_t phi2=360);
   ~TCoinEmcTowers();
   void AddTower(TCoinEmcTower *tower);
   TCoinEmcTower *GetTower() const ;
   SoSeparator *GetBarrel();
   const TDataProvider *GetColorProvider() const { return fColorDataProvider; }
   const TDataProvider *GetSizeProvider () const { return fSizeDataProvider;  }
   const TDataProvider *GetStyleProvider() const { return fSizeDataProvider;  }
   virtual void    ResetProviders();
   virtual void    SetColorProvider(TDataProvider *provider){  fColorDataProvider = provider;}
   virtual void    SetSizeProvider (TDataProvider *provider){  fSizeDataProvider  = provider;}
   virtual void    SetStyleProvider(TDataProvider *provider){  fStyleDataProvider = provider;}
   virtual void    Touchup(const TObject *);
   virtual void UpdateShape(const char *);
  //  ClassDef(TCoinEmcTowers,1)    //TCoinEmcTowers Volume
};

#endif
