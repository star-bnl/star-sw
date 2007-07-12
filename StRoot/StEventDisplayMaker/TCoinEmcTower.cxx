// @(#)root/g3d:$Name:  $:$Id: TCoinEmcTower.cxx,v 1.2 2007/07/12 19:34:21 fisyak Exp $
// Author: Valeri Fine    22/12/04


#include "TCoinEmcTower.h"
#include "TDataProvider.h"
#include "TColor.h"
#include "TROOT.h"
#include "TMath.h"
#ifdef STAR_COIN
#include <Inventor/nodes/SoIndexedFaceSet.h>
#include <Inventor/nodes/SoCoordinate3.h>
#include <Inventor/nodes/SoDrawStyle.h>
#include <Inventor/nodes/SoGroup.h>
#include <Inventor/nodes/SoIndexedFaceSet.h>
#include <Inventor/fields/SoMFVec3f.h> 
#include <Inventor/nodes/SoNormal.h>
#include <Inventor/nodes/SoNormalBinding.h> 
#include <Inventor/nodes/SoMaterial.h>
#include <Inventor/nodes/SoSeparator.h>
#include <Inventor/nodes/SoShapeHints.h>
#endif

#include <qstring.h>

// ClassImp(TCoinEmcTowers)
 
//______________________________________________________________________________
Float_t tAngle(Float_t eta) {
    // Andgle from the pseudorapidity
    static Float_t p2=TMath::PiOver2();
    return  (p2 - 2*TMath::ATan(TMath::Exp(-eta)));
 }
//______________________________________________________________________________
TCoinEmcTowers::TCoinEmcTowers(const Text_t *name, const Text_t *title, Float_t radius
             , Float_t dz
             , Int_t  nSegments 
             , Int_t  nSectors 
             , Float_t theta 
             , Float_t phi1, Float_t phi2)
             : TNamed(name,title)
             , fSegments(nSegments),fSectors(nSectors)
             , fEmcSeparator(0), fThisTowers(0)
             , fColorDataProvider(0)
             , fSizeDataProvider(0)
             , fStyleDataProvider(0)
 {
    if ( fSegments <= 0)  fSegments = 1;
    if ( fSectors  <= 0)  fSectors  = 1;
    // Emc geometry see: http://www.star.bnl.gov/STAR/sno/ice/sn0229.html 
    
    static const Float_t degrad=  TMath::Pi()/180;
    Double_t phi =  (phi2 - phi1)/(2*fSectors);
    Double_t dphirad = -degrad * phi;
    
//    Double_t thetaTowerSize = TMath::ATan(dz/radius)/fSegments; 
    Double_t thetaTowerSize = 0.05;

    Float_t p2=TMath::PiOver2();

    Float_t firstTowerPhi = p2+3*dphirad;

    
    // Double_t dzn   = dz/fSegments;
    
   SetFillColor(kBlue);

   Int_t nSegs;
   Int_t nSectrs; 
   Int_t nPhi;
   Int_t towerId = 0;

   //--------------
   // West portion
   //--------------
 
   phi = firstTowerPhi;
   Float_t ctheta = 0;
   for (nSectrs =0; nSectrs< fSectors; nSectrs++)  
   { 
      for (nPhi=1; nPhi <= 2; nPhi++, phi+=dphirad ) 
      {
         ctheta = 0;
         for (nSegs=0; nSegs < fSegments; nSegs++,ctheta+=thetaTowerSize ) 
         { 
            TCoinEmcTower *t = new TCoinEmcTower(tAngle(ctheta),phi,radius,tAngle(ctheta+thetaTowerSize)-tAngle(ctheta) ,dphirad);
            t->SetTowerId(towerId++);
            AddTower(t);
         }
      }
   }
   //--------------
   // East portion   
   //--------------

   phi = p2-3*dphirad;
   for (nSectrs =0; nSectrs< fSectors; nSectrs++)  
   { 
      for (nPhi=1; nPhi <= 2; nPhi++, phi-=dphirad ) 
      {
//         ctheta = thetaTowerSize;
         ctheta = 0;
         for (nSegs=0; nSegs < fSegments; nSegs++,ctheta+=thetaTowerSize ) 
         { 
            TCoinEmcTower *t  =  new TCoinEmcTower(-tAngle(ctheta),phi,radius,tAngle(ctheta+thetaTowerSize)-tAngle(ctheta),dphirad);
            t->SetTowerId(towerId++);
            AddTower(t);
         }
      }
   }
 }
 //______________________________________________________________________________
 SoSeparator *TCoinEmcTowers::GetBarrel() 
 {
    if (!fEmcSeparator) 
    {
#ifdef STAR_COIN
       fEmcSeparator = new SoSeparator();
       fEmcSeparator->ref();
       fEmcSeparator->setName(GetName());
       
       SoNormalBinding *binding = new SoNormalBinding();
         binding->value = SoNormalBindingElement::PER_FACE_INDEXED;
         
         fEmcSeparator ->addChild(binding);
       
       SoShapeHints * sh = new SoShapeHints;
         sh->vertexOrdering = SoShapeHints::CLOCKWISE;
         sh->shapeType      = SoShapeHints::SOLID;
         sh->faceType       = SoShapeHints::CONVEX;
         
         fEmcSeparator->addChild(sh);
#endif
    }
    return fEmcSeparator;
 }
 //______________________________________________________________________________
 void TCoinEmcTowers::AddTower(TCoinEmcTower *tower)
 {
    if (tower) 
    {
       if (!fThisTowers)   fThisTowers = new TList();
       fThisTowers->Add(tower);
       tower->SetMotherNode(this);
#ifdef STAR_COIN
       GetBarrel()->addChild(tower->GetTower());
#endif
    }
 }

//______________________________________________________________________________
 void TCoinEmcTowers::Touchup(const TObject *shape)
 {
    
     // ROOT Slot to adjust the shape before rendering
    printf(" TCoinEmcTowers::Touchup %s\n", shape->GetName());
 }
//______________________________________________________________________________
TCoinEmcTowers::~TCoinEmcTowers()
{
//*-*-*-*-*-*-*-*-*-*-*-*-*TCoinEmcTowers volume  destructor*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      =============================
    delete fThisTowers;  fThisTowers = 0;
    if (fEmcSeparator) { 
#ifdef STAR_COIN
       fEmcSeparator->unref(); 
#endif
       fEmcSeparator=0; 
    }
}

//______________________________________________________________________________
void TCoinEmcTowers::UpdateShape(const char *)
{
  ResetProviders();
  if (GetSizeProvider()->IsAvailable() )  {
#ifdef STAR_COIN
     TIter next(fThisTowers);
     TObject *t =0;
     while ( (t=next()) ) 
        ((TCoinEmcTower*)t)->UpdateShape();
     fEmcSeparator->touch();
#endif
  }
}

// ClassImp(TCoinEmcTower)

//______________________________________________________________________________
// 
 


// TUBS is a segment of a tube. It has 8 parameters:
//
//     - name       name of the shape
//     - title      shape's title
//     - material  (see TMaterial)
//     - radius     the avreage radiuous of the shape
//     - dz         half length in z
//     - phi        angle +- phi/2
//
//
// NOTE: phi1 should be smaller than phi2. If this is not the case,
//       the system adds 360 degrees to phi2.

 
//______________________________________________________________________________
TCoinEmcTower::TCoinEmcTower(): TObject()
{
//*-*-*-*-*-*-*-*-*-*-*-*TUBS shape default constructor*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                    ==============================
   memset(&fTowerId,0, ((char *)(&fVertices) - (char *)(&fTowerId)));
}


//______________________________________________________________________________
TCoinEmcTower::TCoinEmcTower(Float_t thetapos,  Float_t phipos,Float_t radpos
             ,Float_t thetasize, Float_t phisize, Float_t radsize)

             : fTowerId(-1),fRadiusPosition(radpos)
             , fRadiusSize(radsize)       // The radial size
             , fThetaPosition(thetapos)
             , fThetaSize(thetasize)
             , fPhiPosition(phipos)
             , fPhiSize(phisize)
             , fTowerSeparator(0),fMaterial(0),fVertices(0)
             , fStyle(0), fMotherNode(0), fFaceset(0)
 
{
//*-*-*-*-*-*-*-*-*-*-*-*-*EMC Coin-based shape normal constructor*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      =======================================
   
#ifdef STAR_COIN 
  GetTower()->addChild(SetStyle());
  GetTower()->addChild(SetCurrentColor(kBlue,0));
  GetTower()->addChild(MakeTowerNode());     
#endif
}

//______________________________________________________________________________
 TCoinEmcTower::~TCoinEmcTower()
{
//*-*-*-*-*-*-*-*-*-*-*-*-*TCoinEmcTower shape default destructor*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      =============================
   if (fTowerSeparator) { 
#ifdef STAR_COIN 
      fTowerSeparator->unref(); 
#endif
      fTowerSeparator = 0; 
      fMaterial       = 0;
      fVertices       = 0;
      fStyle          = 0;
   }
}

//______________________________________________________________________________
SoGroup * TCoinEmcTower::GetTower()
{
   if (!fTowerSeparator) 
   {
#ifdef STAR_COIN 
        fTowerSeparator = new SoSeparator();
        fTowerSeparator->ref();
#endif
   }

   return fTowerSeparator;
}

//______________________________________________________________________________
void   TCoinEmcTower::SetTowerId(Int_t id)
{ 
   fTowerId = id; 
   QString towerLabel = QString("EmcTower_%1").arg(fTowerId);
#ifdef STAR_COIN 
   fTowerSeparator->setName((const char*)towerLabel);
   if (fFaceset) fFaceset->setName((const char*)towerLabel);
#endif
}

//______________________________________________________________________________
void TCoinEmcTower::UpdateShape(const char *)
{
   // update the tower size and color if needed
   Bool_t touch = CheckTowerParameters();

   if (touch) {
        SetStyle(true);
   } else {
        SetStyle(false);
   }
#ifdef STAR_COIN 
 //  fTowerSeparator->setName((const char*)towerLabel);
   GetTower()->touch();
#endif
}

//______________________________________________________________________________
Bool_t   TCoinEmcTower::CheckTowerParameters()
{
   Float_t dr = 0;
   TDataProvider *p = (TDataProvider *)fMotherNode->GetSizeProvider();
   if ( p) {
       dr =  p->NextAttribute();
#ifndef CONTROLROOM    
       dr /= 4;
#else
       dr /= 2;
#endif    
   }
    //  large enough to be drawn
    //    dr *=0.5;
    // fRmin  = fRadius-dr;
   ResetSize(dr);
   Int_t colorAttribute = 0;
#ifdef CONTROLROOM    
     Float_t translucent = 0.7;
#else
     Float_t translucent = 0.45;
#endif
   p =(TDataProvider *)fMotherNode->GetColorProvider();
   if (p) {
      colorAttribute = p->NextAttribute();
      if (colorAttribute) {
         SetCurrentColor(colorAttribute,translucent);
      }
   } else {
      SetCurrentColor(int(dr/10)+2,translucent);
   }
#if 0
   if (fStyleDataProvider) {
       SetFillStyle(fStyleDataProvider->NextAttribute());
   } else {
      // SetFillStyle(4025);
   }
#endif
   // printf("  TCoinEmcTower::CheckTowerParameters dr = %f, color = %d  : %d \n", dr,colorAttribute,(dr > 0 &&  colorAttribute)); 
   return (dr > 0 &&  colorAttribute);
}
//______________________________________________________________________________
void  TCoinEmcTowers::ResetProviders()
{
   if (fColorDataProvider)  fColorDataProvider->ResetCounter();
   if (fSizeDataProvider)   fSizeDataProvider ->ResetCounter();  //!
   if (fStyleDataProvider)  fStyleDataProvider->ResetCounter();  //!
}

//______________________________________________________________________________
SoNode *TCoinEmcTower::SetStyle(bool visible)
{
#ifdef STAR_COIN 
   if (!fStyle) {
       fStyle = new SoDrawStyle();
       fStyle->style = visible ? 
              SoDrawStyleElement::FILLED:
              SoDrawStyleElement::INVISIBLE;
   } else {
       fStyle->style.setValue(visible ? 
           SoDrawStyleElement::FILLED:
           SoDrawStyleElement::INVISIBLE);
   }
   // printf("  TCoinEmcTower::CheckTowerParameters dr = %f, color = %d  : %d \n", dr,colorAttribute,(dr > 0 &&  colorAttribute)); 
   return fStyle;
#else
   return 0;
#endif
}
//______________________________________________________________________________
SoNode *TCoinEmcTower::SetCurrentColor(int rootColorIndx, float translucent)
{
   TColor *rgb =  gROOT->GetColor(rootColorIndx);
   SoNode *colorNode = 0;
   if (rgb) {
#ifdef STAR_COIN 
      Float_t rgba[4];
      rgb->GetRGB(rgba[0],rgba[1],rgba[2]);
      rgba[3]=translucent;
      colorNode = SetCurrentColor(rgba);
#endif
   }
   return colorNode;
}
//______________________________________________________________________________
SoNode *TCoinEmcTower::SetCurrentColor(const Float_t *rgba)
{
#ifdef STAR_COIN 
   if ( !fMaterial ) {
      SoMaterial * m = new SoMaterial();
      m->diffuseColor.setValue(rgba[0], rgba[1], rgba[2]);
      m->specularColor.setValue(0.7, 0.7, 0.7); 
      m->transparency = rgba[3]; // fFactor;
      m->shininess = 1.0;
      fMaterial = m;
   } else {
      fMaterial->diffuseColor.setValue(rgba[0], rgba[1], rgba[2]);
      fMaterial->transparency = rgba[3]; // fFactor;
   }
   return fMaterial;
#else
   return 0;
#endif
}


//______________________________________________________________________________
void TCoinEmcTower::ResetSize(float newSize)
{
   if (TMath::Abs(fRadiusSize - newSize) > 0.05*fRadiusSize ) {
      fRadiusSize = newSize;
      float Y1O = (fRadiusPosition + fRadiusSize)*sin(fPhiPosition);
      float X1O = (fRadiusPosition + fRadiusSize)*cos(fPhiPosition);

      float Y2O = (fRadiusPosition + fRadiusSize)*sin(fPhiPosition + fPhiSize);
      float X2O = (fRadiusPosition + fRadiusSize)*cos(fPhiPosition + fPhiSize);

      float Z1O = (fRadiusPosition + fRadiusSize)*sin(fThetaPosition);
      float Z2O = (fRadiusPosition + fRadiusSize)*sin(fThetaPosition + fThetaSize);

#ifdef STAR_COIN 
      // 4 = (X1O, Z1O, Y1O);
      fTowerVertices[4].setValue(X1O,Y1O,Z1O);

      // 5 = (X1O, Z2O, Y1O);
      fTowerVertices[5].setValue(X1O,Y1O,Z2O);

      // 6 = (X2O, Z2O, Y2O);
      fTowerVertices[6].setValue(X2O,Y2O,Z2O);

      // 7 = (X2O, Z1O, Y2O);
      fTowerVertices[7].setValue(X2O,Y2O,Z1O);
//      printf(" ResetSize z=%f size=%f\n x,y,z = %f %f %f  x2,y2,z2 = %f %f %f\n",
//           fRadiusPosition, newSize, X1O,Y1O, Z1O,  X2O,Y2O, Z2O);
#endif
   }
}

//______________________________________________________________________________
SoNode* TCoinEmcTower::MakeTowerNode()
{
     // Create the QuadeStrip face set
#ifdef STAR_COIN 
   SoGroup *shapeGroup = new SoGroup();
   shapeGroup->setName("EmcTowerShape");
   static int gTowerCoordInx[] = 
   { 
        0,1,2,3, -1  // 0 inner radius along
      , 0,4,5,1, -1  // 1 left radius
      , 1,5,6,2, -1  // 2 forward 0Z
      , 3,2,6,7, -1  // 3 -"left radius"
      , 0,3,7,4, -1  // 4 - "forward 0Z"
      , 4,7,6,5, -1  // 5 -"inner radius" = outer radius
   };
   
  // 0 = (X1I, Z1I, Y1I);
  // 1 = (X1I, Z2I, Y1I);
  // 2 = (X2I, Z2I, Y2I);
  // 3 = (X2I, Z1I, Y2I);

  // 4 = (X1O, Z1O, Y1O);
  // 5 = (X1O, Z2O, Y1O);
  // 6 = (X2O, Z2O, Y2O);
  // 7 = (X2O, Z1O, Y2O);

   fFaceset  = new SoIndexedFaceSet();
   fFaceset->setName("EmcTowerVertices");
   fFaceset->coordIndex.setValuesPointer(sizeof(gTowerCoordInx)/sizeof(int),gTowerCoordInx);


// Calculate Z inner:  
    float Z1I = fRadiusPosition*sin(fThetaPosition);
    float Z2I = fRadiusPosition*sin(fThetaPosition +fThetaSize);

// Calculate XY inner
    float Y1I = fRadiusPosition*sin(fPhiPosition);
    float X1I = fRadiusPosition*cos(fPhiPosition);

    float Y2I = fRadiusPosition*sin(fPhiPosition + fPhiSize);
    float X2I = fRadiusPosition*cos(fPhiPosition + fPhiSize);
    
    // 0 = (X1I, Z1I, Y1I);
   fTowerVertices[0].setValue(X1I,Y1I,Z1I);
   
   // 1 = (X1I, Z2I, Y1I);
   fTowerVertices[1].setValue(X1I,Y1I,Z2I);
   
   // 2 = (X2I, Z2I, Y2I);
   fTowerVertices[2].setValue(X2I,Y2I,Z2I);
   
   // 3 = (X2I, Z1I, Y2I);
   fTowerVertices[3].setValue(X2I,Y2I,Z1I);
//   printf(" MakeTowerNodee z=%f \n x,y,z = %f %f %f  x2,y2,z2 = %f %f %f\n",
//           fRadiusPosition, X1I,Y1I, Z1I,  X2I,Y2I, Z2I);

   float newSize = fRadiusSize;
   fRadiusSize =  0;
   ResetSize(newSize);  

   fVertices = new SoCoordinate3();             // tower vertices
   fVertices->point.setValuesPointer(sizeof( fTowerVertices)/sizeof(SbVec3f),fTowerVertices);
   
   SoNormal *normals = new SoNormal();
   SoMFVec3f &vector = normals->vector;

//  ----  Normals --------------
//  0 - inner radius along
// 5 Outer raius = -"inner radius" = outer radius
//      , 4,7,6,5, -1  // 5 -"inner radius" = outer radius
   SbVec3f normal(X1I,Y1I,0);
   normal.normalize();
   vector.set1Value(5,normal);
   normal.negate();
   vector.set1Value(0,normal);
   fFaceset->normalIndex.set1Value(0,0);
   fFaceset->normalIndex.set1Value(5,5);


// 1 left     = (2,6) x (2,3)
//             , 0,4,5,1, -1  // 1 left radius

   normal = (fTowerVertices[6]-fTowerVertices[2]);
   SbVec3f norm = normal.cross(fTowerVertices[3]-fTowerVertices[2]);
   norm.normalize();
   vector.set1Value(1,norm);
   fFaceset->normalIndex.set1Value(1,1);
      

//      , 4,7,6,5, -1  // 5 -"inner radius" = outer radius

// 2 Forward  = (1,5) x (1,2)
//      , 1,5,6,2, -1  // 2 forward 0Z 
   normal = (fTowerVertices[6]-fTowerVertices[2]);
   norm = normal.cross(fTowerVertices[1]-fTowerVertices[2]);
   norm.normalize();
   vector.set1Value(2,norm);
   fFaceset->normalIndex.set1Value(2,2);

// 3 Right    = (1,5) x (1.0) check
//      , 3,2,6,7, -1  // 3 -"left radius"
   normal = (fTowerVertices[6]-fTowerVertices[3]);
   norm = normal.cross(fTowerVertices[2]-fTowerVertices[3]);
   norm.normalize();
   vector.set1Value(3,norm);
   fFaceset->normalIndex.set1Value(3,3);


// 4 Backward = (0,4) x (0,3)
//      , 0,3,7,4, -1  // 4 - "forward 0Z"
   normal = (fTowerVertices[4]-fTowerVertices[0]);
   norm = normal.cross(fTowerVertices[3]-fTowerVertices[0]);
   norm.normalize();
   vector.set1Value(4,normal.cross(fTowerVertices[3]-fTowerVertices[0]));
   fFaceset->normalIndex.set1Value(4,4);
   
   shapeGroup->addChild(normals);
   shapeGroup->addChild(fVertices);
   shapeGroup->addChild(fFaceset);   

  return shapeGroup;
#else
  return 0;
#endif
}
