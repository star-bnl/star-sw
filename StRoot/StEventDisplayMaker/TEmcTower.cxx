// @(#)root/g3d:$Name:  $:$Id: TEmcTower.cxx,v 1.3 2007/02/01 22:39:16 fine Exp $
// Author: Valeri Fine    22/12/04


#include "TEmcTower.h"
#include "TVirtualPad.h"
#include "TVirtualGL.h"
#include "TDataProvider.h"
#include "TRotMatrix.h"
#include "TVolumePosition.h"
#include "TGeometry.h"
#include "TPadView3D.h"
#include "TMath.h"

ClassImp(TEmcTowers)

//______________________________________________________________________________
TEmcTowers::TEmcTowers(const Text_t *name, const Text_t *title, Float_t radius,
              Float_t dz, Int_t  nSegments, Int_t  nSectors, Float_t phi1, Float_t phi2): 
      TVolume(name, title,(TShape *) 0, ""),fSegments(nSegments),fSectors(nSectors),fBaseEmcTower(0)
     ,fThisTower(0){
    if ( fSegments <= 0)  fSegments = 1;
    if ( fSectors  <= 0)  fSectors  = 1;
    // Emc geometry see: http://www.star.bnl.gov/STAR/sno/ice/sn0229.html 
    fMatrix     =  new TRotMatrix();
    fInitMatrix =  new TRotMatrix();
    TRotMatrix *fInvInitMatrix =  new TRotMatrix();
    fInvMatrix  =  new TRotMatrix();
    
    static const Float_t degrad=  TMath::Pi()/180;
    Double_t phi =  (phi2 - phi1)/(2*fSectors);
    Double_t phirad = -degrad * phi;

    Double_t invrotate[9] = {
         TMath::Cos(phirad), -TMath::Sin(phirad), 0
        ,TMath::Sin(phirad),  TMath::Cos(phirad), 0
        ,              0,                0, 1        
    };

    Double_t rotate[9] = {
          TMath::Cos(phirad), TMath::Sin(phirad), 0
        ,-TMath::Sin(phirad), TMath::Cos(phirad), 0
        ,              0,              0,         1        
    };

    Float_t p2=TMath::PiOver2();
    
    Double_t initrotate[9] = {
          TMath::Cos(p2+3*phirad), TMath::Sin(p2+3*phirad),   0
        ,-TMath::Sin(p2+3*phirad), TMath::Cos(p2+3*phirad),  0
        ,              0,                0,                  1        
    };
    
    Double_t invinitrotate[9] = {
          TMath::Cos(p2-3*phirad), TMath::Sin(p2-3*phirad),   0
        ,-TMath::Sin(p2-3*phirad), TMath::Cos(p2-3*phirad),   0
        ,              0,                0,                   1        
    };

    SetVisibility(kThisUnvisible);
   
    fMatrix->SetMatrix(rotate);
    fInitMatrix->SetMatrix(initrotate);
    fInvMatrix ->SetMatrix(invrotate);   
    fInvInitMatrix->SetMatrix(invinitrotate);
    
    Double_t dzn   = dz/fSegments;

    fInitialPosition = new TVolumePosition(0, 0,  0,   0,   fInitMatrix); 
    fZStep           = new TVolumePosition(0, 0,  0,  dzn );
    fEZStep          = new TVolumePosition(0, 0,  0, -dzn );
    TVolumePosition fZhalfStep  (0, 0,  0,  dzn/2 );
    TVolumePosition fEZhalfStep (0, 0,  0, -dzn/2 );
    fEZStep          = new TVolumePosition(0, 0,  0, -dzn );
    fFullZStep       = new TVolumePosition(0, 0,  0, -dz  );
    TVolumePosition fFull2ZStep(0, 0,  0, -2*dz  );
    TVolumePosition fInitStep(0, 0,  0, -(dz-dzn/2)  );
    fFullEZStep      = new TVolumePosition(0, 0,  0,  dz  );
    fAngleStep       = new TVolumePosition(0, 0,  0,   0,    fMatrix);
    TVolumePosition  invAngleStep(0, 0,  0,   0,  fInvMatrix);
    
   // create shapesdz/nSegments
   fThisTower = new TEmcTower(name,title, "", radius, dzn/2, -phi/2, +phi/2);
   fBaseEmcTower = new TVolume("EmcTower","EmcTower",fThisTower);
   fBaseEmcTower->SetLineColor(kBlue);
   fBaseEmcTower->Mark();
//   fEastEmcTower = new TVolume("EastEmcTower","EastEmcTower",fThisTower);
//   fWestEmcTower = new TVolume("WestEmcTower","WestEmcTower",fThisTower);
   Int_t nSegs;
   Int_t nSectrs; 
   Int_t nPhi;
   Int_t towerId = 0;
#if 1   
   //--------------
   // West portion
   //--------------
   TVolumePosition *currentPosition = 0;
   TVolumePosition *nextPosition    = new TVolumePosition(0, 0,  0,   0,   fInitMatrix); //fInitialPosition;
   nextPosition->Mult(fZhalfStep);
   for (nSectrs =0; nSectrs< fSectors; nSectrs++)  
   { 
     for (nPhi=1; nPhi <= 2; nPhi++ ) 
     {
        for (nSegs=0; nSegs < fSegments; nSegs++ ) 
        {
           currentPosition = nextPosition;
           currentPosition->SetNode(fBaseEmcTower);
           Add(fBaseEmcTower,currentPosition);
           nextPosition = new TVolumePosition(currentPosition, fZStep);
           nextPosition->SetId(towerId++);
        }
        TVolumePosition * newPosition = new TVolumePosition(nextPosition, fFullZStep);
        newPosition->Mult(*fAngleStep);
        delete nextPosition; // redundant position        
        nextPosition = newPosition; // redundant position
        nextPosition->SetId(towerId);
   }
   }
   delete nextPosition; // redundant position
   
   //--------------
   // East portion   
   //--------------
   nextPosition    = new TVolumePosition(0, 0,  0,   0,  fInvInitMatrix); // fInitialPosition;
   nextPosition->Mult(fEZhalfStep);
   for (nSectrs =0; nSectrs< fSectors; nSectrs++)  
   { 
     for (nPhi=1; nPhi <= 2; nPhi++ ) 
     {
        for (nSegs=0; nSegs < fSegments; nSegs++ ) 
        {
           currentPosition = nextPosition;
           currentPosition->SetNode(fBaseEmcTower);
           Add(fBaseEmcTower,currentPosition);
           nextPosition = new TVolumePosition(currentPosition, fEZStep);
           nextPosition->SetId(towerId++);
        }
        TVolumePosition * newPosition = new TVolumePosition(nextPosition, fFullEZStep);
        newPosition->Mult(invAngleStep);
        delete nextPosition; // redundant position
        nextPosition = newPosition; // redundant position
        nextPosition->SetId(towerId);
    }
   }
   delete nextPosition; // redundant position
#else
   //--------------
   // West portion
   //--------------
   TVolumePosition *currentPosition = 0;
   TVolumePosition *nextPosition    = new TVolumePosition(0, 0,  0,   0, fInitMatrix  ); //fInitialPosition;
   nextPosition->Mult(fInitStep);
   for (nSectrs =0; nSectrs< 2*fSectors; nSectrs++)  
   { 
        for (nSegs=0; nSegs < 2*fSegments; nSegs++ ) 
        {
           currentPosition = nextPosition;
           currentPosition->SetNode(fBaseEmcTower);
           Add(fBaseEmcTower,currentPosition);
           nextPosition = new TVolumePosition(currentPosition, fZStep);
        }
        TVolumePosition * newPosition = new TVolumePosition(nextPosition, &fFull2ZStep);
        newPosition->Mult(*fAngleStep);
        delete nextPosition; // redundant position
        nextPosition = newPosition; // redundant position
   }
   delete nextPosition; // redundant position   
#endif   
 }
 //______________________________________________________________________________
 void TEmcTowers::Touchup(const TObject *shape)
 {
    
     // ROOT Slot to adjust the shape before rendering
    printf(" TEmcTowers::Touchup %s\n", shape->GetName());
 }
//______________________________________________________________________________
TEmcTowers::~TEmcTowers()
{
//*-*-*-*-*-*-*-*-*-*-*-*-*TEmcTowers volume  destructor*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      =============================
   delete fMatrix;          fMatrix = 0;
   // delete fInitialPosition; fInitialPosition = 0; is deleted by TVolume alone
   delete fZStep;           fZStep      = 0;
   delete fAngleStep;       fAngleStep  = 0;
   delete fEZStep;          fEZStep     = 0;
   delete fFullZStep;       fFullZStep  = 0;
   delete fFullEZStep;      fFullEZStep = 0;
 }


//______________________________________________________________________________
void TEmcTowers::Paint(Option_t *option)
{
   ResetProviders();
   if (GetSizeProvider()->IsAvailable() )   TVolume::Paint(option);
}

// ClassImp(TEmcTower)

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
TEmcTower::TEmcTower(): TTUBS(),fColorDataProvider(0),fSizeDataProvider(0),fStyleDataProvider(0)
{
//*-*-*-*-*-*-*-*-*-*-*-*TUBS shape default constructor*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                    ==============================
}


//______________________________________________________________________________
TEmcTower::TEmcTower(const char *name, const char *title, const char *material, Float_t radius,
              Float_t dz, Float_t phi1, Float_t phi2) : TTUBS()
, fColorDataProvider(0),fSizeDataProvider(0),fStyleDataProvider(0)
{
//*-*-*-*-*-*-*-*-*-*-*-*-*TUBS shape normal constructor*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      =============================
  if (name || title || material) {}


   // TTUBE  data-members

    fRmin   = radius;
    fRmax   = radius;

    fDz   = dz;
    fNdiv = 1;

    fCoTab = 0;
    fSiTab = 0;

    fAspectRatio = 1;

// TTUBS data-members

    fPhi1 = phi1;
    fPhi2 = phi2;

 // Own data-members

    fRadius    = radius;

    MakeTableOfCoSin();
}

//______________________________________________________________________________
 TEmcTower::~TEmcTower()
{
//*-*-*-*-*-*-*-*-*-*-*-*-*TEmcTower shape default destructor*-*-*-*-*-*-*-*-*-*-*-*-*
//*-*                      =============================
}
//______________________________________________________________________________
const TBuffer3D &TEmcTower::GetBuffer3D(Int_t reqSections) const
{
   // Change the shape parameters
   Int_t section= ((TEmcTower *)this)->CheckTowerParameters() ? reqSections : 0 ;
   return TTUBS::GetBuffer3D(section);
}
//______________________________________________________________________________
void TEmcTower::Paint(Option_t *option)
{
   //*-*-*-*-*-*-*-*-*-*-*-*Paint Referenced volume with current parameters*-*-*-*
   //*-*                   ==============================================
   //*-*
   //*-*  vis =  1 (default) shape is drawn
   //*-*  vis =  0 shape is not drawn but its sons may be not drawn
   //*-*  vis = -1 shape is not drawn. Its sons are not drawn
   //*-*  vis = -2 shape is drawn. Its sons are not drawn
   //*-*
   //*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
   Bool_t touch = CheckTowerParameters();
   if (touch) {
      if (option && option[0] && strstr(option,"MakeShape")) {
         // printf(" TEmcTower::Paint %s\n", GetName());
         SetVisibility(1);
      } else {
         TPadView3D *view3D = (TPadView3D*)gPad->GetView3D();
         if (view3D) {
            TAttFill::Modify();
            view3D->SetLineAttr(GetLineColor(),GetLineWidth(),option);
         }
         TTUBS::Paint(option);
      }
   } else {
      SetVisibility(0);
   }
}
//______________________________________________________________________________
Bool_t   TEmcTower::CheckTowerParameters()
{
   Float_t dr = 0;
   if (fSizeDataProvider) {
       dr =  fSizeDataProvider->NextAttribute();
#ifndef CONTROLROOM    
       dr /= 4;
#endif    
   }
    //  large enough to be drawn
    //    dr *=0.5;
    // fRmin  = fRadius-dr;
   fRmax  = fRadius+dr;
   Int_t colorAttribute = 0;
   if (fColorDataProvider) {
      colorAttribute = fColorDataProvider->NextAttribute();
      if (colorAttribute) {
        SetLineColor(colorAttribute);
        SetFillColor(colorAttribute);
      }
   } else {
      SetLineColor(int(dr/10)+2);
      SetFillColor(int(dr/10)+2);
   }
   if (fStyleDataProvider) {
         //      shape->SetLineStyle(GetLineStyle());
         //      shape->SetLineWidth(GetLineWidth());
      SetFillStyle(fStyleDataProvider->NextAttribute());
   } else {
      // SetFillStyle(4025);
#ifdef CONTROLROOM    
      SetFillStyle(4070);
#else
      SetFillStyle(4045);
#endif
   }
   // printf("  TEmcTower::CheckTowerParameters dr = %f, color = %d  : %d \n", dr,colorAttribute,(dr > 0 &&  colorAttribute)); 
   return (dr > 0 &&  colorAttribute);
}
//______________________________________________________________________________
void  TEmcTower::ResetProviders()
{
   printf(" --------- Resetting provider \n");
   if (fColorDataProvider)  fColorDataProvider->ResetCounter();
   if (fSizeDataProvider)   fSizeDataProvider->ResetCounter ();   //!
   if (fStyleDataProvider)  fStyleDataProvider->ResetCounter();  //!
}
