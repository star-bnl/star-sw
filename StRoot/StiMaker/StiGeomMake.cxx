// $Id: StiGeomMake.cxx,v 2.1 2006/12/03 17:53:55 fine Exp $
// Author: Valeri Fine, Dec 2006

#include "StiGeomMake.h"

#include "TVolumePosition.h"
#include "TVolume.h"
#include "TRotMatrix.h"
#include "TTUBS.h"
#include "TBRIK.h"
#include "TMath.h"
#include "TGeometry.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiMaterial.h"
//
#if 0
//_____________________________________________________________________________
static Bool_t CompareMatrix(TRotMatrix &a,TRotMatrix &b)
{
  // the code was borrowed from St_geant_Maker 
  double *pa=a.GetMatrix(); double *pb=b.GetMatrix();
  for (int i=0; i<9; i++)  if (pa[i]!=pb[i]) return kFALSE;
  return kTRUE;
}
#endif

//_____________________________________________________________________________
static TRotMatrix *GetMatrix(float angle)
{
   // the code was borrowed from St_geant_Maker 
   TRotMatrix *pattern= 0;
   if (angle != 0) {
      Double_t m[9] = {0};
      m[0] = TMath::Sin(angle);
      m[4] = TMath::Cos(angle);
      
      if (!gGeometry) new TGeometry(); // it assigns the global pointer gGeometry
      
      THashList *list = gGeometry->GetListOfMatrices();
      char mname[20];
      int n=list->GetSize(); sprintf(mname,"matrix%d",n+1);
      pattern=new TRotMatrix(mname,mname,m);
      list->Remove(pattern);
   }
   return pattern;
}

//______________________________________________________________________
TShape *StiGeomMake::MakeShape(const StiShape *shape, const char*material)      
{
   TShape *rootShape=0;
   if (shape) {
     switch (shape->getShapeCode()) {
        case kPlanar:
           rootShape = MakeShape(*(const StiPlanarShape *)shape,material);
           break;
        case kCylindrical:
           rootShape = MakeShape(*(const StiCylindricalShape *)shape,material);
           break;
        default: assert(0);
     }
   }
   return rootShape;
}

//______________________________________________________________________
TShape *StiGeomMake::MakeShape(const StiPlanarShape &shape,const char*material)
{
   return 
      new TBRIK((const char*)shape.getName().c_str()
             , "StiPlanarShape"
             , material
             , shape.getHalfWidth()
             , shape.getThickness()
             , shape.getHalfDepth() );
}

//______________________________________________________________________
TShape *StiGeomMake::MakeShape(const StiCylindricalShape &shape,const char*material) 
{
   return 
         new  TTUBS((const char*)shape.getName().c_str()
                  , "StiCylindricalShape"
                  , material
                  ,  shape.getOuterRadius() - shape.getThickness() // rmin
                  ,  shape.getOuterRadius()                       // rmax
                  ,  shape.getHalfDepth()                         // Dz
                  ,  -shape.getOpeningAngle()/2 
                  ,  +shape.getOpeningAngle()/2 );
             
}


//______________________________________________________________________
TVolume *StiGeomMake::MakeVolume(StiDetectorBuilder &builder)
{
  // builder.reset();
  TVolume *detectorVolume = new TVolume(builder.getName().c_str(),"StiDetectorBuilder",(TShape *)0 );
  while(builder.hasMore()) 
  {
     StiDetector *next = builder.next();  
     TShape     *shape = MakeShape(next->getShape()
                        ,(const char*)next->getMaterial()->getName().c_str() ); 
     StiPlacement *place = next->getPlacement();
     TVolumePosition * postion =
        detectorVolume->Add(
                        new TVolume((const char*)next->getName().c_str(),"StiDetector",shape)
                        , 0
                        , place ? place->getNormalRadius()              : 0
                        , place ? place->getZcenter()                   : 0
                        , place ? GetMatrix(place->getCenterRefAngle()) : 0
                       );
     postion->SetMatrixOwner();
  }
  return detectorVolume;
}
