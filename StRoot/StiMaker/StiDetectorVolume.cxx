// $Id: StiDetectorVolume.cxx,v 2.10 2018/04/10 11:32:10 smirnovd Exp $
// Author: Valeri Fine, Dec 2006

#include "StiDetectorVolume.h"

#include "TVolumePosition.h"
#include "TRotMatrix.h"
#include "TTUBS.h"
#include "TBRIK.h"
#include "TMath.h"
#include "TFile.h"
#include "TGeometry.h"
#include "Sti/StiPlanarShape.h"
#include "Sti/StiCylindricalShape.h"
#include "Sti/StiGenericDetectorGroup.h"
#include "Sti/StiDetector.h"
#include "Sti/StiDetectorGroups.h"
#include "Sti/StiDetectorBuilder.h"
#include "Sti/StiPlacement.h"
#include "Sti/StiMaterial.h"
#include "Sti/StiToolkit.h"

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
inline static TRotMatrix *GetMatrix(float angle)
{
   // Fill the matrix object with the rotation matrix 
   Double_t m[9] = {
       TMath::Cos(angle), -TMath::Sin(angle), 0
      ,TMath::Sin(angle),  TMath::Cos(angle), 0
      ,      0          ,         0         , 1        
   };            
   //fRotMatrix.SetMatrix(m);
   TRotMatrix *mat = new TRotMatrix();
   mat->SetMatrix(m);
   return mat;
}

//_____________________________________________________________________________
// class TVolume decorator for StiDetector's
// 
//_____________________________________________________________________________

// class TVolume decorator for StiDetector's
//_____________________________________________________________________________
StiDetectorVolume::StiDetectorVolume(StiToolkit &tool, const TString &detectorName, unsigned int select)
 :  TVolume("RnD","Sti",(TShape*)0),fDetector(0) 
{ 
   MakeDetector(tool,detectorName, select);
}
 
//_____________________________________________________________________________
StiDetectorVolume::StiDetectorVolume(const StiDetectorBuilder &builder, unsigned int select) :
 TVolume(builder.getName().c_str(),"StiDetectorBuilder",(TShape *)0),fDetector(0) 
{ 
   MakeVolume(builder,select);
}
 
//_____________________________________________________________________________
StiDetectorVolume::StiDetectorVolume(StiDetector *detector) :
 TVolume(),fDetector(detector) { }

//_____________________________________________________________________________
StiDetectorVolume::StiDetectorVolume(StiDetector *detector,const Text_t* name, const Text_t* title, const Text_t* shapename, Option_t* option):
TVolume( name, title, shapename, option),fDetector(detector) { }

//_____________________________________________________________________________
StiDetectorVolume::StiDetectorVolume(StiDetector *detector,const Text_t* name, const Text_t* title, TShape* shape, Option_t* option):
 TVolume(name,title,shape,option ),fDetector(detector) { }
      
//_____________________________________________________________________________
void   StiDetectorVolume::Browse(TBrowser *b) 
{
   TVolume::Browse(b);
}
//_____________________________________________________________________________
char *StiDetectorVolume::GetObjectInfo(Int_t px, Int_t py) const
{
   return TVolume::GetObjectInfo(px, py);
}
//_____________________________________________________________________________
void StiDetectorVolume::MakeDetector(StiToolkit &tool, const TString &detectorName, unsigned int select)
{
   // Construct the TVolume from the StToolKit
   StiDetectorGroups *groups=tool.getDetectorGroups();
   vector<StiGenericDetectorGroup *>::iterator it = groups->begin();
   for (; it != groups->end(); ++it) {
      StiGenericDetectorGroup *group = *it;
      const StiDetectorBuilder &builder = *group->getDetectorBuilder();
      TString builderName = (const char*)builder.getName().c_str();
      if ( detectorName.IsNull() || (builderName.BeginsWith(detectorName,TString::kIgnoreCase)) ) 
                     Add(new  StiDetectorVolume(builder,select));
      else {
         LOG_INFO << "Skip " <<  (const char*)builder.getName().c_str() << " detector" << endm; 
      }
   }
}
//_____________________________________________________________________________
void StiDetectorVolume::MakeVolume(const StiDetectorBuilder &builder, unsigned int select)
{
  // Construct the TVolume from the StDetectorBuilder
  unsigned int nRows = builder.getNRows();
  LOG_INFO << "Builder: " << builder.getName().c_str() << " has " << nRows << " rows" << endm;
  for (unsigned int i=0; i < nRows; i++) {
     unsigned int nSectors = builder.getNSectors(i);
     Int_t iColor  = 3 + i%6;
     for (unsigned int j=0;j<nSectors;j++) 
     {
        StiDetector *next = builder.getDetector(i,j) ;
        if (!next) {
           LOG_ERROR << "The is no detector for row " << i <<" sector " << j << endm;
           continue;
        }
        if (select  &&  (    ( select == kActive   && !next->isActive()) 
                         ||  
                             ( select == kPassivie && next->isActive() )
                        ) ) continue;
        const StiShape *stiShape = next->getShape();
        TShape     *shape = MakeShape(stiShape
                        ,(const char*)next->getMaterial()->getName().c_str() ); 
        StiPlacement *place = next->getPlacement();
        TString canonicName = (const char*)next->getName().c_str();
        canonicName.ReplaceAll("/","_");
        StiDetectorVolume *nextVolume = new StiDetectorVolume(next,(const char*)canonicName,"StiDetector",shape);
        nextVolume->SetFillColor(iColor); nextVolume->SetLineColor(iColor);
        bool planar = (stiShape->getShapeCode() == kPlanar);
        TVolumePosition  *position = 0;
        if (planar) {
           if (place->getNormalRefAngle() != 0) {
              position = new TVolumePosition(0, 0, 0, 0, GetMatrix(place->getNormalRefAngle()));
              position->SetMatrixOwner();
              TVolumePosition  *translate= new TVolumePosition (0
                                       ,place->getNormalYoffset()
                                       ,place->getNormalRadius()
                                       ,place->getZcenter()
                                      );
              position->Mult(*translate );
              delete translate;
           } else {
              position = new TVolumePosition(0
					     ,place->getNormalYoffset()
					     ,place->getNormalRadius()
					     ,place->getZcenter()
					     ,GetMatrix(place->getNormalRefAngle())
					     );
           }
        }  else  {
           position = new TVolumePosition(0, 0, 0, place->getZcenter(), GetMatrix(place->getNormalRefAngle()));
        }
        position->SetNode(nextVolume);
        Add(nextVolume,position);
#if 1
	nextVolume->Print(); 
	TShape *sh = nextVolume->GetShape();
	sh->Print();
	//	cout << "Material: " << sh->GetMaterial()->GetName() << " RadL. = " << sh->GetMaterial()->GetRadLength() << endl;
	if (sh->InheritsFrom("TBRIK")) {
	  TBRIK *brik = (TBRIK *) sh;
	  cout << " dx " << brik->GetDx()
	       << " dy " << brik->GetDy()
	       << " dz " << brik->GetDz() << endl;
	} else {
	  if (sh->InheritsFrom("TTUBE")) {
	    TTUBE *tube = (TTUBE *) sh;
	    cout << " Rmin " << tube->GetRmin()
		 << " Rmax " << tube->GetRmax()
		 << " dz " << tube->GetDz();
	  }
	  if (sh->InheritsFrom("TTUBS")) {
	    TTUBS *tubs = (TTUBS *) sh;
	    cout << " Phi1 " << tubs->GetPhi1()
		 << " Phi2 " << tubs->GetPhi2();
	  }
	  cout << endl;
	}
	position->Print();
#endif
     }
  }
}      

//_____________________________________________________________________________
TShape *StiDetectorVolume::MakeShape(const StiShape *shape, const char*material)      
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

//_____________________________________________________________________________
TShape *StiDetectorVolume::MakeShape(const StiPlanarShape &shape,const char*material)
{
   return 
      new TBRIK((const char*)shape.getName().c_str()
             , "StiPlanarShape"
             , material
             , shape.getHalfWidth()
             , shape.getThickness()/2
             , shape.getHalfDepth() );
}

//_____________________________________________________________________________
TShape *StiDetectorVolume::MakeShape(const StiCylindricalShape &shape,const char*material) 
{
   return (shape.getOpeningAngle() < (TMath::TwoPi()-0.001) ) ? 
         new  TTUBS((const char*)shape.getName().c_str()
                  , "StiCylindricalShape"
                  , material
                  ,  shape.getOuterRadius() - shape.getThickness() // rmin
                  ,  shape.getOuterRadius()                        // rmax
                  ,  shape.getHalfDepth()                          // Dz
                  ,  -shape.getOpeningAngle()*TMath::RadToDeg()/2
                  ,  +shape.getOpeningAngle()*TMath::RadToDeg()/2 )
         :
         new  TTUBE((const char*)shape.getName().c_str()
                  , "StiCylindricalShape"
                  ,  material
                  ,  shape.getOuterRadius() - shape.getThickness() // rmin
                  ,  shape.getOuterRadius()                        // rmax
                  ,  shape.getHalfDepth()                          // Dz
                  );             
}



/*!
 * Save Sti geometry created by this builder in a root file. The Sti volumes are
 * converted into drawable root objects with the help of StiMaker/StiDetectorBuilder.
 * Note: The StiDetectorVolume object is created on the heap in order to avoid
 * disturbance in the current BFC library linking order.
 */
void StiDetectorVolume::SaveGeometry(const std::string fileName) const
{
   TFile fileTmp(fileName.c_str(), "RECREATE");
   this->Write();
   fileTmp.Close();
}
