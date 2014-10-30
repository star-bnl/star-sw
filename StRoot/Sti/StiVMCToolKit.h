#ifndef StiVMCToolKit_h
#define StiVMCToolKit_h

#include "TString.h"
#include "TGeoManager.h"
#include "TGeoPhysicalNode.h"
#include "TGeoVolume.h"
#include "TGeoShape.h"
#include "TGeoMaterial.h"
#include "TGeoMedium.h"
#include "TGeoMatrix.h"
#include "TGeoCone.h"
//#include "TGeoParaboloid.h"
#include "TGeoPara.h"
#include "TGeoArb8.h"
#include "TGeoPatternFinder.h"
#include "TGeoPcon.h"
#include "TGeoPgon.h"
#include "TGeoPolygon.h"
#include "TGeoSphere.h"   
#include "TGeoTorus.h"
#include "TGeoTrd1.h"
#include "TGeoTrd2.h"
#include "TGeoTube.h"
#include "TGeoXtru.h"
#include "TGeoEltu.h"
class Elem_t;
struct VolumeMap_t {
  const Char_t *name;
  const Char_t *comment;
  const Char_t *path;
  const Char_t *set;
  const Char_t *det;
};

namespace StiVMCToolKit {
  void              PrintShape(TGeoShape *shape);							 
  Int_t             Add2ElementList(Int_t NElem,const TGeoMaterial *mat, Elem_t *ElementList);	 
  Int_t             Merge2ElementList(Int_t NElem,  Elem_t *ElementList, 				 
  				    Int_t NElemD, Elem_t *ElementListD, Double_t weight);		 
  Int_t             NormolizeElementList(Int_t NElem, Elem_t *ElementList);				 
  Double_t          GetWeight(TGeoNode *nodeT = 0, const TString &pathT = "HALL_1/CAVE_1/SVTT_1", 		 
			      Int_t *NElem = 0, Elem_t *ElementList = 0);				 
  Double_t          GetVolumeWeight(TGeoVolume *volT, Int_t *NElem = 0, Elem_t *ElementList = 0);		 
  void              MakeAverageVolume(TGeoVolume *volT, TGeoShape *&newshape, TGeoMedium *&newmed, 
        			      Double_t *xyzM=0);	 
  TGeoManager      *GetVMC();                                                                        
  TGeoPhysicalNode *Alignment(const TGeoNode *nodeT,const Char_t *pathT, TGeoVolume *volT, 
        		      TGeoShape *newshape, TGeoMedium* newmed);
  TGeoPhysicalNode *LoopOverNodes(const TGeoNode *nodeT, const Char_t *pathT, const Char_t *name = 0, void ( *callback)(TGeoPhysicalNode *nodeP)=0);
  void              TestVMC4Reconstruction();
  void              GetVMC4Reconstruction(const Char_t *pathT=0, const Char_t *nameT=0);
  Double_t          GetShapeVolume(TGeoShape *shape);
  void              MakeVolume(TGeoPhysicalNode *nodeP);
  Double_t          GetPotI(const TGeoMaterial *mat);
  Double_t          Nice(Double_t phi);
  void              SetDebug(Int_t m);
  Int_t             Debug();
}
#endif
