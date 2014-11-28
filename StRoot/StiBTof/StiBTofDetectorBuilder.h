#ifndef StiBTofDetectorBuilder_H
#define StiBTofDetectorBuilder_H
#include "Sti/StiDetectorBuilder.h"
#include "THashList.h"
class TGeoHMatrix;
class StiBTofDetectorBuilder : public StiDetectorBuilder {
 public:
  StiBTofDetectorBuilder(bool active) :
   StiDetectorBuilder("BTof",active) {}
  virtual ~StiBTofDetectorBuilder() {SafeDelete(fRotList);}
  virtual void buildDetectors(StMaker&source);
  void useVMCGeometry();
  static THashList *RotMatrices() {return fRotList;}
 protected:
  StiMaterial * _fcMaterial;
  static THashList *fRotList;
};
#endif 
