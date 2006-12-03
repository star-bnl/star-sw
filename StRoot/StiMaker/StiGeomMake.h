#ifndef STAR_StiGeomMake
#define STAR_StiGeomMake
// $Id: StiGeomMake.h,v 2.2 2006/12/03 18:18:13 fine Exp $
// Author: Valeri Fine, Dec 2006

#include "TVolume.h"

class TVolumePosition;
class TShape;

class StiPlacement;
class StiPlanarShape;
class StiCylindricalShape;
class StiDetectorBuilder;
class StiShape;
class StiDetector;

class StiGeomMake {
  protected:
     static TShape          *MakeShape(const StiPlanarShape &shape,const char*material);
     static TShape          *MakeShape(const StiCylindricalShape &shape,const char*material); 
  public:      
     StiGeomMake(){}
    ~StiGeomMake(){}
     static TShape          *MakeShape(const StiShape *shape,const char*material);     
     static TVolume         *MakeVolume(StiDetectorBuilder &builder);
};

class StiDetectorVolume : public TVolume {
   // class TVolume decorator for StiDetector's
   private:
      StiDetector &fDetector;
   
      StiDetectorVolume(TNode& node);
      StiDetectorVolume(const TVolume&);
   public:
      StiDetectorVolume(StiDetector &detector);
      StiDetectorVolume(StiDetector &detector,const Text_t* name, const Text_t* title, const Text_t* shapename, Option_t* option = "");
      StiDetectorVolume(StiDetector &detector,const Text_t* name, const Text_t* title, TShape* shape, Option_t* option = "");
      
      virtual ~StiDetectorVolume(){;}
      virtual void   Browse(TBrowser *b);
      virtual char *GetObjectInfo(Int_t px, Int_t py) const;
      
    ClassDef(StiDetectorVolume,0); // TVolume Sti decorator
};
#endif
