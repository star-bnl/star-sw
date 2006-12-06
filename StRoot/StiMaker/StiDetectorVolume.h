#ifndef STAR_StiGeomMake
#define STAR_StiGeomMake
// $Id: StiDetectorVolume.h,v 2.1 2006/12/06 00:47:52 fine Exp $
// Author: Valeri Fine, Dec 2006

#include "TVolume.h"

class TVolumePosition;
class TShape;
class TRotMatrix;

class StiPlacement;
class StiPlanarShape;
class StiCylindricalShape;
class StiDetectorBuilder;
class StiShape;
class StiDetector;
class StiToolkit;

class StiDetectorVolume : public TVolume {
   // class TVolume decorator for StiDetector's
   private:
      StiDetector *fDetector;          //!
      // TRotMatrix  fRotMatrix;          //!
   
      StiDetectorVolume(TNode& node);
      StiDetectorVolume(const TVolume&);
   protected:
      // TRotMatrix             *GetMatrix(float angle);
   
      StiDetectorVolume(StiDetector *detector);
      StiDetectorVolume(StiDetector *detector,const Text_t* name, const Text_t* title, const Text_t* shapename, Option_t* option = "");
      StiDetectorVolume(StiDetector *detector,const Text_t* name, const Text_t* title, TShape* shape, Option_t* option = "");
      void MakeDetector(StiToolkit &tool);
      static TShape  *MakeShape(const StiShape            *shape,const char*material);     
      static TShape  *MakeShape(const StiPlanarShape      &shape,const char*material);
      static TShape  *MakeShape(const StiCylindricalShape &shape,const char*material); 
      void MakeVolume(const StiDetectorBuilder &builder);
   public:
      StiDetectorVolume(){;}
      StiDetectorVolume(StiToolkit &tool);
      StiDetectorVolume(const StiDetectorBuilder &builder);
      
      virtual ~StiDetectorVolume(){;}
      virtual void   Browse(TBrowser *b);
      virtual char *GetObjectInfo(Int_t px, Int_t py) const;
      
    ClassDef(StiDetectorVolume,0); // TVolume Sti decorator
};
#endif
