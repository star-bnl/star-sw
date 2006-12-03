#ifndef STAR_StiGeomMake
#define STAR_StiGeomMake
// $Id: StiGeomMake.h,v 2.1 2006/12/03 17:53:55 fine Exp $
// Author: Valeri Fine, Dec 2006

class TVolumePosition;
class TVolume;
class TShape;

class StiPlacement;
class StiPlanarShape;
class StiCylindricalShape;
class StiDetectorBuilder;
class StiShape;
 
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
#endif
