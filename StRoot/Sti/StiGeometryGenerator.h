//StiGeometryGenerator.h
//M.L. Miller (Yale Software)
//04/01

#ifndef StiGeometryGenerator_HH
#define StiGeometryGenerator_HH

class StiGeometryTransform;
class St_svg_geom;
class St_svg_config;
class St_svg_shape;

#include "StMaker.h"

//--------------------
// Material Properties
// units: cm & g/cm^3

// here, we approximate the hybrid as a uniform rectangular solid.  In order
// to be as realistic as possible, we enforce the 1.21% radiation length given
// on the SVT site, a density close to most of the hybrid volume, and an
// overall thickness close to that of the hybrid.  In other words, these
// numbers do not correspond to real material.
#define STI_HYBRID_RADLENGTH   8.3  // chosen to match realistic thickness
#define STI_HYBRID_DENSITY     2.5  // Mostly BeO (2.86), some dielectric (1.5)
#define STI_HYBRID_THICKNESS   0.1  // real thickness is O(1mm)
// this doesn't appear in the database:
#define STI_HYBRID_WIDTH        2.  // extent in local y
#define STI_HYBRID_DEPTH       54.  // extend in z

class StiGeometryGenerator : public StMaker {
 public:
    
    StiGeometryGenerator(const char* name = "StiGeometryGenerator");
    virtual ~StiGeometryGenerator();

    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StiGeometryGenerator.h,v 1.1 2001/05/18 20:47:00 bnorman Exp $ built "__DATE__" "__TIME__; return cvs;}	

private:
    void buildTpc();
    void buildSvg(); // SVT + SSD

    StiGeometryTransform *mGeometryTransform; //! 
    St_svg_geom   *m_svg_geom;    //!
    St_svg_config *m_svg_config;  //!
    St_svg_shape  *m_svg_shape;   //!

    
private:
    ClassDef(StiGeometryGenerator, 1)

        // root of directory all detectors
        char* m_szGeomDirectory; //!

};
#endif
