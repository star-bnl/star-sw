//StiDetectorContainer.h
//M.L. Miller (Yale Software)
//02/02/01

//Container class for StiDetectorLayers
//Is a map, so full STL functionality provided
//Is a map, not multimap, so each key must be unique!

#ifndef StiDetectorContainer_HH
#define StiDetectorContainer_HH

#include <map>
#include "StiMapUtilities.h"

class StiDetector;
class StiMaterial;
class StiDetPolygon;

typedef map<double, StiDetPolygon*> detectormap;
typedef detectormap::value_type detectorMapValType;

typedef map<MaterialMapKey, StiMaterial*> materialmap;
typedef materialmap::value_type materialMapValType;

class StiDetectorContainer : public detectormap
{
public:
    
    virtual ~StiDetectorContainer();

    //Singleton Access
    static StiDetectorContainer* instance();
    static void kill();

    //Gets/sets
    void setDraw(bool val) {mdraw=val;}
    StiMaterial* material(const MaterialMapKey& key) const;
    const materialmap& materialMap() const;

    
    //Build functions
    virtual void buildMaterials(const char* buildDirectory);
    virtual void buildPolygons(const char* buildDirectory);
    virtual void buildDetectors(const char* buildDirectory); //Temp build from txt file using SCL parser

    //Action
    void reset(); //full internal reset of interator structure
    void push_back(StiDetector* layer);
    void push_back(StiDetPolygon* poly);
    void clearAndDestroy();
    
    //Navigation

    //Dereference current
    StiDetector* operator*() const;
    
    //Step out radially
    void moveOut();
    //Step in radially
    void moveIn();

    //Step around in increasing phi (clockwise if viewing sectors 1-12 from the membrane, increasing phi in STAR TPC global coordinates)
    void movePlusPhi();
    //ibid
    void moveMinusPhi();
    
    //Utilities
    void print() const;
    
private:

    //Singleton Management
    StiDetectorContainer();
    static StiDetectorContainer* sinstance;

protected:
    bool mdraw;
    materialmap mmaterialmap;
    
protected:
    
    //iterator implementation
    detectormap::const_iterator mcurrent;
    
};

#endif



