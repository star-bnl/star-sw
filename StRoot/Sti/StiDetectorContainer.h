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
    
    virtual void buildMaterials(const char* buildDirectory);
    virtual void buildPolygons(const char* buildDirectory);
    virtual void buildDetectors(const char* buildDirectory); //Temp build from txt file using SCL parser

    void setDraw(bool val) {mdraw=val;}

    void push_back(StiDetector* layer);
    void push_back(StiDetPolygon* poly);
    
    void clearAndDestroy();
    void print() const;
    
    StiMaterial* material(const MaterialMapKey& key) const;
    void reset(); //full internal reset of interator structure

    const materialmap& materialMap() const;

private:

    //Singleton Management
    StiDetectorContainer();
    static StiDetectorContainer* sinstance;

protected:
    bool mdraw;

    materialmap mmaterialmap;

protected:
    
    //iterator implementation
    //DetectorMapKey mkey; //Store one to avoid constructor calls
    //detectormap::const_iterator mcurrent;
    
};

#endif



