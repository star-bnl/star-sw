//StiDetectorContainer.h
//M.L. Miller (Yale Software)
//02/02/01

//Container class for StiDetectorLayers
//Is a map, so full STL functionality provided
//Is a map, not multimap, so each key must be unique!

#ifndef StiDetectorContainer_HH
#define StiDetectorContainer_HH

#include <vector>
using std::vector;
#include <map>
using std::map;
#include "StiMapUtilities.h"

#include "StiFactoryTypedefs.h"

using std::map;

class StiDetector;
class StiMaterial;

class StiDetectorContainer
{
public:

    virtual ~StiDetectorContainer();

    //Singleton Access
    static StiDetectorContainer* instance();
    static void kill();

    //Temporary, MLM 7/30/01
    data_node* root() const {return mroot;}
    
    //Build functions
    virtual void buildDetectors(const char* buildDirectory, data_node_factory* nodefactory,
				detector_factory* detfactory);

    //Action
    void reset(); //full internal reset of interator structure
//    void clearAndDestroy();
    
    //Navigation

    //Dereference current
    StiDetector* operator*() const;
    
    //Step out radially
    void moveOut();
    //Step in radially
    void moveIn();

    //Step around in increasing phi (clockwise if viewing sectors 1-12 from the membrane,
    //increasing phi in STAR TPC global coordinates)
    void movePlusPhi();
    void moveMinusPhi();

    //Set iterators to the detector nearest to this guy (Useful if you get lost)
    void setToDetector(StiDetector* layer);
    
    //Set iterators to the first detector in the yaer closest to this position
    void setToDetector(double position);
    
    //Set iterators to the position nearest this guy
    void setToDetector(double position, double angle);
    
    //Utilities
    void print() const;
    
private:

    //The Tree
    data_node* mroot;
    data_node* mregion; //current region (mid/forward/backward rapidity, etc)
    
    //iterators
    data_node_vec::const_iterator mradial_it;
    data_node_vec::const_iterator mphi_it;
    
    //Singleton Management
    StiDetectorContainer();
    static StiDetectorContainer* sinstance;
    
};

#endif



