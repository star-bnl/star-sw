/*
 * StiDetector represents a detector for the purposes of ITTF tracking.
 * It contains all information about the geometry of the detector and
 * the necessary physical properties for incorporating it in tracking.
 */

#ifndef STI_DETECTOR_HH
#define STI_DETECTOR_HH 1

#include <string>
using std::string;

class StiMaterial;
class StiShape;
class StiPlacement;

//This is a little convoluted, but we need it:
class StiDetector;
template <class T> class StiCompositeTreeNode;

class StiDetector {
    
public:

    // con/destructor
    StiDetector();
    virtual ~StiDetector();
    
    // accessors
    bool isOn() const {return on;}
    bool isActive() const { return active; }
    bool isContinuousMedium() const { return continuousMedium; }
    bool isDiscreteScatterer() const { return discreteScatterer; }

    StiMaterial* getGas() const { return gas; }
    StiMaterial* getMaterial() const { return material; }

    StiShape* getShape() const { return shape; }
    StiPlacement* getPlacement() const { return placement; }

    const string& getName() const {return name;}
    
    // mutators
    void setIsOn(bool val) {on = val;}
    void setIsActive(bool val) {active = val;}
    void setIsContinuousMedium(bool val) {continuousMedium = val;}
    void setIsDiscreteScatterer(bool val) {discreteScatterer = val;}

    void setGas(StiMaterial *val){ gas = val; }
    void setMaterial(StiMaterial *val){ material = val; }

    void setShape(StiShape *val){ shape = val; }
    void setPlacement(StiPlacement *val){ placement = val; }

    void setName(const string& val){ name=val;}

    //action
    virtual void build(){}  //for now, build from SCL parsable ascii file

    virtual void copy(StiDetector &detector);
    
    //This is a bit of a hack, but we leave ourselves a reverse connection between
    // a detector and the tree node that it's stored on.
    void setTreeNode(StiCompositeTreeNode<StiDetector>* val) {mNode=val;}
    StiCompositeTreeNode<StiDetector>* getTreeNode() const {return mNode;}
    
protected:
    
    // logical switches
    bool on;                  // toggle this layer on/off.  (off => NOT added to detector container)
    bool active;              // does the object provide hit information?
    bool continuousMedium;    // is this a continuous scatterer?  (yes => scatterer info given by "gas" below)
    bool discreteScatterer;   // is this a discrete scatterer?    (yes => scatterer given by "material" below)

    // material information
    StiMaterial *gas;           // gas representing the atmosphere in 
    //   (if it's a continuous medium) and/or  radially outward from the detector.
    StiMaterial *material;      // material composing the discrete scatterer
    
    // physical location / orientation
    StiShape     *shape;
    StiPlacement *placement;

    string name;

    StiCompositeTreeNode<StiDetector>* mNode;

};

//ostream& operator<<(ostream& os, const StiDetector& d);

#endif
