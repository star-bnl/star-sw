/*
 * StiDetector represents a detector for the purposes of ITTF tracking.
 * It contains all information about the geometry of the detector and
 * the necessary physical properties for incorporating it in tracking.
 */

#ifndef STI_DETECTOR_HH
#define STI_DETECTOR_HH

#include <string>

class StiMaterial;

class StiDetector {
    
public:

    enum StiShapeCode {kPlanar = 1, kCylindrical}; // shapeCode constants
    
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

    int getShapeCode() const { return shapeCode; }
    double getCenterRadius() const { return centerRadius; }
    double getCenterRefAngle() const { return centerRefAngle; }
    double getOrientationAngle() const { return orientationAngle; }
    double getHalfWidth() const { return halfWidth; }
    double getNormalRadius() const { return normalRadius; }
    double getNormalRefAngle() const { return normalRefAngle; }
    double getYmin() const { return yMin; }
    double getYmax() const { return yMax; }

    double getActivePosition() const {return activePosition; }
    double getHalfDepth() const { return halfDepth; }
    double getZCenter() const {return zCenter;}
    double getThickness() const { return thickness; }

    int getSector() const {return sector;}
    int getPadrow() const {return padrow;}
    const char* getName() const {return name;}
    
    // mutators
    void setIsOn(bool val) {on = val;}
    void setIsActive(bool val) {active = val;}
    void setIsContinuousMedium(bool val) {continuousMedium = val;}
    void setIsDiscreteScatterer(bool val) {discreteScatterer = val;}

    void setGas(StiMaterial *val){ gas = val; }
    void setMaterial(StiMaterial *val){ material = val; }

    void setShapeCode(StiShapeCode val) {shapeCode = val;}

    // when changing the center representation, update the normal one
    void setCenterRep(double cRadius, double cRefAngle, double oAngle, double hWidth);    
    void setCenterRadius(double val){ centerRadius = val; updateNormalRep(); }
    void setCenterRefAngle(double val){centerRefAngle = val; updateNormalRep(); }    
    void setOrientationAngle(double val){ orientationAngle= val; updateNormalRep(); }    
    void setHalfWidth(double val){ halfWidth= val; updateNormalRep(); }
    
    // when changing the normal representation, update the center one
    void setNormalRep(double nRadius, double nRefAngle,double minY, double maxY);    
    void setNormalRadius(double val){ normalRadius = val; updateCenterRep(); }    
    void setNormalRefAngle(double val){ normalRefAngle = val; updateCenterRep(); }    
    void setYmax(double val){ yMax = val; updateCenterRep(); }    
    void setYmin(double val){ yMin = val; updateCenterRep(); }
    

    void setThickness(double val) {thickness = val;}
    void setActivePosition(double val) {activePosition = val;}
    void setZCenter(double val) {zCenter = val;}
    void setHalfDepth(double val) {halfDepth = val;}

    void setSector(int val) {sector = val;}
    void setPadrow(int val) {padrow = val;}
    void setName(const char *val){	strncpy(name, val, 99);}

    //action
    virtual void build(const char* infile);  //for now, build from SCL parsable ascii file
    virtual void write(const char* szFileName);
    
protected:

    //--------------------------------------------------------------------
    // methods for keeping our 2 plane representations in sync

    // Sets the "normal" representation based on the current values of  the "center" representation
    void updateNormalRep();

    // Sets the "center" representation based on the current values of the "normal" representation
    void StiDetector::updateCenterRep();
    
    // logical switches
    bool on;                  // toggle this layer on/off.  (off => NOT added to detector container)
    bool active;              // does the object provide hit information?
    bool continuousMedium;    // is this a continuous scatterer?  (yes => scatterer info given by "gas" below)
    bool discreteScatterer;   // is this a discrete scatterer?    (yes => scatterer given by "material" below)

    // material information
    StiMaterial *gas;           // gas representing the atmosphere in 
    //   (if it's a continuous medium) and/or  radially inward from the detector.
    StiMaterial *material;      // material composing the discrete scatterer
    
    // physical location / orientation
    StiShapeCode shapeCode;     // 1 if planar, 2 if circular

    // 1) for planar objects we store 2 representations
    //   a) Most useful for detector navigation, vector to object's center + angle _from_ radius _to_ normal & half-width
    double centerRadius;
    double centerRefAngle;
    double orientationAngle;
    double halfWidth;
    //   b) Most useful for Kalman, normal vector to object's plane + max & min y_local relative to the normal.
    double normalRadius;
    double normalRefAngle;
    double yMax;
    double yMin;

    // 2) for cylindrical objects, we use the variables above:
    //    centerRadius = normalRadius = radius of circle
    //    centerRefAngle = normalRefAngle = ref angle to center of arc
    //    orientationAngle = 0
    //    halfWidth = yMax = -yMin = half pathlength of arc

    // 3) these are common to both geometries:
    double activePosition;     // detector plane location in local x (cm)
    double zCenter;            // center of detector in z (global) in cm
    double halfDepth;          // 1/2 extent in z in cm
    double thickness;          // perpendicular thickness of material

    //------------------------------------------------------------------------
    // naming
    int sector;  //Generalized sector (azimuthal ordering)
    int padrow;  //Generalized padrow (radial ordering)
    char name[100];  //Name of the class, a char to avoid template problems

};

//Non-members--------------------------
//ostream& operator<<(ostream&, const StiDetector&);

#endif
