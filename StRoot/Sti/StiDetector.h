/*
 * StiDetector represents a detector for the purposes of ITTF tracking.
 * It contains all information about the geometry of the detector and
 * the necessary physical properties for incorporating it in tracking.
 */

#ifndef STI_DETECTOR_HH
#define STI_DETECTOR_HH

#include "TObject.h"
#include <string>

class StiDetector : public TObject{
    
public:

    enum StiShapeCode {kPlanar = 1, kCircular}; // shapeCode constants
    
    // con/destructor
    StiDetector();
    virtual ~StiDetector();
    
    //Gets
    Bool_t isOn() const {return on;}
    Bool_t isActive() const { return active; }
    Bool_t isContinuousMedium() const { return continuousMedium; }
    Bool_t isDiscreteScatterer() const { return discreteScatterer; }
    double getDensity() const { return density; }
    double getThickness() const { return thickness; }
    double getHalfWidth() const { return halfWidth; }
    double getHalfDepth() const { return halfDepth; }
    double getZCenter() const {return zcenter;}
    double getMaterialRadLength() const { return radLength; }
    double getRadLengthThickness() const { return thickness/radLength; }
    double getPosition() const { return position; }
    double getRefAngle() const { return refAngle; }
    int getSector() const {return sector;}
    int getPadrow() const {return padrow;}
    const char* getName() const {return name;}
    //const string& getName() const {return name;}
    Int_t getShapeCode() const { return shapeCode; }
    
    //Sets
    void setIsOn(bool val) {on = val;}
    void setIsActive(bool val) {active = val;}
    void setIsContinuousMedium(bool val) {continuousMedium = val;}
    void setIsDiscreteScatterer(bool val) {discreteScatterer = val;}
    void setDensity(double val) {density = val;}
    void setThickness(double val) {thickness = val;}
    void setHalfWidth(double val) {halfWidth = val;}
    void setZCenter(double val) {zcenter = val;}
    void setHalfDepth(double val) {halfDepth = val;}
    void setRadLength(double val) {radLength = val;}
    void setPosition(double val) {position = val;}
    void setRefAngle(double val) {refAngle = val;}
    void setShapeCode(StiShapeCode val) {shapeCode = val;}
    void setSector(int val) {sector = val;}
    void setPadrow(int val) {padrow = val;}
    void setName(char* val) {name = val;}
    //void setName(const string& val) {name = val;}

    //action
    virtual void build(const char* infile);  //for now, build from SCL parsable ascii file
    
protected:

    Bool_t on;                  // toggle this layer on/off.  off -> NOT added to detector container
    Bool_t active;              // does the object provide hit information?
    Bool_t continuousMedium;    // is this a continuous scatterer?
    Bool_t discreteScatterer;   // is this a discrete scatterer?
    double density;            // g/cm^3 [STAR units]
    double thickness;          // extent in local x (global r) in cm
    double halfWidth;          // 1/2 extent in local y (global phi) in cm
    double zcenter;                 // center of detector in z (global) projection in cm
    double halfDepth;          // 1/2 extent in z in cm
    double radLength;          // cm
    double position;           // perpendicular distance to global origin
    double refAngle;           // angle of normal to object in global coords
    StiShapeCode shapeCode;            // 1 if planar, 2 if circular
    int sector;  //Generalized sector
    int padrow;  //Generalized padrow
    char* name;               //Name of the class, a char so that we don't have any template problems
    //string name;               //Name of the class

};

//Non-members--------------------------

ostream& operator<<(ostream&, const StiDetector&);

#endif
