//StiDetectorLayerContainer.h
//M.L. Miller (Yale Software)
//02/02/01

//Container class for StiDetectorLayers
//Is a map, so full STL functionality provided
//Is a map, not multimap, so each key must be unique!

#ifndef StiDetectorLayerContainer_HH
#define StiDetectorLayerContainer_HH

#include <map>
#include "StiMapUtilities.h"

class StiDetector;

typedef map<DetectorMapKey, StiDetector*> detectormap;
typedef detectormap::value_type detectorMapValType;

class StiDetectorLayerContainer : public detectormap
{
public:
    
    //enum StiSectorBounds {kMinSector=1, kMaxSector=4}; //Needed for steps from one sector to another
    
    virtual ~StiDetectorLayerContainer();

    //Singleton Access
    static StiDetectorLayerContainer* instance();
    static void kill();
    
    virtual void build(const char* buildDirectory); //Temp build from txt file using SCL parser
    virtual void buildNext(const char* buildDirectory);

    bool hasMoreToBuild() const {return !mdone;}
    void buildReset() {mpadrow = mminpadrow; msector = mminsector; mdone=false;}
    void setPadrows(int min, int max) {mminpadrow=min; mmaxpadrow=max;}
    void setSectors(int min, int max) {mminsector=min; mmaxsector=max;}
    void setDraw(bool val) {mdraw=val;}

    void push_back(StiDetector* layer);
    void clearAndDestroy();
    void print() const;
    void print(DetectorMapKey*) const;
    
    void reset(); //full internal reset of interator structure
    
    //Dereference current iterator
    const StiDetector* StiDetectorLayerContainer::operator*() const;
    
    //Set iterator at outermost padrow of the sector closest to this refangle
    void setRefDetector(double refangle); //Not implemented yet
    
    //Set iterator at position closest to this point
    void setRefDetector(double refangle, double position); //Not implemented yet
    
    //Set iterator at outermost padrow of the sector closest to this sector
    void setRefDetector(int sector); 

    //Set iterator at position.  If it doesn't exist, calls setRefDetector(int sector)
    void setRefDetector(int sector, int padrow);
        
    //False if step not available
    bool padrowStepPlus(); //step to next radial layer in same sector (in)
    bool padrowStepMinus(); //step to next radial layer in same sector (out)
    
    //Will wrap around 2pi, only return false if error
    bool sectorStepPlus(); //positive step in ref-angle to same padrow
    bool sectorStepMinus(); //negative step in ref-angle to same padrow

    //Not yet implemented.  Will be important for any forward detector
    bool zStepPlus(); //positive step in z
    bool zStepMinus(); //negative step in z
    
private:

    //Singleton Management
    StiDetectorLayerContainer();
    static StiDetectorLayerContainer* sinstance;

protected:
    bool mdraw;
    bool mdone;
    
    //bounds for building stetp-by-step
    int mminsector;
    int mmaxsector;
    int mminpadrow;
    int mmaxpadrow;
    int mpadrow;
    int msector;

protected:
    
    //iterator implementation
    DetectorMapKey mkey; //Store one to avoid constructor calls
    detectormap::const_iterator mcurrent;
    
};

#endif



