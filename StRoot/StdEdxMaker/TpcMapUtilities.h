//TpcMapUtilities.h
//M.L. Miller, Yale Software, 7/00

#ifndef TpcMapUtilities_h
#define TpcMapUtilities_h

#include "StThreeVectorD.hh"

//Structure for map key
struct HitMapKey {
    bool operator==(const HitMapKey&) const;
    int sector;
    int padrow;
};

//Structure for ordering map key
struct MapKeyLessThan{
    bool operator() (const HitMapKey&, const HitMapKey&) const;
};

//Structure to hold 3d global points in a plane of each padrow
class PadrowLocation {
public:
    PadrowLocation();
    PadrowLocation(const StThreeVectorD&, const StThreeVectorD&, const StThreeVectorD&);
    virtual ~PadrowLocation();

    //Access
    const StThreeVectorD& outsidePoint() const;
    const StThreeVectorD& centerPoint() const;
    const StThreeVectorD& insidePoint() const;
    void print() const;
    
private:
    StThreeVectorD m_TopPoint;
    StThreeVectorD m_MidPoint;
    StThreeVectorD m_BotPoint;
};

#endif
