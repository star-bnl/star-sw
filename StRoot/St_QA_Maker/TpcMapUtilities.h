///////////////////////////////////////////////////////////////////////////
// $Id: TpcMapUtilities.h,v 1.1 2000/08/09 18:57:44 lansdell Exp $
//
// Author: M.L. Miller, Yale
//
///////////////////////////////////////////////////////////////////////////
//
// Description: TPC sector gains map utilities
//
///////////////////////////////////////////////////////////////////////////
//
// $Log: TpcMapUtilities.h,v $
// Revision 1.1  2000/08/09 18:57:44  lansdell
// improvements in TPC gains code reduces CPU time per event by factor of 2
//
//
///////////////////////////////////////////////////////////////////////////

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
