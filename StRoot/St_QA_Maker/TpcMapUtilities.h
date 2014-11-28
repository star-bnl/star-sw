///////////////////////////////////////////////////////////////////////////
// $Id: TpcMapUtilities.h,v 1.3 2006/05/20 14:52:43 genevb Exp $
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
// Revision 1.3  2006/05/20 14:52:43  genevb
// Make define variable unique for QA too
//
// Revision 1.2  2006/05/20 03:17:21  genevb
// Changed MapKey to MapQAKey to make it unique for QA
//
// Revision 1.1  2000/08/09 18:57:44  lansdell
// improvements in TPC gains code reduces CPU time per event by factor of 2
//
//
///////////////////////////////////////////////////////////////////////////

#ifndef TpcMapQAUtilities_h
#define TpcMapQAUtilities_h

#include "StThreeVectorD.hh"

//Structure for map key
struct HitMapQAKey {
    bool operator==(const HitMapQAKey&) const;
    int sector;
    int padrow;
};

//Structure for ordering map key
struct MapQAKeyLessThan{
    bool operator() (const HitMapQAKey&, const HitMapQAKey&) const;
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
