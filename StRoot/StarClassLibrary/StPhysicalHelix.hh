/***************************************************************************
 *
 * $Id: StPhysicalHelix.hh,v 1.1 1999/01/30 03:59:04 fisyak Exp $
 *
 * Author: Brian Lasiuk, Sep 1997
 ***************************************************************************
 *
 * Description: 
 * Parametrization of a physical helix. See the SCL user guide for more.
 * 
 ***************************************************************************
 *
 * $Log: StPhysicalHelix.hh,v $
 * Revision 1.1  1999/01/30 03:59:04  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:27:59  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_PHYSICAL_HELIX_HH
#define ST_PHYSICAL_HELIX_HH

#include "StThreeVector.hh"
#include "StHelix.hh"

class StPhysicalHelix : public StHelix {
public:
    // Requires: momentum, origin, signed Magnetic Field
    //           and Charge of particle (+/- 1)
    StPhysicalHelix(const StThreeVector<double>&,
		    const StThreeVector<double>&,
		    double, double);
    
    // curvature, dip angle, phase, origin, h
    StPhysicalHelix(double, double, double,
		    const StThreeVector<double>&, int h=-1);
    
    virtual ~StPhysicalHelix();

    // Requires:  signed Magnetic Field
    StThreeVector<double> momentum(double) const;     // returns the momentum at origin
    StThreeVector<double> momentumAt(double, double); // returns momemtum at S
    int                   charge(double)   const;     // returns charge of particle
    
protected:
    StPhysicalHelix();
};

#endif
