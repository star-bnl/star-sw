/***************************************************************************
 *
 * $Id: StPhysicalHelixD.hh,v 1.1 1999/01/30 03:59:05 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 ***************************************************************************
 *
 * Description:
 *
 * Remarks:   This is a 'handmade' specialisation of StPhysicalHelix
 *            with StThreeVector<T> replaced by StThreeVectorD
 *            and pair<T, T> replaced by pairD.
 *            This code contains no templates.
 *
 ***************************************************************************
 *
 * $Log: StPhysicalHelixD.hh,v $
 * Revision 1.1  1999/01/30 03:59:05  fisyak
 * Root Version of StarClassLibrary
 *
 * Revision 1.1  1999/01/23 00:28:00  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_PHYSICAL_HELIX_D_HH
#define ST_PHYSICAL_HELIX_D_HH

#include "StThreeVectorD.hh"
#include "StHelixD.hh"

class StPhysicalHelixD : public StHelixD {
public:
    // Requires: momentum, origin, signed Magnetic Field
    //           and Charge of particle (+/- 1)
    StPhysicalHelixD(const StThreeVectorD&,
		     const StThreeVectorD&,
		     double, double);
    
    // curvature, dip angle, phase, origin, h
    StPhysicalHelixD(double, double, double,
		     const StThreeVectorD&, int h=-1);
    
    StPhysicalHelixD();
    virtual ~StPhysicalHelixD();

    // Requires:  signed Magnetic Field
    StThreeVectorD momentum(double) const;     // returns the momentum at origin
    StThreeVectorD momentumAt(double, double); // returns momemtum at S
    int            charge(double)   const;     // returns charge of particle

#ifdef __ROOT__
    ClassDef(StPhysicalHelixD,1)
#endif
};
#endif
