/***************************************************************************
 *
 * $Id: vecTest3.cc,v 1.2 2003/09/02 17:59:38 perev Exp $
 *
 * Author: Thomas Ullrich, Oct 1998
 ***************************************************************************
 *
 * Description: Tests various features of StLorentzVector
 *
 ***************************************************************************
 *
 * $Log: vecTest3.cc,v $
 * Revision 1.2  2003/09/02 17:59:38  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/02/17 12:44:05  ullrich
 * New Revision
 *
 * Revision 1.1  1999/01/23 00:26:56  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include <math.h>
#include "StGlobals.hh"
#include "StLorentzVector.hh"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "Randomize.h"

int main() 
{
    HepJamesRandom engine;
    RandFlat rflat(engine);
    
    //
    //	Generate 2-body decay: phi->e+e-
    //
    cout << "This programs generates a phi meson with random momentum px, py, pz" << endl; 
    cout << "and lets it decay into electrons. Their momenta are then calculated" << endl; 
    cout << "in the lab frame. Tests essentially StLorentzVector<>::boost()" << endl;
    
    //
    // 	Create parent particle, here a phi(1020)
    //	with momentum random momenta 0-1 GeV/c
    //
    StThreeVector<double> p(rflat.shoot()*GeV, rflat.shoot()*GeV, rflat.shoot()*GeV);
    StLorentzVector<double> parent(p, p.massHypothesis(1020*MeV));

    cout << "parent particle: " << endl;
    cout << "\t4-momentum: " << parent << endl;
    cout << "\tinvariant mass: " << abs(parent) << endl;
    
    //
    //	Let the phi decay into two electrons.
    //	The easiest way to do this is in the CM of the parent.
    //
    double mass1 = electron_mass_c2;
    double mass2 = electron_mass_c2;
    double massParent  = abs(parent);
    double E1  = (massParent*massParent + mass1*mass1 - mass2*mass2)/(2.*massParent);
    double E2  = massParent - E1;
    double p1  = ::sqrt((E1 + mass1)*(E1 - mass1));
    double p2  = ::sqrt((massParent*massParent-(mass1+mass2)*(mass1+mass2))*
                      (massParent*massParent-(mass1-mass2)*(mass1-mass2)))/(2.*massParent);

    //
    //	Orientation in decaying particle rest frame
    //
    double costheta = 2*rflat.shoot() - 1;
    double sintheta = ::sqrt((1 + costheta)*(1 - costheta));
    double phi      = 2*pi*rflat.shoot();

    //
    //	Create daughters
    //
    StThreeVector<double>   momentum(p1*sintheta*cos(phi),
				     p1*sintheta*sin(phi),
				     p1*costheta);
    StLorentzVector<double> daughter1(E1, momentum);
    StLorentzVector<double> daughter2(E2, -momentum);
    
    cout << "decay particles in CM frame of parent: " << endl;
    cout << "daughter1: " << endl;
    cout << "\t4-momentum: " << daughter1 << endl;
    cout << "\tinvariant mass: " << abs(daughter1) << endl;
    cout << "daughter2: " << endl;
    cout << "\t4-momentum: " << daughter2 << endl;
    cout << "\tinvariant mass: " << abs(daughter2) << endl;
    
    //
    //	Boost secondary particles into the lab
    //
    daughter1 = daughter1.boost(parent);
    daughter2 = daughter2.boost(parent);
    
    cout << "decay particles in lab frame: " << endl;
    cout << "daughter1: " << endl;
    cout << "\t4-momentum: " << daughter1 << endl;
    cout << "\tinvariant mass: " << abs(daughter1) << endl;
    cout << "daughter2: " << endl;
    cout << "\t4-momentum: " << daughter2 << endl;
    cout << "\tinvariant mass: " << abs(daughter2) << endl;

    //
    //	Cross-check: reconstruct parent from daughters
    //
    StLorentzVector<double> check = daughter1+daughter2;
    cout << "cross-check: reconstructed parent" << endl;
    cout << "\t4-momentum: " << check << endl;
    cout << "\tinvariant mass: " << abs(check) << endl;
	
    return 0;
}
