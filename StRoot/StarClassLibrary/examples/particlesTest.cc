/***************************************************************************
 *
 * $Id: particlesTest.cc,v 1.2 2003/09/02 17:59:38 perev Exp $
 *
 * Author: Thomas Ullrich, May 1999 
 ***************************************************************************
 *
 * Description:
 * Program to test Particle Definitions
 *
 ***************************************************************************
 *
 * $Log: particlesTest.cc,v $
 * Revision 1.2  2003/09/02 17:59:38  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.1  1999/05/17 11:10:41  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <Stiostream.h>
#include <typeinfo>
#include "StParticleTable.hh"
#include "StParticleTypes.hh"

int main()
{
    int i;
    
    //
    //  Write all particle definitions to stdout
    //
    StParticleTable *table = StParticleTable::instance();
    table->dump();
    
     //
    //  Print all particles without PDG encoding
    //
    cout << "The following particles have no PDG encoding:" << endl;
    StVecPtrParticleDefinition vec = table->allParticles();
    for (i=0; i<vec.size(); i++) {
	if (vec[i]->pdgEncoding() == 0)
	    cout << vec[i]->name().c_str() << endl;
    }

    //
    //  Check consistency of GEANT3 lookup table
    //
    StParticleDefinition *p1, *p2;
    for (i=0; i<100; i++) {
	p1 = table->findParticleByGeantId(14);
	if (p1) {
	    p2 = table->findParticle(p1->name());
	    if (*p1 != *p2)
		cerr << "WARNING: inconsistency in lookup table" << endl;
	}
    }
    return 0;
}
