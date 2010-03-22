/***************************************************************************
 *
 * $Id: StParticleTable.cc,v 1.7 2010/03/22 21:32:34 jwebb Exp $
 *
 * Author: Thomas Ullrich, May 99 (based on Geant4 code, see below) 
 ***************************************************************************
 *
 * The design of the StParticleDefinition class and all concrete
 * classes derived from it is largely based on the design of the 
 * G4ParticleDefinition class from Geant4 (RD44).
 * Although the code is in large parts different (modified or rewritten)
 * and adapted to the STAR framework the basic idea stays the same.
 *
 ***************************************************************************
 *
 * $Log: StParticleTable.cc,v $
 * Revision 1.7  2010/03/22 21:32:34  jwebb
 * And set the pdg ID to geant ID mapping.
 *
 * Revision 1.6  2010/01/28 21:54:20  jwebb
 * Added the Sigma(1385) baryons.
 *
 * Revision 1.5  2010/01/28 20:05:20  jwebb
 * Modifications to StParticleTable.cc (1) add the 'new' particle classes
 * defined on 01/28/10 to the table and, (2) add the existing J/Psi and B
 * mesons to the table.  Also defined a preprocessor macro to make reading
 * and modifying the code a bit easier.
 *
 * Revision 1.4  2000/04/06 22:25:38  ullrich
 * Added phi and omega. More STAR specific Geant IDs.
 *
 * Revision 1.3  1999/12/21 15:14:23  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.2  1999/09/24 01:23:52  fisyak
 * Reduced Include Path
 *
 * Revision 1.1  1999/05/14 18:48:14  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StParticleTable.hh"
#include "StParticleDefinition.hh"

#include "StarPDGEncoding.hh"

#if defined (__SUNPRO_CC) && __SUNPRO_CC < 0x500
#include <ospace/stl/src/treeaux.cpp> // CC4.2 with ObjectSpace only
#endif

StParticleTable* StParticleTable::mParticleTable = 0;

StParticleTable::~StParticleTable() {/* noop */}

StParticleTable::StParticleTable()
{
    //
    // Setup Geant3 -> PDG table
    // Note, the STAR specific definitions
    //
    typedef mGeantPdgMapType::value_type geantPdgPairType;

#define Geant2Pdg(X,Y) mGeantPdgMap.insert(geantPdgPairType(X,Y))
    
    Geant2Pdg(1, 22);      // gamma
    Geant2Pdg(2, -11);     // e+
    Geant2Pdg(3, 11);      // e-
    Geant2Pdg(4, 12);      // neutrino (ambigious)
    Geant2Pdg(5, -13);     // mu+
    Geant2Pdg(6, 13);      // mu-
    Geant2Pdg(7, 111);     // pi0
    Geant2Pdg(8, 211);     // pi+
    Geant2Pdg(9, -211);    // pi-
    Geant2Pdg(10, 130);    // K0_long
    Geant2Pdg(11, 321);    // K+
    Geant2Pdg(12, -321);   // K-
    Geant2Pdg(13, 2112);   // n
    Geant2Pdg(14, 2212);   // p
    Geant2Pdg(15, -2212);  // anti_p
    Geant2Pdg(16, 310);    // K0_short
    Geant2Pdg(17, 221);    // eta
    Geant2Pdg(18, 3122);   // lambda
    Geant2Pdg(19, 3222);   // sigma+
    Geant2Pdg(20, 3212);   // sigma0
    Geant2Pdg(21, 3112);   // sigma-
    Geant2Pdg(22, 3322);   // Xi0
    Geant2Pdg(23, 3312);   // Xi-
    Geant2Pdg(24, 3334);   // Omega
    Geant2Pdg(25, -2112);  // anti_n
    Geant2Pdg(26, -3122);  // anti_lambda
    Geant2Pdg(27, -3222);  // anti_sigma-
    Geant2Pdg(28, -3212);  // anti_sigma0
    Geant2Pdg(29, -3112);  // anti_sigma+
    Geant2Pdg(30, -3322);  // anti_Xi0
    Geant2Pdg(31, -3312);  // anti_Xi+
    Geant2Pdg(32, -3334);  // anti_omega+ 
    Geant2Pdg(33, -15);    // anti_tau (STAR def.)
    Geant2Pdg(34, 15);     // tau (STAR def.)
    Geant2Pdg(35, 411);    // D+  (STAR def.)
    Geant2Pdg(36, -411);   // D-  (STAR def.)
    Geant2Pdg(37, 421);    // D0  (STAR def.)
    Geant2Pdg(38, -421);   // anti_D0 (STAR def.)
    Geant2Pdg(39, 431);    // Ds+ (STAR def.)
    Geant2Pdg(40, -431);   // Ds- (STAR def.)
    Geant2Pdg(41, 4122);   // lambda_c+ (STAR def.)
    Geant2Pdg(42, 24);     // W+  (STAR def.)
    Geant2Pdg(43, -24);    // W-  (STAR def.)
    Geant2Pdg(44, 23);     // Z0  (STAR def.)

    Geant2Pdg(52, kHyperTriton); // Star def. HyperTriton (fake pdg id)

    Geant2Pdg( 60, +413 ); // D*+
    Geant2Pdg( 61, -413 ); // D*-
    Geant2Pdg( 62, +423 ); // D*0
    Geant2Pdg( 63, -423 ); // D*0 bar

    Geant2Pdg(70, +521);    // B+ meson
    Geant2Pdg(71, -521);    // B- meson
    Geant2Pdg(72, +511);    // B0 meson
    Geant2Pdg(73, -511);    // B0-bar meson

    Geant2Pdg(150, 223);   // omega meson (STAR def.)
    Geant2Pdg(151, 333);   // phi meson (STAR def.)
    Geant2Pdg(152, 113);   // rho meson (STAR def.)
    Geant2Pdg(153, 213);   // rho+ meson (STAR def.)
    Geant2Pdg(154, -213);  // rho- meson (STAR def.)
    Geant2Pdg(155, 311);   // K0 (STAR def.)
    Geant2Pdg(156, -311);  // anti_K0 (STAR def.)

    Geant2Pdg( 160, 443 );     // JPsi
    Geant2Pdg( 149, kDalitz ); // pi0 --> e+ e- gamma

    Geant2Pdg( 161,    553); // Upsilon(1S)
    Geant2Pdg( 162, 100553); // Upsilon(2S)
    Geant2Pdg( 163, 200553); // Uspilon(3S)
    Geant2Pdg( 164,    553); // Upsilon(1S) -- mu+ mu- channel w/ incorrect partial width
    Geant2Pdg( 165, 100553); // Upsilon(2S) -- mu+ mu- channel w/ incorrect partial width
    Geant2Pdg( 166, 200553); // Uspilon(3S) -- mu+ mu- channel w/ incorrect partial width

    Geant2Pdg( 701, +3224 ); // Sigma 1385 +
    Geant2Pdg( 702, +3114 ); // Sigma 1385 -
    Geant2Pdg( 703, -3114 ); // Sigma 1385 plus bar
    Geant2Pdg( 704, -3224 ); // Sigma 1385 minus bar 

    Geant2Pdg( +706, +20003122 ); // Lambda 1520 
    Geant2Pdg( -706, -20003122 ); // Lambda 1520 

#undef Geant2Pdg

}

StParticleTable::StParticleTable(const StParticleTable &) {/* private */}
   
StParticleTable* StParticleTable::instance()
{
    return particleTable();
}

StParticleTable* StParticleTable::particleTable()
{
    if (!mParticleTable) mParticleTable =  new StParticleTable;
    return mParticleTable;
}

unsigned int StParticleTable::entries() const {return mNameMap.size();}

unsigned int StParticleTable::size() const {return mNameMap.size();}

bool StParticleTable::contains(const string& name) const
{
    return (findParticle(name) != 0);
}

bool StParticleTable::contains(int pdgId) const
{
    return (findParticle(pdgId) != 0);
}
    
bool StParticleTable::containsGeantId(int geantId) const
{
    return (findParticleByGeantId(geantId) != 0);
}
 
StParticleDefinition* StParticleTable::findParticle(const string& name)  const
{
    mNameMapType::const_iterator i = mNameMap.find(name);
    if (i == mNameMap.end())
	return 0;
    else
 	return (*i).second;
}

StParticleDefinition* StParticleTable::findParticle(int pdgId)  const
{
    mPdgMapType::const_iterator p =  mPdgMap.find(pdgId);
    if (p == mPdgMap.end())
	return 0;
    else
 	return (*p).second;
}
    
StParticleDefinition* StParticleTable::findParticleByGeantId(int geantId) const
{
    //
    //  Two ways to find the particle:
    //  1. If it's an elementary particle its in the PDG list 
    //  2. If it is a nucleus/ion find it via the name list
    //
    StParticleDefinition *p = 0;
    switch (geantId) {
    case 45:
	p = findParticle(string("deuteron"));
	break;
    case 46:
	p = findParticle(string("triton"));
	break;
    case 47:
	p = findParticle(string("alpha"));
	break;
    case 49:
	p = findParticle(string("He3"));
	break;
    case 50:
	p = findParticle(string("opticalphoton"));
	break;
    default:
	mGeantPdgMapType::const_iterator i =  mGeantPdgMap.find(geantId);
	if (i != mGeantPdgMap.end())
	    p = findParticle((*i).second);
	break;
    }
    return p;
}

void StParticleTable::insert(StParticleDefinition* p)
{
    typedef mPdgMapType::value_type pdgPairType;
    typedef mNameMapType::value_type namePairType;

    if (p->pdgEncoding() != 0)
	mPdgMap.insert(pdgPairType(p->pdgEncoding(), p));
    mNameMap.insert(namePairType(p->name(), p));
}

void StParticleTable::erase(StParticleDefinition* p)
{
    mPdgMapType::iterator i =  mPdgMap.find(p->pdgEncoding());
    if (i != mPdgMap.end()) mPdgMap.erase(i);
    
    mNameMapType::iterator j =  mNameMap.find(p->name());
    if (j != mNameMap.end()) mNameMap.erase(j);   
}

void StParticleTable::dump(ostream& os)
{
    mNameMapType::iterator i;
    for (i = mNameMap.begin(); i != mNameMap.end(); ++i)
	cout << *((*i).second) << endl;
}


StVecPtrParticleDefinition
StParticleTable::allParticles() const
{
    StVecPtrParticleDefinition vec;
    mNameMapType::const_iterator i;
    for (i = mNameMap.begin(); i != mNameMap.end(); ++i)
	vec.push_back((*i).second);
    return vec;
}




