/***************************************************************************
 *
 * $Id: StParticleTable.cc,v 1.1 1999/05/14 18:48:14 ullrich Exp $
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
 * Revision 1.1  1999/05/14 18:48:14  ullrich
 * Initial Revision
 *
 * Revision 1.2  1999/09/24 01:23:52  fisyak
 * Reduced Include Path
 *
 * Revision 1.1  1999/05/14 18:48:14  ullrich
 * Initial Revision
#include "ospace/stl/src/treeaux.cpp" // CC4.2 with ObjectSpace only
 **************************************************************************/
#if defined(__SUNPRO_CC) && defined(OS_OBJECTSPACE) 
#include "StParticleDefinition.hh"

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
    
    mGeantPdgMap.insert(geantPdgPairType(1, 22));      // gamma
    mGeantPdgMap.insert(geantPdgPairType(2, -11));     // e+
    mGeantPdgMap.insert(geantPdgPairType(3, 11));      // e-
    mGeantPdgMap.insert(geantPdgPairType(4, 12));      // neutrino (ambigious)
    mGeantPdgMap.insert(geantPdgPairType(5, -13));     // mu+
    mGeantPdgMap.insert(geantPdgPairType(6, 13));      // mu-
    mGeantPdgMap.insert(geantPdgPairType(7, 111));     // pi0
    mGeantPdgMap.insert(geantPdgPairType(8, 211));     // pi+
    mGeantPdgMap.insert(geantPdgPairType(9, -211));    // pi-
    mGeantPdgMap.insert(geantPdgPairType(10, 130));    // K0_long
    mGeantPdgMap.insert(geantPdgPairType(11, 321));    // K+
    mGeantPdgMap.insert(geantPdgPairType(12, -321));   // K-
    mGeantPdgMap.insert(geantPdgPairType(13, 2112));   // n
    mGeantPdgMap.insert(geantPdgPairType(14, 2212));   // p
    mGeantPdgMap.insert(geantPdgPairType(15, -2212));  // anti_p
    mGeantPdgMap.insert(geantPdgPairType(16, 310));    // K0_short
    mGeantPdgMap.insert(geantPdgPairType(17, 221));    // eta
    mGeantPdgMap.insert(geantPdgPairType(18, 3122));   // lambda
    mGeantPdgMap.insert(geantPdgPairType(19, 3222));   // sigma+
    mGeantPdgMap.insert(geantPdgPairType(20, 3212));   // sigma0
    mGeantPdgMap.insert(geantPdgPairType(21, 3112));   // sigma-
    mGeantPdgMap.insert(geantPdgPairType(22, 3322));   // Xi0
    mGeantPdgMap.insert(geantPdgPairType(23, 3312));   // Xi-
    mGeantPdgMap.insert(geantPdgPairType(24, 3334));   // Omega
    mGeantPdgMap.insert(geantPdgPairType(25, -2112));  // anti_n
    mGeantPdgMap.insert(geantPdgPairType(26, -3122));  // anti_lambda
    mGeantPdgMap.insert(geantPdgPairType(27, -3222));  // anti_sigma-
    mGeantPdgMap.insert(geantPdgPairType(28, -3212));  // anti_sigma0
    mGeantPdgMap.insert(geantPdgPairType(29, -3112));  // anti_sigma+
    mGeantPdgMap.insert(geantPdgPairType(30, -3322));  // anti_Xi0
    mGeantPdgMap.insert(geantPdgPairType(31, -3312));  // anti_Xi+
    mGeantPdgMap.insert(geantPdgPairType(32, -3334));  // anti_omega+ 
    mGeantPdgMap.insert(geantPdgPairType(33, -15));    // anti_tau (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(34, 15));     // tau (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(35, 411));    // D+  (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(36, -411));   // D-  (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(37, 421));    // D0  (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(38, -421));   // anti_D0 (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(39, 431));    // Ds+ (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(40, -431));   // Ds- (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(41, 4122));   // lambda_c+ (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(42, 24));     // W+  (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(43, -24));    // W-  (STAR def.)
    mGeantPdgMap.insert(geantPdgPairType(44, 23));     // Z0  (STAR def.)
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




