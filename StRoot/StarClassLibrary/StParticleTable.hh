/***************************************************************************
 *
 * $Id: StParticleTable.hh,v 1.1 1999/05/14 18:50:00 ullrich Exp $
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
 * $Log: StParticleTable.hh,v $
 * Revision 1.1  1999/05/14 18:50:00  ullrich
 * Initial Revision
 *
 * Revision 1.2  1999/12/21 15:14:26  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 1.1  1999/05/14 18:50:00  ullrich
 * Initial Revision
 *
 **************************************************************************/
#define StParticleTable_hh

#include <iostream.h>
#include <string>
using std::map;
using std::string;
#endif

#ifndef __CINT__
#ifdef ST_NO_TEMPLATE_DEF_ARGS
typedef vector<StParticleDefinition*, allocator<StParticleDefinition*> > StVecPtrParticleDefinition;
#else
typedef vector<StParticleDefinition*> StVecPtrParticleDefinition;
#endif
#endif // __CINT__

class StParticleTable {
public:
    virtual ~StParticleTable();
    
    static StParticleTable* particleTable();
    static StParticleTable* instance();
    
    unsigned int entries() const;
    unsigned int size() const;
    
    bool contains(const string &) const;              // by name
    bool contains(int) const;                         // by PDG encoding
    bool containsGeantId(int) const;                  // by Geant3 id
    
    StParticleDefinition* findParticle(const string&)  const; // by name    
    StParticleDefinition* findParticle(int)  const;           // by PDG encoding   
    StParticleDefinition* findParticleByGeantId(int) const;   // by Geant3 id
    
    void insert(StParticleDefinition*);
    void erase(StParticleDefinition*);
    
    void dump(ostream& = cout);

    StVecPtrParticleDefinition allParticles() const;
    
private:
    StParticleTable();
    StParticleTable(const StParticleTable &right);
    
    static StParticleTable *mParticleTable;

#ifndef __CINT__
#ifdef ST_NO_TEMPLATE_DEF_ARGS
    // as soon as Sun CC4.2 is gone this goes as well
    typedef map<int, int, less<int>,
	allocator< pair<const int, int> > >
    mGeantPdgMapType;
    typedef map<int, StParticleDefinition*, less<int>,
	allocator< pair<const int,StParticleDefinition*> > >
    mPdgMapType;
    typedef map<string,	StParticleDefinition*, less<string>,
	allocator< pair<const string,StParticleDefinition*> > >
    mNameMapType;
#else
    typedef map<int, int>                      mGeantPdgMapType;
    typedef map<int, StParticleDefinition*>    mPdgMapType;
    typedef map<string, StParticleDefinition*> mNameMapType;
#endif
#endif // __CINT__
    
    mGeantPdgMapType   mGeantPdgMap;     // Geant3 IDs only
    mPdgMapType        mPdgMap;          // PDG IDs only
    mNameMapType       mNameMap;         // complete list
};
#endif






