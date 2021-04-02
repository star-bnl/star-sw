#ifndef __StarDpmjet3_h__
#define __StarDpmjet3_h__

/*!
  \class StarDpmjet3
  \brief Interface to Dpmjet3
  StarDpmjet3 provides the STAR user interface to the Dpmjet3 event generator.
 */

#include "StarGenerator/BASE/StarGenerator.h"
#include "Dpmjet3.h"
#include <map>
using namespace std;

class StarDpmjet3 : public StarGenerator
{
public:
    StarDpmjet3(const Char_t *name="Dpmjet3");
    ~StarDpmjet3(){ /* nada */ };
    
    void setProjectile(int m, int a) {mBeam[0]=m;  mBeam[1]=a;}
    void setTarget(int m, int a)     {mBeam[2]=m;  mBeam[3]=a;}
    void setEnergy(int e1, int e2)   {mEnergy[0]=e1;  mEnergy[1]=e2;}

    Int_t Init();
    Int_t Generate();
    StarGenStats Stats();
	
    /// Returns a reference to the /PYJETS/ common block
    static DtEvt1_t &dtevt1(){ return *address_of_dtevt1(); }
    static DtGlcp_t &dtglcp(){ return *address_of_dtglcp(); }
    
protected:    
    map<Int_t,Int_t> mStatusCode;

private:
    int mBeam[6];// ={197,79,197,79,0,0};
    float mEnergy[6];//={3.85,3.85,0,0,0,0};

    ClassDef(StarDpmjet3,1);
};

#endif
