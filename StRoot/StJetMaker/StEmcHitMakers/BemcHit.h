//BemcHit.h
//R. Fatemi (IUCF)
//9/04

#ifndef BemcHit_HH
#define BemcHit_HH

#include "StThreeVectorD.hh"
#include <iostream>
using namespace std;

#include "StJetMaker/StEmcHitMakers/EmcHit.h"

class BemcHit : public EmcHit
{
public:
    BemcHit() {mId = kBarrelEmcTowerId;}
    virtual ~BemcHit() {};

protected:
};

//inlines --------------------------------

inline ostream& operator<<(ostream& os, const BemcHit& h)
{
    return os <<"adc:\t"<<h.adc()<<"\te:\t"<<h.energy()<<"\teta:\t"<<h.position().pseudoRapidity()<<"\tphi:\t"<<h.position().phi();
}

#endif
