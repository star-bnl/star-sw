//EemcHit.h
//R. Fatemi (IUCF)
//9/04

#ifndef EemcHit_HH
#define EemcHit_HH

#include "StThreeVectorD.hh"
#include <iostream>
using namespace std;

#include "StJetMaker/StEmcHitMakers/EmcHit.h"

class EEmcDbItem;

class EemcHit : public EmcHit
{
public: 
    EemcHit() {mId=kEndcapEmcTowerId ;}
    virtual ~EemcHit() {};

    //initialize:
    void setSector(int v);
    void setSubSector(int v);
    void setEtaBin(int v);

    //Detector indexing
    int sector() const;
    int subSector() const;
    int etaBin() const;
    
    //Database item.  Apparently this is only good for real data, and not for simulation, so check for void.
    const EEmcDbItem* dbItem(StEEmcDbMaker*);
    
protected:

    int mSector;//sector: [1,12] and indexed from 1!!!!!
    int mSubSector;//subsector 1-5
    int mEtaBin;//etabin 1-12
};

//inlines --------------------------------

inline ostream& operator<<(ostream& os, const EemcHit& h)
{
    return os <<"adc:\t"<<h.adc()<<"\te:\t"<<h.energy()<<"\teta:\t"<<h.position().pseudoRapidity()<<"\tphi:\t"<<h.position().phi();
}

inline void EemcHit::setSector(int v)
{
    mSector=v;
}

inline void EemcHit::setSubSector(int v)
{
    mSubSector=v;
}

inline void EemcHit::setEtaBin(int v)
{
    mEtaBin=v;
}

inline int EemcHit::sector() const
{
    return mSector;
}

inline int EemcHit::subSector() const
{
    return mSubSector;
}

inline int EemcHit::etaBin() const
{
    return mEtaBin;
}
    
#endif
