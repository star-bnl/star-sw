//EmcHit.h
//M.L. Miller (MIT)
//9/04

#ifndef EmcHit_HH
#define EmcHit_HH

#include "StThreeVectorD.hh"
#include <iostream>
using namespace std;

#include "StDetectorId.h"

class EmcHit
{
public:
    EmcHit() {};
    virtual ~EmcHit() {};

    //initialize:
    void setRawAdc(int v);
    void setAdc(double v);
    void setEnergy(double v);
    void setPosition(const StThreeVectorD& v);
    void setCorrectedPosition(const StThreeVectorD& v);

    //energy and adc
    int rawAdc() const;
    double adc() const;
    double energy() const;

    //location of center of tower. NOTE: before you use the eta, you must correct for the vertex position.
    const StThreeVectorD& position() const;

    //corrected position of the emc-hit (see position())
    const StThreeVectorD& correctedPosition() const;

    //type of hit (set automatically by derived class constructor)
    StDetectorId detectorId() const {return mId;}


protected:

    int mRawAdc;//no pedestal subtraction
    double mAdc; //ped subtracted
    double mEnergy;//energy from calibration
    StThreeVectorD mPosition;
    StThreeVectorD mCorrectedPosition;
    StDetectorId mId;
};

//inlines --------------------------------

inline ostream& operator<<(ostream& os, const EmcHit& h)
{
    return os <<"adc:\t"<<h.adc()<<"\te:\t"<<h.energy()<<"\teta:\t"<<h.position().pseudoRapidity()<<"\tphi:\t"<<h.position().phi();
}

inline void EmcHit::setRawAdc(int v)
{
    mRawAdc=v;
}

inline void EmcHit::setAdc(double v)
{
    mAdc=v;
}

inline void EmcHit::setEnergy(double v)
{
    mEnergy=v;
}

inline void EmcHit::setPosition(const StThreeVectorD& v)
{
    mPosition=v;
}

inline void EmcHit::setCorrectedPosition(const StThreeVectorD& v)
{
    mCorrectedPosition=v;
}

inline int EmcHit::rawAdc() const
{
    return mRawAdc;
}

inline double EmcHit::adc() const
{
    return mAdc;
}

inline double EmcHit::energy() const
{
    return mEnergy;
}

inline const StThreeVectorD& EmcHit::position() const
{
    return mPosition;
}

inline const StThreeVectorD& EmcHit::correctedPosition() const
{
    return mCorrectedPosition;
}

#endif
