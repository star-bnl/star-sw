/***************************************************************************
 *
 * $Id: StFpdCollection.cxx,v 2.2 2002/01/09 15:37:55 ullrich Exp $
 *
 * Author: Akio Ogawa, Jan 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFpdCollection.cxx,v $
 * Revision 2.2  2002/01/09 15:37:55  ullrich
 * AdcSum functions and scaler infos added.
 *
 * Revision 2.1  2002/01/03 20:57:36  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StFpdCollection.h"
#include <iostream>

static const char rcsid[] = "$Id: StFpdCollection.cxx,v 2.2 2002/01/09 15:37:55 ullrich Exp $";

ClassImp(StFpdCollection)

StFpdCollection::StFpdCollection()
{
    int i;
    for(i=0; i<mMaxAdc; i++){mAdc[i] = 0;}
    for(i=0; i<mMaxTdc; i++){mTdc[i] = 0;}
    for(i=0; i<mMaxRegisters; i++){mReg[i] = 0;}
    for(i=0; i<mMaxPedestal; i++){mPed[i] = 0;}
    for(i=0; i<mMaxScalers; i++){mScl[i] = 0;}
}

StFpdCollection::~StFpdCollection() {/* noop */}

unsigned short*
StFpdCollection::adc() {return mAdc;}

unsigned short*
StFpdCollection::tdc() {return mTdc;}

unsigned short
StFpdCollection::registers(unsigned int n) const
{
    if(n < mMaxRegisters) return mReg[n];
    return 0;
}

unsigned int
StFpdCollection::scaler(unsigned int n) const
{
    if(n < mMaxScalers) return mScl[n];
    return 0;
}

unsigned short*
StFpdCollection::pedestal()
{
    return mPed;
}

void
StFpdCollection::setAdc(unsigned int n, unsigned short val)
{
    if(n < mMaxAdc) mAdc[n] = val;
}

void
StFpdCollection::setTdc(unsigned int n, unsigned short val)
{
    if(n < mMaxTdc) mTdc[n] = val;
}

void
StFpdCollection::setRegister(unsigned int n, unsigned short val)
{
    if(n < mMaxRegisters) mReg[n] = val;
}

void
StFpdCollection::setPedestal(unsigned int n, unsigned short val)
{
    if(n < mMaxPedestal) mPed[n] = val;
}

void
StFpdCollection::setScaler(unsigned int n, unsigned int val)
{
    if(n < mMaxScalers) mScl[n] = val;
}

unsigned short
StFpdCollection::north(unsigned int n)
{
    if(n>0 && n<=mMaxPMTpEEMC) return mAdc[n+48]; 
    return 9999;
}

unsigned short
StFpdCollection::south(unsigned int n)
{
    if(n>0 && n<=mMaxPMTPbg) return mAdc[n+32]; 
    return 9999;
}

unsigned short
StFpdCollection::top(unsigned int n)
{
    if(n>0 && n<=mMaxPMTPbg) return mAdc[n-1]; 
    return 9999;
}

unsigned short
StFpdCollection::bottom(unsigned int n)
{
    if(n>0 && n<=mMaxPMTPbg) return mAdc[n+16]; 
    return 9999;
}

unsigned short
StFpdCollection::smdx(unsigned int n)
{
    if(n>0 && n<=mMaxPMTSmdX) return mAdc[n+120]; 
    return 9999;
}

unsigned short
StFpdCollection::smdy(unsigned int n)
{
    if(n>0 && n<=mMaxPMTSmdY) return mAdc[n+60]; 
    return 9999;
}

unsigned short
StFpdCollection::pres1(unsigned int n)
{
    if(n>0 && n<=mMaxPMTpEEMC) return mAdc[n+220]; 
    return 9999;
}

unsigned short
StFpdCollection::pres2(unsigned int n)
{
    if(n>0 && n<=mMaxPMTpEEMC) return mAdc[n+236]; 
    return 9999;
}

unsigned int
StFpdCollection::sumAdcNorth()
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTpEEMC; i++){sum+=(unsigned int)north(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcSouth()
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTPbg; i++){sum+=(unsigned int)south(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcTop()
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTPbg; i++){sum+=(unsigned int)top(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcBottom()
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTPbg; i++){sum+=(unsigned int)bottom(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcPreShower1()
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTpEEMC; i++){sum+=(unsigned int)pres1(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcPreShower2()
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTpEEMC; i++){sum+=(unsigned int)pres2(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcSmdX()
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTSmdX; i++){sum+=(unsigned int)smdx(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcSmdY()
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTSmdY; i++){sum+=(unsigned int)smdy(i);}
    return sum;
}

void
StFpdCollection::setToken(unsigned int val)
{
    mToken=val;
}

void
StFpdCollection::dump()
{
    unsigned int i;
    cout << "FPD data dump     token " << mToken << endl;
    cout << "North  ADC "; for(i=1; i<13;  i++){cout << north(i)  << " ";}; cout << endl;
    cout << "South  ADC "; for(i=1; i<17;  i++){cout << south(i)  << " ";}; cout << endl;
    cout << "Top    ADC "; for(i=1; i<17;  i++){cout << top(i)    << " ";}; cout << endl;
    cout << "Bottom ADC "; for(i=1; i<17;  i++){cout << bottom(i) << " ";}; cout << endl;
    cout << "PreShower1 "; for(i=1; i<13;  i++){cout << pres1(i) << " ";}; cout << endl;
    cout << "PreShower2 "; for(i=1; i<13;  i++){cout << pres2(i) << " ";}; cout << endl;
    cout << "SMD X      "; for(i=1; i<101; i++){cout << smdx(i) << " ";}; cout << endl;
    cout << "SMD Y      "; for(i=1; i<61;  i++){cout << smdx(i) << " ";}; cout << endl;
    cout << "Scalers    "; for(i=1; i<128; i++){cout << mScl[i] << " ";}; cout << endl;
}
