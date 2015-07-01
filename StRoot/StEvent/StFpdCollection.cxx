/***************************************************************************
 *
 * $Id: StFpdCollection.cxx,v 2.5 2003/09/02 17:58:05 perev Exp $
 *
 * Author: Akio Ogawa, Jan 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFpdCollection.cxx,v $
 * Revision 2.5  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.4  2002/09/25 14:04:17  akio
 * Bug fix in the service functions, no change in data
 *
 * Revision 2.3  2002/01/17 18:38:10  ullrich
 * Several new methods added. Bug fixed.
 *
 * Revision 2.2  2002/01/09 15:37:55  ullrich
 * AdcSum functions and scaler infos added.
 *
 * Revision 2.1  2002/01/03 20:57:36  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StFpdCollection.h"
#include "Stiostream.h"

static const char rcsid[] = "$Id: StFpdCollection.cxx,v 2.5 2003/09/02 17:58:05 perev Exp $";

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

const unsigned short*
StFpdCollection::adc() const {return mAdc;}

unsigned short*
StFpdCollection::tdc() {return mTdc;}

const unsigned short*
StFpdCollection::tdc() const {return mTdc;}

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

const unsigned short*
StFpdCollection::pedestal() const
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
StFpdCollection::north(unsigned int n) const
{
    if(n>0 && n<=mMaxPMTpEEMC) return mAdc[n+47]; 
    return 9999;
}

unsigned short
StFpdCollection::south(unsigned int n) const
{
    if(n>0 && n<=mMaxPMTPbg) return mAdc[n+31]; 
    return 9999;
}

unsigned short
StFpdCollection::top(unsigned int n) const
{
    if(n>0 && n<=mMaxPMTPbg) return mAdc[n-1]; 
    return 9999;
}

unsigned short
StFpdCollection::bottom(unsigned int n) const
{
    if(n>0 && n<=mMaxPMTPbg) return mAdc[n+15]; 
    return 9999;
}

unsigned short
StFpdCollection::smdx(unsigned int n) const
{
    if(n>0 && n<=mMaxPMTSmdX) return mAdc[n+59]; 
    return 9999;
}

unsigned short
StFpdCollection::smdy(unsigned int n) const
{
    if(n>0 && n<=mMaxPMTSmdY) return mAdc[n+119]; 
    return 9999;
}

unsigned short
StFpdCollection::pres1(unsigned int n) const
{
    if(n>0 && n<=mMaxPMTpEEMC) return mAdc[n+219]; 
    return 9999;
}

unsigned short
StFpdCollection::pres2(unsigned int n) const
{
    if(n>0 && n<=mMaxPMTpEEMC) return mAdc[n+235]; 
    return 9999;
}

unsigned short
StFpdCollection::southVeto() const
{
    return mAdc[253]; 
}

unsigned int
StFpdCollection::sumAdcNorth() const
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTpEEMC; i++){sum+=(unsigned int)north(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcSouth() const
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTPbg; i++){sum+=(unsigned int)south(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcTop() const
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTPbg; i++){sum+=(unsigned int)top(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcBottom() const
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTPbg; i++){sum+=(unsigned int)bottom(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcPreShower1() const
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTpEEMC; i++){sum+=(unsigned int)pres1(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcPreShower2() const
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTpEEMC; i++){sum+=(unsigned int)pres2(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcSmdX() const
{
    unsigned int sum=0;
    for(int i=1; i<=mMaxPMTSmdX; i++){sum+=(unsigned int)smdx(i);}
    return sum;
}

unsigned int
StFpdCollection::sumAdcSmdY() const
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
    cout << "SMD X      "; for(i=1; i<61;  i++){cout << smdx(i) << " ";}; cout << endl;
    cout << "SMD Y      "; for(i=1; i<101; i++){cout << smdy(i) << " ";}; cout << endl;
    cout << "South Veto  " << southVeto() << endl;
    cout << "Scalers    "; for(i=0; i<128; i++){printf(" %x ",mScl[i]);}; cout << endl;
}
