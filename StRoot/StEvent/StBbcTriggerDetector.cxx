/***************************************************************************
 *
 * $Id: StBbcTriggerDetector.cxx,v 2.2 2002/09/25 14:04:17 akio Exp $
 *
 * Author: Akio Ogawa, Jan 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StBbcTriggerDetector.cxx,v $
 * Revision 2.2  2002/09/25 14:04:17  akio
 * Bug fix in the service functions, no change in data
 *
 * Revision 2.1  2002/01/03 20:57:37  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StBbcTriggerDetector.h"

static const char rcsid[] = "$Id: StBbcTriggerDetector.cxx,v 2.2 2002/09/25 14:04:17 akio Exp $";

ClassImp(StBbcTriggerDetector)

StBbcTriggerDetector::StBbcTriggerDetector()
{
    int i;
    for(i=0; i<mMaxPMTs; i++){mAdc[i] = 0; mTdc[i] = 0;}
    for(i=0; i<mMaxRegisters; i++){mReg[i] = 0;}
    for(i=0; i<mMaxPedData; i++){mPed[i] = 0;}
    for(i=0; i<mMaxScalars; i++){mScl[i] = 0;}
}

StBbcTriggerDetector::~StBbcTriggerDetector() {/* noop */}

unsigned short
StBbcTriggerDetector::adc(unsigned int pmt) const
{
    if(pmt < mMaxPMTs) return mAdc[pmt];
    else return 9999;
}

unsigned short
StBbcTriggerDetector::tdc(unsigned int pmt) const
{
    if(pmt < mMaxPMTs) return mTdc[pmt];
    else return 9999;
}

unsigned short
StBbcTriggerDetector::bbcRegister(unsigned int n) const
{
    if(n < mMaxRegisters) return mReg[n];
    else return 9999;
}

unsigned short
StBbcTriggerDetector::pedestalData(unsigned int n) const
{
    if(n < mMaxPedData) return mPed[n];
    else return 9999;
}

unsigned int
StBbcTriggerDetector::scalar(unsigned int n) const
{
    if(n < mMaxScalars) return mScl[n];
    else return 0;
}

void
StBbcTriggerDetector::setAdc(unsigned int pmt, unsigned short val)
{
    if(pmt < mMaxPMTs) mAdc[pmt] = val;
}

void
StBbcTriggerDetector::setTdc(unsigned int pmt, unsigned short val)
{
    if(pmt < mMaxPMTs) mTdc[pmt] = val;
}

void
StBbcTriggerDetector::setRegister(unsigned int id, unsigned short val)
{
    if(id < mMaxRegisters) mReg[id] = val;
}

void
StBbcTriggerDetector::setPedestal(unsigned int id, unsigned short val)
{
    if(id < mMaxPedData) mPed[id] = val;
}

void
StBbcTriggerDetector::setScalar(unsigned int id, unsigned int val)
{
    if(id < mMaxScalars) mScl[id] = val;
}

int
StBbcTriggerDetector::nHitEast()
{
    int nhit=0;
    for(int i=0; i<8; i++) {if(tdc(i)>0 && tdc(i)<1500) {nhit++;}}
    return nhit;
}

int
StBbcTriggerDetector::nHitWest()
{
    int nhit=0;
    for(int i=0; i<8; i++) {if(tdc(i+16)>0 && tdc(i+16)<1500) {nhit++;}}
    return nhit;
}

int
StBbcTriggerDetector::nHitAll(){return nHitEast()+nHitWest();}

int
StBbcTriggerDetector::adcSumEast()
{
    int sum=0;
    for(int i=0; i<8; i++){sum+=adc(i);}
    return sum;
}

int
StBbcTriggerDetector::adcSumWest()
{
    int sum=0;
    for(int i=0; i<8; i++){sum+=adc(i+16);}
    return sum;
}

int
StBbcTriggerDetector::adcSumAll(){return adcSumEast()+adcSumWest();}

float
StBbcTriggerDetector::zVertex()
{
    int i;
    unsigned short east=2000, west=2000;
    for(i=0; i<8; i++){
	if(tdc(i)>0    && tdc(i)<east   ) east=tdc(i);
	if(tdc(i+16)>0 && tdc(i+16)<west) west=tdc(i+16);
    }
    if(east<2000 && west<2000) return (float(east)-float(west))*5.0;
    else return -9999.0;			       
}

void
StBbcTriggerDetector::dump()
{
    unsigned int i;
    cout << "BBC data dump" << endl;
    cout << "East small tile ADC "; for(i=0; i<8; i++){cout << adc(i)    << " ";}; cout << endl;
    cout << "East large tile ADC "; for(i=0; i<8; i++){cout << adc(i+8)  << " ";}; cout << endl;
    cout << "West small tile ADC "; for(i=0; i<8; i++){cout << adc(i+16) << " ";}; cout << endl;
    cout << "West large tile ADC "; for(i=0; i<8; i++){cout << adc(i+24) << " ";}; cout << endl;
    cout << "East small tile TDC "; for(i=0; i<8; i++){cout << tdc(i)    << " ";}; cout << endl;
    cout << "East large tile TDC "; for(i=0; i<8; i++){cout << tdc(i+8)  << " ";}; cout << endl;
    cout << "West small tile TDC "; for(i=0; i<8; i++){cout << tdc(i+16) << " ";}; cout << endl;
    cout << "West large tile TDC "; for(i=0; i<8; i++){cout << tdc(i+24) << " ";}; cout << endl;
    cout << "Number of hits east " << nHitEast() << " west " << nHitWest() << " Total " << nHitAll() << endl;
    cout << "ADC sum east " << adcSumEast() << " west " << adcSumWest() << " Total " << adcSumAll() << endl;
    cout << "z vertex position = " << zVertex() << " cm" << endl;
    cout << "Registers = "; for(i=0; i<mMaxRegisters; i++){cout << bbcRegister(i) << " ";}; cout << endl;
    cout << "Scalars = "; for(i=0; i<mMaxScalars; i++){cout << scalar(i) << " ";}; cout << endl;
}
