/***************************************************************************
 *
 * $Id: StBbcTriggerDetector.cxx,v 2.14 2009/11/23 16:34:05 fisyak Exp $
 *
 * Author: Akio Ogawa, Jan 2002
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StBbcTriggerDetector.cxx,v $
 * Revision 2.14  2009/11/23 16:34:05  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.13  2009/02/03 15:52:12  fisyak
 * Add include
 *
 * Revision 2.12  2008/08/15 17:33:04  ullrich
 * Modified zVertex(). Implemented calibrated BBC vertex z position code.
 *
 * Revision 2.11  2007/10/11 21:39:56  ullrich
 * Removed unused variable i.
 *
 * Revision 2.10  2007/07/11 23:06:45  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.9  2007/04/24 14:52:23  ullrich
 * Fixed bug in BBC unpacking (Akio).
 *
 * Revision 2.8  2004/11/02 21:20:20  ullrich
 * Modifications by Akio for Run 5.
 *
 * Revision 2.7  2004/08/03 17:22:15  ullrich
 * Major update by Akio and Marco.
 *
 * Revision 2.6  2004/02/11 01:42:09  ullrich
 * Added new constructor to load data from StTriggerData.
 *
 * Revision 2.5  2003/09/02 17:58:05  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 2.4  2003/02/05 23:17:34  ullrich
 * Corrected bug in adcSumWest(). Index needed offset.
 *
 * Revision 2.3  2003/01/23 23:23:28  ullrich
 * Modified to cope with changes in how BBC data is loaded for Run3.
 *
 * Revision 2.2  2002/09/25 14:04:17  akio
 * Bug fix in the service functions, no change in data
 *
 * Revision 2.1  2002/01/03 20:57:37  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include <math.h>
#include "StBbcTriggerDetector.h"
#include "Stiostream.h"
#include "tables/St_dst_TrgDet_Table.h"
#include "StTriggerData.h"
#include "TMath.h"
static const char rcsid[] = "$Id: StBbcTriggerDetector.cxx,v 2.14 2009/11/23 16:34:05 fisyak Exp $";

ClassImp(StBbcTriggerDetector)
    
StBbcTriggerDetector::StBbcTriggerDetector()
{
    memset(mBeg,0,mEnd-mBeg);
    mYear=2002;
    mDSMVTX=0;
}

StBbcTriggerDetector::StBbcTriggerDetector(const dst_TrgDet_st& t)
{
    int east_q_map[24] = { 8  , 5  , 4  , 40 , 37 , 36 , 7  , 6  ,
		       3  , 2  , 1  , 39 , 38 , 35 , 34 , 33 ,
		       72 , 71 , 70 , 69 , 68 , 67 , 66 , 65 };
    int east_t_map[16] = { 16 , 13 , 12 , 48 , 45 , 44 , 15 , 14 ,
		       11 , 10 , 9  , 47 , 46 , 43 , 42 , 41 };    
    int west_q_map[24] = { 24 , 21 , 20 , 56 , 53 , 52 , 23 , 22 ,
		       19 , 18 , 17 , 55 , 54 , 51 , 50 , 49 ,
		       80 , 79 , 78 , 77 , 76 , 75 , 74 , 73 };
    int west_t_map[16] = { 32 , 29 , 28 , 64 , 61 , 60 , 31 , 30 ,
		       27 , 26 , 25 , 63 , 62 , 59 , 58 , 57 };

    memset(mBeg,0,mEnd-mBeg);
    int i;
    mYear=2003;
    for (i=0; i<24; i++){ 
	mAdc[i   ] = (unsigned short) t.BBC[east_q_map[i]-1]; 
	mAdc[i+24] = (unsigned short) t.BBC[west_q_map[i]-1]; 
    }
    for (i=0; i<16; i++){ 
	mTdc[i   ] = (unsigned short) t.BBC[east_t_map[i]-1]; 
	mTdc[i+24] = (unsigned short) t.BBC[west_t_map[i]-1]; 
    }
    mDSMVTX = 0;
}

StBbcTriggerDetector::StBbcTriggerDetector(const StTriggerData& t)
{
    //
    //  This is a temporary fix only. In future this
    //  class will become obsolete and users should
    //  get this info from StTriggerData.
    //  This info is only a subset of what is available
    //  in StTriggerData.
    //  tu 2/10/2004
    //
    memset(mBeg,0,mEnd-mBeg);
    int i,maxt;
    mYear = t.year();
    if (mYear<2005)
        maxt=16;
    else
        maxt=24;

    for (i=0; i<24; i++){ 
	mAdc[i   ] = (unsigned short) t.bbcADC(east, i+1); 
	mAdc[i+24] = (unsigned short) t.bbcADC(west, i+1); 
    }
    for (i=0; i<maxt; i++){ 
	mTdc[i   ] = (unsigned short) t.bbcTDC(east, i+1); 
	mTdc[i+24] = (unsigned short) t.bbcTDC(west, i+1); 
    }
    mDSMVTX=t.bbcTimeDifference();
}

StBbcTriggerDetector::~StBbcTriggerDetector() {/* noop */}

unsigned short
StBbcTriggerDetector::adc(unsigned int pmt) const
{
    if (pmt < mMaxPMTs)
        return mAdc[pmt];
    else
        return 9999;
}

unsigned short
StBbcTriggerDetector::tdc(unsigned int pmt) const
{
    if (pmt < mMaxPMTs)
        return mTdc[pmt];
    else
        return 9999;
}

unsigned short
StBbcTriggerDetector::bbcRegister(unsigned int n) const
{
    if (n < mMaxRegisters)
        return mReg[n];
    else
        return 9999;
}

unsigned short
StBbcTriggerDetector::pedestalData(unsigned int n) const
{
    if (n < mMaxPedData)
        return mPed[n];
    else
        return 9999;
}

unsigned int
StBbcTriggerDetector::scalar(unsigned int n) const
{
    if (n < mMaxScalars)
        return mScl[n];
    else
        return 0;
}

void
StBbcTriggerDetector::setAdc(unsigned int pmt, unsigned short val)
{
    if (pmt < mMaxPMTs) mAdc[pmt] = val;
}

void
StBbcTriggerDetector::setTdc(unsigned int pmt, unsigned short val)
{
    if (pmt < mMaxPMTs) mTdc[pmt] = val;
}

void
StBbcTriggerDetector::setRegister(unsigned int id, unsigned short val)
{
    if (id < mMaxRegisters) mReg[id] = val;
}

void
StBbcTriggerDetector::setPedestal(unsigned int id, unsigned short val)
{
    if (id < mMaxPedData) mPed[id] = val;
}

void
StBbcTriggerDetector::setScalar(unsigned int id, unsigned int val)
{
    if (id < mMaxScalars) mScl[id] = val;
}

int
StBbcTriggerDetector::nHitEast()
{
    int nhit=0;
    if (mYear==2002) {
        for (int i=0; i<8;  i++) {if (tdc(i)>0 && tdc(i)<1500) {nhit++;}}
    }
    else if (mYear==2003) {
        for (int i=0; i<16; i++) {if (adc(i)>5)                {nhit++;}}
    }
    else {
        cerr << "StBbcTriggerDetector::nHitEast: No longer supported after 2003. Do not use!" << endl;
    }
    return nhit;
}

int
StBbcTriggerDetector::nHitWest()
{
    int nhit=0;
    if (mYear==2002)      {
        for (int i=0; i<8;  i++) {if (tdc(i+16)>0 && tdc(i+16)<1500) {nhit++;}}
    }
    else if (mYear==2003) {
        for (int i=0; i<16; i++) {if (adc(i+24)>5)                   {nhit++;}}
    }
    else{
        cerr << "StBbcTriggerDetector::nHitWest: No longer supported after 2003. Do not use!" << endl;
    }
    return nhit;
}

int
StBbcTriggerDetector::nHitEastLarge()
{
    int nhit=0;
    if (mYear==2002)      {
        for (int i=8;  i<15; i++) {if (tdc(i)>0 && tdc(i)<1500) {nhit++;}}  //exclude 15 = clock
    }  
    else if (mYear==2003) {
        for (int i=16; i<24; i++) {if (adc(i)>5)                {nhit++;}}
    }
    else{
        cerr << "StBbcTriggerDetector::nHitEastlarge: No longer supported after 2003. Do not use!" << endl;
    }
    return nhit;
}

int
StBbcTriggerDetector::nHitWestLarge()
{
    int nhit=0;
    if (mYear==2002)      {
        for (int i=8;  i<15; i++) {if (tdc(i+16)>0 && tdc(i+16)<1500) {nhit++;}}  //exclude 15 = clock
    } 
    else if (mYear==2003) {
        for (int i=16; i<24; i++) {if (adc(i+24)>5)                   {nhit++;}}
    }
    else {
        cerr << "StBbcTriggerDetector::nHitWestLarge: No longer supported after 2003. Do not use!" << endl;
    }
    return nhit;
}

int
StBbcTriggerDetector::nHit() {return nHitEast()+nHitWest();}

int
StBbcTriggerDetector::nHitLarge() {return nHitEastLarge()+nHitWestLarge();}

int
StBbcTriggerDetector::nHitAll() {return nHit()+nHitLarge();}

int
StBbcTriggerDetector::adcSumEast()
{
    int sum=0;
    if (mYear==2002)      {
        for (int i=0; i<8;  i++){sum+=adc(i);}
    }
    else if (mYear>=2003) {
        for (int i=0; i<16; i++){sum+=adc(i);}
    }
    return sum;
}

int
StBbcTriggerDetector::adcSumWest()
{
    int sum=0;
    if (mYear==2002)      {
        for (int i=0; i<8;  i++){sum+=adc(i+16);}
    }
    else if (mYear>=2003) {
        for (int i=0; i<16; i++){sum+=adc(i+24);}
    }
    return sum;
}

int
StBbcTriggerDetector::adcSumEastLarge()
{
    int sum=0;
    if (mYear==2002)      {
        for (int i=8;  i<15; i++){sum+=adc(i);}   //exclude 15 = clock
    }
    else if (mYear>=2003) {
        for (int i=16; i<24; i++){sum+=adc(i);}
    }
    return sum;
}

int
StBbcTriggerDetector::adcSumWestLarge()
{
    int sum=0;
    if (mYear==2002)      {
        for (int i=8;  i<15; i++){sum+=adc(i);}    //exclude 15 = clock
    }
    else if (mYear>=2003) {
        for (int i=16; i<24; i++){sum+=adc(i+24);}
    }
    return sum;
}

int
StBbcTriggerDetector::adcSum(){return adcSumEast()+adcSumWest();}

int
StBbcTriggerDetector::adcSumLarge(){return adcSumEastLarge()+adcSumWestLarge();}

int
StBbcTriggerDetector::adcSumAll() {return adcSum()+adcSumLarge();}

int 
StBbcTriggerDetector::tdcEarliestEast()
{
    if (mYear==2002){
	unsigned short earliest=2000;
	for (int i=0; i<8; i++){
	    if (tdc(i)>0 && tdc(i)<earliest ) earliest=tdc(i);
	}
	if (earliest<2000)
	    return earliest;
	else
	    return -9999;			
    }
    else if (mYear==2003){
	unsigned short earliest=0;
	for (int i=0; i<16; i++){
	    if (tdc(i)<245 && tdc(i)>earliest && adc(i)>5 ) earliest=tdc(i);
	}
	if (earliest>0)
	    return earliest;
	else
	    return -9999;
    }
    else{
        cerr << "StBbcTriggerDetector::tdcEarliestEast: No longer supported after 2003. Do not use!" << endl;
    }
    return -9999;
}

int 
StBbcTriggerDetector::tdcEarliestWest()
{
    if (mYear==2002){
	unsigned short earliest=2000;
	for (int i=16; i<24; i++){
	    if (tdc(i)>0 && tdc(i)<earliest ) earliest=tdc(i);
	}
	if (earliest<2000)
	    return earliest;
	else
	    return -8888;			
    }
    else if (mYear==2003){
	unsigned short earliest=0;
	for (int i=24; i<40; i++){
	    if (tdc(i)<245 && tdc(i)>earliest && adc(i)>5 ) earliest=tdc(i);
	}
	if (earliest>0)
	    return earliest;
	else
	    return -8888;
    }
    else{
        cerr << "StBbcTriggerDetector::tdcEarliestWest: No longer supported after 2003. Do not use!" << endl;
    }
    return -8888;
}

float
StBbcTriggerDetector::zVertex()
{
    if (mYear==2002){
        unsigned short east=tdcEarliestEast();
        unsigned short west=tdcEarliestWest();
        if (east<2000 && west<2000 && east!=0 && west!=0)
	  return (float(east-west))*5.0; //sign for common start
        else
	  return -9999.0;			       
    }
    else if (mYear==2003){
        unsigned short east=tdcEarliestEast();
        unsigned short west=tdcEarliestWest();
        if (east>0 && west>0)
	  return float(west-east)*3.0; //sign for common stop 
        else
	  return -9999.0;
    }
    else{
        float slope=2.0;
        float tc[4]={49.89, -3.155, 4.537, -0.1471};
        float off=-13.77l;
        float offset[2][16] = {
            {-2.00982,  4.09827, 4.83372, -4.71373,
             -8.33428,  -8.51657, -2.16193, 14.6726,
             1.61522,  0.605228, 4.98944, -1.86768,
             -2.46653, -2.70569, -4.33065, -5.07069},
            {-4.90599,  -2.62884, -2.87498, 5.58336,
             0.100682,  0.065195, -0.35114, -2.96953,
             1.2031,  2.94575, -1.93577, -1.13641,
             1.86768, 4.27345, 7.0431, -1.16071}
        };
        float maxtac[2]={0.0, 0.0};
        for(int iew=0; iew<2; iew++){
            for(int ich=0; ich<16; ich++){
                int j=iew*24+ich;	  
                float tac = tdc(j) - offset[iew][ich] - (tc[0]+tc[1]/TMath::Exp(-tc[2]*TMath::Power(adc(j),tc[3])))/slope;
                if(adc(j)>10 && tac<200 && tac>maxtac[iew]) {maxtac[iew]=tac;}
            }
        }
        if(maxtac[0]>0.0 && maxtac[1]>0.0){
            float zvt=slope*(maxtac[1]-maxtac[0])+off;
            return zvt;
        }
    }
    return -9999.0;
}

void
StBbcTriggerDetector::dump()
{
    unsigned int i;
    cout << "BBC data (" << mYear << ") dump" << endl;
    if (mYear==2002){
        cout << "East small tile ADC "; for (i=0; i<8; i++){cout << adc(i)    << " ";}; cout << endl;
        cout << "East large tile ADC "; for (i=0; i<8; i++){cout << adc(i+8)  << " ";}; cout << endl;
        cout << "West small tile ADC "; for (i=0; i<8; i++){cout << adc(i+16) << " ";}; cout << endl;
        cout << "West large tile ADC "; for (i=0; i<8; i++){cout << adc(i+24) << " ";}; cout << endl;
        cout << "East small tile TDC "; for (i=0; i<8; i++){cout << tdc(i)    << " ";}; cout << endl;
        cout << "East large tile TDC "; for (i=0; i<8; i++){cout << tdc(i+8)  << " ";}; cout << endl;
        cout << "West small tile TDC "; for (i=0; i<8; i++){cout << tdc(i+16) << " ";}; cout << endl;
        cout << "West large tile TDC "; for (i=0; i<8; i++){cout << tdc(i+24) << " ";}; cout << endl;
        cout << "Number of hits east " << nHitEast() << " west " << nHitWest() << " Total " << nHit() << endl;
        cout << "ADC sum east " << adcSumEast() << " west " << adcSumWest() << " Total " << adcSum() << endl;
        cout << "z vertex position = " << zVertex() << " cm" << endl;
        cout << "Registers = "; for (i=0; i<mMaxRegisters; i++){cout << bbcRegister(i) << " ";}; cout << endl;
        cout << "Scalars = "; for (i=0; i<mMaxScalars; i++){cout << scalar(i) << " ";}; cout << endl;
    }
    else if (mYear>2002) {
        cout << "East small tile ADC "; for (i=0; i<16; i++){cout << adc(i)    << " ";}; cout << endl;
        cout << "West small tile ADC "; for (i=0; i<16; i++){cout << adc(i+24) << " ";}; cout << endl;
        cout << "East small tile TDC "; for (i=0; i<16; i++){cout << tdc(i)    << " ";}; cout << endl;
        cout << "West small tile TDC "; for (i=0; i<16; i++){cout << tdc(i+24) << " ";}; cout << endl;
        cout << "East large tile ADC "; for (i=0; i<8; i++) {cout << adc(i+16) << " ";}; cout << endl;
        cout << "West large tile ADC "; for (i=0; i<8; i++) {cout << adc(i+40) << " ";}; cout << endl;
        if (mYear>2004) {
	  cout << "East large tile TDC "; for (i=0; i<8; i++) {cout << tdc(i+16) << " ";}; cout << endl;
	  cout << "West large tile TDC "; for (i=0; i<8; i++) {cout << tdc(i+40) << " ";}; cout << endl;
        }
        cout << "Number of hits east " << nHitEast() << " west " << nHitWest() << " Total " << nHit() << endl;
        cout << "ADC sum east " << adcSumEast() << " west " << adcSumWest() << " Total " << adcSum() << endl;
        cout << "Earliest TDC east " << tdcEarliestEast() << " west " << tdcEarliestWest() << endl;
        cout << "z vertex position = " << zVertex() << " cm" << endl;
    }
}
