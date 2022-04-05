/***************************************************************************
 *
 * $Id: StRHICfCollection.cxx,v 2.1 2018/12/11 19:52:21 ullrich Exp $
 *
 * Author: Akio Ogawa, Minho Kim
 ***************************************************************************
 *
 * Description: RHICf data a
 *
 ***************************************************************************
 *
 * $Log: StRHICfCollection.cxx,v $
 * Revision 2.1  2018/12/11 19:52:21  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#include "StEvent/StRHICfCollection.h"

static const char rcsid[] = "$Id: StRHICfCollection.cxx,v 2.1 2018/12/11 19:52:21 ullrich Exp $";

ClassImp(StRHICfCollection)

StRHICfCollection::StRHICfCollection()
{
    cout << "reset();" << endl;
    reset();
}

StRHICfCollection::~StRHICfCollection() { /* no op */ }

void StRHICfCollection::reset()
{
    mLocalRunNumber = 0;
    mLocalEventNumber = 0;
    mTrgm = 0;
    memset(mPlateE,         0,sizeof(mPlateE));
    memset(mBarE,           0,sizeof(mBarE));
    memset(mCad,            0,sizeof(mCad));
    memset(mTdc,            0,sizeof(mTdc));
    memset(mGpio,           0,sizeof(mGpio));
}

bool  StRHICfCollection::check(unsigned int tower, unsigned int plate)
{
    if(tower>=kRHICfNtower) return false;
    if(plate>=kRHICfNplate) return false;
    
    return true;
}

bool StRHICfCollection::check(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int bar)
{
    if(tower>=kRHICfNtower) return false;
    if(layer>=kRHICfNlayer) return false;
    if(xy>=kRHICfNxy) return false;
    if(tower==0 && bar>=kRHICfNbarSmall) return false;
    if(tower==1 && bar>=kRHICfNbarLarge) return false;
    
    return true;
}

bool StRHICfCollection::checkCad(unsigned int num)
{
    if(num>=kRHICfNcad) return false;
    
    return true;
}

bool StRHICfCollection::checkTdc(unsigned int num)
{
    if(num>=kRHICfNtdc) return false;
    
    return true;
}

bool StRHICfCollection::checkGpio(unsigned int num)
{
    if(num>=kRHICfNgpio) return false;
    
    return true;
}

//=========== Set ===========//

void StRHICfCollection::setLocalRunNumber(unsigned int run) {mLocalRunNumber=run;}
void StRHICfCollection::setLocalEventNumber(unsigned int event) {mLocalEventNumber=event;}
void StRHICfCollection::setTrgm(unsigned int trgm) {mTrgm=trgm;}

void StRHICfCollection::setPlateE(unsigned int tower, unsigned int plate, float E)
{
    if(check(tower,plate)) mPlateE[tower][plate]=E;
}

void StRHICfCollection::setBarE(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int bar, float E)
{
    if(check(tower,layer,xy,bar)) mBarE[tower][layer][xy][bar]=E;
}

void StRHICfCollection::setCad(unsigned int cadnum, unsigned int cad)
{
    if(checkCad(cadnum)) mCad[cadnum]=cad;
}

void StRHICfCollection::setTdc(unsigned int tdcnum, unsigned int tdc)
{
    if(checkTdc(tdcnum)) mTdc[tdcnum]=tdc;
}

void StRHICfCollection::setGpio(unsigned int gpionum, unsigned int gpio)
{
    if(checkGpio(gpionum)) mGpio[gpionum]=gpio;
}

//=========== Get ===========//

unsigned int StRHICfCollection::localRunNumber() const { return mLocalRunNumber; }
unsigned int StRHICfCollection::localEventNumber() const { return mLocalEventNumber; }
unsigned int StRHICfCollection::trgm() const { return mTrgm; }

float StRHICfCollection::plateE(unsigned int tower, unsigned int plate)
{
    if(check(tower,plate)) return mPlateE[tower][plate];
    return 0.;
}

float StRHICfCollection::barE(unsigned int tower, unsigned int layer, unsigned int xy, unsigned int bar)
{
    if(check(tower,layer,xy,bar)) return mBarE[tower][layer][xy][bar];
    return 0.;
}

unsigned int StRHICfCollection::cad(unsigned int cadnum)
{
    if(checkCad(cadnum)) return mCad[cadnum];
    return 0;
}

unsigned int StRHICfCollection::tdc(unsigned int tdcnum)
{
    if(checkTdc(tdcnum)) return mTdc[tdcnum];
    return 0;
}

unsigned int StRHICfCollection::gpio(unsigned int gpionum)
{
    if(checkGpio(gpionum)) return mGpio[gpionum];
    return 0;
}

void StRHICfCollection::print(int option)
{
    cout << Form("Local Run# = %d",mLocalRunNumber) << endl;
    cout << Form("Local Evt# = %d",mLocalEventNumber) << endl;
    cout << Form("Trgm = %d", mTrgm) << endl;
    
    for(int it=0;it<kRHICfNtower;it++){
        for(int ipl=0;ipl<kRHICfNplate;ipl++){
            printf("it:%d, ipl:%d, E = %f\n", it, ipl, mPlateE[it][ipl]);
        }
    }
    for(int it=0;it<kRHICfNtower;it++){
        for(int il=0;il<kRHICfNlayer;il++){
            for(int ixy=0;ixy<kRHICfNxy;ixy++){
                int nbar;
                if(it==0) nbar = kRHICfNbarSmall;
                if(it==1) nbar = kRHICfNbarLarge;
                for(int ibar=0;ibar<nbar;ibar++){
                    printf("it:%d, il%d, ixy:%d, ibar:%d, E = %f\n", it, il, ixy, ibar, mBarE[it][il][ixy][ibar]);
                }
            }
        }
    }
    
    for(int ic=0;ic<kRHICfNcad;ic++) printf("CAD = %d\n", mCad[ic]);
    for(int it=0;it<kRHICfNtdc;it++) printf("TDC = %d\n", mTdc[it]);
    for(int ig=0;ig<kRHICfNgpio;ig++) printf("GPIO = %d\n", mGpio[ig]);
}

