/***************************************************************************
 *
 * $Id: StL0Trigger.cxx,v 2.12 2006/07/18 14:41:01 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL0Trigger.cxx,v $
 * Revision 2.12  2006/07/18 14:41:01  ullrich
 * Removed 'else if (runNumber < 7000000)'. Originally John Nelson
 * suggested to replace 7000000 by 8000000 but Akio said that
 * converting the else if to an else is better for now.
 *
 * Revision 2.11  2005/10/10 19:24:38  ullrich
 * run number range check, and updated for run5.
 *
 * Revision 2.10  2004/08/03 17:22:16  ullrich
 * Major update by Akio and Marco.
 *
 * Revision 2.9  2003/07/29 19:50:53  ullrich
 * Fix for spin bits added.
 *
 * Revision 2.8  2003/04/30 20:59:23  ullrich
 * Modified bunchCrossingId7bit() to deal with run
 * numbers > 4000000. Code from Eleanor.
 *
 * Revision 2.7  2002/11/26 02:19:11  perev
 * StEventMaker ITTF modif
 *
 * Revision 2.6  2002/02/15 00:18:13  ullrich
 * Changed signature of bunchCrossingId7bit().
 *
 * Revision 2.5  2002/01/09 15:37:12  ullrich
 * Bunch crossing id and spin bit extraction functions added.
 *
 * Revision 2.4  2001/07/19 00:04:06  ullrich
 * Updated to handle new trigger info.
 *
 * Revision 2.3  2001/04/05 04:00:51  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.2  1999/12/21 15:08:59  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.1  1999/10/28 22:25:59  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.0  1999/10/12 18:42:24  ullrich
 * Completely Revised for New Version
 *
 **************************************************************************/
#include <algorithm>
#include "StL0Trigger.h"
#include "StTriggerData.h"
#include "tables/St_dst_L0_Trigger_Table.h"
#include "tables/St_dst_TrgDet_Table.h"

#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StL0Trigger.cxx,v 2.12 2006/07/18 14:41:01 ullrich Exp $";

ClassImp(StL0Trigger)

StL0Trigger::StL0Trigger() 
{
    mMwcCtbMultiplicity = 0;
    mMwcCtbDipole = 0;
    mMwcCtbTopology = 0;
    mMwcCtbMoment = 0;
    fill_n(mCoarsePixelArray, static_cast<int>(mMaxPixels), 0);
    mDsmInput = 0;
    mDetectorBusy = 0; 
    mTriggerToken = 0;
    mDsmAddress = 0;  
    mAddBits = 0;   
    fill_n(mLastDsmArray, static_cast<unsigned short>(mMaxLastDsm), 0);
    fill_n(mBcDataArray, static_cast<unsigned short>(mMaxBcData), 0);
}

void StL0Trigger::set(const dst_L0_Trigger_st *t)
{
    if (!t)          return;
    if (TestBit(1))  return;
    SetBit(1);
    mTriggerWord        = t->TriggerWd;
    mTriggerActionWord  = t->TriggerActionWd;
    mMwcCtbMultiplicity = t->MWC_CTB_mul;
    mMwcCtbDipole       = t->MWC_CTB_dipole;
    mMwcCtbTopology     = t->MWC_CTB_topology;
    mMwcCtbMoment       = t->MWC_CTB_moment;
    copy(t->CPA+0, t->CPA+mMaxPixels, mCoarsePixelArray);
    mDsmInput = t->DSMInput;
    mDetectorBusy = t->DetectorBusy; 
    mTriggerToken = t->TrgToken;
    mDsmAddress = t->DSMAddress;  
    mAddBits = t->addBits;   
}

void StL0Trigger::set(const dst_TrgDet_st* n)
{
    // from dst_TrgDet_st
    if (!n)  		return;
    if (TestBit(2))	return;
    SetBit(2);
    copy(n->lastDSM+0, n->lastDSM+mMaxLastDsm, mLastDsmArray);
    copy(n->BCdata +0, n->BCdata+mMaxBcData  , mBcDataArray );
}

void StL0Trigger::set(const StTriggerData* t)
{
    // from StTriggerData
    if(t){  	   
      SetBit(2);
      mTriggerWord = t->triggerWord();
      mTriggerActionWord = t->actionWord();
      mMwcCtbMultiplicity=0;
      mMwcCtbDipole=0;
      mMwcCtbTopology=0;
      mMwcCtbMoment=0;
      mDsmInput=t->dsmInput();
      mDetectorBusy=0;
      mTriggerToken=t->trgToken();
      mDsmAddress=t->dsmAddress();
      mAddBits = t->mAddBits();
      for(int i=0; i<mMaxLastDsm; i++){mLastDsmArray[i]=t->lastDSM(i);}
      for(int i=0; i<mMaxBcData;  i++){mBcDataArray[i]=t->bcData(i);}
    }
}

StL0Trigger::~StL0Trigger() { /* noop */ }

unsigned int
StL0Trigger::coarsePixelArraySize() {return mMaxPixels;}

int
StL0Trigger::coarsePixelArray(unsigned int i)
{
    if (i < mMaxPixels)
        return mCoarsePixelArray[i];
    else
        return 0;
}

unsigned int
StL0Trigger::lastDsmArraySize() const {return mMaxLastDsm;}

unsigned short
StL0Trigger::lastDsmArray(unsigned int i)
{
    if (i < mMaxLastDsm)
        return mLastDsmArray[i];
    else
        return 0;
}

unsigned int
StL0Trigger::bcDataArraySize() const {return mMaxBcData;}

unsigned short
StL0Trigger::bcDataArray(unsigned int i)
{
    if (i < mMaxBcData)
        return mBcDataArray[i];
    else
        return 0;
}   

int
StL0Trigger::mwcCtbMultiplicity() const { return mMwcCtbMultiplicity;}

int
StL0Trigger::mwcCtbDipole() const { return mMwcCtbDipole;}

int
StL0Trigger::mwcCtbTopology() const { return mMwcCtbTopology;}

int
StL0Trigger::mwcCtbMoment() const { return mMwcCtbMoment;}

unsigned short
StL0Trigger::dsmInput() const {return mDsmInput;}

unsigned char
StL0Trigger::detectorBusy() const {return mDetectorBusy;}

unsigned short
StL0Trigger::triggerToken() const {return mTriggerToken;}

unsigned short
StL0Trigger::dsmAddress() const {return mDsmAddress;}

unsigned char
StL0Trigger::addBits() const {return mAddBits;}

void
StL0Trigger::setMwcCtbMultiplicity(int val) { mMwcCtbMultiplicity = val; }

void
StL0Trigger::setMwcCtbDipole(int val) { mMwcCtbDipole = val; }
   
void
StL0Trigger::setMwcCtbTopology(int val) { mMwcCtbTopology = val; }

void
StL0Trigger::setMwcCtbMoment(int val) { mMwcCtbMoment = val; }

void
StL0Trigger::setCoarsePixelArray(unsigned int i, int val)
{
    if (i < mMaxPixels)
        mCoarsePixelArray[i] = val;
}

void
StL0Trigger::setDsmInput(unsigned short val) {mDsmInput = val;}

void
StL0Trigger::setDetectorBusy(unsigned char val) {mDetectorBusy = val;} 

void
StL0Trigger::setTriggerToken(unsigned short val) {mTriggerToken = val;}

void
StL0Trigger::setDsmAddress(unsigned short val) {mDsmAddress = val;}  

void
StL0Trigger::setAddBits(unsigned char val) {mAddBits = val;}   


void
StL0Trigger::setLastDsmArray(unsigned int i, unsigned short val)
{
    if (i < mMaxLastDsm)
        mLastDsmArray[i] = val;
}

void
StL0Trigger::setBcDataArray(unsigned int i, unsigned short val)
{
    if (i < mMaxBcData)
        mBcDataArray[i] = val;
}

unsigned int
StL0Trigger::bunchCrossingId7bit(int runNumber) const
{
    unsigned int b7dat = mBcDataArray[2];
    //
    //  This code was added after discussions with
    //  Akio, Jamie, and Eleanor. According to
    //  Eleanor it's just the the 7 least significant
    //  bits in mBcDataArray[2]. Jamie tested it and
    //  says it works.
    //  April 30, 2003 (tu)
    //
    if (runNumber>4000000)
	return b7dat & 0x7f;

    //
    //  This code is the original version from Hank and
    //  since it appears to work (Akio) I leave it as is.
    //  April 30, 2003 (tu)
    //
    int b7=0, ibits; 
    if(runNumber<3010000) { ibits=7; }  /* before run 3010* this should be 7 */
    else { ibits=6; }     

    for (int i=0; i<7; i++) {
      b7 += (!((b7dat>>(ibits-i)) & 0x1) << (i)) & 0x7f;
    }
    return b7;
}

unsigned int
StL0Trigger::bunchCrossingId() const
{
    unsigned long long bxinghi,bxing1,bxinglo,bxing,bx0,bx1,bx2;
    int b120;
    bxinghi = mBcDataArray[3];
    bxing1 =  mBcDataArray[10];
    bxinglo = (bxing1 << 16) + mBcDataArray[11];
    bxing = (bxinghi << 32) + bxinglo;
    bx0 = bxing;
    bx1 = bx0/120;
    bx2 = bx1 * 120;
    b120 = bx0 - bx2;
    return static_cast<unsigned int>(b120);
} 

int
StL0Trigger::spinBits(int runNumber) const
{
    int ldsm0,spin1,spin2,spin3,spin4;
    if (runNumber<4000000){
	ldsm0 = mLastDsmArray[0];
	spin1 = (ldsm0>>8) & 0x1;
	spin2 = (ldsm0>>9) & 0x1;
	spin3 = (ldsm0>>10) & 0x1;
	spin4 = (ldsm0>>11) & 0x1;
	return spin1+spin2*2+spin3*4+spin4*8;
    }
    else {
	ldsm0 = (mLastDsmArray[7]/16)%256;
	spin1 = (ldsm0/ 2)%2;
	spin2 = (ldsm0/ 4)%2;
	spin3 = (ldsm0/32)%2;
	spin4 = (ldsm0/64)%2;
	return spin1+spin2*2+spin3*4+spin4*8;
    }
} 

int
StL0Trigger::spinBitYellowUp(int runNumber) const
{
    if (runNumber<4000000){
	return (mLastDsmArray[0]>>8) & 0x1;
    }
    else {
	int ldsm0 = (mLastDsmArray[7]/16)%256;
	return (ldsm0/2)%2;
    }
} 

int
StL0Trigger::spinBitYellowDown(int runNumber) const
{
    if (runNumber<4000000){
	return (mLastDsmArray[0]>>9) & 0x1;
    }
    else {
	int ldsm0 = (mLastDsmArray[7]/16)%256;
	return (ldsm0/ 4)%2;
    }
} 

int
StL0Trigger::spinBitBlueUp(int runNumber) const
{
    if (runNumber<4000000){
	return (mLastDsmArray[0]>>9) & 0x1;
    }
    else {
	int ldsm0 = (mLastDsmArray[7]/16)%256;
	return (ldsm0/32)%2;
    }
} 

int
StL0Trigger::spinBitBlueDown(int runNumber) const
{
    if (runNumber<4000000){
	return (mLastDsmArray[0]>>10) & 0x1;
    }
    else {
	int ldsm0 = (mLastDsmArray[7]/16)%256;
	return (ldsm0/64)%2;
    }
} 
