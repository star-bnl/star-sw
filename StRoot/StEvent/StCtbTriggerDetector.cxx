/***************************************************************************
 *
 * $Id: StCtbTriggerDetector.cxx,v 2.4 2000/05/09 10:22:17 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbTriggerDetector.cxx,v $
 * Revision 2.4  2000/05/09 10:22:17  ullrich
 * Updated to cope with modified dst_TrgDet.idl
 *
 * Revision 2.3  1999/12/20 12:54:45  ullrich
 * Adapted changed in trigger table dst_TrgDet
 *
 * Revision 2.2  1999/10/28 22:24:58  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:28  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StCtbTriggerDetector.h"
#include "tables/St_dst_TrgDet_Table.h"

static const char rcsid[] = "$Id: StCtbTriggerDetector.cxx,v 2.4 2000/05/09 10:22:17 ullrich Exp $";

ClassImp(StCtbTriggerDetector)

StCtbTriggerDetector::StCtbTriggerDetector()
{
    int i, j, k;
    for(i=0; i<mMaxTrays; i++)
	for(j=0; j<mMaxSlats; j++)
	    for(k=0; k<mMaxEventSamples; k++) {
		mMips[i][j][k] = 0;
		mTime[i][j][k] = 0;
	    }

    for(i=0; i<mMaxAux; i++)
	for(j=0; j<mMaxEventSamples; j++)
	    mAux[i][j] = 0;
    
    mNumberOfPreSamples = 0;
    mNumberOfPostSamples = 0;
}

StCtbTriggerDetector::StCtbTriggerDetector(const dst_TrgDet_st& t)
{
    int i, j, k;
    for(i=0; i<mMaxTrays; i++)
	for(j=0; j<mMaxSlats; j++)
	    for(k=0; k<mMaxEventSamples; k++) {
		mMips[i][j][k] = t.nCtb[i][j][k];
		mTime[i][j][k] = t.timeCtb[i][j][k];
	    }

    for(i=0; i<mMaxAux; i++)
	for(j=0; j<mMaxEventSamples; j++)
	    mAux[i][j] = t.ctbaux[i][j];
    
    mNumberOfPreSamples = t.npre;
    mNumberOfPostSamples = t.npost;
}

StCtbTriggerDetector::~StCtbTriggerDetector() {/* noop */}

UInt_t
StCtbTriggerDetector::numberOfTrays() const {return mMaxTrays;}

UInt_t
StCtbTriggerDetector::numberOfSlats() const {return mMaxSlats;}

UInt_t
StCtbTriggerDetector::numberOfAuxWords() const {return mMaxAux;}

UInt_t
StCtbTriggerDetector::numberOfPreSamples() const {return mNumberOfPreSamples;}

UInt_t
StCtbTriggerDetector::numberOfPostSamples() const {return mNumberOfPostSamples;}

Float_t
StCtbTriggerDetector::mips(UInt_t i, UInt_t j, UInt_t k) const
{
    return mMips[i][j][k];
}

Char_t
StCtbTriggerDetector::time(UInt_t i, UInt_t j, UInt_t k) const
{
    return mTime[i][j][k];
}
    
Float_t
StCtbTriggerDetector::aux(UInt_t i, UInt_t j) const
{
    return mAux[i][j];
}

void
StCtbTriggerDetector::setMips(UInt_t i, UInt_t j, UInt_t k, Float_t val)
{
    mMips[i][j][k] = val;
}

void
StCtbTriggerDetector::setTime(UInt_t i, UInt_t j, UInt_t k, Char_t val)
{
    mTime[i][j][k] = val;
}

void
StCtbTriggerDetector::setAux(UInt_t i, UInt_t j, Float_t val)
{
    mAux[i][j] = val;
}

void
StCtbTriggerDetector::setNumberOfPreSamples(UInt_t val)
{
    mNumberOfPreSamples = val;
}

void
StCtbTriggerDetector::setNumberOfPostSamples(UInt_t val)
{
    mNumberOfPostSamples = val;
}

