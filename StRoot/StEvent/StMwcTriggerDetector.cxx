/***************************************************************************
 *
 * $Id: StMwcTriggerDetector.cxx,v 2.4 2000/05/09 10:22:25 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcTriggerDetector.cxx,v $
 * Revision 2.4  2000/05/09 10:22:25  ullrich
 * Updated to cope with modified dst_TrgDet.idl
 *
 * Revision 2.3  1999/12/21 15:09:04  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/10/28 22:26:07  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:58  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StMwcTriggerDetector.h"
#include "tables/St_dst_TrgDet_Table.h"

static const char rcsid[] = "$Id: StMwcTriggerDetector.cxx,v 2.4 2000/05/09 10:22:25 ullrich Exp $";

ClassImp(StMwcTriggerDetector)

StMwcTriggerDetector::StMwcTriggerDetector()
{
    int i, j, k;
    for(i=0; i<mMaxSectors; i++)
	for(j=0; j<mMaxSubSectors; j++)
	    for(k=0; k<mMaxEventSamples; k++)
		mMips[i][j][k] = 0;

    for(i=0; i<mMaxAux; i++)
	for(j=0; j<mMaxEventSamples; j++)
	    mAux[i][j] = 0;
    
    mNumberOfPreSamples = 0;
    mNumberOfPostSamples = 0;
}

StMwcTriggerDetector::StMwcTriggerDetector(const dst_TrgDet_st& t)
{
    int i, j, k;
    for(i=0; i<mMaxSectors; i++)
	for(j=0; j<mMaxSubSectors; j++)
	    for(k=0; k<mMaxEventSamples; k++) 
		mMips[i][j][k] = t.nMwc[i][j][k];
	    
    for(i=0; i<mMaxAux; i++)
	for(j=0; j<mMaxEventSamples; j++)
	    mAux[i][j] = t.mwcaux[i][j];
    
    mNumberOfPreSamples = t.npre;
    mNumberOfPostSamples = t.npost;
}

StMwcTriggerDetector::~StMwcTriggerDetector() {/* noop */}

UInt_t
StMwcTriggerDetector::numberOfSectors() const {return mMaxSectors;}

UInt_t
StMwcTriggerDetector::numberOfSubSectors() const {return mMaxSubSectors;}

UInt_t
StMwcTriggerDetector::numberOfPreSamples() const {return mNumberOfPreSamples;}

UInt_t
StMwcTriggerDetector::numberOfPostSamples() const {return mNumberOfPostSamples;}

UInt_t
StMwcTriggerDetector::numberOfAuxWords() const {return mMaxAux;}

Float_t
StMwcTriggerDetector::mips(UInt_t i, UInt_t j, UInt_t k) const
{
    return mMips[i][j][k];
}

Float_t
StMwcTriggerDetector::aux(UInt_t i, UInt_t j) const
{
    return mAux[i][j];
}

void
StMwcTriggerDetector::setMips(UInt_t i, UInt_t j, UInt_t k, Float_t val)
{
    mMips[i][j][k] = val;
}

void
StMwcTriggerDetector::setAux(UInt_t i, UInt_t j, Float_t val)
{
    mAux[i][j] = val;
}

void
StMwcTriggerDetector::setNumberOfPreSamples(UInt_t val)
{
    mNumberOfPreSamples = val;
}

void
StMwcTriggerDetector::setNumberOfPostSamples(UInt_t val)
{
    mNumberOfPostSamples = val;
}
