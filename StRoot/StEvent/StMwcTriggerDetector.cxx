/***************************************************************************
 *
 * $Id: StMwcTriggerDetector.cxx,v 2.8 2018/03/15 22:00:50 smirnovd Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcTriggerDetector.cxx,v $
 * Revision 2.8  2018/03/15 22:00:50  smirnovd
 * Revert removal of StMwcTriggerDetector constructor
 *
 * This constructor was removed by Yuri on 2009-11-23 leaving the declaration
 * behind in the corresponding header file. However, this constructor is used in
 * existing StTriggerDetectorCollection()
 *
 * Revision 2.7  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.6  2007/07/11 23:06:45  perev
 * Cleanup+fix StXXXTriggerDetector
 *
 * Revision 2.5  2001/04/05 04:00:51  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

static const char rcsid[] = "$Id: StMwcTriggerDetector.cxx,v 2.8 2018/03/15 22:00:50 smirnovd Exp $";

ClassImp(StMwcTriggerDetector)

StMwcTriggerDetector::StMwcTriggerDetector()
{
    memset(mBeg,0,mEnd-mBeg);
}

StMwcTriggerDetector::StMwcTriggerDetector(const dst_TrgDet_st& t)
{
    memset(mBeg,0,mEnd-mBeg);
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

unsigned int
StMwcTriggerDetector::numberOfSectors() const {return mMaxSectors;}

unsigned int
StMwcTriggerDetector::numberOfSubSectors() const {return mMaxSubSectors;}

unsigned int
StMwcTriggerDetector::numberOfPreSamples() const {return mNumberOfPreSamples;}

unsigned int
StMwcTriggerDetector::numberOfPostSamples() const {return mNumberOfPostSamples;}

unsigned int
StMwcTriggerDetector::numberOfAuxWords() const {return mMaxAux;}

float
StMwcTriggerDetector::mips(unsigned int i, unsigned int j, unsigned int k) const
{
    return mMips[i][j][k];
}

float
StMwcTriggerDetector::aux(unsigned int i, unsigned int j) const
{
    return mAux[i][j];
}

void
StMwcTriggerDetector::setMips(unsigned int i, unsigned int j, unsigned int k, float val)
{
    mMips[i][j][k] = val;
}

void
StMwcTriggerDetector::setAux(unsigned int i, unsigned int j, float val)
{
    mAux[i][j] = val;
}

void
StMwcTriggerDetector::setNumberOfPreSamples(unsigned int val)
{
    mNumberOfPreSamples = val;
}

void
StMwcTriggerDetector::setNumberOfPostSamples(unsigned int val)
{
    mNumberOfPostSamples = val;
}
