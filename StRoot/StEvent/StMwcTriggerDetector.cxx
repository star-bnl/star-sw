/***************************************************************************
 *
 * $Id: StMwcTriggerDetector.cxx,v 2.2 1999/10/28 22:26:07 ullrich Exp $
 *
 * Author: Thomas Ullrich, Sep 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMwcTriggerDetector.cxx,v $
 * Revision 2.2  1999/10/28 22:26:07  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
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
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StMwcTriggerDetector.cxx,v 2.2 1999/10/28 22:26:07 ullrich Exp $";

ClassImp(StMwcTriggerDetector)

StMwcTriggerDetector::StMwcTriggerDetector()
{
    fill_n(mMips, static_cast<int>(mMaxMwcSectors), 0);
}

StMwcTriggerDetector::StMwcTriggerDetector(const dst_TrgDet_st& t)
{
    copy(t.nMwc+0, t.nMwc+mMaxMwcSectors, mMips);
}

StMwcTriggerDetector::~StMwcTriggerDetector() {/* noop */}

UInt_t
StMwcTriggerDetector::numberOfMwcSectors() const {return mMaxMwcSectors;}

Float_t
StMwcTriggerDetector::mips(UInt_t i) const
{
    if (i < mMaxMwcSectors)
        return mMips[i];
    else
        return 0;
}

void
StMwcTriggerDetector::setMips(UInt_t i, Float_t val)
{
    if (i < mMaxMwcSectors)
        mMips[i] = val;
}
