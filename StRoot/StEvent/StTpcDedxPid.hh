/***************************************************************************
 *
 * $Id: StTpcDedxPid.hh,v 1.1 1999/04/08 14:56:30 ullrich Exp $
 *
 * Author: Craig Ogilvie, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcDedxPid.hh,v $
 * Revision 1.1  1999/04/08 14:56:30  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcDedxPid_hh
#define StTpcDedxPid_hh
#include "StDedxPid.hh"

class StTpcDedxPid : public StDedxPid {
public: 
    StTpcDedxPid(const StGlobalTrack&);
    ~StTpcDedxPid();
    
    int detectorInfoAvailable() const;
    int meetsStandardPid() const;
    double numberOfSigma(double mass) const;
    double meanPidFunction(double mass) const;
    double sigmaPidFunction(double mass) const;
};


#endif
