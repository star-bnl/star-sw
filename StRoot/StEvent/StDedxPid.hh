/***************************************************************************
 *
 * $Id: StDedxPid.hh,v 1.1 1999/04/08 14:56:33 ullrich Exp $
 *
 * Author: Craig Ogilvie and Thomas Ullrich, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StDedxPid.hh,v $
 * Revision 1.1  1999/04/08 14:56:33  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StDedxPid_hh
#define StDedxPid_hh

class StGlobalTrack;

class StDedxPid {
public: 
    StDedxPid(const StGlobalTrack&);
    virtual ~StDedxPid();

    virtual int    detectorInfoAvailable() const       = 0;
    virtual int    meetsStandardPid() const            = 0;
    virtual double numberOfSigma(double mass) const    = 0;
    virtual double meanPidFunction(double mass) const  = 0;
    virtual double sigmaPidFunction(double mass) const = 0;
    
protected:
    const StGlobalTrack& mTrack;
};


#endif
