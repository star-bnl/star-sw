/***************************************************************************
 *
 * $Id: StTpcDedxPid.hh,v 1.2 1999/05/20 16:17:37 ogilvie Exp $
 *
 * Author: Craig Ogilvie, April 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcDedxPid.hh,v $
 * Revision 1.2  1999/05/20 16:17:37  ogilvie
 * added static dedx calibration data members, set, get functions
 *
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
    StTpcDedxPid();
    ~StTpcDedxPid();
    
    int detectorInfoAvailable() const;
    int meetsStandardPid() const;
    double numberOfSigma(double mass) const;
    double meanPidFunction(double mass) const;
    double sigmaPidFunction(double mass) const;

    void setTpcDedxGain(double gain) ;
    void setTpcDedxOffset(double offset) ;
    void setTpcDedxRise(double rise) ;
    double getTpcDedxGain() ;
    double getTpcDedxOffset() ;
    double getTpcDedxRise() ;
private:
  static double mTpcDedxGain ;
  static double mTpcDedxOffset ;
  static double mTpcDedxRise ;  

};


#endif
