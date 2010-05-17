/*!
 * \class StBTofHeader 
 * \author Xin Dong, Nov 2008
 */
/***************************************************************************
 *
 * $Id: StBTofHeader.h,v 2.4 2010/05/17 17:47:16 ullrich Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description:
 *   Barrel TOF header data, contains the TOF data header, vpd summary and
 * other event-wise information.
 *
 ***************************************************************************
 *
 * $Log: StBTofHeader.h,v $
 * Revision 2.4  2010/05/17 17:47:16  ullrich
 * Increase version number to 2.
 *
 * Revision 2.3  2010/05/12 15:12:03  ullrich
 * Added member mNTzero and access methods.
 *
 * Revision 2.2  2009/01/15 00:45:27  ullrich
 * mTriggerTime becomes array, setVpdVz() gets default argument.
 *
 * Revision 2.1  2008/12/22 20:30:57  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#ifndef StBTofHeader_hh
#define StBTofHeader_hh

#include <Stiostream.h>
#include "StObject.h"
#include "StEnumerations.h"

class StBTofHeader : public StObject {
public:
    enum {MAXFIBER=4, MAXVPD=19, MAXVPDVZ=20};
    
    StBTofHeader();
    ~StBTofHeader();

    short          fiberHeader(int fiberId) const;
    unsigned int   fiberTriggerWord(int fiberId) const;
    unsigned int   vpdHitPattern(StBeamDirection eastwest) const;
    unsigned short numberOfVpdHits(StBeamDirection eastwest) const;
    bool           isVpdHit(StBeamDirection eastwest, int tubeId) const;
    float          vpdVz(int rank=0) const;
    double         tStart() const;
    double         tStartError() const;
    double         tDiff() const;
    double         vpdTime(StBeamDirection eastwest, int tubeId) const;
    unsigned int   triggerTime(int fiberId) const;
    int            nTzero() const;
    
    void         setFiberHeader(int fiberId, short val);
    void         setFiberTriggerWord(int fiberId, unsigned int val);
    void         setVpdHit(StBeamDirection eastwest, int tubeId);
    void         removeVpdHit(StBeamDirection eastwest, int tubeId);
    void         setVpdHitPattern(StBeamDirection eastwest, unsigned int val);
    void         setVpdVz(float vz, int rank=0);
    void         setTStart(double t);
    void         setTStartError(double t_err);
    void         setTDiff(double tdiff);
    void         setVpdTime(StBeamDirection eastwest, int tubeId, double t);
    void         setTriggerTime(unsigned int tdc, int fiberId);
    void         setNTzero(short n);

protected:
    Short_t      mFiberHeader[MAXFIBER];
    UInt_t       mFiberTriggerWord[MAXFIBER];
    UInt_t       mVpdHitPattern[2];
    Float_t      mVpdVz[MAXVPDVZ];
    Double_t     mTStart;
    Double_t     mTStartErr;
    Double_t     mTDiff;
    Double_t     mVpdTime[2][MAXVPD];
    UInt_t       mTriggerTime[MAXFIBER];
    Short_t      mNTzero;

    ClassDef(StBTofHeader,2)
};

#endif
