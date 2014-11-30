/*!
 * \class StTrackMassFit 
 */
/***************************************************************************
 *
 * $Id: StTrackMassFit.h,v 1.2 2014/01/14 14:48:24 fisyak Exp $
 *
 * Author: Yuri Fisyak, April 5, 2013
 ***************************************************************************
 *
 * Description: Keep results of track mass fit
 *
 ***************************************************************************
 *
 * $Log: StTrackMassFit.h,v $
 * Revision 1.2  2014/01/14 14:48:24  fisyak
 * Freeze
 *
 * Revision 1.1.1.1  2013/07/23 14:13:30  fisyak
 *
 *
 * Revision 2.3  2013/07/16 14:29:04  fisyak
 * Restore mass fit tracks
 *
 * Revision 2.1  2013/04/05 15:08:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTrackMassFit_hh
#define StTrackMassFit_hh

#include "StTrack.h"
#include "KFParticle/KFParticle.h"

class StTrackMassFit;
std::ostream&  operator<<(std::ostream& os,  const StTrackMassFit& t);
class StTrackMassFit : public StTrack {
public:
  StTrackMassFit(int key = 0, KFParticle *particle = 0)  : mKFParticle((particle) ? new KFParticle(*particle) : 0) {setKey(key);}
    StTrackMassFit(const StTrackMassFit&);
    StTrackMassFit& operator=(const StTrackMassFit&);
    ~StTrackMassFit()  {SafeDelete(mKFParticle);}
    
    virtual StTrackType     type() const {return (!vertex()) ? massFit : massFitAtVx; }
    const KFParticle* kfParticle() const {return mKFParticle;}
    KFParticle*       kfParticle()       {return mKFParticle;}
    void setKFParticle(KFParticle* particle = 0) {mKFParticle = (particle) ? new KFParticle(*particle) : 0;}
    virtual void Print(Option_t *option="") const {std::cout << option << *this << std::endl; }
    
protected:
    KFParticle *mKFParticle;
    
    ClassDef(StTrackMassFit,1)
};
#endif
