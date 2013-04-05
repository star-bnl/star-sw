/*!
 * \class StTrackMassFit 
 */
/***************************************************************************
 *
 * $Id: StTrackMassFit.h,v 2.1 2013/04/05 15:08:41 ullrich Exp $
 *
 * Author: Yuri Fisyak, April 5, 2013
 ***************************************************************************
 *
 * Description: Keep results of track mass fit
 *
 ***************************************************************************
 *
 * $Log: StTrackMassFit.h,v $
 * Revision 2.1  2013/04/05 15:08:41  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#ifndef StTrackMassFit_hh
#define StTrackMassFit_hh

#include "StTrack.h"
#include "KFParticle.h"

class StTrackMassFit;
ostream&  operator<<(ostream& os,  const StTrackMassFit& t);

class StTrackMassFit : public StTrack {
public:
    StTrackMassFit(int key = 0, StTrackType type = massFit, KFParticle *particle = 0);
    StTrackMassFit(const StTrackMassFit&);
    StTrackMassFit& operator=(const StTrackMassFit&);
    ~StTrackMassFit();
    
    StTrackType     type() const;
    const StVertex* vertex() const;
    
    const KFParticle* kfParticle() const;
    KFParticle* kfParticle();
    void setKFParticle(KFParticle* kfParticle);
    void setType(StTrackType type = massFit);
    void Print(Option_t *option="") const;
    
protected:
    StTrackType mType;
    KFParticle *mKFParticle;
    
    ClassDef(StTrackMassFit,1)
};

inline StTrackMassFit::StTrackMassFit(int key, StTrackType type, KFParticle *particle) : mType(type), mKFParticle(particle) {setKey(key);}
inline StTrackMassFit::~StTrackMassFit() {SafeDelete(mKFParticle);}
inline StTrackType     StTrackMassFit::type() const  { return mType; }
inline const StVertex* StTrackMassFit::vertex() const  { return 0; }
inline const KFParticle* StTrackMassFit::kfParticle() const  {return mKFParticle;}
inline KFParticle* StTrackMassFit::kfParticle()  {return mKFParticle;}
inline void StTrackMassFit::setKFParticle(KFParticle* kfParticle) {mKFParticle=kfParticle;}
inline void StTrackMassFit::setType(StTrackType type) {mType = type;}
inline void StTrackMassFit::Print(Option_t *option) const {cout << option << *this << endl; }

#endif
