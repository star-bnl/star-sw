/***************************************************************************
 *
 * $Id: StFourPMaker.h,v 1.1 2003/04/04 21:35:32 thenry Exp $
 * $Log: StFourPMaker.h,v $
 * Revision 1.1  2003/04/04 21:35:32  thenry
 * Base class for creating lists of four vectors for use with the
 * StJetMaker
 *
 * Revision 1.0  2003/02/27 21:38:10  thenry
 * Created by Thomas Henry
 *
 * Author: Thomas Henry February 2003
 ***************************************************************************
 *
 * Description:  Maker which creates a list of Four Momentums (base class)
 *
 ***************************************************************************/
#ifndef StFourPMaker_h
#define StFourPMaker_h
#include "StMaker.h"

#define MAXTRACKS 10000

class StEvent;
class StEmcClusterCollection;
class StEmcPoint;
class StMuDst;
class StMuEmcCollection;
class StMuDstMaker;
class StMuTrackFourVec;

class StFourPMaker : public StMaker {
public:
    StFourPMaker(const char *name, StMuDstMaker *pevent);
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    StMuEmcCollection* getStMuEmcCollection(void);
    StMuTrackFourVec* getTracks() { return tracks; };
    Int_t numTracks(void) { return nTracks; };

protected:
    StMuEmcCollection* muEmcCol;    //!
    StMuTrackFourVec* tracks;       //!
    Int_t nTracks;                  //!
    StMuDstMaker *muDst;           //!

    float me, mp, mpi, mk;         //!

    ClassDef(StFourPMaker,1)
};
#endif
