/***************************************************************************
 *
 * $Id: StFourPMaker.h,v 1.4 2008/04/05 18:40:23 tai Exp $
 * $Log: StFourPMaker.h,v $
 * Revision 1.4  2008/04/05 18:40:23  tai
 * cleaned up the codes
 *
 * Revision 1.3  2004/11/30 19:01:42  mmiller
 * Back compatibility for pre P04k bemc corrupt events
 *
 * Revision 1.2  2004/10/25 22:19:09  mmiller
 * Updates to deal with new storage/retreival of emc data from mudst.
 *
 * Revision 1.1  2004/10/12 18:18:04  mmiller
 * Add StFourPMakers subdirectory
 *
 * Revision 1.2  2004/09/14 17:27:15  mmiller
 * Fixed bug (lack of StFourPMaker::Clear()).
 *
 * Revision 1.1  2004/07/08 15:41:03  mmiller
 * First release of StJetMaker.  Mike's commit of full clean of StJetFinder, StJetMaker, and StSpinMaker.  builds and runs in dev.
 *
 * Revision 1.5  2003/09/10 19:47:20  perev
 * ansi corrs
 *
 * Revision 1.4  2003/08/27 23:19:49  thenry
 * Added track level eta cutting.
 *
 * Revision 1.3  2003/05/09 21:01:34  thenry
 * removed "../" from #include statement
 *
 * Revision 1.2  2003/04/24 14:15:16  thenry
 * These changes are really the first working version of the StFourPMakers
 * and teh StJetMakers.  This is all c++ stl implementation, and by virtue of
 * that fact, StEmcTpcFourPMaker bady needs to be speed optimized.
 *
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
#include <map>
#include "StMaker.h"
#include "StJetFinder/StProtoJet.h"
#include "StJetMaker/StMuTrackFourVec.h"

class StEvent;
class StEmcClusterCollection;    
typedef map<long, StMuTrackFourVec, less<long> > TrackPile;

class StEmcPoint;
class StMuDst;
class StMuEmcCollection;
class StMuDstMaker;

typedef StProtoJet::FourVecList FourList;

class StFourPMaker : public StMaker {
public:
    StFourPMaker(const char *name, StMuDstMaker *pevent);
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
    virtual void Clear(const Option_t*);

    FourList &getTracks() { return tracks; };
    Int_t numTracks(void) { return tracks.size(); };

    void SetEtaLow(Float_t eta_low) { eta_low_lim = eta_low; };
    void SetEtaHigh(Float_t eta_high) { eta_high_lim = eta_high; };
    Float_t GetEtaLow(void) const { return eta_low_lim; };
    Float_t GetEtaHigh(void) const { return eta_high_lim; };

    ///Access to BEMC event corruption flag
    bool bemcCorrupt() const {return mCorrupt;}

    TrackPile tPile; 
protected:

    FourList tracks;       //!
    StMuDstMaker *muDst;           //!
    ///Is this event BEMC corrupted?
    bool mCorrupt; 


public:
    const double me;	
    const double mpr;
    const double mpi;
    const double mk;

    Float_t          eta_high_lim;
    Float_t          eta_low_lim;

    ClassDef(StFourPMaker,0)
};
#endif



