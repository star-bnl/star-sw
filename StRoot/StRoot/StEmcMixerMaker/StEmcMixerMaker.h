/*!\class StEmcMixerMaker
\author Alexandre A. P. Suaide

This is the basic embedding scheme for EMC. This maker gets the output
of two StEvent objects and merge the EMC hits of the second StEvent into
the first one. Before doing that, the maker erases all the clusters and
points, so cluster finder and point maker should run again. The order
of the StEvent objects are defined by the macro you are using to run
the maker. See ./macros for examples.

The basic HIT embedding was extracted by an old maker written by Subhasis.

This maker also have full conectivity to the EMC database so it will embed
hits only if the tower is active in the real data. This connectivity can be
disabled, if desired. If disabled, the full detector is active and the program
will embed all hits of the second event into the first.

This maker also has the feature of embed the global and primary tracks of
the second StEvent into the first but THIS IS NOT A TPC EMBED. This feature
can be turned off, if desired.
*/

#ifndef STAR_StEmcMixerMaker
#define STAR_StEmcMixerMaker
#include "StMaker.h"
#include "StMessMgr.h"
#include <TH1.h>
#include <TH2.h>

#define NDETECTORS 4
#define NMODULES 120
#define MAXCHANNELS 18000
#define EMCCHANNELS 4800
#define SMDCHANNELS 18000


class StEvent;
class StEmcGeom;
class StEmcRawHit;
                                                      
class StEmcMixerMaker : public StMaker 
{
  private:
    StEvent       *mEvent1;
    StEvent       *mEvent2;
    StEmcGeom     *mGeom[NDETECTORS];
    
    Int_t         mStatus[NDETECTORS][MAXCHANNELS];
    
    Bool_t        mClear;
    Bool_t        mAddHits;
    Bool_t        mUseDB;
    Bool_t        mFakeTrackEmbed;
    Bool_t        mEmbedAll;

    TH1F          *m_hit_1;
    TH1F          *m_hit_2;
    TH1F          *m_edep_1;
    TH1F          *m_edep_2;
    
    Int_t         mEvent1Source;
    Int_t         mEvent2Source;
  
    virtual Int_t addHits(); 
    virtual Int_t addTracks(); 
    void          clearPoints();
    void          clearClusters();
    Bool_t        getEvents();
    void          getDB();
    Bool_t        checkHit(Int_t, StEmcRawHit*);

  protected:

  public: 

                  StEmcMixerMaker(const char *name="emcEmbed");
    virtual       ~StEmcMixerMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
  
    void          printHits(StEvent*); ///< Prints all EMC hits in the StEvent object
    
    void          setClear(Bool_t a)        { mClear = a; } ///< Set kTRUE to clear EMC clusters and points
    void          setAddFlag(Bool_t a)      { mAddHits = a; } ///< Set kTRUE to embed hits
    void          setFakeTrack(Bool_t a)    { mFakeTrackEmbed = a; } ///< Set kTRUE to merge tracks
    void          setEmbedAll(Bool_t a)     { mEmbedAll = a; } ///< Set kTRUE to embedd all hits even if the first emcCollection has no hits
	void          setPrint(Bool_t a)        { LOG_INFO << "::setPrint() is obsolete.  Use logger config file to set verbosity instead." << endm; } ///< Obsolete function; users can control messages with logger config file.
    
    virtual const char *GetCVS() const {static const char cvs[]="Tag $Name:  $ $Id: StEmcMixerMaker.h,v 1.6 2014/08/06 11:43:05 jeromel Exp $ built " __DATE__ " " __TIME__ ; return cvs;}

  ClassDef(StEmcMixerMaker,0) 
};
#endif








