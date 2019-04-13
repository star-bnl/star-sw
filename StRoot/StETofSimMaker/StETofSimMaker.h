/***************************************************************************
 *
 * $Id$
 *
 * Author: Florian Seck, July 2018
 ***************************************************************************
 *
 * Description: StETofSimMaker - class to create StETofHits out of GEANT
 *              hits in several steps:
 *              - get hits from GEANT
 *              - combine different "hits" in several gas gaps into one hit
 *              - create a corresponding StETofHit with
 *                      -- smeared local x,y position according to detector
 *                         resolution
 *                      -- smeared time of flight
 *
 ***************************************************************************
 *
 * $Log$
 *
 ***************************************************************************/
#ifndef STETOFSIMMAKER_H
#define STETOFSIMMAKER_H

#include <string>
#include <vector>
#include <map>

#include "StMaker.h"

class TH1;
class StEvent;
class StETofCollection;
class St_g2t_ctf_hit;
struct g2t_ctf_hit_st;
class CombinedGeantHit;



class StETofSimMaker : public StMaker {

public:
    StETofSimMaker( const char *name = "etofSim" );
    ~StETofSimMaker();

    Int_t   Init();
    Int_t   InitRun(   Int_t );
    Int_t   FinishRun( Int_t );
    Int_t   Make();
    Int_t   Finish();

    void    setWriteHistos( const bool writeHist );
    void    setHistoFileName();


protected:

    // internal functions ----------------------------------------------------    
    bool    isFileExisting( const std::string& filename );
    void    combineRawHits();
    void    fastDetectorResponse();
    void    mergeClusters( vector< CombinedGeantHit >& v );
    void    fillEvent();

    vector< int > convertVolumeId( const int& );

    int     plane(   const int&  volume_id );
    int     sector(  const int&  volume_id );
    int     counter( const int&  volume_id );
    int     gap(     const int&  volume_id );
    int     strip(   const int&  volume_id );

    int     calcVolumeIndex(   const vector< int >& );
    int     calcDetectorIndex( const vector< int >& );

    void    fillRawHitsToHistograms( const St_g2t_ctf_hit* g2t_eto_hits );
    void    fillCombinedHitsToHistograms();
    void    fillMergedHitsToHistograms();

    void    bookHistograms();
    void    writeHistograms();

    // member variables ------------------------------------------------------
    St_DataSet*         mGeantData;
    StEvent*            mEvent;
    StETofCollection*   mETofCollection;

    map< int, vector< g2t_ctf_hit_st* > >  mRawHitMap;      // key: track pointer id, value: vector of hit pointers
    vector< CombinedGeantHit >             mCombinedHitVec;
    vector< CombinedGeantHit >             mMergedHitVec;

    vector< int >       mIdMultiplier;

    bool                mWriteHistos;
    std::string         mHistoFileName;

    std::string         mTotFileName;

    std::map< std::string, TH1* >  mHistograms;

    virtual const Char_t *GetCVS() const { static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs; }
    ClassDef( StETofSimMaker, 1 )
};


inline int  StETofSimMaker::plane(   const int&  volume_id )  { return   volume_id / mIdMultiplier.at( 0 ); }
inline int  StETofSimMaker::sector(  const int&  volume_id )  { return ( volume_id % mIdMultiplier.at( 0 ) ) / mIdMultiplier.at( 1 ); }
inline int  StETofSimMaker::counter( const int&  volume_id )  { return ( volume_id % mIdMultiplier.at( 1 ) ) / mIdMultiplier.at( 2 ); }
inline int  StETofSimMaker::gap(     const int&  volume_id )  { return ( volume_id % mIdMultiplier.at( 2 ) ) / mIdMultiplier.at( 3 ); }
inline int  StETofSimMaker::strip(   const int&  volume_id )  { return   volume_id % mIdMultiplier.at( 3 ); }


inline void StETofSimMaker::setWriteHistos( const bool writeHist ) { mWriteHistos = writeHist; }

#endif /* StETofSimMaker_h */
