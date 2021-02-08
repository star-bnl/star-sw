/***************************************************************************
 *
 * $Id: StETofMatchMaker.h,v 1.1 2019/02/19 19:52:28 jeromel Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: StETofMatchMaker - class to match StETofHits to tracks.
 *              The matching is done in several steps:
 *              - get a list of eTOF hits
 *              - check for each track if its helix has an intersection with
 *                the eTOF active volumes (MRPCs gas gaps)
 *              - resolve matching ambiguities 
 *
 ***************************************************************************
 *
 * $Log: StETofMatchMaker.h,v $
 * Revision 1.1  2019/02/19 19:52:28  jeromel
 * Reviewed code provided by F.Seck
 *
 *
 ***************************************************************************/
#ifndef STETOFMATCHMAKER_H     
#define STETOFMATCHMAKER_H

#include <string>
#include <vector>
#include <map>

#include "StMaker.h"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"

class StEvent;
class StMuDst;
class StETofGeometry;
class StTrack;
class StMuTrack;
class StTrackGeometry;

class TH1;

/// ETOF track class
class ETofTrack{
    public:
        ETofTrack():pt(-999.),eta(-999.),phi(-999.),nFtPts(0),nDedxPts(0),flag(0),nHitsPoss(999),dEdx(-999.),nSigmaPion(-999.){};
        ETofTrack( const StTrack *sttrack );
        ETofTrack( const StMuTrack *mutrack );
        ~ETofTrack(){}

        Double_t pt;
        Double_t eta;
        Double_t phi;
        Int_t    nFtPts;   
        Int_t    nDedxPts;   
        Int_t    flag;   
        Int_t    nHitsPoss;
        Double_t dEdx; 
        Double_t nSigmaPion; 
};


class StETofMatchMaker : public StMaker {

private:
    struct StructETofHit{
        Int_t           sector;
        Int_t           plane;
        Int_t           counter;
        Int_t           strip;
        Double_t        localX;
        Double_t        localY;
        Double_t        tot;
        Double_t        clusterSize;
        Int_t           index2ETofHit;
        StThreeVectorD  globalPos;
        Int_t           trackId;
        Double_t        theta;
        Int_t           matchFlag;
        Double_t        deltaX;
        Double_t        deltaY;
        Double_t        beta;
        Double_t        pathLength;
        Double_t        tof;
    };

    typedef std::vector< StructETofHit > eTofHitVec;
    typedef std::vector< StructETofHit >::iterator eTofHitVecIter;

public:
    StETofMatchMaker( const char* name = "etofMatch" );  // default constructor
    ~StETofMatchMaker();
    
    Int_t  Init();              // process start-up options
    Int_t  InitRun(   Int_t );  // initialize geometry
    Int_t  FinishRun( Int_t );  // clean up geometry
    Int_t  Make();              // main match algorithm
    Int_t  Finish();            // print summary & write QA histograms



    /// read matching parameters from file
    void setFileNameMatchParam( const char* fileName );

    void setOuterGeometry( const bool outerGeom  );
    void setIsSim( const bool isSim );
    void setDoQA(  const bool doQA  );  
    void setDebug( const bool debug );



private:
    // internal subfunctions ----------------------------------------------------------------------   
    StETofGeometry* etofGeometry() const; // method to retrieve the ETofGeom

    void    readETofDetectorHits(   eTofHitVec& detectorHitVec  );
    void    findTrackIntersections( eTofHitVec& intersectionVec );

    void    fillIndexToPrimaryMap();
    void    cleanUpTraits();

    bool    validTrack( const StTrack*   );
    bool    validTrack( const StMuTrack* );
    bool    validTrack( const ETofTrack& );

    void    extrapolateTrackToETof( eTofHitVec& intersectionVec, const StPhysicalHelixD& theHelix, const int& iNode, int& nCrossings );
    
    void    matchETofHits(          eTofHitVec& detectorHitVec,      eTofHitVec& intersectionVec, eTofHitVec& matchCandVec );
    void    sortSingleMultipleHits( eTofHitVec& matchCandVec,        eTofHitVec& singleTrackMatchVec, std::vector< eTofHitVec >& multiTrackMatchVec );
    void    finalizeMatching(       eTofHitVec& singleTrackMatchVec, eTofHitVec& finalMatchVec );

    void    fillPidTraits(         eTofHitVec& finalMatchVec );
    void    calculatePidVariables( eTofHitVec& finalMatchVec );

    double  startTime();
    double  etofPathLength( const StThreeVectorD& beginPoint, const StThreeVectorD& endPoint, const double& curvature );
    double  expectedTimeOfFlight( const double& pathLength, const double& momentum, const double& mass );

    void    fillQaHistograms( eTofHitVec& finalMatchVec );
    void    bookHistograms();
    void    setHistFileName();
    void    writeHistograms();

    StTrackGeometry* trackGeometry( StTrack* ) const; // get track geometry for (outer) helix


    // internal containers ------------------------------------------------------------------------
    StEvent*          mEvent;
    StMuDst*          mMuDst;
    StETofGeometry*   mETofGeom;   // pointer to the ETof geometry utility class

    std::string       mFileNameMatchParam;   // name of parameter file for matching parameters

    Bool_t            mIsStEventIn;
    Bool_t            mIsMuDstIn;

    Bool_t            mOuterTrackGeometry;  // if true -> use outer track geometry for extrapolation
    Bool_t            mIsSim;
    Bool_t            mDoQA;
    Bool_t            mDebug;


    std::map< Int_t, Int_t >  mIndex2Primary;

    Double_t                  mMatchRadius;
    std::vector< Double_t >   mTrackCuts;

    std::string                    mHistFileName;
    std::map< std::string, TH1* >  mHistograms;

    virtual const Char_t *GetCVS() const { static const char cvs[]="Tag $Name:  $Id: built " __DATE__ " " __TIME__ ; return cvs; }

    ClassDef( StETofMatchMaker, 0 )
};


inline void StETofMatchMaker::setFileNameMatchParam( const char* fileName )  { mFileNameMatchParam  = fileName;   }
inline void StETofMatchMaker::setOuterGeometry(      const bool outerGeom  ) { mOuterTrackGeometry  = outerGeom;  }
inline void StETofMatchMaker::setIsSim( const bool isSim ) { mIsSim = isSim; }
inline void StETofMatchMaker::setDoQA(  const bool doQA  ) { mDoQA  = doQA;  }
inline void StETofMatchMaker::setDebug( const bool debug ) { mDebug = debug; }

inline StETofGeometry* StETofMatchMaker::etofGeometry() const { return mETofGeom; }

#endif