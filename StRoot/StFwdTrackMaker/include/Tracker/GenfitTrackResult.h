#ifndef GENFIT_TRACK_RESULT_H
#define GENFIT_TRACK_RESULT_H

#include <memory>
#include <vector>
#include "GenFit/FitStatus.h"
#include <map>
#include <unordered_map>

// Utility class for evaluating ID and QA truth
struct MCTruthUtils {

    static int dominantContribution(Seed_t hits, double &qa) {
        // track_id, hits on track
        std::unordered_map<int,int> truth;
        for ( auto hit : hits ) {
            FwdHit* fhit = dynamic_cast<FwdHit*>(hit);
            truth[ fhit->_tid ]++;
        }

        if ( truth.size() == 0 ){
            return -1;
        }

        using namespace std;
        using P = decltype(truth)::value_type;
        auto dom = max_element(begin(truth), end(truth), [](P a, P b){ return a.second < b.second; });

        // QA represents the percentage of hits which
        // vote the same way on the track
        if ( hits.size() > 0 )
            qa = double(dom->second) / double(hits.size()) ;
        else
            qa = 0;

        return dom->first;
    };
};

class EventStats {
    public:
    TString classname() const { return "EventStats"; }
    void reset(){
        mNumSeeds = 0;
        mNumEpdHits = 0; // across all track types, did we find an EPD hit?
        


        mAttemptedFits = 0;
        mGoodFits = 0;
        mFailedFits = 0;

        mAttemptedGlobalFits = 0;
        mFailedGlobalFits = 0;
        mGoodGlobalFits = 0;
        mFailedGlobalRefits = 0;
        mGoodGlobalRefits = 0;

        mNumFwdVertices = 0;
        mAttemptedPrimaryFits = 0;
        mGoodPrimaryFits = 0;
        mFailedPrimaryFits = 0;
        mFailedPrimaryRefits = 0;
        mGoodPrimaryRefits = 0;

        mAttemptedBeamlineFits = 0;
        mGoodBeamlineFits = 0;
        mFailedBeamlineFits = 0;
        mFailedBeamlineRefits = 0;
        mGoodBeamlineRefits = 0;

        mAttemptedSecondaryFits = 0;
        mGoodSecondaryFits = 0;
        mFailedSecondaryFits = 0;
        mFailedSecondaryRefits = 0;
        mGoodSecondaryRefits = 0;

        numGlobalFoundHits.clear();
        numBeamlineFoundHits.clear();
        numPrimaryFoundHits.clear();
        numSecondaryFoundHits.clear();

        mGlobalNumEpdFoundHits.clear();
        mBeamlineNumEpdFoundHits.clear();
        mPrimaryNumEpdFoundHits.clear();
        mSecondaryNumEpdFoundHits.clear();

        mStep1Duration.clear();
        mStep2Duration.clear();
        mStep3Duration.clear();
        mStep4Duration.clear();
        mSeedFindingDuration.clear();
        mGlobalFitDuration.clear();
        mBeamlineFitDuration.clear();
        mPrimaryFitDuration.clear();
        mSecondaryFitDuration.clear();
    }
    int mNumSeeds = 0;
    int mNumEpdHits = 0; // across all track types, did we find an EPD hit?
    

    int mAttemptedFits = 0;
    int mGoodFits = 0;
    int mFailedFits = 0;

    int mAttemptedGlobalFits = 0;
    int mFailedGlobalFits = 0;
    int mGoodGlobalFits = 0;
    int mFailedGlobalRefits = 0;
    int mGoodGlobalRefits = 0;

    int mNumFwdVertices = 0;
    int mAttemptedPrimaryFits = 0;
    int mGoodPrimaryFits = 0;
    int mFailedPrimaryFits = 0;
    int mFailedPrimaryRefits = 0;
    int mGoodPrimaryRefits = 0;

    int mAttemptedBeamlineFits = 0;
    int mGoodBeamlineFits = 0;
    int mFailedBeamlineFits = 0;
    int mFailedBeamlineRefits = 0;
    int mGoodBeamlineRefits = 0;

    int mAttemptedSecondaryFits = 0;
    int mGoodSecondaryFits = 0;
    int mFailedSecondaryFits = 0;
    int mFailedSecondaryRefits = 0;
    int mGoodSecondaryRefits = 0;

    vector<int> numGlobalFoundHits;
    vector<int> numBeamlineFoundHits;
    vector<int> numPrimaryFoundHits;
    vector<int> numSecondaryFoundHits;

    vector<int> mGlobalNumEpdFoundHits;
    vector<int> mBeamlineNumEpdFoundHits;
    vector<int> mPrimaryNumEpdFoundHits;
    vector<int> mSecondaryNumEpdFoundHits;

    vector<float> mStep1Duration;
    vector<float> mSeedFindingDuration;
    vector<float> mStep2Duration;
    vector<float> mStep3Duration;
    vector<float> mStep4Duration;
    vector<float> mGlobalFitDuration;
    vector<float> mBeamlineFitDuration;
    vector<float> mPrimaryFitDuration;
    vector<float> mSecondaryFitDuration;
};

class GenfitTrackResult {
public:
    GenfitTrackResult(){}
    GenfitTrackResult(  Seed_t &seed, std::shared_ptr<genfit::Track> track ) {
        set( seed, track );
    }
    ~GenfitTrackResult(){
        // Clear();
    }
    void Clear() {
        if ( mTrack ){
            mTrack->Clear();
            mTrack.reset(); // inform the shared pointer to release the memory
        }
    }
    void set(   Seed_t &seeds, std::shared_ptr<genfit::Track> track ){
        setSeed( seeds );
        setTrack( track );
    }
    void setSeed( Seed_t &seed ){
        mSeed = seed;
        mIdTruth = MCTruthUtils::dominantContribution( seed, mQaTruth );
        LOG_INFO << "GenFitTrackResult::mIdTruth = " << mIdTruth << ", QaTruth = " << mQaTruth << endm;
    }
    void setTrack( std::shared_ptr<genfit::Track> track ){
        if (track == nullptr) {
            LOG_ERROR << "GenfitTrackResult::setTrack called with nullptr track" << endm;
            this->mTrack                    = nullptr;
            this->mTrackRep                 = nullptr;

            this->mIsFitConverged           = false;
            this->mIsFitConvergedFully      = false;
            this->mIsFitConvergedPartially  = false;
            this->mNFailedPoints            = 99;
            this->mCharge                   = 0;
            this->mChi2                     = -1;
            return;
        }
        try {
            // this->track = new genfit::Track(*track);
            mTrack      = track;
            mTrack      ->setMcTrackId(mIdTruth);
            mStatus     = mTrack->getFitStatus();
            mTrackRep   = mTrack->getCardinalRep();

            mIsFitConverged          = mStatus->isFitConverged();
            mIsFitConvergedFully     = mStatus->isFitConvergedFully();
            mIsFitConvergedPartially = mStatus->isFitConvergedPartially();
            mNFailedPoints           = mStatus->getNFailedPoints();
            mCharge                  = mStatus->getCharge();
            mChi2                    = mStatus->getChi2();

            if ( mIsFitConverged ){
                LOG_INFO << "GTR Setting momentum from track" << endm;
                mMomentum = mTrackRep->getMom( mTrack->getFittedState(0, mTrackRep) );
            }
            LOG_DEBUG << "GenfitTrackResult::set Track successful" << endm;
        } catch ( genfit::Exception &e ) {
            LOG_ERROR << "Unable to set track -> GenfitException: " << e.what() << endm;
            this->mTrack                    = nullptr;
            this->mTrackRep                 = nullptr;

            this->mIsFitConverged           = false;
            this->mIsFitConvergedFully      = false;
            this->mIsFitConvergedPartially  = false;
            this->mNFailedPoints            = 99;
            this->mCharge                   = 0;
            this->mChi2                     = -1;
        }
    }

    /** @brief Set the DCA and primary vertex for the event
     *
     */
    void setDCA( TVector3 pv ){
        mPV = pv;
        if ( mTrack ){
            try {
                auto dcaState = mTrack->getFittedState( 0 );
                // this->mTrackRep->extrapolateToPoint( dcaState, mPV );
                TVector3 beamDirection = TVector3(0,0,1);
                this->mTrackRep->extrapolateToLine( dcaState, mPV, beamDirection );
                this->mDCA = dcaState.getPos();
            } catch ( genfit::Exception &e ) {
                LOG_ERROR << "CANNOT GET DCA : GenfitException: " << e.what() << endm;
                this->mDCA = TVector3(99,99,99);
            }

        }
    }
    size_t numFtt() const {
        size_t n = 0;
        for ( auto hit : mSeed ){
            if ( dynamic_cast<FwdHit*>(hit)->_detid == kFttId ){
                n++;
            }
        }
        return n;
    }
    size_t numFst() const {
        size_t n = 0;
        for ( auto hit : mSeed ){
            if ( dynamic_cast<FwdHit*>(hit)->_detid == kFstId ){
                n++;
            }
        }
        return n;
    }
    size_t numPV() const {
        size_t n = 0;
        for ( auto hit : mSeed ){
            if ( dynamic_cast<FwdHit*>(hit)->isPV() ){
                n++;
            }
        }
        return n;
    }

    void mergeSeeds( GenfitTrackResult &other ){
        // combine the unique Ftt and Fst seeds
        for ( auto hit : other.mSeed ){
            if ( std::find( mSeed.begin(), mSeed.end(), hit ) == mSeed.end() ){
                mSeed.push_back( hit );
            }
        }
    }

    Seed_t                          mSeed;
    TVector3                        mPV; // as a TVector3
    TVector3                        mMomentum;
    float                           mCharge = 0;
    float                           mChi2 = -1;
    genfit::FitStatus               *mStatus = nullptr;
    genfit::AbsTrackRep             *mTrackRep = nullptr;
    std::shared_ptr<genfit::Track>  mTrack = nullptr;
    bool                            mIsFitConverged = false;
    bool                            mIsFitConvergedFully = false;
    bool                            mIsFitConvergedPartially = false;
    size_t                          mNFailedPoints = 0;
    TVector3                        mDCA;
    int                             mIdTruth = -1;
    double                          mQaTruth = 0;

    int                             mIndex = 0;
    int                             mTrackType = 0;
    int                             mVertexIndex = 0;
    int                             mGlobalTrackIndex = 0;
}; // GenfitTrackResult

#endif // GENFIT_TRACK_RESULT_H