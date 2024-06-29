#ifndef FWD_TRACKER_H
#define FWD_TRACKER_H

#include "Fit/Fitter.h"
#include "TFile.h"
#include "TGraph2D.h"
#include "TH1.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TRandom3.h"
#include "TTree.h"
#include "TVector3.h"
#include "TLorentzVector.h"

#include <algorithm>
#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include <fstream>

#include "StFwdTrackMaker/include/Tracker/FwdHit.h"
#include "StFwdTrackMaker/include/Tracker/FwdDataSource.h"
#include "StFwdTrackMaker/include/Tracker/TrackFitter.h"

#include "Criteria/Criteria.h"
#include "Criteria/ICriterion.h"
#include "KiTrack/Automaton.h"
#include "KiTrack/SegmentBuilder.h"
#include "KiTrack/SubsetHopfieldNN.h"

#include "StFwdTrackMaker/include/Tracker/CriteriaKeeper.h"

#include "GenFit/FitStatus.h"
#include "GenFit/GFRaveVertexFactory.h"

#include "StFwdTrackMaker/FwdTrackerConfig.h"
#include "StFwdTrackMaker/Common.h"

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
        mAttemptedFits = 0;
        mFailedFits = 0;
        mGoodFits = 0;
        mGoodCardinals = 0;

        mAttemptedReFits = 0;
        mFailedReFits = 0;
        mGoodReFits = 0;
        mPossibleReFit = 0;
        mStep1Duration.clear();
        mStep2Duration.clear();
        mStep3Duration.clear();
        mStep4Duration.clear();
        mFitDuration.clear();
    }
    int mNumSeeds = 0;
    int mAttemptedFits = 0;
    int mFailedFits = 0;
    int mGoodFits = 0;
    int mGoodCardinals = 0;

    int mAttemptedReFits = 0;
    int mFailedReFits = 0;
    int mGoodReFits = 0;
    int mPossibleReFit = 0;
    vector<float> mStep1Duration;
    vector<float> mStep2Duration;
    vector<float> mStep3Duration;
    vector<float> mStep4Duration;
    vector<float> mFitDuration;
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
        if ( track ){
            track->Clear();
        }
    }
    void set(   Seed_t &seeds, std::shared_ptr<genfit::Track> track ){
        setSeeds( seeds );
        setTrack( track );
    }
    void setSeeds( Seed_t &seeds ){
        for ( auto seed : seeds ){
            if ( dynamic_cast<FwdHit*>(seed)->isFst() ){
                fstSeed.push_back( seed );
            } else {
                fttSeed.push_back( seed );
            }
        }
    }
    void setTrack( std::shared_ptr<genfit::Track> track ){
        try {
            // this->track = new genfit::Track(*track);
            this->track = track;
            this->status = this->track->getFitStatus();
            this->trackRep = this->track->getCardinalRep();

            this->isFitConverged = this->status->isFitConverged();
            this->isFitConvergedFully = this->status->isFitConvergedFully();
            this->isFitConvergedPartially = this->status->isFitConvergedPartially();
            this->nFailedPoints = this->status->getNFailedPoints();
            this->charge = this->status->getCharge();

            this->nPV = this->track->getNumPoints() - (numFTT() + numFST());
            chi2 = status->getChi2();

            LOG_DEBUG << "Converged?: " << isFitConverged << ", Fully?: " << isFitConvergedFully << ", Partially?: " << isFitConvergedPartially << ", nFailedPoints: " << nFailedPoints << ", charge: " << charge << ", chi2=" << chi2 << endm;
            LOG_DEBUG << "GTR has track->numPoints=" << this->track->getNumPoints() << " vs. numFST: " << numFST() << ", numFTT: " << numFTT() << endm;
            if ( isFitConvergedPartially == true && this->track->getNumPoints() != nFailedPoints ){
                this->momentum = this->trackRep->getMom( this->track->getFittedState(0, this->trackRep) );
            }
            LOG_DEBUG << "GenfitTrackResult::set Track successful" << endm;

        } catch ( genfit::Exception &e ) {
            LOG_ERROR << "CANNOT GET TRACK : GenfitException: " << e.what() << endm;
            this->track = nullptr;
            this->trackRep = nullptr;

            this->isFitConverged = false;
            this->isFitConvergedFully = false;
            this->isFitConvergedPartially = false;
            this->nFailedPoints = 99;
            this->charge = 0;
        }
    }

    size_t numFTT() const { return fttSeed.size();}
    size_t numFST() const { return fstSeed.size();}

    std::string Report(){
        std::stringstream ss;
        ss << "GenfitTrackResult::Print()" << "\n";
        if ( track ){
            ss << "Total numPoints on track: " << track->getNumPoints() << "\n";
        }
        ss << "\tFTT Seed: " << fttSeed.size() << " : [";
        for (auto hit : fttSeed) {
            ss << dynamic_cast<FwdHit *>(hit)->_id << ", ";
        }
        ss << "]\n";

        ss << "\tFST Seed: " << fstSeed.size() << " : [";
        for (auto hit : fstSeed) {
            ss << dynamic_cast<FwdHit *>(hit)->_id << ", ";
        }
        ss << "]\n";
        ss << "\tPV: " << pv.X() << ", " << pv.Y() << ", " << pv.Z() << "\n";
        ss << "\tMomentum: " << momentum.Pt() << ", " << momentum.Eta() << ", " << momentum.Phi() << "\n";
        ss << "\tCharge: " << charge << "\n";
        ss << "\tnPV: " << nPV << "\n";
        ss << "\tisFitConverged: " << isFitConverged << "\n";
        ss << "\tisFitConvergedFully: " << isFitConvergedFully << "\n";
        ss << "\tisFitConvergedPartially: " << isFitConvergedPartially << "\n";
        ss << "\tnFailedPoints: " << nFailedPoints << "\n";
        
        return ss.str();
    }

    void mergeSeeds( GenfitTrackResult &other ){
        // combine the unique Ftt and Fst seeds
        for ( auto hit : other.fttSeed ){
            if ( std::find( fttSeed.begin(), fttSeed.end(), hit ) == fttSeed.end() ){
                fttSeed.push_back( hit );
            }
        }
        for ( auto hit : other.fstSeed ){
            if ( std::find( fstSeed.begin(), fstSeed.end(), hit ) == fstSeed.end() ){
                fstSeed.push_back( hit );
            }
        }
    }

    Seed_t fttSeed;
    Seed_t fstSeed;
    TVector3 pv;
    TVector3 momentum;
    float charge;
    float chi2 = -1;
    size_t nPV = 0;
    genfit::FitStatus *status = nullptr;
    genfit::AbsTrackRep *trackRep = nullptr;
    std::shared_ptr<genfit::Track> track = nullptr;
    bool isFitConverged = false;
    bool isFitConvergedFully = false;
    bool isFitConvergedPartially = false;
    size_t nFailedPoints = 0;
    size_t theNumFTT = 0;
}; // GenfitTrackResult

class ForwardTrackMaker {
  public:
    ForwardTrackMaker() : mConfigFile("config.xml"), mEventVertex(-999, -999, -999) {
        // noop
    }

    const std::vector<GenfitTrackResult> &getTrackResults() const { return mTrackResults; }
    const std::vector<Seed_t> &getTrackSeeds() const { return mRecoTracks; }
    const EventStats &getEventStats() const { return mEventStats; }

    void Clear(){
        for ( auto gtr : mTrackResults ){
            gtr.Clear();
        }
        mTrackResults.clear();
    }

    /**
     * @brief Set the Config File object
     *
     * @param cf : config filename
     */
    void setConfigFile(std::string cf) {
        mConfigFile = cf;
    }

    /**
     * @brief Set the Save Criteria Values object
     *
     * @param save : true to save crit values
     */
    void setSaveCriteriaValues(bool save) {
        mSaveCriteriaValues = save;
    }

    // Adopt external configuration file
    void setConfig(FwdTrackerConfig cfg) { mConfig = cfg; }
    // Adopt external hit loader
    void setData(std::shared_ptr<FwdDataSource>data) { mDataSource = data; }

    /**
     * @brief Initialize FwdTracker
     *
     * @param geoCache : name of cached geometry file
     * @param genHistograms : generate histograms
     */
    virtual void initialize( TString geoCache, bool genHistograms) {
        mGeoCache = geoCache;
        mDoTrackFitting = mConfig.get<bool>("TrackFitter:active", true);

        if (!mConfig.exists("TrackFitter"))
            mDoTrackFitting = false;
    } //initialize

    /**
     * @brief Loads Criteria from XML configuration.
     * Utility function for loading criteria from XML config.
     * @param path : path in config to load
     * @return vector of ICriterion pointers
    */
    std::vector<KiTrack::ICriterion *> loadCriteria(string path) {

        std::vector<KiTrack::ICriterion *> crits;
        auto paths = mConfig.childrenOf(path);

        for (string p : paths) {
            string name = mConfig.get<string>(p + ":name", "");
            bool active = mConfig.get<bool>(p + ":active", true);

            if (false == active) {
                continue;
            }

            float vmin = mConfig.get<float>(p + ":min", 0);
            float vmax = mConfig.get<float>(p + ":max", 1);

            KiTrack::ICriterion * crit = nullptr;
            if ( name == "Crit2_BDT" ){
                // crit = new BDTCrit2( vmin, vmax );
                LOG_WARN << "BDT Criteria not implemented/out of date" << endm;
            } else {
                crit = KiTrack::Criteria::createCriterion(name, vmin, vmax);
            }

            crit->setSaveValues(mSaveCriteriaValues);

            if (mSaveCriteriaValues)
                crits.push_back(new CriteriaKeeper(crit)); // CriteriaKeeper intercepts values and saves them
            else
                crits.push_back(crit);

        }

        return crits;
    } // loadCriteria

    /**
     * @brief Clear the loaded criteria
     * @param crits : vector of ICriterion pointers to properly clear
     */
    void clearCriteria( std::vector<KiTrack::ICriterion *> &crits ){
        for ( size_t i = 0; i < crits.size(); i++ ){
            if ( crits[i] ){
                delete crits[i];
                crits[i] = nullptr;
            }
        }
        crits.clear();
    }

    /**
     * @brief Get the Criteria Values object
     *
     * @param crit_name : Criteria to get
     * @return std::vector<float> : list of values
     */
    std::vector<float> getCriteriaValues(std::string crit_name) {
        std::vector<float> em;
        if (mSaveCriteriaValues != true) {
            return em;
        }

        for (auto crit : mTwoHitCrit) {
            if (crit_name == crit->getName()) {
                auto critKeeper = static_cast<CriteriaKeeper *>(crit);
                return critKeeper->getValues();
            }
        }

        for (auto crit : mThreeHitCrit) {
            if (crit_name == crit->getName()) {
                auto critKeeper = static_cast<CriteriaKeeper *>(crit);
                return critKeeper->getValues();
            }
        }

        return em;
    } //getCriteriaValues

    /**
     * @brief Get the Criteria All Values object
     *
     * @param crit_name : Criteria values to get
     * @return std::vector<std::map < std::string , float >> : map of values
     */
    std::vector<std::map < std::string , float >> getCriteriaAllValues(std::string crit_name) {
        std::vector<std::map < std::string , float >> em;
        if (mSaveCriteriaValues != true) {
            return em;
        }

        for (auto crit : mTwoHitCrit) {
            if (crit_name == crit->getName()) {
                auto critKeeper = static_cast<CriteriaKeeper *>(crit);
                return critKeeper->getAllValues();
            }
        }

        for (auto crit : mThreeHitCrit) {
            if (crit_name == crit->getName()) {
                auto critKeeper = static_cast<CriteriaKeeper *>(crit);
                return critKeeper->getAllValues();
            }
        }

        return em;
    } // getCriteriaAllValues

    /**
     * @brief Get the Criteria Track Ids object
     *
     * @param crit_name : Name of criteria to get track ids for
     * @return std::vector<int> : list of track ids
     */
    std::vector<int> getCriteriaTrackIds(std::string crit_name) {
        std::vector<int> em;
        if (mSaveCriteriaValues != true) {
            return em;
        }

        for (auto crit : mTwoHitCrit) {
            if (crit_name == crit->getName()) {
                auto critKeeper = static_cast<CriteriaKeeper *>(crit);
                return critKeeper->getTrackIds();
            }
        }

        for (auto crit : mThreeHitCrit) {
            if (crit_name == crit->getName()) {
                auto critKeeper = static_cast<CriteriaKeeper *>(crit);
                return critKeeper->getTrackIds();
            }
        }

        return em;
    } //getCriteriaTrackIds

    /**
     * @brief Clear the saved values for two hit and three hit criteria
     *
     */
    void clearSavedCriteriaValues() {
        if (mSaveCriteriaValues != true) {
            return;
        }

        for (auto crit : mTwoHitCrit) {
            auto critKeeper = static_cast<CriteriaKeeper *>(crit);
            critKeeper->clear();
        }

        for (auto crit : mThreeHitCrit) {
            auto critKeeper = static_cast<CriteriaKeeper *>(crit);
            critKeeper->clear();
        }
    } // clearSavedCriteria

    /**
     * @brief Determine the total num of hits in the hitmap
     *
     * @param hitmap : hitmap to consider
     * @return size_t : total num of hits
     */
    size_t nHitsInHitMap(FwdDataSource::HitMap_t &hitmap) {
        size_t n = 0;
        for (auto kv : hitmap) {
            n += kv.second.size();
        }
        return n;
    }

    /**
     * @brief Remove used hits from the hit map
     *
     * @param hitmap : hitmap with hits used this round
     * @param tracks : tracks formed from hits
     */
    void removeHits(FwdDataSource::HitMap_t &hitmap, std::vector<Seed_t> &tracks) {

        for (auto track : tracks) {
            for (auto hit : track) {
                int sector = hit->getSector();

                auto hitit = std::find(hitmap[sector].begin(), hitmap[sector].end(), hit);

                if (hitit != hitmap[sector].end()) {
                    hitmap[sector].erase(hitit);
                    mTotalHitsRemoved++;
                }

            } // loop on hits in track
        }         // loop on track
    } // removeHits


    /** @brief merges the FST and FTT hitmaps into a single hitmap
     *  The FTT hits are shifted by the number of FST sectors
     *  @param hitmap1: FST hitmap
     *  @param hitmap2: FTT hitmap
     *  @return void
    */
    void mergeHitmaps( FwdDataSource::HitMap_t &hitmap1, FwdDataSource::HitMap_t &hitmap2 ){
        static const int numFstSectors = 3;
        for ( auto kv : hitmap2 ){
            for ( auto hit : kv.second ){
                dynamic_cast<FwdHit*>( hit )->setSector( hit->getSector() + numFstSectors );
                hitmap1[kv.first + numFstSectors].push_back( hit );
            }
        }
    } // mergeHitmaps

    /**
     * @brief Combines tracks formed from only FST hits with tracks formed only from FTT hits
     *
     */
    void mergeTracks() {
        // merge tracks in the mTrackResults vector
        // if they have similar momentum but do not share any hits
        // this is a crude way to merge tracks that are similar
        // but have different hit content
        if (mTrackResults.size() < 2)
            return;

        map<int, bool> used;

        LOG_INFO << "Merging tracks. Starting with " << mTrackResults.size() << " tracks" << endm;
        std:vector<GenfitTrackResult> newResults;
        for (size_t i = 0; i < mTrackResults.size(); i++) {
            if (used[i] == true)
                continue;
            vector<GenfitTrackResult> compatibleTracks;
            compatibleTracks.push_back(mTrackResults[i]);
            bool t1HasFst = mTrackResults[i].numFST() > 0;
            bool t1HasFtt = mTrackResults[i].numFTT() > 0;

            for (size_t j = i; j < mTrackResults.size(); j++) {
                if ( i == j ) continue;
                if (used[j] == true)
                    continue;
                bool t2HasFst = mTrackResults[j].numFST() > 0;
                bool t2HasFtt = mTrackResults[j].numFTT() > 0;
                float dr = mTrackResults[i].momentum.DeltaR(mTrackResults[j].momentum);
                float dpt = fabs( mTrackResults[i].momentum.Pt() - mTrackResults[j].momentum.Pt() );
                float dphi = fabs( mTrackResults[i].momentum.Phi() - mTrackResults[j].momentum.Phi() );
                float deta = fabs( mTrackResults[i].momentum.Eta() - mTrackResults[j].momentum.Eta() );
                LOG_INFO << "T1: " << TString::Format( "#fst=%lu, #ftt=%lu", mTrackResults[i].numFST(), mTrackResults[i].numFTT() ) << endm;
                LOG_INFO << "T2: " << TString::Format( "#fst=%lu, #ftt=%lu", mTrackResults[j].numFST(), mTrackResults[j].numFTT() ) << endm;
                LOG_INFO << "Track " << TString::Format( "[%lu][%lu] dR=%f, dPt=%f, dPhi=%f, dEta=%f", i, j, dr, dpt, dphi, deta ) <<  endm;
                
                // merge tracks if they are close in momentum
                // and if they are FST + FTT, but not FST + FST or FTT + FTT                
                if (mTrackResults[i].momentum.DeltaR(mTrackResults[j].momentum) < 0.1 && (t1HasFst != t2HasFst) && (t1HasFtt != t2HasFtt )) {
                    LOG_INFO << "Merging" << endm;
                    // merge forward to allow for more than 2 tracks to be merged
                    compatibleTracks.push_back(mTrackResults[j]);
                    used[j] = true;
                    break;
                }
            } // loop on j

            if ( compatibleTracks.size() > 1 ){
                LOG_INFO << "Merging " << compatibleTracks.size() << " tracks" << endm;
                for ( auto ct : compatibleTracks ){
                    LOG_INFO << ct.Report() << endm;
                }
                // merge the tracks
                auto mergedTrack = mTrackResults[i];
                for ( auto ct : compatibleTracks ){
                    mergedTrack.mergeSeeds( ct );

                    // merge the tracks
                    mergedTrack.track->mergeTrack( ct.track.get() );
                }

                mTrackFitter->performFit( mergedTrack.track );
                mergedTrack.setTrack( mergedTrack.track );
                LOG_INFO << mergedTrack.Report() << endm;
                newResults.push_back( mergedTrack );
            } else {
                newResults.push_back( mTrackResults[i] );
            }

        } // loop on i
        mTrackResults.clear();
        mTrackResults = newResults;

        LOG_INFO << "Merged tracks complete. Now have " << mTrackResults.size() << " tracks" << endm;
    }

    /**
     * @brief Perform track finding and fitting on an event
     *
     * @param iEvent : event number
     */
    void doEvent() {
        /************** Cleanup ****************************************/
        // Moved cleanup to the start of doEvent, so that the fit results
        // persist after the call
        mRecoTracks.clear();
        mTrackResults.clear();
        mEventStats.reset();
        /************** Cleanup **************************/
        
        mTotalHitsRemoved = 0;

        /*************************************************************/
        // Step 1
        // Load and sort the hits
        /*************************************************************/
        long long itStart = FwdTrackerUtils::nowNanoSecond();
        FwdDataSource::HitMap_t &fttHitmap = mDataSource->getFttHits();
        FwdDataSource::HitMap_t &fstHitmap = mDataSource->getFstHits();

        string hitmapSource = mConfig.get<string>("TrackFinder:source", "ftt");
        mSeedSource = kSeqSeed; // default to FST
        if (hitmapSource == "fst")
            mSeedSource = kFstSeed;
        else if (hitmapSource == "ftt")
            mSeedSource = kFttSeed;
        else if (hitmapSource == "seq")
            mSeedSource = kSeqSeed;
        else if (hitmapSource == "sim")
            mSeedSource = kSimSeed;
        LOG_DEBUG << "Performing Fwd Seed finding with mode: " << mConfig.get<string>("TrackFinder:source", "ftt") << " = " << mSeedSource << endm;

        
        LOG_DEBUG << "FTT Hitmap has:" << fttHitmap.size() << " hits to consider" << endm;
        for (auto hp : fttHitmap){
            LOG_DEBUG << "HITMAP [" << hp.first << ", { ";
            for ( auto h: hp.second ){
                LOG_DEBUG << "z=" << h->getZ() << " ";
            }
            LOG_DEBUG << " }" << endm;
        }
        
        LOG_DEBUG << "Forward Track seed finding using FST" << endm;
        LOG_DEBUG << "FST Hitmap has:" << fstHitmap.size() << " hits to consider" << endm;
        for (auto hp : fstHitmap){
            LOG_DEBUG << "HITMAP [" << hp.first << ", { ";
            for ( auto h: hp.second ){
                LOG_DEBUG << "z=" << h->getZ() << " s=" << h->getSector() << " ";
            }
            LOG_DEBUG << " }" << endm;
        }

        FwdDataSource::McTrackMap_t &mcTrackMap = mDataSource->getMcTracks();

        long long duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        
        mEventStats.mStep1Duration.push_back( duration );

        bool mcTrackFinding = true;
        if (mConfig.exists("TrackFinder")){
            mcTrackFinding = false;
        }
        if (mConfig.exists("TrackFinder") && mConfig.get<bool>( "TrackFinder:mc", false ) == false ){
            mcTrackFinding = false;
        }
        if (mConfig.exists("TrackFinder") && mConfig.get<bool>( "TrackFinder:active", true ) == false){
            mcTrackFinding = true;
        }

        /***********************************************/
        // MC Track Finding
        if (mcTrackFinding) {
            LOG_DEBUG << "MC TRACK FINDING " << endm;
            doMcTrackFinding(mcTrackMap, mSeedSource);

            if (mSeedSource == kSeqSeed){
                mergeTracks();
            }
            /***********************************************/
            // REFIT with FST or FTT hits accordingly
            if (mConfig.get<bool>("TrackFitter:refit", false)) {
                if (mSeedSource == kFstSeed || mSeedSource == kSeqSeed)
                    addFstHitsMc();
                if (mSeedSource != kFstSeed )
                    addFttHitsMc();
            } else {
                LOG_DEBUG << "Skipping Refit (MC tracking)" << endm;
            }
            /***********************************************/
            return;
        }
        /***********************************************/

        /***********************************************/
        // Standard Track Finding
        // plus initial fit
        size_t nIterations = mConfig.get<size_t>("TrackFinder:nIterations", 0);
        for (size_t iIteration = 0; iIteration < nIterations; iIteration++) {
            if ( mSeedSource == kSimSeed){
                mergeHitmaps( fstHitmap, fttHitmap );
            } else {
                if ( mSeedSource == kFstSeed || mSeedSource == kSeqSeed ){
                    doTrackIteration(iIteration, fstHitmap);
                }
                if ( mSeedSource == kFttSeed || mSeedSource == kSeqSeed){
                    doTrackIteration(iIteration, fttHitmap);
                }
            } 
        } // iIteration
        /***********************************************/

        /***********************************************/
        // REFIT with Fst or Ftt hits
        if (mConfig.get<bool>("TrackFitter:refit", false)) {
            if ( mSeedSource == kFstSeed )
                addFttHits();
            else if ( mSeedSource == kFttSeed )
                addFstHits();
        } else {
            LOG_DEBUG << "Skipping track refit" << endm;
        }
        /***********************************************/
    } // doEvent

    /**
     * @brief Perform a single fit from seed points
     *
     * @param seed : seed points from either FTT or FST
     */
    void fitTrack(Seed_t &seed) {
        mEventStats.mNumSeeds++;
        if (!mDoTrackFitting)
            return;

        std::shared_ptr<genfit::Track> genTrack;
        mEventStats.mAttemptedFits++;
        double vertex[3] = { mEventVertex.X(), mEventVertex.Y(), mEventVertex.Z() };

        double * pVertex = 0;
        if ( fabs(mEventVertex.X()) < 150 ){
            pVertex = vertex; // only use it if it has been set from default
        }

        bool useMcSeed = mConfig.get<bool>("TrackFitter:mcSeed", false);
        if (true == useMcSeed) {
            /*******************************************************/
            // Only for Simulation
            // Calculate the MC info first and check filters
            int idt = 0;
            double qual = 0;
            //  Get the quality and MC truth id
            idt = MCTruthUtils::dominantContribution(seed, qual);

            TVector3 mcSeedMom;
            auto mctm = mDataSource->getMcTracks();
            // get the MC track momentum if we can (may be used for state seed)
            if (mctm.count(idt)) {
                auto mct = mctm[idt];
                mcSeedMom.SetPtEtaPhi(mct->mPt, mct->mEta, mct->mPhi);
            }
            /*******************************************************/

            // use the MC pt, eta, phi as the seed for fitting
            mTrackFitter->fitTrack(seed, pVertex, &mcSeedMom);
        } else {
            // Normal case, real data
            mTrackFitter->fitTrack(seed, pVertex);
        }

        if (mTrackFitter->getTrack() != nullptr ){
            genTrack = mTrackFitter->getTrack();
            GenfitTrackResult gtr;
            gtr.set( seed, genTrack );

            if (gtr.status && gtr.status->isFitConvergedFully()) {
                mEventStats.mGoodFits++;
                
            } else {
                mEventStats.mFailedFits++;
            }

            if ( genTrack->getFitStatus(genTrack->getCardinalRep())->isFitConverged()) {
                mEventStats.mGoodCardinals++;
            }
            mTrackResults.push_back( gtr );
        } else {
            mEventStats.mFailedFits++;
        }
        LOG_DEBUG << "FwdTracker::fitTrack complete" << endm;
    } // fitTrack

    /**
     * @brief Loop on track seeds and fit each one
     *
     * @param trackSeeds : Track seeds
     */
    void doTrackFitting( std::vector<Seed_t> &trackSeeds) {
        long long itStart = FwdTrackerUtils::nowNanoSecond();

        // Fit each accepted track seed
        LOG_DEBUG << "Starting Track fitting loop on " << trackSeeds.size() << " track seeds" << endm;
        size_t index = 0;
        for (auto t : trackSeeds) {
            LOG_DEBUG << "Track seed fit #" << index << endm;
            fitTrack(t);
            index++;
        }
        long long itEnd = FwdTrackerUtils::nowNanoSecond();
        long long duration = (itEnd - itStart) * 1e-6; // milliseconds
        mEventStats.mFitDuration.push_back( duration );
        float perGood = (float) mEventStats.mGoodFits / (float) mEventStats.mAttemptedFits;
        float perFailed = (float) mEventStats.mFailedFits / (float) mEventStats.mAttemptedFits;
        float perCardinals = (float) mEventStats.mGoodCardinals / (float) mEventStats.mGoodFits;
        LOG_DEBUG << "Track Fitting Results:" <<
                    TString::Format( "Attempts = %lu, Good = %lu (%f%), Failed = %lu (%f%), GoodCardinals = %lu (%f%)", mEventStats.mAttemptedFits, mEventStats.mGoodFits, perGood, mEventStats.mFailedFits, perFailed, mEventStats.mGoodCardinals, perCardinals ) << endm;
        LOG_DEBUG << "Track fitting took " << duration << "ms" << endm;
    } // doTrackFitting

    /**
     * @brief MC track finding builds track seeds from available hits using MC association
     *
     * @param mcTrackMap : Mc tracks
     * @param useFttAsSource : Use FTT for seeds or (false) use Fst
     */
    void doMcTrackFinding(FwdDataSource::McTrackMap_t &mcTrackMap, int seedSource) {
        LOG_INFO << "Running MC Seed Finding, mode: " << seedSource << endm;

        // If we want sequential MC track finding then do them each individually
        if ( seedSource == kSeqSeed ){
            doMcTrackFinding( mcTrackMap, kFstSeed );
            doMcTrackFinding( mcTrackMap, kFttSeed );
            return;
        }

        mRecoTracksThisIteration.clear();
        // we will build reco tracks from each McTrack
        for (auto kv : mcTrackMap) {

            auto mc_track = kv.second;


            if (seedSource == kFttSeed && mc_track->mFttHits.size() < 2){ // require min 4 FTT hits on track
                continue;
            }

            if (seedSource == kFstSeed && mc_track->mFstHits.size() < 2 ) { // require min 3 FST hits on track
                continue;
            }

            if ( seedSource == kSimSeed && mc_track->mFstHits.size() < 2 && mc_track->mFttHits.size() < 2 ){
                continue;
            }

            std::set<size_t> uvid;
            Seed_t track;

            if ( seedSource != kFstSeed ){ // FTT is used unless we are ONLY considering FST
                for (auto h : mc_track->mFttHits) {
                    track.push_back(h);
                    uvid.insert(static_cast<FwdHit *>(h)->_vid);
                }
            }
            if (seedSource != kFttSeed ) { // FST
                for (auto h : mc_track->mFstHits) {
                    track.push_back(h);
                    uvid.insert(static_cast<FwdHit *>(h)->_vid);
                }
            }

            if (uvid.size() == track.size()) { // only add tracks that have one hit per volume
                mRecoTracksThisIteration.push_back(track);
                int idt = 0;
                double qual = 0;
                idt = MCTruthUtils::dominantContribution(track, qual);
            } else {
                //Skipping track that doesnt have hits on all layers
            }
        }

        LOG_DEBUG << "McTrackFinding Found: " << mRecoTracksThisIteration.size() << " tracks" << endm;
        doTrackFitting(mRecoTracksThisIteration);

        // Now save to the main reco track list
        mRecoTracks.insert( mRecoTracks.end(), mRecoTracksThisIteration.begin(), mRecoTracksThisIteration.end() );
    } //doMcTrackFinding


    /** sliceHitMapInPhi
    * @brief Slices a hitmap into a phi section
    *
    * @param inputMap INPUT hitmap to process
    * @param outputMap OUTPUT hitmap, will be cleared and filled with only the hits from inputMap that are within phi region
    * @param phi_min The minimum phi to accept
    * @param phi_max The maximum Phi to accept
    *
    * @returns The number of hits in the outputMap
    */
    size_t sliceHitMapInPhi( FwdDataSource::HitMap_t &inputMap, FwdDataSource::HitMap_t &outputMap, float phi_min, float phi_max ){
        size_t n_hits_kept = 0;

        outputMap.clear(); // child STL containers will get cleared too
        for ( auto kv : inputMap ){
            for ( KiTrack::IHit* hit : kv.second ){
                TVector3 vec(hit->getX(), hit->getY(), hit->getZ() );
                if ( vec.Phi() < phi_min || vec.Phi() > phi_max ) continue;

                // now add the hits to the sliced map
                outputMap[kv.first].push_back( hit );
                n_hits_kept ++;
            } // loop on hits
        } // loop on map
        return n_hits_kept;
    } // sliceHitMapInPhi

    /** doTrackingOnHitmapSubset
     * @brief Does track finding steps on a subset of hits (phi slice)
     * @param iIteration: tracking iteration (for determining params)
     * @param hitmap: the hitmap to use, should already be subset of original
     * @returns a list of track seeds
     */
    vector<Seed_t> doTrackingOnHitmapSubset( size_t iIteration, FwdDataSource::HitMap_t &hitmap  ) {
        long long itStart = FwdTrackerUtils::nowNanoSecond();

        std::vector<Seed_t> acceptedTracks;
        std::vector<Seed_t> rejectedTracks;
        /*************************************************************/
        // Step 2
        // build 2-hit segments (setup parent child relationships)
        /*************************************************************/
        // Initialize the segment builder with sorted hits
        KiTrack::SegmentBuilder builder(hitmap);

        // Load the criteria used for 2-hit segments
        // This loads from XML config if available
        std::string criteriaPath = "TrackFinder.Iteration[" + std::to_string(iIteration) + "].SegmentBuilder";

        if (false == mConfig.exists(criteriaPath)) {
            // Use the default for all iterations if it is given.
            // If not then no criteria will be applied
            criteriaPath = "TrackFinder.SegmentBuilder";
        }

        clearCriteria( mTwoHitCrit );
        mTwoHitCrit = loadCriteria(criteriaPath);
        builder.addCriteria(mTwoHitCrit);

        // Setup the connector (this tells it how to connect hits together into segments)
        std::string connPath = "TrackFinder.Iteration[" + std::to_string(iIteration) + "].Connector";

        if (false == mConfig.exists(connPath))
            connPath = "TrackFinder.Connector";

        unsigned int distance = mConfig.get<unsigned int>(connPath + ":distance", 1);
        if (mSeedSource == 1){
            distance = 2; // set distance to 2 for FTT
        }

        FwdConnector connector(distance);
        builder.addSectorConnector(&connector);
        LOG_DEBUG << "Connector added: " << endm;
        // Get the segments and return an automaton object for further work

        KiTrack::Automaton automaton = builder.get1SegAutomaton();
        LOG_DEBUG << TString::Format( "nSegments=%lu", automaton.getSegments().size() ).Data() << endm;
        LOG_DEBUG << TString::Format( "nConnections=%u", automaton.getNumberOfConnections() ).Data() << endm;

        if (automaton.getNumberOfConnections() > 9000 ){
            LOG_ERROR << "Got too many connections, bailing out of tracking" << endm;
            return acceptedTracks;
        }

        // at any point we can get a list of tracks out like this:
        // std::vector < std::vector< KiTrack::IHit* > > tracks = automaton.getTracks();
        // we can apply an optional parameter <nHits> to only get tracks with >=nHits in them

        long long duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        mEventStats.mStep2Duration.push_back(duration);
        itStart = FwdTrackerUtils::nowNanoSecond();

        /*************************************************************/
        // Step 3
        // build 3-hit segments from the 2-hit segments
        /*************************************************************/
        automaton.clearCriteria();
        automaton.resetStates();
        criteriaPath = "TrackFinder.Iteration[" + std::to_string(iIteration) + "].ThreeHitSegments";

        if (false == mConfig.exists(criteriaPath))
            criteriaPath = "TrackFinder.ThreeHitSegments";

        clearCriteria( mThreeHitCrit );
        mThreeHitCrit = loadCriteria(criteriaPath);
        automaton.addCriteria(mThreeHitCrit);

        duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        mEventStats.mStep3Duration.push_back( duration );
        if (duration > 2000 || automaton.getNumberOfConnections() > 9000){
            LOG_WARN << "The Three Hit Criteria took more than 200ms to process, duration: " << duration << " ms" << endm;
            LOG_WARN << "bailing out (skipping subset HNN)" << endm;
            std::vector<Seed_t> acceptedTracks;
            std::string subsetPath = "TrackFinder.Iteration[" + std::to_string(iIteration) + "].SubsetNN";
            size_t minHitsOnTrack = mConfig.get<size_t>(subsetPath + ":min-hits-on-track", FwdSystem::sNFttLayers);
            acceptedTracks = automaton.getTracks(minHitsOnTrack);
            return acceptedTracks;
        }
        itStart = FwdTrackerUtils::nowNanoSecond();

        LOG_DEBUG << TString::Format( "nSegments=%lu", automaton.getSegments().size() ).Data() << endm;
        LOG_DEBUG << TString::Format( "nConnections=%u", automaton.getNumberOfConnections() ).Data() << endm;

        /*************************************************************/
        // Step 4
        // Get the tracks from the possible tracks that are the best subset
        /*************************************************************/
        std::string subsetPath = "TrackFinder.Iteration[" + std::to_string(iIteration) + "].SubsetNN";

        if (false == mConfig.exists(subsetPath))
            subsetPath = "TrackFinder.SubsetNN";

        //  only for debug really
        bool findSubsets = mConfig.get<bool>(subsetPath + ":active", true);

        if (findSubsets) {
            size_t minHitsOnTrack = mConfig.get<size_t>(subsetPath + ":min-hits-on-track", 7);
            // Getting all tracks with at least minHitsOnTrack hits on them
            std::vector<Seed_t> tracks = automaton.getTracks(minHitsOnTrack);

            float omega = mConfig.get<float>(subsetPath + ".Omega", 0.75);
            float stableThreshold = mConfig.get<float>(subsetPath + ".StableThreshold", 0.1);
            float Ti = mConfig.get<float>(subsetPath + ".InitialTemp", 2.1);
            float Tf = mConfig.get<float>(subsetPath + ".InfTemp", 0.1);

            KiTrack::SubsetHopfieldNN<Seed_t> subset;
            subset.add(tracks);
            subset.setOmega(omega);
            subset.setLimitForStable(stableThreshold);
            subset.setTStart(Ti);
            subset.setTInf(Tf);

            SeedCompare comparer;
            SeedQual quality;

            subset.calculateBestSet(comparer, quality);

            acceptedTracks = subset.getAccepted();

            // this call takes a long time due to possible huge combinatorics.
            // rejectedTracks = subset.getRejected();
            // LOG_DEBUG << "We had " << tracks.size() << " tracks. Accepted = " << acceptedTracks.size() << ", Rejected = " << rejectedTracks.size() << endm;

        } else { // the subset and hit removal
            size_t minHitsOnTrack = mConfig.get<size_t>(subsetPath + ":min-hits-on-track", FwdSystem::sNFttLayers);
            acceptedTracks = automaton.getTracks(minHitsOnTrack);
        }// subset off

        duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        mEventStats.mStep4Duration.push_back( duration );
        if (duration > 500){
            LOG_WARN << "The took more than 500ms to process, duration: " << duration << " ms" << endm;
            LOG_WARN << "We got " << acceptedTracks.size() << " tracks this round" << endm;
        }
        LOG_DEBUG << "We got " << acceptedTracks.size() << " tracks this round" << endm;
        return acceptedTracks;
    } // doTrackingOnHitmapSubset

    /**
     * @brief Main tracking procedure
     *
     * @param iIteration : The track iteration
     * @param hitmap : the hitmap of available hits per plane
     */
    void doTrackIteration(size_t iIteration, FwdDataSource::HitMap_t &hitmap) {

        // empty the list of reco tracks for the iteration
        mRecoTracksThisIteration.clear();

        // check to see if we have hits!
        size_t nHitsThisIteration = nHitsInHitMap(hitmap);

        const int minHitsToConsider = 3;
        if (nHitsThisIteration < minHitsToConsider) {
            // No hits left in the hitmap! Skipping this iteration
            LOG_INFO << "No hits to consider in this iteration, skipping" << endm;
            return;
        }

        std::string pslPath = "TrackFinder.Iteration["+ std::to_string(iIteration) + "]:nPhiSlices";
        if ( false == mConfig.exists( pslPath ) ) pslPath = "TrackFinder:nPhiSlices";
        size_t phi_slice_count = mConfig.get<size_t>( pslPath, 1 );

        if ( phi_slice_count <= 1 || phi_slice_count > 100 ) { // no phi slicing!
            LOG_DEBUG << "Tracking without phi slices" << endm;
            /*************************************************************/
            // Steps 2 - 4 here
            /*************************************************************/
            auto acceptedTracks = doTrackingOnHitmapSubset( iIteration, hitmap );
            mRecoTracksThisIteration.insert( mRecoTracksThisIteration.end(), acceptedTracks.begin(), acceptedTracks.end() );
        } else {

            FwdDataSource::HitMap_t slicedHitMap;

            if ( phi_slice_count == 0 || phi_slice_count > 100 ){
                LOG_WARN << "Invalid phi_slice_count = " << phi_slice_count << ", resetting to 1" << endm;
                phi_slice_count= 1;
            }
            float phi_slice = 2 * TMath::Pi() / (float) phi_slice_count;
            for ( size_t phi_slice_index = 0; phi_slice_index < phi_slice_count; phi_slice_index++ ){

                float phi_min = phi_slice_index * phi_slice - TMath::Pi();
                float phi_max = (phi_slice_index + 1) * phi_slice - TMath::Pi();
                LOG_INFO << TString::Format( "phi slice = (%f, %f)", phi_min, phi_max ) << endm;

                /*************************************************************/
                // Step 1A
                // Slice the hitmap into a phi section if needed
                // If we do that, check again that we arent wasting time on empty sections
                /*************************************************************/
                size_t nHitsThisSlice = 0;
                if ( phi_slice_count > 1 ){
                    nHitsThisSlice = sliceHitMapInPhi( hitmap, slicedHitMap, phi_min, phi_max );
                    if ( nHitsThisSlice < minHitsToConsider ) {
                        continue;
                    }
                } else { // no need to slice
                    // I think this incurs a copy, maybe we can find a way to avoid.
                    slicedHitMap = hitmap;
                }

                /*************************************************************/
                // Steps 2 - 4 here
                /*************************************************************/
                auto acceptedTracks = doTrackingOnHitmapSubset( iIteration, slicedHitMap );
                mRecoTracksThisIteration.insert( mRecoTracksThisIteration.end(), acceptedTracks.begin(), acceptedTracks.end() );
            } //loop on phi slices
        }// if loop on phi slices

        /*************************************************************/
        // Step 5
        // Remove the hits from any track that was found
        /*************************************************************/
        std::string hrmPath = "TrackFinder.Iteration["+ std::to_string(iIteration) + "].HitRemover";
        if ( false == mConfig.exists( hrmPath ) ) hrmPath = "TrackFinder.HitRemover";

        if ( true == mConfig.get<bool>( hrmPath + ":active", true ) ){
            removeHits( hitmap, mRecoTracksThisIteration );
        }

        LOG_DEBUG << " FITTING " << mRecoTracksThisIteration.size() << " now" << endm;

        if ( mRecoTracksThisIteration.size() < 10001 ){
            doTrackFitting( mRecoTracksThisIteration );
        } else {
            LOG_ERROR << "BAILING OUT of fit, too many track candidates" << endm;
        }

        // Add the set of all accepted tracks (this iteration) to our collection of found tracks from all iterations
        mRecoTracks.insert( mRecoTracks.end(), mRecoTracksThisIteration.begin(), mRecoTracksThisIteration.end() );

    } // doTrackIteration

    /**
     * @brief Adds compatible FST hits to tracks seeded with FTT
     *
     */
    void addFstHitsMc() {
        FwdDataSource::HitMap_t hitmap = mDataSource->getFstHits();

        for (size_t i = 0; i < mTrackResults.size(); i++) {
            GenfitTrackResult &gtr = mTrackResults[i];

            if ( gtr.status->isFitConverged() == false || gtr.momentum.Perp() < 1e-3) {
                LOG_DEBUG << "Skipping addFstHitsMc, fit failed" << endm;
                return;
            }
            mEventStats.mPossibleReFit++;
            
            Seed_t fst_hits_for_this_track(3, nullptr);
            Seed_t fst_hits_for_this_track_nnull;

            for (size_t j = 0; j < 3; j++) {
                for (auto h0 : hitmap[j]) {
                    if (dynamic_cast<FwdHit *>(h0)->_tid == gtr.track->getMcTrackId()) {
                        fst_hits_for_this_track[j] = h0;
                        fst_hits_for_this_track_nnull.push_back(h0);
                        break;
                    }
                } // loop on hits in this layer of hitmap
            }     // loop on hitmap layers

            size_t nFstHitsFound = 0;
            if ( fst_hits_for_this_track[0] != nullptr ) nFstHitsFound++;
            if ( fst_hits_for_this_track[1] != nullptr ) nFstHitsFound++;
            if ( fst_hits_for_this_track[2] != nullptr ) nFstHitsFound++;
            LOG_DEBUG << "Found " << nFstHitsFound << " FST Hits on this track (MC lookup)" << endm;

            if (nFstHitsFound >= 1) {
                mEventStats.mAttemptedReFits++;

                Seed_t combinedSeed;
                LOG_DEBUG << "Found " << gtr.fttSeed.size() << " existing FTT seed points" << endm;
                LOG_DEBUG << "Adding " << fst_hits_for_this_track_nnull.size() << " new FST seed points" << endm;
                combinedSeed.insert( combinedSeed.begin(), fst_hits_for_this_track_nnull.begin(), fst_hits_for_this_track_nnull.end() );
                combinedSeed.insert( combinedSeed.end(), gtr.fttSeed.begin(), gtr.fttSeed.end() );

                double vertex[3] = { mEventVertex.X(), mEventVertex.Y(), mEventVertex.Z() };

                if ( fabs(gtr.momentum.Z()) > 10000 ){
                    // this seems to help some fits perform better
                    gtr.momentum.SetXYZ( gtr.momentum.X(), gtr.momentum.Y(), 1000 );
                }
                LOG_DEBUG << "Using previous fit momentum as the seed: " << TString::Format( "(pX=%f, pY=%f, pZ=%f)", gtr.momentum.X(), gtr.momentum.Y(), gtr.momentum.Z() ) << endm;

                mTrackFitter->fitTrack(combinedSeed, vertex, &(gtr.momentum) );
            } // we have 3 Si hits to refit with
        } // loop on the tracks
    } // addFstHitsMc

    /**
     * @brief Adds compatible FTT hits to tracks seeded with FST
     *
     */
    void addFttHits() {
        FwdDataSource::HitMap_t hitmap = mDataSource->getFttHits();

        for ( auto &t : mTrackResults ){
            if (t.isFitConvergedFully != true) continue;
            mEventStats.mPossibleReFit++;

            Seed_t hits_near_plane0;
            Seed_t hits_near_plane1;
            Seed_t hits_near_plane2;
            Seed_t hits_near_plane3;
            Seed_t hits_to_add;
            try {
                auto msp3 = mTrackFitter->projectToFtt(3, t.track);
                auto msp2 = mTrackFitter->projectToFtt(2, t.track);
                auto msp1 = mTrackFitter->projectToFtt(1, t.track);
                auto msp0 = mTrackFitter->projectToFtt(0, t.track);

                // now look for Ftt hits near these
                hits_near_plane3 = findFttHitsNearProjectedState(hitmap[3], msp3);
                hits_near_plane2 = findFttHitsNearProjectedState(hitmap[2], msp2);
                hits_near_plane1 = findFttHitsNearProjectedState(hitmap[1], msp1);
                hits_near_plane0 = findFttHitsNearProjectedState(hitmap[0], msp0);

                LOG_DEBUG << " Found #FTT hits on planes " << TString::Format( "[%ld, %ld, %ld, %ld]", hits_near_plane0.size(), hits_near_plane1.size(), hits_near_plane2.size(), hits_near_plane3.size() ) << endm;
            } catch (genfit::Exception &e) {
                // Failed to project to Si disk: ", e.what()
                LOG_WARN << "Unable to get Ftt projections: " << e.what() << endm;
            }

            if ( hits_near_plane0.size() > 0 )
                hits_to_add.push_back( hits_near_plane0[0] );
            if ( hits_near_plane1.size() > 0 )
                hits_to_add.push_back( hits_near_plane1[0] );
            if ( hits_near_plane2.size() > 0 )
                hits_to_add.push_back( hits_near_plane2[0] );
            if ( hits_near_plane3.size() > 0 )
                hits_to_add.push_back( hits_near_plane3[0] );

            if ( hits_to_add.size() > 0 ){

                
                mEventStats.mAttemptedReFits++;
                

                Seed_t combinedSeed;
                LOG_DEBUG << "Found " << t.fstSeed.size() << " existing FST seed points" << endm;
                combinedSeed.insert( combinedSeed.begin(), t.fstSeed.begin(), t.fstSeed.end() ); // this is goofed but will fix
                LOG_DEBUG << "Adding " << hits_to_add.size() << " new FTT seed points" << endm;
                combinedSeed.insert( combinedSeed.end(), hits_to_add.begin(), hits_to_add.end() );

                double vertex[3] = { mEventVertex.X(), mEventVertex.Y(), mEventVertex.Z() };
                LOG_DEBUG << "Using previous fit momentum as the seed: " << TString::Format( "(pt=%f, eta=%f, phi=%f)", t.momentum.Pt(), t.momentum.Eta(), t.momentum.Phi() ) << endm;

                mTrackFitter->fitTrack(combinedSeed, vertex, &(t.momentum) );

                auto status = mTrackFitter->getTrack()->getFitStatus();
                if ( status->isFitConvergedFully() ){
                    LOG_DEBUG << "Track Refit with FTT points converged" << endm;
                    t.set( hits_to_add, mTrackFitter->getTrack() );
                    LOG_DEBUG << "Track Refit with " << t.track->getNumPoints() << " points" << endm;
                    mEventStats.mGoodReFits++;
                } else {
                    mEventStats.mFailedReFits++;
                    LOG_DEBUG << "Track Refit with FTT points FAILED" << endm;
                }
            }
        } // loop over tracks
    } // addFttHits

    /**
     * @brief Adds compatible FTT hits using MC info
     *
     */
    void addFttHitsMc() {
        FwdDataSource::HitMap_t hitmap = mDataSource->getFttHits();


        for (size_t i = 0; i < mTrackResults.size(); i++) {
            GenfitTrackResult &gtr = mTrackResults[i];

            if ( gtr.status->isFitConverged() == false || gtr.momentum.Perp() < 1e-6) {
                LOG_DEBUG << "Skipping addFttHitsMc on this track, fit failed" << endm;
                return;
            }

            mEventStats.mPossibleReFit++;
            

            Seed_t fttHitsForThisTrack;

            size_t nFttHitsFound = 0;
            for (size_t j = 0; j < 4; j++) {
                for (auto h0 : hitmap[j]) {
                    if (dynamic_cast<FwdHit *>(h0)->_tid == gtr.track->getMcTrackId()) {
                        fttHitsForThisTrack.push_back( h0 );
                        nFttHitsFound++;
                        break;
                    }
                } // loop on hits in this layer of hitmap
            } // loop on hitmap layers

            LOG_DEBUG << "Found " << nFttHitsFound << " FTT Hits on this track (MC lookup)" << endm;

            if (nFttHitsFound >= 1) {
                mEventStats.mAttemptedReFits++;

                Seed_t combinedSeed;
                LOG_DEBUG << "Found " << gtr.fstSeed.size() << " existing FST seed points" << endm;
                combinedSeed.insert( combinedSeed.begin(), gtr.fstSeed.begin(), gtr.fstSeed.end() ); // this is goofed but will fix
                LOG_DEBUG << "Adding " << fttHitsForThisTrack.size() << " new FTT seed points" << endm;
                combinedSeed.insert( combinedSeed.end(), fttHitsForThisTrack.begin(), fttHitsForThisTrack.end() );

                double vertex[3] = { mEventVertex.X(), mEventVertex.Y(), mEventVertex.Z() };
                LOG_DEBUG << "Using previous fit momentum as the seed: " << endm;
                LOG_DEBUG << TString::Format( "(px=%f, py=%f, pz=%f)", gtr.momentum.Px(), gtr.momentum.Py(), gtr.momentum.Pz() ) << endm;
                // this prevents an exception when momentum is outrageous
                if ( fabs(gtr.momentum.Pz()) > 100000 )
                    gtr.momentum.SetZ( 10000 );
                LOG_DEBUG << TString::Format( "(pt=%f, eta=%f, phi=%f)", gtr.momentum.Pt(), gtr.momentum.Eta(), gtr.momentum.Phi() ) << endm;

                mTrackFitter->fitTrack(combinedSeed, vertex, &(gtr.momentum) );

                auto status = mTrackFitter->getTrack()->getFitStatus();
                if ( status && status->isFitConvergedFully() ){
                    LOG_DEBUG << "Track Refit with FTT points converged" << endm;
                    gtr.set( fttHitsForThisTrack, mTrackFitter->getTrack() );
                    LOG_DEBUG << "Track Refit with " << gtr.track->getNumPoints() << " points" << endm;
                    mEventStats.mGoodReFits++;
                } else {
                    mEventStats.mFailedReFits++;
                    LOG_DEBUG << "Track Refit with FTT points FAILED" << endm;
                }
            } // we have at least one Fst hit to refit with
        } // loop on the global tracks
    } // add Ftt hits via MC associations

    /**
     * @brief Adds compatible FST hits to a track seeded from FTT
     *
     */
    void addFstHits() {
        FwdDataSource::HitMap_t hitmap = mDataSource->getFstHits();

        // loop on global tracks
        for (size_t i = 0; i < mTrackResults.size(); i++) {
            GenfitTrackResult &gtr = mTrackResults[i];

            if (gtr.isFitConverged == false) {
                // Original Track fit did not converge, skipping
                return;
            }
            mEventStats.mPossibleReFit++;

            Seed_t hits_near_disk0;
            Seed_t hits_near_disk1;
            Seed_t hits_near_disk2;
            try {
                auto msp2 = mTrackFitter->projectToFst(2, gtr.track);
                auto msp1 = mTrackFitter->projectToFst(1, gtr.track);
                auto msp0 = mTrackFitter->projectToFst(0, gtr.track);

                // now look for Si hits near these
                hits_near_disk2 = findFstHitsNearProjectedState(hitmap[2], msp2);
                hits_near_disk1 = findFstHitsNearProjectedState(hitmap[1], msp1);
                hits_near_disk0 = findFstHitsNearProjectedState(hitmap[0], msp0);
            } catch (genfit::Exception &e) {
                // Failed to project to Si disk: ", e.what()
                LOG_WARN << "Unable to get Ftt projections: " << e.what() << endm;
            }

            vector<KiTrack::IHit *> hits_to_add;
            Seed_t fst_hits_for_this_track_nnull;

            size_t nFstHitsFound = 0; // this is really # of disks on which a hit is found

            //  TODO: HANDLE multiple points found?
            if ( hits_near_disk0.size() >= 1 ) {
                hits_to_add.push_back( hits_near_disk0[0] );
                fst_hits_for_this_track_nnull.push_back( hits_near_disk0[0] );
                nFstHitsFound++;
            } else {
                hits_to_add.push_back( nullptr );
            }
            if ( hits_near_disk1.size() >= 1 ) {
                hits_to_add.push_back( hits_near_disk1[0] );
                fst_hits_for_this_track_nnull.push_back( hits_near_disk1[0] );
                nFstHitsFound++;
            } else {
                hits_to_add.push_back( nullptr );
            }
            if ( hits_near_disk2.size() >= 1 ) {
                hits_to_add.push_back( hits_near_disk2[0] );
                fst_hits_for_this_track_nnull.push_back( hits_near_disk2[0] );
                nFstHitsFound++;
            } else {
                hits_to_add.push_back( nullptr );
            }

            if (nFstHitsFound >= 1) {
                mEventStats.mAttemptedReFits++;

                Seed_t combinedSeed;
                LOG_DEBUG << "Found " << gtr.fttSeed.size() << " existing FTT seed points" << endm;
                LOG_DEBUG << "Adding " << fst_hits_for_this_track_nnull.size() << " new FST seed points" << endm;
                combinedSeed.insert( combinedSeed.begin(), fst_hits_for_this_track_nnull.begin(), fst_hits_for_this_track_nnull.end() );
                combinedSeed.insert( combinedSeed.end(), gtr.fttSeed.begin(), gtr.fttSeed.end() );

                double vertex[3] = { mEventVertex.X(), mEventVertex.Y(), mEventVertex.Z() };

                if ( fabs(gtr.momentum.Z()) > 10000 ){
                    // this seems to help some fits perform better
                    gtr.momentum.SetXYZ( gtr.momentum.X(), gtr.momentum.Y(), 1000 );
                }
                LOG_DEBUG << "Using previous fit momentum as the seed: " << TString::Format( "(pX=%f, pY=%f, pZ=%f)", gtr.momentum.X(), gtr.momentum.Y(), gtr.momentum.Z() ) << endm;

                mTrackFitter->fitTrack(combinedSeed, vertex, &(gtr.momentum) );

                
                if (mTrackFitter->getTrack()->getFitStatus()->isFitConvergedFully() == false) {
                    mEventStats.mFailedReFits++;
                    LOG_DEBUG << "refitTrackWithFstHits failed refit" << endm;
                } else {
                    LOG_DEBUG << "refitTrackWithFstHits successful refit" << endm;
                    mEventStats.mGoodReFits++;
                    gtr.set( fst_hits_for_this_track_nnull, mTrackFitter->getTrack() );
                }
                
            } else {
                // unable to refit
            }
        } // loop on globals
    } // addFstHits

    /**
     * @brief Finds FST hits near projected state
     *
     * @param available_hits : FST hits to consider
     * @param msp : measured state on plabe from existing track projection
     * @param dphi : search distance in phi
     * @param dr : search distance in r
     * @return Seed_t : compatible FST hits
     */
    Seed_t findFstHitsNearProjectedState(Seed_t &available_hits, genfit::MeasuredStateOnPlane &msp, double dphi = 0.004 * 20.5, double dr = 2.75 * 2) {
        double probe_phi = TMath::ATan2(msp.getPos().Y(), msp.getPos().X());
        double probe_r = sqrt(pow(msp.getPos().X(), 2) + pow(msp.getPos().Y(), 2));

        Seed_t found_hits;

        for (auto h : available_hits) {
            double h_phi = TMath::ATan2(h->getY(), h->getX());
            double h_r = sqrt(pow(h->getX(), 2) + pow(h->getY(), 2));
            double mdphi = fabs(h_phi - probe_phi);
            if (mdphi > 2*3.1415926)
                mdphi = mdphi - 2*3.1415926;

            if ( mdphi < dphi && fabs( h_r - probe_r ) < dr) { // handle 2pi edge
                found_hits.push_back(h);
            }
        }

        return found_hits;
    } // findFstHitsNearProjectedState

    /**
     * @brief Finds FTT hits near projected state
     *
     * @param available_hits : FTT hits to consider
     * @param msp : measured state on plane from existing track fit projection
     * @param dx : search distance in x
     * @param dy : search distance in y
     *
     * @return compatible FTT hits
    */
    Seed_t findFttHitsNearProjectedState(Seed_t &available_hits, genfit::MeasuredStateOnPlane &msp, double dx = 1.5, double dy = 1.5) {

        Seed_t found_hits;
        double probe_phi = TMath::ATan2(msp.getPos().Y(), msp.getPos().X());
        double probe_r = sqrt(pow(msp.getPos().X(), 2) + pow(msp.getPos().Y(), 2));

        TLorentzVector lv1, lv2;
        lv1.SetPxPyPzE( msp.getPos().X(), msp.getPos().Y(), 0, 1 );

        double mindx = 99;
        double mindy = 99;
        double mindr = 99;
        double mindp = 99;
        KiTrack::IHit *closest = nullptr;

        for (auto h : available_hits) {

            lv2.SetPxPyPzE( h->getX(), h->getY(), 0, 1 );
            double sr = lv1.Pt() - lv2.Pt();
            double sp =  lv1.DeltaPhi( lv2 );
            double sx = h->getX() - msp.getPos().X();
            double sy = h->getY() - msp.getPos().Y();

            if ( fabs(sr) < fabs(mindr) )
                mindr = sr;
            if ( fabs(sp) < fabs(mindp) ){
                mindp = sp;
                closest = h;
            }
            if ( fabs(sx) < fabs(mindx) )
                mindx = sx;
            if ( fabs(sy) < fabs(mindy) )
                mindy = sy;

        } // loop h

        if (  fabs(mindp) < 0.04*5 && fabs(mindr) < 9 ) {
            found_hits.push_back(closest);
        }

        return found_hits;
    } // findFttHitsNearProjectedState

    bool getSaveCriteriaValues() { return mSaveCriteriaValues; }
    std::vector<KiTrack::ICriterion *> getTwoHitCriteria() { return mTwoHitCrit; }
    std::vector<KiTrack::ICriterion *> getThreeHitCriteria() { return mThreeHitCrit; }

    TrackFitter *getTrackFitter() { return mTrackFitter; }
    void setEventVertex( TVector3 v ) { mEventVertex = v; }
    TVector3 getEventVertex() { return mEventVertex; }

  protected:
    unsigned long long int nEvents;

    bool mDoTrackFitting = true;
    bool mSaveCriteriaValues = false;
    enum SeedSource { kFstSeed = 0, kFttSeed, kSimSeed, kSeqSeed };
    int mSeedSource = 0; // 0 = FST, 1 = FTT, 2 = FST+FTT simultaneous, 3 = FST+FTT sequential

    FwdTrackerConfig mConfig;
    std::string mConfigFile;
    size_t mTotalHitsRemoved;

    std::vector<GenfitTrackResult> mTrackResults;

    std::vector<Seed_t> mRecoTracks; // the tracks recod from all iterations
    std::vector<Seed_t> mRecoTracksThisIteration;

    // Metrics about the event
    EventStats mEventStats;

    // Set to the Primary vertex for the event
    TVector3 mEventVertex;


    std::shared_ptr<FwdDataSource> mDataSource;

    TrackFitter *mTrackFitter = nullptr;

    std::vector<KiTrack::ICriterion *> mTwoHitCrit;
    std::vector<KiTrack::ICriterion *> mThreeHitCrit;

    // histograms of the raw input data
    TString mGeoCache;
};

#endif
