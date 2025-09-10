#ifndef FWD_TRACKER_H
#define FWD_TRACKER_H

#include "Fit/Fitter.h"
#include "FitterUtils.h"
#include "TFile.h"
#include "TGraph2D.h"
#include "TH1.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TRandom3.h"
#include "TTree.h"
#include "TVector3.h"
#include "TLorentzVector.h"
#include "TDecompChol.h"

#include <algorithm>
#include <memory>
#include <string>
#include <vector>
#include <unordered_map>
#include <fstream>
#include <numeric>

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
#include "GenFit/GFRaveTrackParameters.h"

#include "StFwdTrackMaker/FwdTrackerConfig.h"
#include "StFwdTrackMaker/Common.h"

#include "StEvent/StFwdTrack.h"

#include "StFwdTrackMaker/include/Tracker/GenfitTrackResult.h"


class ForwardTrackMaker {
  public:
    ForwardTrackMaker() : mConfigFile("config.xml"), mEventVertex(-999, -999, -999) {
        // noop
    }

    const std::vector<GenfitTrackResult> &getTrackResults() const { return mTrackResults; }
    const std::vector<Seed_t> &getTrackSeeds() const { return mTrackSeeds; }
    const std::vector<genfit::GFRaveVertex*> &getVertices() const { return mFwdVertices; }
    const EventStats &getEventStats() const { return mEventStats; }

    void Clear(){
        for ( auto gtr : mTrackResults ){
            gtr.Clear();
        }
        mTrackResults.clear();
        if (mTrackFitter) {
            mTrackFitter->clear();
        }
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

        mBeamlineHit.setXYZDetId( 0, 0, 0, kTpcId );
        mBeamlineHit._covmat(0,0) = 0.1;
        mBeamlineHit._covmat(1,1) = 0.1;
        mBeamlineHit._covmat(2,2) = 100*100;
    } //initialize

    /**
     * @brief Loads Criteria from XML configuration.
     * Utility function for loading criteria from XML config.
     * @param path : path in config to load
     * @return vector of ICriterion pointers
    */
    std::vector<KiTrack::ICriterion *> loadCriteria(string path) {
        LOG_DEBUG << "Loading Criteria from path: " << path << endm;
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
                LOG_DEBUG << "Creating Criterion: " << name << ", min: " << vmin << ", max: " << vmax << endm;
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

    /** @brief cleanup the event-wise data structures
     *
     */
    void cleanup() {
        /************** Cleanup ****************************************/
        // Moved cleanup to the start of doEvent, so that the fit results
        // persist after the call
        mTrackSeeds.clear();
        mTrackResults.clear();
        mEventStats.reset();
        mTotalHitsRemoved = 0;
        mFwdVertices.clear();
        mFwdVerticesAsHits.clear();
        /************** Cleanup **************************/
    }

    bool useMcTrackFinding(){
        /*************************************************************/
        // Determine if we should use MC seed finding
        /*************************************************************/
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
        return mcTrackFinding;
    }

    /**
     * @brief Perform the track finding
     * Creates a list of track seeds from the hitmaps
     * Retrieve them using `getTrackSeeds()`
     */
    void findTrackSeeds() {
        cleanup();
        /*************************************************************/
        // Get the hitmaps
        /*************************************************************/
        long long itStart = FwdTrackerUtils::nowNanoSecond();
        FwdDataSource::HitMap_t &fttHitmap = mDataSource->getFttHits();
        FwdDataSource::HitMap_t &fstHitmap = mDataSource->getFstHits();

        /*************************************************************/
        // Determine seed finding mode
        /*************************************************************/
        string hitmapSource = mConfig.get<string>("TrackFinder:source", "fst");
        LOG_INFO << "Hitmap Source: " << hitmapSource << endm;
        mSeedSource = kSeqSeed; // default to FST
        if (hitmapSource == "fst")
            mSeedSource = kFstSeed;
        else if (hitmapSource == "ftt")
            mSeedSource = kFttSeed;
        else if (hitmapSource == "seq")
            mSeedSource = kSeqSeed;
        else if (hitmapSource == "sim")
            mSeedSource = kSimSeed;
        else if (hitmapSource == "NONE"){
            LOG_INFO << "Seed finding has been disabled, source=NONE!" << endl;
            return;
        } else {
            LOG_WARN << "Unknown TrackFinder:source = " << hitmapSource << ", using defalt (fst)" << endl;
            hitmapSource = "fst";
        }
        LOG_INFO << "Performing Fwd Seed finding with mode: " << hitmapSource << " = " << endm;
        FwdDataSource::McTrackMap_t &mcTrackMap = mDataSource->getMcTracks();

        long long duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds

        if (kProfile) mEventStats.mStep1Duration.push_back( duration );

        /*************************************************************/
        // DO MC Track Finding (if set to do so)
        if (useMcTrackFinding()) {
            doMcTrackFinding(mcTrackMap, mSeedSource);
            if (kProfile) mEventStats.mNumSeeds = mTrackSeeds.size();
            long long duration2 = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
            if (kProfile) mEventStats.mSeedFindingDuration.push_back( duration2 );
            return;
        } else {
            LOG_DEBUG << "Performing Standard Track Finding" << endm;
        }
        /*************************************************************/

        /*************************************************************/
        // Standard Track Finding
        size_t nIterations = mConfig.get<size_t>("TrackFinder:nIterations", 0);
        for (size_t iIteration = 0; iIteration < nIterations; iIteration++) {
            if ( mSeedSource == kSimSeed){
                mergeHitmaps( fstHitmap, fttHitmap );
            } else {
                if ( mSeedSource == kFstSeed || mSeedSource == kSeqSeed ){
                    doSeedFindingIteration(iIteration, fstHitmap);
                }
                if ( mSeedSource == kFttSeed || mSeedSource == kSeqSeed){
                    doSeedFindingIteration(iIteration, fttHitmap);
                }
            }
        } // iIteration
        /*************************************************************/
        if (kProfile) mEventStats.mNumSeeds = mTrackSeeds.size();
        long long duration2 = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (kProfile) mEventStats.mSeedFindingDuration.push_back( duration2 );

        if (verbose) {
            LOG_INFO << "Found " << mTrackSeeds.size() << " seeds in " << nIterations << " iterations" << endm;
            LOG_INFO << "Total Hits removed: " << mTotalHitsRemoved << endm;

            int sCount = 0;
            for ( auto &seed : mTrackSeeds ) {
                string hitSummary = "";
                for ( auto &hit : seed ) {
                    auto fwdHit = dynamic_cast<FwdHit*>(hit);
                    if (fwdHit == nullptr) continue;
                    hitSummary += TString::Format( "id=%d@%d", fwdHit->_id, hit->getSector() ).Data();
                }
                LOG_INFO << "Seed [" << sCount << "] with " << seed.size() << " hits : " << hitSummary <<  endm;
                sCount++;
            } 

        }
    } // FindTrackSeeds


    std::vector< genfit::GFRaveVertex * > findFwdVertices( const vector<GenfitTrackResult> &globalTracks ){
        
        // The RAVE factory needs the (bare) track pointers for vertex finding
        vector<genfit::Track*> tracks;
        for ( auto gtr : globalTracks ){
            if ( gtr.mTrack ){
                tracks.push_back( gtr.mTrack.get() );
                LOG_DEBUG << "Adding track with track obj: " << gtr.mTrack.get() << endm;
            }
        }
        LOG_DEBUG << "FwdTracker::findFwdVertices with " << tracks.size() << " tracks" << endm;

        //  we are not using this because secondary vertices should not neccessarily be at the beamline
        bool useBeamConstraint = false;
        if ( useBeamConstraint ){
            // TODO: load "official" beamline constraint parameters
            TMatrixDSym bscm(3);
            const double bssXY = 2.0;
            bscm(0, 0) = bssXY*bssXY;
            bscm(1, 1) = bssXY*bssXY;
            bscm(2, 2) = 50.5 * 50.5;
            mGFRVertexFactory.setBeamspot( TVector3( 0, 0, 0 ), bscm );
        }

        mGFRVertexFactory.findVertices( &mFwdVertices, tracks, useBeamConstraint );
        LOG_DEBUG << "mFwdVertices.size() = " << mFwdVertices.size() << endm;
        if (verbose){
            for ( auto vert : mFwdVertices ){
                LOG_DEBUG << TString::Format( "GFRaveVertex vertex @(%f, %f, %f)\n\n", vert->getPos().X(), vert->getPos().Y(), vert->getPos().Z() ) << endm;
                LOG_DEBUG << "GFRaveVertex" << endm;
                LOG_DEBUG << "Ndf: " << vert->getNdf() << ", Chi2: " << vert->getChi2() << ", Id: " << vert->getId() << endm;
                LOG_DEBUG << "Number of tracks: " << vert->getNTracks() << endm;
            }
        }

        return mFwdVertices;
    }

    /**
     * @brief Perform a single fit from seed points
     *
     * @param seed : seed points from either FTT or FST
     * @param includeVertex : include the primary vertex in the fit or not
     * @return GenfitTrackResult : result of the fit
     */
    GenfitTrackResult fitTrack(Seed_t &seed, TVector3 *momentumSeedState = nullptr) {
        LOG_DEBUG << "FwdTracker::fitTrack->" << endm;
        if (kProfile) mEventStats.mAttemptedFits++;
        // We will build this up as we go
        GenfitTrackResult gtr;

        LOG_DEBUG << "--Setting seed on GenfitTrackResult, seed has " << seed.size() << " hits" << endm;
        // First, set the seed information
        gtr.setSeed( seed ); // TODO: ADD momentum and charge

        // If we are using a provided momentum state
        if ( momentumSeedState ){
            LOG_DEBUG << "--FitTrack with provided momentum seed state" << endm;
            mTrackFitter->fitTrack( seed, momentumSeedState );
        } else {
            LOG_DEBUG << "--FitTrack without provided momentum seed state" << endm;
            mTrackFitter->fitTrack( seed );
        }

        /*******************************************************/
        // Get the track from the fitter
        // and set the track in the GenfitTrackResult
        if (mTrackFitter->getTrack() != nullptr && mTrackFitter->getTrack()->getFitStatus()->isFitConvergedPartially()) {
            LOG_DEBUG << "--FitTrack is valid, setting seed and track" << endm;
            gtr.set( seed, mTrackFitter->getTrack() );

            LOG_INFO << "--isFitConvergedPartially() = " << gtr.mIsFitConvergedPartially << endm;
            LOG_INFO << "--isFitConverged() = " << gtr.mIsFitConverged << endm;
            LOG_INFO << "--isFitConvergedFully() = " << gtr.mIsFitConvergedFully << endm;

            if (kProfile){
                if (gtr.mIsFitConvergedFully) {
                    mEventStats.mGoodFits++;
                } else {
                    mEventStats.mFailedFits++;
                }
            }
        } else { // set the track as a failed fit, but keep the seed info
            LOG_ERROR << "--FitTrack is nullptr or fit did not converge, setting seed only" << endm;
            gtr.setSeed( seed, mTrackFitter->getCurrentSeedMomentum(), mTrackFitter->getCurrentSeedCharge() ); // TODO: ADD momentum and charge
            if (kProfile) mEventStats.mFailedFits++;
        }
        LOG_DEBUG << "<-FwdTracker::fitTrack complete" << endm;
        return gtr;
    } // fitTrack

    GenfitTrackResult refitTrack( GenfitTrackResult gtrGlobal ) {
        LOG_DEBUG << "FwdTracker::refitTrack->" << endm;
        const bool doRefit = mConfig.get<bool>("TrackFitter:refit", false);
        if ( !doRefit ){
            LOG_DEBUG << "Refit is disabled, returning nill track result" << endm;
            GenfitTrackResult nil;
            return nil;
        }
        if ( !gtrGlobal.mIsFitConvergedFully ){
            LOG_DEBUG << "Cannot refit a track that did not converge, returning nill track result" << endm;
            GenfitTrackResult nil;
            return nil;
        }

        int numHitsFound = 0;
        if (mSeedSource != kFttSeed){ // Look for FTT hits if it was not the original seed source
            for ( int i = 0; i < FwdSystem::sNFttLayers; i++ ){
                numHitsFound += addFttHits( gtrGlobal, i );
            }
        }
        if (mSeedSource != kFstSeed ){ // Look for FST hits if it was not the original seed source
            for ( int i = 0; i < FwdSystem::sNFstLayers; i++ ){
                numHitsFound += addFstHits( gtrGlobal, i );
            }
        }

        int numEpdFound = addEpdHits( gtrGlobal );
        numHitsFound += numEpdFound;

        if (kProfile) {
            if ( gtrGlobal.mTrackType == StFwdTrack::kGlobal ){
                mEventStats.numGlobalFoundHits.push_back( numHitsFound );
                mEventStats.mGlobalNumEpdFoundHits.push_back(numEpdFound);
            } else if ( gtrGlobal.mTrackType == StFwdTrack::kBeamlineConstrained ){
                mEventStats.numBeamlineFoundHits.push_back( numHitsFound );
                mEventStats.mBeamlineNumEpdFoundHits.push_back(numEpdFound);
            } else if ( gtrGlobal.mTrackType == StFwdTrack::kPrimaryVertexConstrained ){
                mEventStats.numPrimaryFoundHits.push_back( numHitsFound );
                mEventStats.mPrimaryNumEpdFoundHits.push_back(numEpdFound);
            } else if ( gtrGlobal.mTrackType == StFwdTrack::kForwardVertexConstrained ){
                mEventStats.numSecondaryFoundHits.push_back( numHitsFound );
                mEventStats.mSecondaryNumEpdFoundHits.push_back(numEpdFound);
            }
        }

        auto gtrGlobalRefit = fitTrack( gtrGlobal.mSeed, &gtrGlobal.mMomentum );
        gtrGlobalRefit.setDCA( mEventVertex );

        return gtrGlobalRefit;
    }

    std::vector<GenfitTrackResult> doGlobalTrackFitting( const std::vector<Seed_t> &trackSeeds ){
        LOG_DEBUG << ">>doGlobalTrackFitting" << endm;
        long long itStart = FwdTrackerUtils::nowNanoSecond();

        std::vector<GenfitTrackResult> globalTracks;

        // Should we try to refit the track with aadditional points from other detectors?
        const bool doRefit = mConfig.get<bool>("TrackFitter:refit", false);
        LOG_INFO << "TrackFitter:refit = " << doRefit << endm;

        LOG_DEBUG << "Starting track fitting loop, mTrackResults.size() = " << mTrackResults.size() << endm;
        LOG_DEBUG << "Starting Track fitting loop on " << trackSeeds.size() << " track seeds" << endm;
        size_t index = 0;
        for (auto t : trackSeeds) {
            if (kProfile) mEventStats.mAttemptedGlobalFits++;
            // GenfitTrackResult gtrGlobalRefit; // will store refit if needed
            LOG_DEBUG << "\tTrack seed initial global fit #" << index << endm;
            /***********************************************************************************************************/
            // Tracking Step 1
            // Fit each accepted track seed

            // If we are using MC momentum get it from associated track
            
            int idt = 0;
            double qual = 0;
            //  Get the quality and MC truth id
            idt = MCTruthUtils::dominantContribution(t, qual);
            LOG_INFO << "\t\tMc Match idTruth=" << idt << ", quality = " << qual << endm;
            
            // Fit the track seed and get the GenfitTrackResult
            GenfitTrackResult gtrGlobal = fitTrack(t);
            gtrGlobal.setDCA( mEventVertex );
            gtrGlobal.mIndex = index;
            gtrGlobal.mTrackType = StFwdTrack::kGlobal;
            LOG_DEBUG << "\tGLOBAL Fit: Track seed with " << gtrGlobal.mSeed.size() << " hits" << endm;
            // End Step 1
            /*******************************************************/

            // if the first fit fails then we cannot proceed with the refit steps
            if (gtrGlobal.mIsFitConvergedFully == false) {
                LOG_WARN << "\tInitial global fitting failed for seed " << index << endm;
                LOG_DEBUG << "\tFitting failed for seed " << index << endm;
                LOG_DEBUG << "\tSkipping the refit steps but saving the seed and failed fit" << endm;
                globalTracks.push_back( gtrGlobal );
                if (kProfile) mEventStats.mFailedGlobalFits++;
                index++;
                continue;
                // BREAK OUT OF THE LOOP
            }
            if (kProfile) mEventStats.mGoodGlobalFits++;
            if (doRefit == false) {
                LOG_INFO << "\tRefit is disabled, saving the seed and initial fit" << endm;
                globalTracks.push_back( gtrGlobal );
                index++;
                continue;
                // BREAK OUT OF THE LOOP
            }

            /***********************************************************************************************************/
            // Tracking Step 2
            // Look for additional hits in the other tracking detector
            // and add the new hits to the track

            GenfitTrackResult gtrGlobalRefit = refitTrack( gtrGlobal );
            gtrGlobalRefit.mIndex = index;
            gtrGlobalRefit.mTrackType = StFwdTrack::kGlobal;
            // End Step 2
            /***********************************************************************************************************/

            /***********************************************************************************************************/
            // Tracking Step 3: Save the best global track result
            if ( gtrGlobalRefit.mIsFitConvergedFully ){
                globalTracks.push_back( gtrGlobalRefit ); // save this global track result
                gtrGlobal.Clear(); // clear the original global track result since we will save the refit
                if (kProfile) mEventStats.mGoodGlobalRefits++;
            } else {
                globalTracks.push_back( gtrGlobal ); // save the original global track result
                gtrGlobalRefit.Clear(); // clear the refit since it failed
                if (kProfile) mEventStats.mFailedGlobalRefits++;
            }            
            // End Step 4
            /***********************************************************************************************************/
        } // loop on track seeds


        long long duration1 = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (kProfile) mEventStats.mGlobalFitDuration.push_back( duration1 );
        // SUMMARY OF STATUS
        if (verbose > 0 && kProfile) {
            float perGood = (float) mEventStats.mGoodGlobalFits / (float) mEventStats.mAttemptedGlobalFits;
            float perFailed = (float) mEventStats.mFailedGlobalFits / (float) mEventStats.mAttemptedGlobalFits;
            float perGoodRefit = (float) mEventStats.mGoodGlobalRefits / (float) mEventStats.mGoodGlobalFits;
            float perFailedRefit = (float) mEventStats.mFailedGlobalRefits / (float) mEventStats.mGoodGlobalFits;
            float sum = std::accumulate(mEventStats.numGlobalFoundHits.begin(), mEventStats.numGlobalFoundHits.end(), 0.0f);
            float avgAdded = sum / mEventStats.numGlobalFoundHits.size();
         
            LOG_INFO << "\tGlobal Track Fitting Results " << Form( "(took %lld ms):", duration1 ) <<
                    TString::Format(
                        "Attempts = %d, Good = %d (%f%%), Failed = %d (%f%%), <#hits> added = %f, Refit:Good = %d (%f%%), Failed = %d (%f%%)",
                        mEventStats.mAttemptedGlobalFits,
                        mEventStats.mGoodGlobalFits,
                        perGood,
                        mEventStats.mFailedGlobalFits,
                        perFailed,
                        avgAdded,
                        mEventStats.mGoodGlobalRefits,
                        perGoodRefit,
                        mEventStats.mFailedGlobalRefits,
                        perFailedRefit
                    ) << endm;
        }

        return globalTracks;
    }

    std::vector<GenfitTrackResult> doPrimaryTrackFitting( std::vector<GenfitTrackResult> globalTracks) {
        if (verbose){
            LOG_INFO << ">>doPrimaryTrackFitting" << Form("( #globals = %lu )", globalTracks.size()) << endm;
        }
        long long itStart = FwdTrackerUtils::nowNanoSecond();

        std::vector<GenfitTrackResult> primaryTracks;

        size_t index = 0;
        for (auto &gtr : globalTracks) {
            if (kProfile) mEventStats.mAttemptedPrimaryFits ++;
            if (verbose){
                LOG_INFO << "Refitting Track " << index << ", McId=" << gtr.mIdTruth << " with Primary Vertex, seed already has: " << gtr.mSeed.size() << " hits" << endm;
                LOG_INFO << "mEventVertexHit: " << mEventVertexHit.getX() << ", " << mEventVertexHit.getY() << ", " << mEventVertexHit.getZ() << endm;

                LOG_INFO << "This fit is for a global that converged? = " << gtr.mIsFitConvergedFully << endm;
            }
            // just use the global track to build the track that will use the PV also
            Seed_t seedWithPV = gtr.mSeed;
            seedWithPV.push_back( &mEventVertexHit );
            

            GenfitTrackResult gtrPV = fitTrack(seedWithPV, &gtr.mMomentum);
            gtrPV.mTrackType = StFwdTrack::kPrimaryVertexConstrained;
            gtrPV.mGlobalTrackIndex = gtr.mIndex;
            gtrPV.mVertexIndex = 0;


            if ( gtrPV.mIsFitConvergedPartially ) {
                if (kProfile) mEventStats.mGoodPrimaryFits++;
            } else {
                if (kProfile) mEventStats.mFailedPrimaryFits++;

                if (kSaveFailedFits) {
                    primaryTracks.push_back( gtrPV );
                } else {
                    gtrPV.Clear();
                }
                continue;
            }
            // only do this for a track the converges -> that we can project
            gtrPV.setDCA( mEventVertex );
            
            LOG_INFO << "\tInitial fit complete, now refitting with additional points" << endm;
            // refit the track with additional points
            GenfitTrackResult gtrPVRefit = refitTrack( gtrPV );
            gtrPVRefit.mIndex = index;
            gtrPVRefit.mTrackType = StFwdTrack::kPrimaryVertexConstrained;
            gtrPVRefit.mGlobalTrackIndex = gtr.mIndex;
            gtrPVRefit.mVertexIndex = 0;

            if ( gtrPVRefit.mIsFitConvergedPartially ){
                primaryTracks.push_back( gtrPVRefit );
                gtrPV.Clear(); // clear the original global track result since we will save the refit
                if (kProfile) mEventStats.mGoodPrimaryRefits++;
            } else {
                if (kProfile) mEventStats.mFailedPrimaryRefits++;
                gtrPVRefit.Clear(); // clear the refit since it failed
                primaryTracks.push_back( gtrPV );
                LOG_WARN << "\tFWD primary track refit failed (both initial + refit)" << endm;
            }
            
            index++;
        }

        long long duration1 = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (kProfile) mEventStats.mPrimaryFitDuration.push_back( duration1 );

        
        if (verbose > 0 && kProfile) { 
            // SUMMARY OF STATUS
            float perGoodPrim = (float) mEventStats.mGoodPrimaryFits / (float) mEventStats.mAttemptedPrimaryFits;
            float perFailedPrim = (float) mEventStats.mFailedPrimaryFits / (float) mEventStats.mAttemptedPrimaryFits;
            float perGoodRefit = (float) mEventStats.mGoodPrimaryRefits / (float) mEventStats.mGoodPrimaryFits;
            float perFailedRefit = (float) mEventStats.mFailedPrimaryRefits / (float) mEventStats.mGoodPrimaryFits;
            float sum = std::accumulate(mEventStats.numPrimaryFoundHits.begin(), mEventStats.numPrimaryFoundHits.end(), 0.0f);
            float avgAdded = sum / mEventStats.numPrimaryFoundHits.size();
            LOG_INFO << "\tPrimary Track Fitting Results " << Form( "(took %lld ms):", duration1 ) <<
                    TString::Format(
                        "Attempts = %d, Good = %d (%f%%), Failed = %d (%f%%), <#hits> added = %f, Refit:Good = %d (%f%%), Failed = %d (%f%%)",
                        mEventStats.mAttemptedPrimaryFits,
                        mEventStats.mGoodPrimaryFits,
                        perGoodPrim,
                        mEventStats.mFailedPrimaryFits,
                        perFailedPrim,
                        avgAdded,
                        mEventStats.mGoodPrimaryRefits,
                        perGoodRefit,
                        mEventStats.mFailedPrimaryRefits,
                        perFailedRefit
                    ) << endm;
        }

        return primaryTracks;
    }

    std::vector<GenfitTrackResult> doBeamlineTrackFitting( std::vector<GenfitTrackResult> globalTracks) {
        if (verbose){
            LOG_INFO << ">>doBeamlineTrackFitting" << Form("( #globals = %lu )", globalTracks.size()) << endm;
        }
        long long itStart = FwdTrackerUtils::nowNanoSecond();

        std::vector<GenfitTrackResult> beamlineTracks;

        size_t index = 0;
        for (auto &gtr : globalTracks) {
            if (kProfile) mEventStats.mAttemptedBeamlineFits ++;
            if (verbose){
                LOG_INFO << "doBeamlineTrackFitting>>" << index << " McId=" << gtr.mIdTruth << " with Beamline, seed already has: " << gtr.mSeed.size() << " hits" << endm;
                LOG_INFO << "mBeamlineHit: " << mBeamlineHit.getX() << ", " << mBeamlineHit.getY() << ", " << mBeamlineHit.getZ() << endm;

                LOG_INFO << "This fit is for a global that converged? = " << gtr.mIsFitConvergedFully << endm;
            }
            // just use the global track to build the track that will use the PV also
            Seed_t seedWithPV = gtr.mSeed;
            seedWithPV.push_back( &mBeamlineHit );

            GenfitTrackResult gtrPV; 
            
            if ( true || gtr.mIsFitConvergedFully == false ){
                // if we do not provide a momentum seed state then the setup will compute one using the selected scheme
                gtrPV = fitTrack(seedWithPV);    
            } else {
                // Only use the momentum of the global track if it converged
                gtrPV = fitTrack(seedWithPV, &gtr.mMomentum);
            }

            gtrPV.mTrackType = StFwdTrack::kBeamlineConstrained;
            gtrPV.mGlobalTrackIndex = gtr.mIndex;
            gtrPV.mVertexIndex = 0;
            LOG_INFO << "SEED momentum:" << gtr.mMomentum.X() << ", " << gtr.mMomentum.Y() << ", " << gtr.mMomentum.Z() << endm;

            if ( gtrPV.mIsFitConvergedFully ) {
                if (kProfile) mEventStats.mGoodBeamlineFits++;
            } else {
                LOG_DEBUG << "\tInitial Beamline fitting failed for seed " << index << endm;
                if (kProfile) mEventStats.mFailedBeamlineFits++;

                if (kSaveFailedFits) {
                    beamlineTracks.push_back( gtrPV );
                } else {
                    gtrPV.Clear();
                }
                continue;
            }
            gtrPV.setDCA( mEventVertex );
            

            LOG_INFO << "\tInitial Beamline fit completed, now refitting with additional hits" << endm;
            // refit the track with additional points
            GenfitTrackResult gtrPVRefit = refitTrack( gtrPV );
            gtrPVRefit.mIndex = index;
            gtrPVRefit.mTrackType = StFwdTrack::kBeamlineConstrained;
            gtrPVRefit.mGlobalTrackIndex = gtr.mIndex;
            gtrPVRefit.mVertexIndex = 0;


            if ( gtrPVRefit.mIsFitConvergedFully){
                beamlineTracks.push_back( gtrPVRefit );
                gtrPV.Clear(); // clear the original global track result since we will save the refit
                if (kProfile) mEventStats.mGoodBeamlineRefits++;
            } else {
                beamlineTracks.push_back( gtrPV );
                gtrPVRefit.Clear(); // clear the refit since it failed
                if (kProfile) mEventStats.mFailedBeamlineRefits++;
                LOG_DEBUG << "\tBeamline track refit failed" << endm;
            }
            index++;
        }

        long long duration1 = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (kProfile) mEventStats.mBeamlineFitDuration.push_back( duration1 );

        // SUMMARY OF STATUS
        if (verbose > 0 && kProfile) {
            float perGoodPrim = (float) mEventStats.mGoodBeamlineFits / (float) mEventStats.mAttemptedBeamlineFits;
            float perFailedPrim = (float) mEventStats.mFailedBeamlineFits / (float) mEventStats.mAttemptedBeamlineFits;
            float perGoodRefit = (float) mEventStats.mGoodBeamlineRefits / (float) mEventStats.mGoodBeamlineFits;
            float perFailedRefit = (float) mEventStats.mFailedBeamlineRefits / (float) mEventStats.mGoodBeamlineFits;
            float sum = std::accumulate(mEventStats.numBeamlineFoundHits.begin(), mEventStats.numBeamlineFoundHits.end(), 0.0f);
            float avgAdded = sum / mEventStats.numBeamlineFoundHits.size();
         
            LOG_INFO << "\tBeamline Track Fitting Results " << Form( "(took %lld ms):", duration1 ) <<
                    TString::Format(
                        "Attempts = %d, Good = %d (%f%%), Failed = %d (%f%%), <#hits> added = %f, Refit:Good = %d (%f%%), Failed = %d (%f%%)",
                        mEventStats.mAttemptedBeamlineFits,
                        mEventStats.mGoodBeamlineFits,
                        perGoodPrim,
                        mEventStats.mFailedBeamlineFits,
                        perFailedPrim,
                        avgAdded,
                        mEventStats.mGoodBeamlineRefits,
                        perGoodRefit,
                        mEventStats.mFailedBeamlineRefits,
                        perFailedRefit
                    ) << endm;
        }

        return beamlineTracks;
    }

    std::vector<GenfitTrackResult> doSecondaryTrackFitting( std::vector<GenfitTrackResult> globalTracks) {
        mFwdVerticesAsHits.clear();
        if (verbose){
            LOG_INFO << ">>doSecondaryTrackFitting" << Form("( #globals = %lu )", globalTracks.size()) << endm;
        }
        long long itStart = FwdTrackerUtils::nowNanoSecond();

        std::vector<GenfitTrackResult> secondaryTracks;

        size_t index = 0;
        for (auto vtx : mFwdVertices){
            LOG_INFO << "FwdVertex: " << vtx->getId() << ", " << vtx->getPos().X() << ", " << vtx->getPos().Y() << ", " << vtx->getPos().Z() << endm;
            LOG_INFO << "\tnTracks: " << vtx->getNTracks() << endm;
            LOG_INFO << "\tChi2: " << vtx->getChi2() << endm;
            LOG_INFO << "\tNdf: " << vtx->getNdf() << endm;

            TDecompChol decomp( vtx->getCov() );
            if ( !decomp.Decompose() ){
                LOG_WARN << "FwdVertex: " << vtx->getId() << ", covariance matrix is not valid" << endm;
                continue;
            }
            
            FwdHit vtxHit;
            vtxHit.setXYZDetId( vtx->getPos().X(), vtx->getPos().Y(), vtx->getPos().Z(), kTpcId );
            vtxHit._covmat = vtx->getCov();
            mFwdVerticesAsHits.push_back( vtxHit );

            // loop on the tracks in the vertex to find the correct global track
            for ( size_t iVtxTrack = 0; iVtxTrack < vtx->getNTracks(); iVtxTrack++ ){
                genfit::GFRaveTrackParameters* par = vtx->getParameters(iVtxTrack);
                if ( par == nullptr ){
                    LOG_WARN << "FwdVertex: " << vtx->getId() << ", iVtxTrack = " << iVtxTrack << ", par == nullptr" << endm;
                    continue;
                }
                auto vtxTrack = par->getTrack();
                if ( vtxTrack == nullptr ){
                    LOG_WARN << "FwdVertex: " << vtx->getId() << ", iVtxTrack = " << iVtxTrack << ", vtxTrack == nullptr" << endm;
                    continue;
                }
                auto gtr = std::find_if( globalTracks.begin(), globalTracks.end(), [&]( GenfitTrackResult &gtr ) {
                    return gtr.mTrack.get() == vtxTrack;
                });
                if ( gtr == globalTracks.end() ){
                    LOG_WARN << "FwdVertex: " << vtx->getId() << ", iVtxTrack = " << iVtxTrack << ", gtr == globalTracks.end()" << endm;
                    continue;
                }
                LOG_INFO << "FOUND global track for vertex " << vtx->getId() << ", iVtxTrack = " << iVtxTrack << endm;
                
                Seed_t seedWithVtx = gtr->mSeed;
                seedWithVtx.push_back( &mFwdVerticesAsHits.back() );
                GenfitTrackResult gtrPV = fitTrack(seedWithVtx, &gtr->mMomentum);
                if ( gtrPV.mIsFitConvergedFully ) {
                    if (kProfile) mEventStats.mGoodSecondaryFits++;
                } else {
                    if (kProfile) mEventStats.mFailedSecondaryFits++;
                    gtrPV.Clear();
                    continue;
                }
                gtrPV.setDCA( TVector3( vtx->getPos().X(), vtx->getPos().Y(), vtx->getPos().Z() ) );
                gtrPV.mTrackType = StFwdTrack::kForwardVertexConstrained;
                gtrPV.mGlobalTrackIndex = gtr->mIndex;
                gtrPV.mVertexIndex = vtx->getId();

                LOG_INFO << "\tInitial fit complete, now refitting with additional points" << endm;
                // refit the track with additional points
                GenfitTrackResult gtrPVRefit = refitTrack( gtrPV );
                gtrPVRefit.mIndex = index;
                gtrPVRefit.mTrackType = StFwdTrack::kForwardVertexConstrained;
                gtrPVRefit.mGlobalTrackIndex = gtr->mIndex;
                gtrPVRefit.mVertexIndex = vtx->getId();
                if ( gtrPVRefit.mIsFitConvergedPartially ){
                    secondaryTracks.push_back( gtrPVRefit );
                    gtrPV.Clear(); // clear the original global track result since we will save the refit
                    if (kProfile) mEventStats.mGoodSecondaryRefits++;
                } else {
                    secondaryTracks.push_back( gtrPV );
                    gtrPVRefit.Clear(); // clear the refit since it failed
                    if (kProfile) mEventStats.mFailedSecondaryRefits++;
                    LOG_WARN << "\tFWD secondary track refit failed" << endm;
                }
                index++;
            } // loop on tracks in vertex
        } // loop on vertices

        long long duration1 = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (kProfile) mEventStats.mSecondaryFitDuration.push_back( duration1 );

        // SUMMARY OF STATUS
        if (verbose > 0 && kProfile) {
            float perGoodPrim = (float) mEventStats.mGoodSecondaryFits / (float) mEventStats.mAttemptedSecondaryFits;
            float perFailedPrim = (float) mEventStats.mFailedSecondaryFits / (float) mEventStats.mAttemptedSecondaryFits;
            float perGoodRefit = (float) mEventStats.mGoodSecondaryRefits / (float) mEventStats.mGoodSecondaryFits;
            float perFailedRefit = (float) mEventStats.mFailedSecondaryRefits / (float) mEventStats.mGoodSecondaryFits;
            float sum = std::accumulate(mEventStats.numSecondaryFoundHits.begin(), mEventStats.numSecondaryFoundHits.end(), 0.0f);
            float avgAdded = sum / mEventStats.numSecondaryFoundHits.size();
         
            LOG_INFO << "\tSecondary Track Fitting Results " << Form( "(took %lld ms):", duration1 ) <<
                    TString::Format(
                        "Attempts = %d, Good = %d (%f%%), Failed = %d (%f%%), <#hits> added = %f, Refit:Good = %d (%f%%), Failed = %d (%f%%)",
                        mEventStats.mAttemptedSecondaryFits,
                        mEventStats.mGoodSecondaryFits,
                        perGoodPrim,
                        mEventStats.mFailedSecondaryFits,
                        perFailedPrim,
                        avgAdded,
                        mEventStats.mGoodSecondaryRefits,
                        perGoodRefit,
                        mEventStats.mFailedSecondaryRefits,
                        perFailedRefit
                    ) << endm;
        }
        return secondaryTracks;
    }


    /**
     * @brief Loop on track seeds and fit each one
     *
     * Track fitting proceeds in 3 possible iterations
     * 1. Fit seed points (without PV)
     * 2. Look for additional hits in the other tracking detector
     * 3. Refit the track with the additional hits
     * 4. Refit the track with the primary vertex
     * 5. look again and refit any additional hits
     *
     * @param trackSeeds : Track seeds
     */
    void doTrackFitting( const std::vector<Seed_t> &trackSeeds) {
        LOG_DEBUG << ">>doTrackFitting" << endm;
        
        long long itStart = FwdTrackerUtils::nowNanoSecond();

        std::vector<GenfitTrackResult> globalTracks;
        std::vector<GenfitTrackResult> primaryTracks;
        std::vector<GenfitTrackResult> beamlineTracks;
        std::vector<GenfitTrackResult> secondaryTracks;

        // Should we try to refit the track with aadditional points from other detectors?
        const bool doRefit = mConfig.get<bool>("TrackFitter:refit", false);
        LOG_INFO << "TrackFitter:refit = " << doRefit << endm;
        LOG_INFO << "Starting Track fitting loop with " << trackSeeds.size() << " track seeds" << endm;

        const bool do_global_fitting = mConfig.get<bool>("TrackFitter:doGlobalTrackFitting", true);
        if ( !do_global_fitting || !mDoTrackFitting ){
            LOG_WARN << "Event configuration is skipping global track fitting" << endm;
            LOG_WARN << "No Fwd track fitting will be done" << endm;
            // create a dummy track result to keep the seeds
            for ( auto seed : trackSeeds ){
                GenfitTrackResult gtr;
                GenericFitSeeder gfs;
                TVector3 pos = TVector3( 0, 0, 0);
                TVector3 mom = TVector3( 0, 0, 0 ); // default momentum
                int q = 0; // default charge
                gfs.makeSeed( seed, pos, mom, q);
                gtr.setSeed( seed, mom, q ); // this saves the seed info as if it is track info
                gtr.mTrackType = StFwdTrack::kGlobal;
                gtr.mVertexIndex = 0;
                globalTracks.push_back( gtr );
                LOG_DEBUG << "Adding dummy global track with seed: " << seed.size() << " hits" << endm;
            }
            mTrackResults.insert( mTrackResults.end(), globalTracks.begin(), globalTracks.end() );
            return;
        }

        /***********************************************************************************************************/
        // Step 1: Global Tracking
        globalTracks = doGlobalTrackFitting( trackSeeds );
        if (verbose > 0){
            LOG_INFO << "Global track fitting completed, found " << globalTracks.size() << " global tracks from " << trackSeeds.size() << " track seed candidates" << endm;
        }
        // End Step 1
        /***********************************************************************************************************/
        
        /***********************************************************************************************************/
        // Step 2: Find the FWD Vertices
        const bool do_fwd_vertex_finding = mConfig.get<bool>("TrackFitter:findFwdVertices", true);
        if (do_fwd_vertex_finding){
            LOG_DEBUG << "\tPerforming FWD Vertex Finding" << endm;
            auto fwdVertices = findFwdVertices( globalTracks );
            if (kProfile) mEventStats.mNumFwdVertices = fwdVertices.size();
        } else {
            LOG_WARN << "Event configuration is skipping FWD vertex finding" << endm;
            LOG_WARN << "Secondary track fitting will also be skipped" << endm;
        }
        // End Step 2
        /***********************************************************************************************************/

        const bool do_beamline_fitting = mConfig.get<bool>("TrackFitter:doBeamlineTrackFitting", true);
        /***********************************************************************************************************/
        // Step 3: Refit the track with the beamline
        if (do_beamline_fitting){
            beamlineTracks = doBeamlineTrackFitting( globalTracks );
        } else {
            // these are warnings because they should be done in normal productions
            LOG_WARN << "Event configuration is skipping beamline track fitting" << endm;
        }
        // End Step 3
        /***********************************************************************************************************/


        const bool do_fwd_primary_fitting = mConfig.get<bool>("TrackFitter:doPrimaryTrackFitting", true);;
        /***********************************************************************************************************/
        // Step 4: Refit the track with the primary vertex
        if (do_fwd_primary_fitting){
            primaryTracks = doPrimaryTrackFitting( globalTracks );
        } else {
            LOG_WARN << "Event configuration is skipping primary track fitting" << endm;
        }
        // End Step 4
        /***********************************************************************************************************/

        /***********************************************************************************************************/
        // Step 5: Refit the track with the primary vertex
        const bool do_fwd_secondary_fitting = mConfig.get<bool>("TrackFitter:doSecondaryTrackFitting", true);;
        if (do_fwd_secondary_fitting){
            secondaryTracks = doSecondaryTrackFitting( globalTracks );
        } else {
            LOG_INFO << "Event configuration is skipping secondary track fitting" << endm;
        }
        // End Step 5
        /***********************************************************************************************************/

        // Add the global and primary tracks to the results
        LOG_DEBUG << "Ending track fitting loop, mTrackResults.size() = " << mTrackResults.size() << endm;
        mTrackResults.insert( mTrackResults.end(), globalTracks.begin(), globalTracks.end() );
        mTrackResults.insert( mTrackResults.end(), primaryTracks.begin(), primaryTracks.end() );
        mTrackResults.insert( mTrackResults.end(), beamlineTracks.begin(), beamlineTracks.end() );
        mTrackResults.insert( mTrackResults.end(), secondaryTracks.begin(), secondaryTracks.end() );
        LOG_DEBUG << "Copied globals, beamline, primary, and secondary. Now mTrackResults.size() = " << mTrackResults.size() << endm;

        long long duration2 = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        LOG_DEBUG << "Track fitting took " << duration2 << "ms" << endm;
        LOG_DEBUG << "We fit: " 
            << globalTracks.size() << " global tracks, " 
            << primaryTracks.size() << " primary tracks, " 
            << beamlineTracks.size() << " beamline tracks, "
            << secondaryTracks.size() << " secondary tracks. "
            << " Total track fits = " << mTrackResults.size() << endm;
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

        mTrackSeedsThisIteration.clear();
        // we will build reco tracks from each McTrack
        for (auto kv : mcTrackMap) {

            auto mc_track = kv.second;
            LOG_DEBUG << "McTrack[ " << kv.first << " ]: nFtt=" << mc_track->mFttHits.size() << ", nFst=" << mc_track->mFstHits.size() << endm;

            if (seedSource == kFttSeed && mc_track->mFttHits.size() < 2){ // require min 4 FTT hits on track
                continue;
            }

            if (seedSource == kFstSeed && mc_track->mFstHits.size() < 2 ) { // require min 3 FST hits on track
                LOG_DEBUG << "Skipping McSeedFinding bc FST hits < 2" << endm;
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
                mTrackSeedsThisIteration.push_back(track);
            } else {
                //Skipping track that doesnt have hits on all layers
            }
        }

        LOG_DEBUG << "McTrackFinding Found: " << mTrackSeedsThisIteration.size() << " tracks" << endm;
        // doTrackFitting(mTrackSeedsThisIteration);

        // Now save to the main reco track list
        mTrackSeeds.insert( mTrackSeeds.end(), mTrackSeedsThisIteration.begin(), mTrackSeedsThisIteration.end() );
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

    /** doSeedFindingOnHitmapSubset
     * @brief Does track finding steps on a subset of hits (phi slice)
     * @param iIteration: tracking iteration (for determining params)
     * @param hitmap: the hitmap to use, should already be subset of original
     * @returns a list of track seeds
     */
    vector<Seed_t> doSeedFindingOnHitmapSubset( size_t iIteration, FwdDataSource::HitMap_t &hitmap  ) {
        long long itStart = FwdTrackerUtils::nowNanoSecond();

        std::vector<Seed_t> acceptedTrackSeeds;
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
        if (mSeedSource == kFttSeed){
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
            return acceptedTrackSeeds;
        }

        // at any point we can get a list of tracks out like this:
        // std::vector < std::vector< KiTrack::IHit* > > tracks = automaton.getTracks();
        // we can apply an optional parameter <nHits> to only get tracks with >=nHits in them

        long long duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (kProfile) mEventStats.mStep2Duration.push_back(duration);
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
        if (kProfile) mEventStats.mStep3Duration.push_back( duration );
        if (duration > 2000 || automaton.getNumberOfConnections() > 9000){
            LOG_WARN << "The Three Hit Criteria took more than 200ms to process, duration: " << duration << " ms" << endm;
            LOG_WARN << "bailing out (skipping subset HNN)" << endm;

            std::string subsetPath = "TrackFinder.Iteration[" + std::to_string(iIteration) + "].SubsetNN";
            size_t minHitsOnTrack = mConfig.get<size_t>(subsetPath + ":min-hits-on-track", FwdSystem::sNFstLayers);
            acceptedTrackSeeds = automaton.getTracks(minHitsOnTrack);
            return acceptedTrackSeeds;
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
            size_t minHitsOnTrack = mConfig.get<size_t>(subsetPath + ":min-hits-on-track", FwdSystem::sNFstLayers);
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

            SeedCompatible comparer;
            SeedQual quality;

            subset.calculateBestSet(comparer, quality);

            acceptedTrackSeeds = subset.getAccepted();

            // this call takes a long time due to possible huge combinatorics.
            // rejectedTracks = subset.getRejected();
            // LOG_DEBUG << "We had " << tracks.size() << " tracks. Accepted = " << acceptedTrackSeeds.size() << ", Rejected = " << rejectedTracks.size() << endm;

        } else { // the subset and hit removal
            size_t minHitsOnTrack = mConfig.get<size_t>(subsetPath + ":min-hits-on-track", FwdSystem::sNFttLayers);
            acceptedTrackSeeds = automaton.getTracks(minHitsOnTrack);
        }// subset off

        duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (kProfile) mEventStats.mStep4Duration.push_back( duration );
        if (duration > 500){
            LOG_WARN << "The took more than 500ms to process, duration: " << duration << " ms" << endm;
            LOG_WARN << "We got " << acceptedTrackSeeds.size() << " tracks this round" << endm;
        }
        LOG_DEBUG << "We got " << acceptedTrackSeeds.size() << " tracks this round" << endm;
        return acceptedTrackSeeds;
    } // doSeedFindingOnHitmapSubset

    /**
     * @brief Main tracking procedure
     *
     * @param iIteration : The track iteration
     * @param hitmap : the hitmap of available hits per plane
     */
    void doSeedFindingIteration(size_t iIteration, FwdDataSource::HitMap_t &hitmap) {

        // empty the list of reco tracks for the iteration
        mTrackSeedsThisIteration.clear();

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
            auto acceptedTracks = doSeedFindingOnHitmapSubset( iIteration, hitmap );
            mTrackSeedsThisIteration.insert( mTrackSeedsThisIteration.end(), acceptedTracks.begin(), acceptedTracks.end() );
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
                // Step 1
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
                auto acceptedTracks = doSeedFindingOnHitmapSubset( iIteration, slicedHitMap );
                mTrackSeedsThisIteration.insert( mTrackSeedsThisIteration.end(), acceptedTracks.begin(), acceptedTracks.end() );
            } //loop on phi slices
        }// if loop on phi slices

        /*************************************************************/
        // Step 5
        // Remove the hits from any track that was found
        /*************************************************************/
        std::string hrmPath = "TrackFinder.Iteration["+ std::to_string(iIteration) + "].HitRemover";
        if ( false == mConfig.exists( hrmPath ) ) hrmPath = "TrackFinder.HitRemover";

        if ( true == mConfig.get<bool>( hrmPath + ":active", true ) ){
            removeHits( hitmap, mTrackSeedsThisIteration );
        }

        LOG_DEBUG << " Found " << mTrackSeedsThisIteration.size() << " seed tracks this iteration" << endm;
        // Add the set of all accepted tracks (this iteration) to our collection of found tracks from all iterations
        mTrackSeeds.insert( mTrackSeeds.end(), mTrackSeedsThisIteration.begin(), mTrackSeedsThisIteration.end() );
    } // doSeedFindingIteration

    /**
     * @brief Adds compatible FST hits to tracks seeded with FTT
     *
     */
    void addFstHitsMc( GenfitTrackResult &gtr ) {
        FwdDataSource::HitMap_t hitmap = mDataSource->getFstHits();
        if ( gtr.mIsFitConverged == false || gtr.mMomentum.Perp() < 1e-3) {
            LOG_DEBUG << "Skipping addFstHitsMc, fit failed" << endm;
            return;
        }
        Seed_t fstHitsThisTrack;

        for (size_t j = 0; j < 3; j++) {
            for (auto h0 : hitmap[j]) {
                if (dynamic_cast<FwdHit *>(h0)->_tid == gtr.mIdTruth) {
                    fstHitsThisTrack.push_back(h0);
                    break;
                }
            } // loop on hits in this layer of hitmap
        }     // loop on hitmap layers

        LOG_DEBUG << "Found " << gtr.mSeed.size() << " existing seed points" << endm;
        LOG_DEBUG << "Adding " << fstHitsThisTrack.size() << " new FST seed points" << endm;

        if (fstHitsThisTrack.size() >= 1) {
            gtr.mSeed.insert( gtr.mSeed.end(), fstHitsThisTrack.begin(), fstHitsThisTrack.end() );
        } // we have 3 Si hits to refit with
    } // addFstHitsMc

    /**
     * @brief Adds compatible FTT hits to tracks seeded with FST
     *
     * @param gtr : The GenfitTrackResult to add FTT hits to
     * @param disk : The FTT disk number
     * @return Seed_t : The combined seed points
     */
    int addFttHits( GenfitTrackResult &gtr, size_t disk ) {
        FwdDataSource::HitMap_t hitmap = mDataSource->getFttHits();
        if ( disk > 3 ) {
            LOG_WARN << "Invalid FTT disk number: " << disk << ", cannot add Ftt points to track" << endm;
            return 0;
        }
        if (gtr.mIsFitConverged != true)
            return 0;

        Seed_t hits_near_plane;
        try {
            auto msp = mTrackFitter->projectToFtt(disk, gtr.mTrack);

            // now look for Ftt hits near the specified state
            // hits_near_plane = findFttHitsNearProjectedState(hitmap[disk], msp);
            hits_near_plane = findFttStripsNearProjectedState(hitmap[disk], msp);
            LOG_DEBUG << " Found #FTT strips on plane #" << disk << TString::Format( " = [%ld]", hits_near_plane.size() ) << endm;
        } catch (genfit::Exception &e) {
            // Failed to project
            LOG_WARN << "Unable to get Ftt projections: " << e.what() << endm;
        }

        LOG_DEBUG << "Found " << gtr.mSeed.size() << " existing seed points" << endm;

        if ( hits_near_plane.size() > 0 ){
            LOG_DEBUG << "Adding " << hits_near_plane.size() << " new FTT seed points" << endm;
            // check to make sure we dont add duplicates
            std::set<KiTrack::IHit *> hitSet( gtr.mSeed.begin(), gtr.mSeed.end() );
            for ( auto h : hits_near_plane ){
                if ( hitSet.find( h ) != hitSet.end() ){
                    LOG_DEBUG << "Hit already in seed, skipping" << endm;
                    continue;
                } else {
                    gtr.mSeed.push_back( h );
                }
            }            
            return hits_near_plane.size();
        }
        return 0;
    } // addFttHits

    /**
     * @brief Adds compatible FTT hits using MC info
     *
     */
    void addFttHitsMc( GenfitTrackResult &gtr ) {
        LOG_DEBUG << "Looking for FTT hits on this track (MC lookup)" << endm;
        LOG_DEBUG << "Track TruthId = " << gtr.mIdTruth << " vs. " << gtr.mTrack->getMcTrackId() << endm;
        FwdDataSource::HitMap_t hitmap = mDataSource->getFttHits();
        if ( gtr.mIsFitConverged == false || gtr.mMomentum.Perp() < 1e-6) {
            LOG_DEBUG << "Skipping addFttHitsMc on this track, fit failed" << endm;
            return;
        }
        Seed_t fttHitsForThisTrack;

        for (size_t j = 0; j < 4; j++) {
            for (auto h0 : hitmap[j]) {
                if (dynamic_cast<FwdHit *>(h0)->_tid == gtr.mIdTruth) {
                    fttHitsForThisTrack.push_back( h0 );
                    break;
                }
            } // loop on hits in this layer of hitmap
        } // loop on hitmap layers

        LOG_DEBUG << "Found " << fttHitsForThisTrack.size() << " FTT Hits on this track (MC lookup)" << endm;

        if (fttHitsForThisTrack.size() >= 1) {
            gtr.mSeed.insert( gtr.mSeed.end(), fttHitsForThisTrack.begin(), fttHitsForThisTrack.end() );
        } // we have at least one Fst hit to refit with
    } // add Ftt hits via MC associations

    /**
     * @brief Adds compatible FST hits to a track
     *
     * @param gtr : The GenfitTrackResult to add FST hits to
     * @param disk : The FST disk number
     */
    int addFstHits( GenfitTrackResult &gtr, size_t disk ) {
        FwdDataSource::HitMap_t hitmap = mDataSource->getFstHits();
        if (gtr.mIsFitConverged == false) {
            // Original Track fit did not converge, skipping
            return 0;
        }
        if ( disk > 2 ){
            LOG_ERROR << "Invalid FST disk number: " << disk << endm;
            return 0;
        }

        Seed_t nearby_hits;
        try {
            // get measured state on plane at specified disk
            auto msp = mTrackFitter->projectToFst(disk, gtr.mTrack);
            // now look for Si hits near this state
            nearby_hits = findFstHitsNearProjectedState(hitmap[disk], msp);
        } catch (genfit::Exception &e) {
            LOG_WARN << "Unable to get projections: " << e.what() << endm;
        }
        LOG_DEBUG << "Track already has " << gtr.mSeed.size() << " existing seed points" << endm;

        if ( nearby_hits.size() > 0 ){
            LOG_DEBUG << "Adding " << nearby_hits.size() << " new FST seed points from disk " << disk << endm;
            gtr.mSeed.insert( gtr.mSeed.end(), nearby_hits.begin(), nearby_hits.end() );
            return nearby_hits.size();
        }
        return 0;
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
        TLorentzVector lv1, lv2;
        lv1.SetPxPyPzE( msp.getPos().X(), msp.getPos().Y(), 0, 1 );

        if (verbose){
            printf( "findFttHitsNearProjectedState, msp = (%f, %f)\n", msp.getPos().X(), msp.getPos().Y() );
        }

        double mindx = 99;
        double mindy = 99;
        double mindr = 99;
        double mindp = 99;
        KiTrack::IHit *closest = nullptr;
        KiTrack::IHit *sec_closest = nullptr;

        for (auto h : available_hits) {

            lv2.SetPxPyPzE( h->getX(), h->getY(), 0, 1 );
            double sr = fabs(lv1.Pt() - lv2.Pt());
            double sp = fabs(lv1.DeltaPhi( lv2 ));
            double sx = fabs(h->getX() - msp.getPos().X());
            double sy = fabs(h->getY() - msp.getPos().Y());

            if (verbose){
                double hsx = dynamic_cast<FwdHit*>(h)->_covmat(0, 0);
                double hsy = dynamic_cast<FwdHit*>(h)->_covmat(1, 1);
                int tid = dynamic_cast<FwdHit*>(h)->_tid;
                printf( "\t vs. hit@(%f+/-%f, %f+/-%f) => dx=%f, dy=%f, dR=%f, dPhi=%f (tid=%d)\n", h->getX(), hsx, h->getY(), hsy, sx, sy, sr, sp, tid );
            }
    
            if ( sp < mindp ){
                mindp = sp;
                sec_closest = closest;
                closest = h;
                mindx = sx;
                mindy = sy;
                mindr = sr;
            }
            // if ( fabs(sx) < fabs(mindx) )
                
            // if ( fabs(sy) < fabs(mindy) )
            //     mindy = sy;

        } // loop h

        if (  fabs(mindp) < 0.04*5 && fabs(mindr) < 30 && (mindx < 7.5 || mindy < 7.5) ) {
            found_hits.push_back(closest);
            // if ( sec_closest ) {
            //     found_hits.push_back(sec_closest);
            // }
        }
        

        LOG_INFO << "Closest FTT hit to FST state: " << Form( "dR=%f, dPhi=%f, dx=%f, dy=%f (tid=%d) ", mindr, mindp, mindx, mindy, dynamic_cast<FwdHit*>(closest)->_tid ) << endm;;

        return found_hits;
    } // findFttHitsNearProjectedState


    /**
     * @brief Finds FTT strips near projected state, first for horizontal and then for vertical strips
     *
     * @param available_hits : FTT hits to consider
     * @param msp : measured state on plane from existing track fit projection
     * @param dx : search distance in x
     * @param dy : search distance in y
     *
     * @return compatible FTT hits
    */
    Seed_t findFttStripsNearProjectedState(Seed_t &available_hits, genfit::MeasuredStateOnPlane &msp, double thresholdPhi = 0.004 * 3 , double thresholdR = 30, double thresholdX = 7.5, double thresholdY = 7.5) {

        Seed_t found_hits;
        if (available_hits.size() == 0) {
            LOG_WARN << "No FTT hits available to search for near projected state" << endm;
            return found_hits;
        }
        
        // we will find the closest horizontal and vertical strip hits
        // and add them to the found_hits if they pass the threshold
        TLorentzVector lv1, lv2;
        lv1.SetPxPyPzE( msp.getPos().X(), msp.getPos().Y(), 0, 1 );

        if (verbose){
            printf( "findFttHitsNearProjectedState, msp = (%f, %f)\n", msp.getPos().X(), msp.getPos().Y() );
        }

        double horizontalMin_dx = 99;
        double horizontalMin_dy = 99;
        double horizontalMin_dr = 99;
        double horizontalMin_dp = 99;
        KiTrack::IHit *horizontalClosest = nullptr;

        double verticalMin_dx = 99;
        double verticalMin_dy = 99;
        double verticalMin_dr = 99;
        double verticalMin_dp = 99;
        KiTrack::IHit *verticalClosest = nullptr;

        for (auto h : available_hits) {
            
            double hsx = dynamic_cast<FwdHit*>(h)->_covmat(0, 0);
            double hsy = dynamic_cast<FwdHit*>(h)->_covmat(1, 1);

            lv2.SetPxPyPzE( h->getX(), h->getY(), 0, 1 );
            double sr = fabs(lv1.Pt() - lv2.Pt());
            double sp = fabs(lv1.DeltaPhi( lv2 ));
            double sx = fabs(h->getX() - msp.getPos().X());
            double sy = fabs(h->getY() - msp.getPos().Y());

            if (verbose){
                int tid = dynamic_cast<FwdHit*>(h)->_tid;
                printf( "\t vs. hit@(%f+/-%f, %f+/-%f) => dx=%f, dy=%f, dR=%f, dPhi=%f (tid=%d)\n", h->getX(), hsx, h->getY(), hsy, sx, sy, sr, sp, tid );
            }
    
            if ( hsx > hsy ){ // horizontal strip
                if ( sp < horizontalMin_dp ){
                    horizontalMin_dp = sp;
                    horizontalClosest = h;
                    horizontalMin_dx = sx;
                    horizontalMin_dy = sy;
                    horizontalMin_dr = sr;
                }
            } else if ( hsy > hsx ){ // vertical strip
                if ( sp < verticalMin_dp ){
                    verticalMin_dp = sp;
                    verticalClosest = h;
                    verticalMin_dx = sx;
                    verticalMin_dy = sy;
                    verticalMin_dr = sr;
                }
            } else {
                LOG_WARN << "Hit with equal covariance in x and y, skipping" << endm;
            }

        } // loop h

        // check threshold and add the closest horizontal strip hit
        if (  fabs(horizontalMin_dp) < thresholdPhi && fabs(horizontalMin_dr) < thresholdR && (horizontalMin_dx < thresholdX || horizontalMin_dy < thresholdY) ) {
            found_hits.push_back(horizontalClosest);
        }
        
        // check threshold and add the closest vertical strip hit
        if (  fabs(verticalMin_dp) < thresholdPhi && fabs(verticalMin_dr) < thresholdR && (verticalMin_dx < thresholdX || verticalMin_dy < thresholdY) ) {
            found_hits.push_back(verticalClosest);
        }
        

        LOG_INFO << "Closest horizontal FTT strip to FST state: " << Form( "dR=%f, dPhi=%f, dx=%f, dy=%f (tid=%d) ", verticalMin_dr, verticalMin_dp, verticalMin_dx, verticalMin_dy, dynamic_cast<FwdHit*>(verticalClosest)->_tid ) << endm;
        LOG_INFO << "Closest vertical FTT strip to FST state: " << Form( "dR=%f, dPhi=%f, dx=%f, dy=%f (tid=%d) ", horizontalMin_dr, horizontalMin_dp, horizontalMin_dx, horizontalMin_dy, dynamic_cast<FwdHit*>(horizontalClosest)->_tid ) << endm;

        return found_hits;
    } // findFttStripsNearProjectedState

    /**
     * @brief Adds compatible EPD hits to tracks seeded with FST
     *
     * @param gtr : The GenfitTrackResult to add EPD hits to
     * @return Seed_t : The combined seed points
     */
     int addEpdHits( GenfitTrackResult &gtr ) {
        FwdDataSource::HitMap_t hitmap = mDataSource->getEpdHits();
        if (gtr.mIsFitConverged != true)
            return 0;

        Seed_t hits_near_plane;
        try {
            auto msp = mTrackFitter->projectToEpd(gtr.mTrack);

            // now look for Ftt hits near the specified state
            const int plane = 7; // EPD plane number
            hits_near_plane = findEpdHitsNearProjectedState(hitmap[plane], msp);
            LOG_DEBUG << " Found #EPD hits on plane " << TString::Format( " = [%ld]", hits_near_plane.size() ) << endm;
        } catch (genfit::Exception &e) {
            // Failed to project
            LOG_WARN << "Unable to get EPD projections: " << e.what() << endm;
        }

        LOG_DEBUG << "Found " << gtr.mSeed.size() << " existing seed points" << endm;

        if ( hits_near_plane.size() > 0 ){
            LOG_DEBUG << "Adding " << hits_near_plane.size() << " new EPD seed points" << endm;
            // check to make sure we dont add duplicates
            std::set<KiTrack::IHit *> hitSet( gtr.mSeed.begin(), gtr.mSeed.end() );
            for ( auto h : hits_near_plane ){
                if ( hitSet.find( h ) != hitSet.end() ){
                    LOG_DEBUG << "Hit already in seed, skipping" << endm;
                    continue;
                } else {
                    gtr.mSeed.push_back( h );
                    if (kProfile) mEventStats.mNumEpdHits++;
                }
            }            
            return hits_near_plane.size();
        }
        return 0;
    } // addFttHits

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
    Seed_t findEpdHitsNearProjectedState(Seed_t &available_hits, 
            genfit::MeasuredStateOnPlane &msp, 
            double dx = 1.5, double dy = 1.5,
            double dr = 99, double dphi = 0.2
        ) {

        Seed_t found_hits;
        TLorentzVector lv1, lv2;
        lv1.SetPxPyPzE( msp.getPos().X(), msp.getPos().Y(), 0, 1 );

        double mindx = 999;
        double mindy = 999;
        double mindr = 999;
        double mindp = 999;
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

        LOG_INFO << "Closest EPD hit to state: " << Form( "dR=%f, dPhi=%f", mindr, mindp ) << endm;;
        // add the hit if it is close enough
        if (  fabs(mindp) < dphi && fabs(mindr) < dr && fabs(mindx) < dx && fabs(mindy) < dy ) {
            LOG_DEBUG << "Adding EPD hit to track" << endm;
            found_hits.push_back(closest);
        }
        return found_hits;
    } // findFttHitsNearProjectedState

    bool getSaveCriteriaValues() { return mSaveCriteriaValues; }
    std::vector<KiTrack::ICriterion *> getTwoHitCriteria() { return mTwoHitCrit; }
    std::vector<KiTrack::ICriterion *> getThreeHitCriteria() { return mThreeHitCrit; }

    TrackFitter *getTrackFitter() { return mTrackFitter; }
    void setEventVertex( TVector3 v, TMatrixDSym cov ){
        mEventVertex = v;
        // this is the FwdHit we will use in seeds
        mEventVertexHit.setXYZDetId( v.X(), v.Y(), v.Z(), kTpcId );
        for (size_t i=0; i < 3; i++){
            for (size_t j=0; j < 3; j++){
                mEventVertexHit._covmat(i,j) = cov(i,j);
            }
        }
    }
    TVector3 getEventVertex() { return mEventVertex; }

  protected:
    static constexpr bool kSaveFailedFits = true; // max number of track seeds to keep in memory
    static constexpr bool kProfile = false; // set to true to profile the tracking steps
    static constexpr int verbose = 1; // Extra logging at INFO level
    unsigned long long int nEvents;

    bool mDoTrackFitting = true;
    bool mSaveCriteriaValues = false;
    enum SeedSource { kFstSeed = 0, kFttSeed, kSimSeed, kSeqSeed };
    int mSeedSource = 1; // 0 = FST, 1 = FTT, 2 = FST+FTT simultaneous, 3 = FST+FTT sequential

    FwdTrackerConfig mConfig;
    std::string mConfigFile;
    size_t mTotalHitsRemoved;

    std::vector<GenfitTrackResult> mTrackResults;
    std::vector<genfit::GFRaveVertex*> mFwdVertices;

    std::vector<Seed_t> mTrackSeeds; // the tracks recod from all iterations
    std::vector<Seed_t> mTrackSeedsThisIteration;

    // Metrics about the event
    EventStats mEventStats;

    // Set to the Primary vertex for the event
    TVector3 mEventVertex;
    FwdHit mEventVertexHit;
    FwdHit mBeamlineHit;
    vector<FwdHit> mFwdVerticesAsHits;
    genfit::GFRaveVertexFactory mGFRVertexFactory;

    std::shared_ptr<FwdDataSource> mDataSource;

    TrackFitter *mTrackFitter = nullptr;

    std::vector<KiTrack::ICriterion *> mTwoHitCrit;
    std::vector<KiTrack::ICriterion *> mThreeHitCrit;

    // histograms of the raw input data
    TString mGeoCache;
};

#endif
