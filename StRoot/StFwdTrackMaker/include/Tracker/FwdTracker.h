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

#include "StFwdTrackMaker/include/Tracker/FwdHit.h"
#include "StFwdTrackMaker/include/Tracker/FwdDataSource.h"
#include "StFwdTrackMaker/include/Tracker/QualityPlotter.h"
#include "StFwdTrackMaker/include/Tracker/TrackFitter.h"
#include "StFwdTrackMaker/include/Tracker/BDTCriteria.h"

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

class GenfitTrackResult {
public:
    GenfitTrackResult(){}
    GenfitTrackResult(  Seed_t &fttSeed, 
                        Seed_t &fstSeed, 
                        std::shared_ptr<genfit::Track> track ) {
        set( fttSeed, fstSeed, track );
    }
    ~GenfitTrackResult(){
        // Clear();
    }
    void Clear() {
        if ( track ){
            track->Clear();
        }
    }
void set(   Seed_t &fttSeed, 
                Seed_t &fstSeed, 
                std::shared_ptr<genfit::Track> track ){
        this->fttSeed = fttSeed;
        this->fstSeed = fstSeed;

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

            this->momentum = this->trackRep->getMom( this->track->getFittedState(0, this->trackRep) );
            LOG_DEBUG << "GenfitTrackResult::set Track successful" << endm;
            size_t numTotal = numFTT() + numFST();
            LOG_DEBUG << "GTR has track->numPoints=" << this->track->getNumPoints() << " vs. numFST: " << numFST() << ", numFTT: " << numFTT() << endm;

        } catch ( genfit::Exception &e ) {
            LOG_ERROR << "CANNOT GET TRACK" << endm;
            this->track = nullptr;
            this->trackRep = nullptr;

            this->isFitConverged = false;
            this->isFitConvergedFully = false;
            this->isFitConvergedPartially = false;
            this->nFailedPoints = 99;
            this->charge = 0;
        }
    }

    void addFST( Seed_t &nfstSeed, std::shared_ptr<genfit::Track> track ) {
        set( fttSeed, nfstSeed, track );
    }
    void addFTT( Seed_t &nfttSeed, std::shared_ptr<genfit::Track> track ) {
        set( nfttSeed, fstSeed, track );
    }

    size_t numFTT() const { return fttSeed.size();}
    size_t numFST() const { return fstSeed.size();}


    Seed_t fttSeed;
    Seed_t fstSeed;
    TVector3 pv;
    TVector3 momentum;
    double charge;
    size_t nPV = 0;
    genfit::FitStatus *status = nullptr;
    genfit::AbsTrackRep *trackRep = nullptr;
    std::shared_ptr<genfit::Track> track = nullptr;
    bool isFitConverged = false;
    bool isFitConvergedFully = false;
    bool isFitConvergedPartially = false;
    size_t nFailedPoints = 0;
    size_t theNumFTT = 0;

    
    void summary() {
        LOG_INFO << TString::Format( "TrackResult[p=(%f, %f, %f)/(%f, %f, %f), q=%f, nFTT=%lu, nFST=%lu, nPV=%lu, isFitConvergedFully=%d]", momentum.X(), momentum.Y(), momentum.Z(), momentum.Pt(), momentum.Eta(), momentum.Phi(), charge, fttSeed.size(), fstSeed.size(), nPV, isFitConvergedFully ).Data() << endm;
    }
}; // GenfitTrackResult

class ForwardTrackMaker {
  public:
    ForwardTrackMaker() : mConfigFile("config.xml"), mEventVertex(-999, -999, -999) {
        // noop
    }
    
    const std::vector<GenfitTrackResult> &getTrackResults() const { return mTrackResults; }
    const std::vector<Seed_t> &getTrackSeeds() const { return mRecoTracks; }

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
        mGenHistograms = genHistograms;
        if (mGenHistograms) setupHistograms();

        mGeoCache = geoCache;
        mDoTrackFitting = mConfig.get<bool>("TrackFitter:active", true);

        if (!mConfig.exists("TrackFitter"))
            mDoTrackFitting = false;
    } //initialize

    /**
     * @brief Writes QA histograms and config to ROOT file
     * 
     */
    void writeEventHistograms() {
        // no file, dont write anything
        if ( !gDirectory )
            return;

        gDirectory->cd();
        // write out the config we use (do before histos):
        TNamed n("mConfig", mConfig.dump());
        n.Write();

        writeHistograms();

        gDirectory->mkdir("Fit/");
        gDirectory->cd("Fit/");
        mTrackFitter->writeHistograms();
        gDirectory->cd("");
        mQualityPlotter->writeHistograms();
    } //writeEventHistograms

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
                crit = new BDTCrit2( vmin, vmax );
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
     * @brief Counts the number of tracks with nHits hits
     * 
     * @param nHits : number of hits
     * @return size_t : number of tracks of the given length
     */
    size_t countRecoTracks(size_t nHits) {
        size_t n = 0;

        for (auto t : mRecoTracks) {
            if (t.size() == nHits)
                n++;
        }

        return n;
    }

    /**
     * @brief Create QA histograms
     * 
     */
    void setupHistograms() {

        mHist["input_nhits"] = new TH1I("input_nhits", ";# hits", 1000, 0, 1000);
        mHist["nAttemptedFits"] = new TH1I("nAttemptedFits", ";;# attempted fits", 10, 0, 10);
        mHist["nPossibleFits"] = new TH1I("nPossibleFits", ";;# possible fits", 10, 0, 10);
        // refit with silicon
        mHist["nPossibleReFits"] = new TH1I("nPossibleReFits", ";;# possible REfits", 10, 0, 10);
        mHist["nAttemptedReFits"] = new TH1I("nAttemptedReFits", ";;#attempted REfits", 10, 0, 10);
        mHist["nFailedReFits"] = new TH1I("nFailedReFits", ";;# failed REfits", 10, 0, 10);

        mHist["FitStatus"] = new TH1I("FitStatus", ";;# failed REfits", 15, 0, 15);
        FwdTrackerUtils::labelAxis(mHist["FitStatus"]->GetXaxis(), {"Seeds", "AttemptFit", "GoodFit", "BadFit", "GoodCardinal", "PossibleReFit", "AttemptReFit", "GoodReFit", "BadReFit", "w3Si","w2Si", "w1Si", "w0Si" });

        mHist["FitDuration"] = new TH1I("FitDuration", ";Duration (ms)", 5000, 0, 50000);
        mHist["nFstHitsFound"] = new TH2I( "nFstHitsFound", ";FST Disk; n Hits", 5, 0, 5, 10, 0, 10 );

        mHist["Step1Duration"] = new TH1I("Step1Duration", ";Duration (ms)", 500, 0, 500);
        mHist["Step2Duration"] = new TH1I("Step2Duration", ";Duration (ms)", 500, 0, 500);
        mHist["Step3Duration"] = new TH1I("Step3Duration", ";Duration (ms)", 500, 0, 500);
        mHist["Step4Duration"] = new TH1I("Step4Duration", ";Duration (ms)", 500, 0, 500);

        mHist["FttSearchDeltaX"] = new TH1I("FttSearchDeltaX", ";dx", 500, -100, 100);
        mHist["FttSearchDeltaY"] = new TH1I("FttSearchDeltaY", ";dy", 500, -100, 100);
        mHist["FttSearchDeltaR"] = new TH1I("FttSearchDeltaR", ";dr", 500, -100, 100);
        mHist["FttSearchDeltaP"] = new TH1I("FttSearchDeltaP", ";d#phi", 500, -3.14, 3.4);

        mHist["FttSearchMinDeltaX"] = new TH1I("FttSearchMinDeltaX", ";dx", 500, -100, 100);
        mHist["FttSearchMinDeltaY"] = new TH1I("FttSearchMinDeltaY", ";dy", 500, -100, 100);
        mHist["FttSearchMinDeltaR"] = new TH1I("FttSearchMinDeltaR", ";dr", 500, -100, 100);
        mHist["FttSearchMinDeltaP"] = new TH1I("FttSearchMinDeltaP", ";d#phi", 500, -3.14, 3.4);

        mHist["nFttHitsFound"] = new TH2I( "nFttHitsFound", ";Ftt Plane; n Hits", 5, 0, 5, 10, 0, 10 );
    } //setupHistograms

    /**
     * @brief Writes histograms to file
     * 
     */
    void writeHistograms() {
        if ( !mGenHistograms ){
            return;
        }

        for (auto nh : mHist) {
            nh.second->SetDirectory(gDirectory);
            nh.second->Write();
        }
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
        /************** Cleanup **************************/

        if (mGenHistograms ){
            mQualityPlotter->startEvent(); // starts the timer for this event
        }

        mTotalHitsRemoved = 0;

        /*************************************************************/
        // Step 1
        // Load and sort the hits
        /*************************************************************/
        long long itStart = FwdTrackerUtils::nowNanoSecond();
        FwdDataSource::HitMap_t &fttHitmap = mDataSource->getFttHits();
        FwdDataSource::HitMap_t &fstHitmap = mDataSource->getFstHits();

        string hitmapSource = mConfig.get<string>("TrackFinder:source", "ftt");
        bool useFttAsSource = !(hitmapSource == "fst");

        if ( useFttAsSource == false ){
            for (auto hp : fstHitmap){
                LOG_DEBUG << "HITMAP [" << hp.first << ", { ";
                for ( auto h: hp.second ){
                    LOG_DEBUG << "z=" << h->getZ() << " ";
                }
                LOG_DEBUG << " }" << endm;
            }
        }
        FwdDataSource::McTrackMap_t &mcTrackMap = mDataSource->getMcTracks();

        if (mGenHistograms && mDataSource != nullptr) {
            auto hm = mDataSource->getFttHits();
            for (auto hp : hm)
                mHist["input_nhits"]->Fill(hp.second.size());
        }

        long long duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (mGenHistograms)
            mHist["Step1Duration"]->Fill( duration );

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
            doMcTrackFinding(mcTrackMap, useFttAsSource);

            /***********************************************/
            // REFIT with FST or FTT hits accordingly
            if (mConfig.get<bool>("TrackFitter:refit", false)) {
                if (useFttAsSource)
                    addFstHitsMc();
                else 
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
            if (useFttAsSource){
                doTrackIteration(iIteration, fttHitmap);
            } else {
                doTrackIteration(iIteration, fstHitmap);
            }
        } // iIteration
        /***********************************************/

        /***********************************************/
        // REFIT with Fst or Ftt hits
        if (mConfig.get<bool>("TrackFitter:refit", false)) {
            if (useFttAsSource){
                addFstHits();
            } else {
                addFttHits();
            }
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
        
        // make sure this is consistent with above
        string hitmapSource = mConfig.get<string>("TrackFinder:source", "ftt");
        bool useFttAsSource = !(hitmapSource == "fst");

        if ( mGenHistograms ){
            mHist["FitStatus"]->Fill("Seeds", 1);
        }

        if (!mDoTrackFitting)
            return;

        std::shared_ptr<genfit::Track> genTrack;

        mAttemptedFits++;
        if ( mGenHistograms ){
            mHist["FitStatus"]->Fill("AttemptFit", 1);
        }

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
            Seed_t nonSeeds; // none
            GenfitTrackResult gtr;
            if ( useFttAsSource )
                gtr.set( seed, nonSeeds, genTrack );
            else 
                gtr.set( nonSeeds, seed, genTrack );

            if (gtr.status && gtr.status->isFitConvergedFully()) {
                mGoodFits ++;
                if ( mGenHistograms ) mHist["FitStatus"]->Fill("GoodFit", 1);
            } else {
                mFailedFits ++;
                if ( mGenHistograms ) mHist["FitStatus"]->Fill("BadFit", 1);
            }

            if ( genTrack->getFitStatus(genTrack->getCardinalRep())->isFitConverged()) {
                mGoodCardinals++;
                if ( mGenHistograms ) mHist["FitStatus"]->Fill("GoodCardinal", 1);
            }
            mTrackResults.push_back( gtr );
        } else {
            mFailedFits ++;
            if ( mGenHistograms ) mHist["FitStatus"]->Fill("BadFit", 1);
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

        // metrics for this round
        mAttemptedFits = 0;
        mGoodFits = 0;
        mFailedFits = 0;
        mGoodCardinals = 0;

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
        if ( mGenHistograms ){
            this->mHist["FitDuration"]->Fill(duration);
        }

        double perGood = (double) mGoodFits / (double) mAttemptedFits;
        double perFailed = (double) mFailedFits / (double) mAttemptedFits;
        double perCardinals = (double) mGoodCardinals / (double) mGoodFits;
        LOG_DEBUG << "Track Fitting Results:" <<
                    TString::Format( "Attempts = %lu, Good = %lu (%f%), Failed = %lu (%f%), GoodCardinals = %lu (%f%)", mAttemptedFits, mGoodFits, perGood, mFailedFits, perFailed, mGoodCardinals, perCardinals ) << endm;
        LOG_DEBUG << "Track fitting took " << duration << "ms" << endm; 
    } // doTrackFitting

    /**
     * @brief MC track finding builds track seeds from available hits using MC association
     * 
     * @param mcTrackMap : Mc tracks
     * @param useFttAsSource : Use FTT for seeds or (false) use Fst
     */
    void doMcTrackFinding(FwdDataSource::McTrackMap_t &mcTrackMap, bool useFttAsSource = true) {

        mQualityPlotter->startIteration();

        // we will build reco tracks from each McTrack
        for (auto kv : mcTrackMap) {
            
            auto mc_track = kv.second;


            if (useFttAsSource && mc_track->mFttHits.size() < 4){ // require min 4 FTT hits on track
                continue;
            }

            if (!useFttAsSource && mc_track->mFstHits.size() < 3 ) { // require min 3 FST hits on track
                continue;
            }

            std::set<size_t> uvid;
            Seed_t track;

            if ( useFttAsSource ){ // FTT (DEFAULT)
                for (auto h : mc_track->mFttHits) {
                    track.push_back(h);
                    uvid.insert(static_cast<FwdHit *>(h)->_vid);
                }
            } else { // FST 
                for (auto h : mc_track->mFstHits) {
                    track.push_back(h);
                    uvid.insert(static_cast<FwdHit *>(h)->_vid);
                }
            }
            
            if (uvid.size() == track.size()) { // only add tracks that have one hit per volume
                mRecoTracks.push_back(track);
                int idt = 0;
                double qual = 0;
                idt = MCTruthUtils::dominantContribution(track, qual);
            } else {
                //Skipping track that doesnt have hits on all layers 
            }
        }

        LOG_DEBUG << "McTrackFinding Found: " << mRecoTracks.size() << " tracks" << endm;
        
        doTrackFitting(mRecoTracks);

        if ( mGenHistograms ){
            mQualityPlotter->afterIteration(0, mRecoTracks);
        }
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
        
        FwdConnector connector(distance);
        builder.addSectorConnector(&connector);

        // Get the segments and return an automaton object for further work
        
        KiTrack::Automaton automaton = builder.get1SegAutomaton();
        LOG_DEBUG << TString::Format( "nSegments=%lu", automaton.getSegments().size() ).Data() << endm;
        LOG_DEBUG << TString::Format( "nConnections=%u", automaton.getNumberOfConnections() ).Data() << endm;

        if (automaton.getNumberOfConnections() > 900 ){
            LOG_ERROR << "Got too many connections, bailing out of tracking" << endm;
            return acceptedTracks;
        }

        // at any point we can get a list of tracks out like this:
        // std::vector < std::vector< KiTrack::IHit* > > tracks = automaton.getTracks();
        // we can apply an optional parameter <nHits> to only get tracks with >=nHits in them

        long long duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (mGenHistograms)
            mHist["Step2Duration"]->Fill( duration );
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
        automaton.lengthenSegments();

        bool doAutomation = mConfig.get<bool>(criteriaPath + ":doAutomation", true);
        bool doCleanBadStates = mConfig.get<bool>(criteriaPath + ":cleanBadStates", true);

        if (doAutomation) {
            automaton.doAutomaton();
        } else {
            LOG_DEBUG << "Skipping Automation step" << endm;
            //Not running Automation Step
        }

        if (doAutomation && doCleanBadStates) {
            automaton.cleanBadStates();
        }

        duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (mGenHistograms)
            mHist["Step3Duration"]->Fill( duration );
        if (duration > 200 || automaton.getNumberOfConnections() > 900){
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
        if (mGenHistograms)
            mHist["Step4Duration"]->Fill( duration );
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
            return;
        }

        // this starts the timer for the iteration
        if ( mGenHistograms ){
            mQualityPlotter->startIteration();
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
        
        if ( mGenHistograms ){
            mQualityPlotter->afterIteration( iIteration, mRecoTracksThisIteration );
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

            if ( mGenHistograms){
                mHist["FitStatus"]->Fill("PossibleReFit", 1);
            }

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

            if ( mGenHistograms ){
                this->mHist[ "nFstHitsFound" ]->Fill( 1, ( fst_hits_for_this_track[0] != nullptr ? 1 : 0 ) );
                this->mHist[ "nFstHitsFound" ]->Fill( 2, ( fst_hits_for_this_track[1] != nullptr ? 1 : 0 ) );
                this->mHist[ "nFstHitsFound" ]->Fill( 3, ( fst_hits_for_this_track[2] != nullptr ? 1 : 0 ) );
            }

            if (nFstHitsFound >= 1) {
                if ( mGenHistograms ){
                    mHist["FitStatus"]->Fill("AttemptReFit", 1);
                }

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

                if ( mGenHistograms ){
                    if (mTrackFitter->getTrack()->getFitStatus()->isFitConvergedFully() == false) {
                        mHist["FitStatus"]->Fill("BadReFit", 1);
                        LOG_DEBUG << "refitTrackWithFstHits failed refit" << endm;
                    } else {
                        LOG_DEBUG << "refitTrackWithFstHits successful refit" << endm;
                        mHist["FitStatus"]->Fill("GoodReFit", 1);
                        gtr.addFST( fst_hits_for_this_track_nnull, mTrackFitter->getTrack() );
                    }
                }
            } // we have 3 Si hits to refit with

            if ( mGenHistograms ){
                mHist["FitStatus"]->Fill( TString::Format( "w%luSi", nFstHitsFound ).Data(), 1 );
            }
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

            if ( mGenHistograms ){
                mHist["FitStatus"]->Fill("PossibleReFit", 1);
            }

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

            if ( mGenHistograms ){
                this->mHist[ "nFttHitsFound" ]->Fill( 1, hits_near_plane0.size() );
                this->mHist[ "nFttHitsFound" ]->Fill( 2, hits_near_plane1.size() );
                this->mHist[ "nFttHitsFound" ]->Fill( 3, hits_near_plane2.size() );
                this->mHist[ "nFttHitsFound" ]->Fill( 4, hits_near_plane3.size() );
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

                if ( mGenHistograms ){
                    mHist["FitStatus"]->Fill("AttemptReFit", 1);
                }
                
                Seed_t combinedSeed;
                LOG_DEBUG << "Found " << t.fstSeed.size() << " existing FST seed points" << endm;
                combinedSeed.insert( combinedSeed.begin(), t.fstSeed.begin(), t.fstSeed.end() ); // this is goofed but will fix
                LOG_DEBUG << "Adding " << hits_to_add.size() << " new FTT seed points" << endm;
                combinedSeed.insert( combinedSeed.end(), hits_to_add.begin(), hits_to_add.end() );

                double vertex[3] = { mEventVertex.X(), mEventVertex.Y(), mEventVertex.Z() };
                LOG_DEBUG << "Using previous fit momentum as the seed: " << TString::Format( "(pt=%f, eta=%f, phi=%f)", t.momentum.Pt(), t.momentum.Eta(), t.momentum.Phi() ) << endm;

                TVector3 p = mTrackFitter->fitTrack(combinedSeed, vertex, &(t.momentum) );

                auto status = mTrackFitter->getTrack()->getFitStatus();
                if ( status->isFitConvergedFully() ){
                    LOG_DEBUG << "Track Refit with FTT points converged" << endm;
                    t.addFTT( hits_to_add, mTrackFitter->getTrack() );
                    LOG_DEBUG << "Track Refit with " << t.track->getNumPoints() << " points" << endm;
                    if ( mGenHistograms ) mHist["FitStatus"]->Fill("GoodReFit", 1);
                } else {
                    if ( mGenHistograms ) mHist["FitStatus"]->Fill("BadReFit", 1);
                    LOG_DEBUG << "Track Refit with FTT points FAILED" << endm;
                }
            }

            if ( mGenHistograms ){
                mHist["FitStatus"]->Fill( TString::Format( "w%luFtt", hits_to_add.size() ).Data(), 1 );
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

            if ( mGenHistograms){
                mHist["FitStatus"]->Fill("PossibleReFit", 1);
            }

            Seed_t fttHitsForThisTrack;

            size_t nFttHitsFound = 0;
            for (size_t j = 0; j < 4; j++) {
                for (auto h0 : hitmap[j]) {
                    if (dynamic_cast<FwdHit *>(h0)->_tid == gtr.track->getMcTrackId()) {
                        // fttHitsForThisTrack[j] = h0;
                        fttHitsForThisTrack.push_back( h0 );
                        nFttHitsFound++;
                        if ( mGenHistograms ){
                            this->mHist[ "nFttHitsFound" ]->Fill( j+1, ( fttHitsForThisTrack[j] != nullptr ? 1 : 0 ) );
                        }
                        break;
                    }
                } // loop on hits in this layer of hitmap
            } // loop on hitmap layers

            LOG_DEBUG << "Found " << nFttHitsFound << " FTT Hits on this track (MC lookup)" << endm;

            if (nFttHitsFound >= 1) {
                if ( mGenHistograms ){
                    mHist["FitStatus"]->Fill("AttemptReFit", 1);
                }

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
                    gtr.addFTT( fttHitsForThisTrack, mTrackFitter->getTrack() );
                    LOG_DEBUG << "Track Refit with " << gtr.track->getNumPoints() << " points" << endm;
                    if (mGenHistograms) mHist["FitStatus"]->Fill("GoodReFit", 1);
                } else {
                    if (mGenHistograms) mHist["FitStatus"]->Fill("BadReFit", 1);
                    LOG_DEBUG << "Track Refit with FTT points FAILED" << endm;
                }
            } // we have at least one Fst hit to refit with

            if ( mGenHistograms ){
                mHist["FitStatus"]->Fill( TString::Format( "w%luFtt", nFttHitsFound ).Data(), 1 );
            }
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

            if ( mGenHistograms ){
                mHist["FitStatus"]->Fill("PossibleReFit", 1);
            }

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

            if ( mGenHistograms ){
                this->mHist[ "nFstHitsFound" ]->Fill( 1, hits_near_disk0.size() );
                this->mHist[ "nFstHitsFound" ]->Fill( 2, hits_near_disk1.size() );
                this->mHist[ "nFstHitsFound" ]->Fill( 3, hits_near_disk2.size() );
            }

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
                if ( mGenHistograms ){
                    mHist["FitStatus"]->Fill("AttemptReFit", 1);
                }

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

                if ( mGenHistograms ){
                    if (mTrackFitter->getTrack()->getFitStatus()->isFitConvergedFully() == false) {
                        mHist["FitStatus"]->Fill("BadReFit", 1);
                        LOG_DEBUG << "refitTrackWithFstHits failed refit" << endm;
                    } else {
                        LOG_DEBUG << "refitTrackWithFstHits successful refit" << endm;
                        mHist["FitStatus"]->Fill("GoodReFit", 1);
                        gtr.addFST( fst_hits_for_this_track_nnull, mTrackFitter->getTrack() );
                    }
                }
            } else {
                // unable to refit
            }

            if ( mGenHistograms ){
                mHist["FitStatus"]->Fill( TString::Format( "w%luSi", nFstHitsFound ).Data(), 1 );
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

            if ( mGenHistograms ){
                mHist[ "FttSearchDeltaX" ]->Fill( sx );
                mHist[ "FttSearchDeltaY" ]->Fill( sy );
                mHist[ "FttSearchDeltaR" ]->Fill( sr );
                mHist[ "FttSearchDeltaP" ]->Fill( sp );
            }

            
        }

        if ( mGenHistograms ){
            mHist[ "FttSearchMinDeltaX" ]->Fill( mindx );
            mHist[ "FttSearchMinDeltaY" ]->Fill( mindy );
            mHist[ "FttSearchMinDeltaR" ]->Fill( mindr );
            mHist[ "FttSearchMinDeltaP" ]->Fill( mindp );
        }

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

  protected:
    unsigned long long int nEvents;

    bool mDoTrackFitting = true;
    bool mSaveCriteriaValues = false;

    FwdTrackerConfig mConfig;
    std::string mConfigFile;
    size_t mTotalHitsRemoved;
    
    std::vector<GenfitTrackResult> mTrackResults;

    std::vector<Seed_t> mRecoTracks; // the tracks recod from all iterations
    std::vector<Seed_t> mRecoTracksThisIteration;

    // Metrics about the event 
    size_t mAttemptedFits = 0;
    size_t mGoodFits = 0;
    size_t mFailedFits = 0;
    size_t mGoodCardinals = 0;

    // Set to the Primary vertex for the event
    TVector3 mEventVertex;

    QualityPlotter *mQualityPlotter;
    std::shared_ptr<FwdDataSource> mDataSource;

    TrackFitter *mTrackFitter = nullptr;

    std::vector<KiTrack::ICriterion *> mTwoHitCrit;
    std::vector<KiTrack::ICriterion *> mThreeHitCrit;

    // histograms of the raw input data
    bool mGenHistograms = false; // controls these histograms and use of QualityPlotter
    TString mGeoCache;
    std::map<std::string, TH1 *> mHist;  
};

#endif
