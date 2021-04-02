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

class ForwardTrackMaker {
  public:
    ForwardTrackMaker() : mConfigFile("config.xml"), mEventVertex(-999, -999, -999) {
        // noop
    }
    
    const std::vector<Seed_t> &getRecoTracks() const { return mRecoTracks; }
    const std::vector<TVector3> &getFitMomenta() const { return mFitMoms; }
    const std::vector<unsigned short> &getNumFstHits() const { return mNumFstHits; }
    const std::vector<genfit::FitStatus> &getFitStatus() const { return mFitStatus; }
    const std::vector<genfit::AbsTrackRep *> &globalTrackReps() const { return mGlobalTrackReps; }
    const std::vector<genfit::Track *> &globalTracks() const { return mGlobalTracks; }

    void setConfigFile(std::string cf) {
        mConfigFile = cf;
    }

    void setSaveCriteriaValues(bool save) {
        mSaveCriteriaValues = save;
    }

    // Adopt external configuration file
    void setConfig(FwdTrackerConfig cfg) { mConfig = cfg; }
    // Adopt external hit loader
    void setData(std::shared_ptr<FwdDataSource>data) { mDataSource = data; }

    virtual void initialize( bool genHistograms) {
        mGenHistograms = genHistograms;
        if (mGenHistograms) setupHistograms();

        mDoTrackFitting = !(mConfig.get<bool>("TrackFitter:off", false));

        if (!mConfig.exists("TrackFitter"))
            mDoTrackFitting = false;
    }


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
    }

    /** Loads Criteria from XML configuration.
   *
   * Utility function for loading criteria from XML config.
   *
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
    }

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
    };

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
    };

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
    };

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
    }

    size_t nHitsInHitMap(FwdDataSource::HitMap_t &hitmap) {
        size_t n = 0;

        for (auto kv : hitmap) {
            n += kv.second.size();
        }

        return n;
    }

    size_t countRecoTracks(size_t nHits) {
        size_t n = 0;

        for (auto t : mRecoTracks) {
            if (t.size() == nHits)
                n++;
        }

        return n;
    }

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
        mHist["nSiHitsFound"] = new TH2I( "nSiHitsFound", ";Si Disk; n Hits", 5, 0, 5, 10, 0, 10 );

        mHist["Step1Duration"] = new TH1I("Step1Duration", ";Duration (ms)", 500, 0, 500);
        mHist["Step2Duration"] = new TH1I("Step2Duration", ";Duration (ms)", 500, 0, 500);
        mHist["Step3Duration"] = new TH1I("Step3Duration", ";Duration (ms)", 500, 0, 500);
        mHist["Step4Duration"] = new TH1I("Step4Duration", ";Duration (ms)", 500, 0, 500);
    }

    void fillHistograms() {

        if (mGenHistograms && mDataSource != nullptr) {
            auto hm = mDataSource->getFttHits();
            for (auto hp : hm)
                mHist["input_nhits"]->Fill(hp.second.size());
        }
    }

    void writeHistograms() {
        if ( !mGenHistograms ){
            return;
        }

        for (auto nh : mHist) {
            nh.second->SetDirectory(gDirectory);
            nh.second->Write();
        }
    }

    // this is the main event loop.  doEvent processes a single event iEvent...
    void make() {

        int single_event = mConfig.get<int>("Input:event", -1);

        if (single_event >= 0) {
            doEvent(single_event);
            return;
        }

        unsigned long long firstEvent = mConfig.get<unsigned long long>("Input:first-event", 0);

        if (mConfig.exists("Input:max-events")) {
            unsigned long long maxEvents = mConfig.get<unsigned long long>("Input:max-events", 0);

            if (nEvents > maxEvents)
                nEvents = maxEvents;

        }

        // loop over events

        for (unsigned long long iEvent = firstEvent; iEvent < firstEvent + nEvents; iEvent++) {
            doEvent(iEvent);
        }

        if (mGenHistograms){
            mQualityPlotter->finish();
            writeEventHistograms();
        }
    }

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

    void doEvent(unsigned long long int iEvent = 0) {
        /************** Cleanup ****************************************/
        // Moved cleanup to the start of doEvent, so that the fit results
        // persist after the call
        mRecoTracks.clear();
        mRecoTrackQuality.clear();
        mRecoTrackIdTruth.clear();
        mFitMoms.clear();
        mNumFstHits.clear();
        mFitStatus.clear();

        // Clear pointers to the track reps from previous event
        for (auto p : mGlobalTrackReps)
            delete p;

        mGlobalTrackReps.clear();

        // Clear pointers to global tracks
        for (auto p : mGlobalTracks)
            delete p;

        mGlobalTracks.clear();
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
        FwdDataSource::HitMap_t &hitmap = mDataSource->getFttHits();;
        FwdDataSource::McTrackMap_t &mcTrackMap = mDataSource->getMcTracks();

        fillHistograms();
        long long duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (mGenHistograms)
            mHist["Step1Duration"]->Fill( duration );


        bool mcTrackFinding = true;

        if (mConfig.exists("TrackFinder"))
            mcTrackFinding = false;

        /***********************************************/
        // MC Track Finding
        if (mcTrackFinding) {
            doMcTrackFinding(mcTrackMap);

            /***********************************************/
            // REFIT with Silicon hits
            if (mConfig.get<bool>("TrackFitter:refitSi", true)) {
                addSiHitsMc();
            } else {
                // skip Si refit
            }
            /***********************************************/

            if (mGenHistograms ){
                mQualityPlotter->summarizeEvent(mRecoTracks, mcTrackMap, mFitMoms, mFitStatus);
            }
            return;
        }
        /***********************************************/

        /***********************************************/
        // Standard Track Finding
        // plus initial fit
        size_t nIterations = mConfig.get<size_t>("TrackFinder:nIterations", 0);

        for (size_t iIteration = 0; iIteration < nIterations; iIteration++) {
            doTrackIteration(iIteration, hitmap);
        }
        /***********************************************/


        /***********************************************/
        // REFIT with Silicon hits
        if (mConfig.get<bool>("TrackFitter:refitSi", true)) {
            addSiHits();
        } else {
            // Skipping Si Refit
        }
        /***********************************************/

        if ( mGenHistograms ){
            mQualityPlotter->summarizeEvent(mRecoTracks, mcTrackMap, mFitMoms, mFitStatus);
        }
    } // doEvent

    void trackFitting(Seed_t &track) {

        if ( mGenHistograms ){
            mHist["FitStatus"]->Fill("Seeds", 1);
        }

        // Calculate the MC info first and check filters
        int idt = 0;
        double qual = 0;
        idt = MCTruthUtils::dominantContribution(track, qual);
        
        


        TVector3 mcSeedMom;

        auto mctm = mDataSource->getMcTracks();
        // get the MC track momentum if we can
        if (mctm.count(idt)) {
            auto mct = mctm[idt];
            mcSeedMom.SetPtEtaPhi(mct->mPt, mct->mEta, mct->mPhi);
        }


        // Mc Filter
        bool bailout = false;
        if (qual < mConfig.get<float>("TrackFitter.McFilter:quality-min", 0.0)) {
            bailout = true;
            // LOG_INFO << "BAIL OUT on Fit bc quality = " << qual << endm;
        }
        if (mctm.count(idt)) {
            auto mct = mctm[idt];
            mcSeedMom.SetPtEtaPhi(mct->mPt, mct->mEta, mct->mPhi);
            if (mct->mPt < mConfig.get<float>("TrackFitter.McFilter:pt-min", 0.0) ||
                mct->mPt > mConfig.get<float>("TrackFitter.McFilter:pt-max", 1e10)) {
                bailout = true;
                // LOG_INFO << "BAIL OUT on Fit bc Pt = " << mct->mPt << endm;
            }
            if (mct->mEta < mConfig.get<float>("TrackFitter.McFilter:eta-min", 0) ||
                mct->mEta > mConfig.get<float>("TrackFitter.McFilter:eta-max", 1e10)) {
                bailout = true;
                // LOG_INFO << "BAIL OUT on Fit bc eta = " << mct->mEta << endm;
            }
            
        } else {
            // cannot find the track
        }

        // if ( bailout ){

        //     for ( KiTrack::IHit * h : track ){
        //         FwdHit *fh = static_cast<FwdHit*>( h );
        //         LOG_INFO << "Hit on Track from trackId: " << fh->_tid << endm;
        //     }

        // }


        bailout = false;
        // if (bailout)
        //     return;
        // Done with Mc Filter

        TVector3 p;
        p.SetPtEtaPhi( 0, -999, 0 );
        genfit::FitStatus fitStatus;
        
        genfit::AbsTrackRep *trackRep = nullptr;//new genfit::RKTrackRep(211); // pdg for pi+
        genfit::Track *genTrack = nullptr;//new genfit::Track( trackRep, TVector3(0, 0, 0), TVector3(0, 0, 0) );


        if (mDoTrackFitting && !bailout) {
            if ( mGenHistograms ){
                mHist["FitStatus"]->Fill("AttemptFit", 1);
            }

            double vertex[3] = { mEventVertex.X(), mEventVertex.Y(), mEventVertex.Z() };

            double * pVertex = 0;
            if ( fabs(mEventVertex.X()) < 100 ){
                pVertex = vertex; // only use it if it has been set from default
            }

            
            if (true == mConfig.get<bool>("TrackFitter:mcSeed", false)) {
                // use the MC pt, eta, phi as the seed for fitting
                p = mTrackFitter->fitTrack(track, pVertex, &mcSeedMom);
            } else {
                // Normal case, real data
                p = mTrackFitter->fitTrack(track, pVertex);
            }

            if ( mGenHistograms ){
                if (p.Perp() > 1e-3) {
                    mHist["FitStatus"]->Fill("GoodFit", 1);
                } else {
                    mHist["FitStatus"]->Fill("BadFit", 1);
                }
            }

            // assign the fit results to be saved
            fitStatus = mTrackFitter->getStatus();
            genTrack = new genfit::Track(*mTrackFitter->getTrack());
            trackRep = mTrackFitter->getTrackRep()->clone(); // Clone the track rep
            genTrack->setMcTrackId(idt);
            
            if ( mGenHistograms && genTrack->getFitStatus(genTrack->getCardinalRep())->isFitConverged() && p.Perp() > 1e-3) {
                mHist["FitStatus"]->Fill("GoodCardinal", 1);
            }

            // Save everything
            mFitMoms.push_back(p);
            mGlobalTracks.push_back(genTrack);
            mGlobalTrackReps.push_back(trackRep);
            mFitStatus.push_back(fitStatus);
            mRecoTrackQuality.push_back(qual);
            mRecoTrackIdTruth.push_back(idt);
            mNumFstHits.push_back(0);
            
        } // if (mDoTrackFitting && !bailout)
    }

    void doMcTrackFinding(FwdDataSource::McTrackMap_t &mcTrackMap) {

        mQualityPlotter->startIteration();

        // we will build reco tracks from each McTrack
        for (auto kv : mcTrackMap) {
            auto mc_track = kv.second;

            if (mc_track->mHits.size() < 4){ // require min 4 hits on track
                continue;
            }

            std::set<size_t> uvid;
            Seed_t track;

            for (auto h : mc_track->mHits) {
                track.push_back(h);
                uvid.insert(static_cast<FwdHit *>(h)->_vid);
            }

            if (uvid.size() == track.size()) { // only add tracks that have one hit per volume
                mRecoTracks.push_back(track);
                int idt = 0;
                double qual = 0;
                idt = MCTruthUtils::dominantContribution(track, qual);
                mRecoTrackQuality.push_back(qual);
                mRecoTrackIdTruth.push_back(idt);
            } else {
                //Skipping track that doesnt have hits on all layers 
            }
        }

        long long itStart = FwdTrackerUtils::nowNanoSecond();
        // Fit each accepted track seed
        for (auto t : mRecoTracks) {
            trackFitting(t);
        }
        long long itEnd = FwdTrackerUtils::nowNanoSecond();
        long long duration = (itEnd - itStart) * 1e-6; // milliseconds
        if ( mGenHistograms ){
            this->mHist["FitDuration"]->Fill(duration);
        }

        if ( mGenHistograms ){
            mQualityPlotter->afterIteration(0, mRecoTracks);
        }
    }


    /**
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
    }

    /*doTrackingOnHitmapSubset
     * @brief Does track finding steps on a subset of hits (phi slice)
     * @param iIteration: tracking iteration (for determining params)
     * @param hitmap: the hitmap to use, should already be subset of original
     * @returns a list of track seeds
     */
    vector<Seed_t> doTrackingOnHitmapSubset( size_t iIteration, FwdDataSource::HitMap_t &hitmap  ) {
        long long itStart = FwdTrackerUtils::nowNanoSecond();
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

        mTwoHitCrit.clear();
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

        mThreeHitCrit.clear();
        mThreeHitCrit = loadCriteria(criteriaPath);
        automaton.addCriteria(mThreeHitCrit);
        automaton.lengthenSegments();

        bool doAutomation = mConfig.get<bool>(criteriaPath + ":doAutomation", true);
        bool doCleanBadStates = mConfig.get<bool>(criteriaPath + ":cleanBadStates", true);

        if (doAutomation) {
            automaton.doAutomaton();
        } else {
            //Not running Automation Step
        }

        if (doAutomation && doCleanBadStates) {
            automaton.cleanBadStates();
        }

        duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        if (mGenHistograms)
            mHist["Step3Duration"]->Fill( duration );
        if (duration > 200){
            LOG_WARN << "The Three Hit Criteria took more than 200ms to process, duration: " << duration << " ms" << endm;
            LOG_WARN << "bailing out" << endm;
            std::vector<Seed_t> acceptedTracks;
            return acceptedTracks;
        }
        itStart = FwdTrackerUtils::nowNanoSecond();
        /*************************************************************/
        // Step 4
        // Get the tracks from the possible tracks that are the best subset
        /*************************************************************/
        std::string subsetPath = "TrackFinder.Iteration[" + std::to_string(iIteration) + "].SubsetNN";

        if (false == mConfig.exists(subsetPath))
            subsetPath = "TrackFinder.SubsetNN";

        //  only for debug really
        bool findSubsets = mConfig.get<bool>(subsetPath + ":active", true);
        std::vector<Seed_t> acceptedTracks;
        std::vector<Seed_t> rejectedTracks;

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
        return acceptedTracks;
    } // doTrackingOnHitmapSubset

    void doTrackIteration(size_t iIteration, FwdDataSource::HitMap_t &hitmap) {

        // empty the list of reco tracks for the iteration
        mRecoTracksThisItertion.clear();

        // check to see if we have hits!
        size_t nHitsThisIteration = nHitsInHitMap(hitmap);

        if (nHitsThisIteration < 4) {
            // No hits left in the hitmap! Skipping this iteration
            return;
        }

        // this starts the timer for the iteration
        if ( mGenHistograms ){
            mQualityPlotter->startIteration();
        }


        if ( false ) { // no phi slicing!
            /*************************************************************/
            // Steps 2 - 4 here
            /*************************************************************/
            auto acceptedTracks = doTrackingOnHitmapSubset( iIteration, hitmap );
            mRecoTracksThisItertion.insert( mRecoTracksThisItertion.end(), acceptedTracks.begin(), acceptedTracks.end() );
        } else {

            FwdDataSource::HitMap_t slicedHitMap;
            std::string pslPath = "TrackFinder.Iteration["+ std::to_string(iIteration) + "]:nPhiSlices";
            if ( false == mConfig.exists( pslPath ) ) pslPath = "TrackFinder:nPhiSlices";
            size_t phi_slice_count = mConfig.get<size_t>( pslPath, 1 );

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
                    if ( nHitsThisSlice < 4 ) {
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
                mRecoTracksThisItertion.insert( mRecoTracksThisItertion.end(), acceptedTracks.begin(), acceptedTracks.end() );
            } //loop on phi slices
        }// if loop on phi slices

        /*************************************************************/
        // Step 5
        // Remove the hits from any track that was found
        /*************************************************************/
        std::string hrmPath = "TrackFinder.Iteration["+ std::to_string(iIteration) + "].HitRemover";
        if ( false == mConfig.exists( hrmPath ) ) hrmPath = "TrackFinder.HitRemover";

        if ( true == mConfig.get<bool>( hrmPath + ":active", true ) ){
            removeHits( hitmap, mRecoTracksThisItertion );
        }

        for (auto t : mRecoTracksThisItertion) {
            trackFitting(t);
        }

        if ( mGenHistograms ){
            mQualityPlotter->afterIteration( iIteration, mRecoTracksThisItertion );
        }

        // Add the set of all accepted tracks (this iteration) to our collection of found tracks from all iterations
        mRecoTracks.insert( mRecoTracks.end(), mRecoTracksThisItertion.begin(), mRecoTracksThisItertion.end() );

    } // doTrackIteration

    void addSiHitsMc() {
        FwdDataSource::HitMap_t hitmap = mDataSource->getFstHits();

        for (size_t i = 0; i < mGlobalTracks.size(); i++) {

            if (mGlobalTracks[i]->getFitStatus(mGlobalTracks[i]->getCardinalRep())->isFitConverged() == false || mFitMoms[i].Perp() < 1e-3) {
                return;
            }


            if ( mGenHistograms){
                mHist["FitStatus"]->Fill("PossibleReFit", 1);
            }

            Seed_t si_hits_for_this_track(3, nullptr);

            for (size_t j = 0; j < 3; j++) {
                for (auto h0 : hitmap[j]) {
                    if (dynamic_cast<FwdHit *>(h0)->_tid == mGlobalTracks[i]->getMcTrackId()) {
                        si_hits_for_this_track[j] = h0;
                        break;
                    }
                } // loop on hits in this layer of hitmap
            }     // loop on hitmap layers

            size_t nSiHitsFound = 0;
            if ( si_hits_for_this_track[0] != nullptr ) nSiHitsFound++;
            if ( si_hits_for_this_track[1] != nullptr ) nSiHitsFound++;
            if ( si_hits_for_this_track[2] != nullptr ) nSiHitsFound++;

            if ( mGenHistograms ){
                this->mHist[ "nSiHitsFound" ]->Fill( 1, ( si_hits_for_this_track[0] != nullptr ? 1 : 0 ) );
                this->mHist[ "nSiHitsFound" ]->Fill( 2, ( si_hits_for_this_track[1] != nullptr ? 1 : 0 ) );
                this->mHist[ "nSiHitsFound" ]->Fill( 3, ( si_hits_for_this_track[2] != nullptr ? 1 : 0 ) );
            }

            if (nSiHitsFound >= 1) {
                if ( mGenHistograms ){
                    mHist["FitStatus"]->Fill("AttemptReFit", 1);
                }
                TVector3 p = mTrackFitter->refitTrackWithSiHits(mGlobalTracks[i], si_hits_for_this_track);

                if ( mGenHistograms ){
                    if (p.Perp() == mFitMoms[i].Perp()) {
                        mHist["FitStatus"]->Fill("BadReFit", 1);

                    } else {
                        mHist["FitStatus"]->Fill("GoodReFit", 1);
                    }
                }

                mNumFstHits[i] = nSiHitsFound;
                mFitMoms[i] = p;
            } // we have 3 Si hits to refit with

            if ( mGenHistograms ){
                mHist["FitStatus"]->Fill( TString::Format( "w%uSi", nSiHitsFound ).Data(), 1 );
            }

        }     // loop on the global tracks
    }         // ad Si hits via MC associations

    void addSiHits() {
        FwdDataSource::HitMap_t hitmap = mDataSource->getFstHits();

        // loop on global tracks
        for (size_t i = 0; i < mGlobalTracks.size(); i++) {
            if (mGlobalTracks[i]->getFitStatus(mGlobalTracks[i]->getCardinalRep())->isFitConverged() == false) {
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
                auto msp2 = mTrackFitter->projectToFst(2, mGlobalTracks[i]);
                auto msp1 = mTrackFitter->projectToFst(1, mGlobalTracks[i]);
                auto msp0 = mTrackFitter->projectToFst(0, mGlobalTracks[i]);

                // now look for Si hits near these
                hits_near_disk2 = findSiHitsNearMe(hitmap[2], msp2);
                hits_near_disk1 = findSiHitsNearMe(hitmap[1], msp1);
                hits_near_disk0 = findSiHitsNearMe(hitmap[0], msp0);
            } catch (genfit::Exception &e) {
                // Failed to project to Si disk: ", e.what()
            }

            vector<KiTrack::IHit *> hits_to_add;

            size_t nSiHitsFound = 0; // this is really # of disks on which a hit is found

            if ( mGenHistograms ){
                this->mHist[ "nSiHitsFound" ]->Fill( 1, hits_near_disk0.size() );
                this->mHist[ "nSiHitsFound" ]->Fill( 2, hits_near_disk1.size() );
                this->mHist[ "nSiHitsFound" ]->Fill( 3, hits_near_disk2.size() );
            }

            //  TODO: HANDLE multiple points found?
            if ( hits_near_disk0.size() == 1 ) {
                hits_to_add.push_back( hits_near_disk0[0] );
                nSiHitsFound++;
            } else {
                hits_to_add.push_back( nullptr );
            }
            if ( hits_near_disk1.size() == 1 ) {
                hits_to_add.push_back( hits_near_disk1[0] );
                nSiHitsFound++;
            } else {
                hits_to_add.push_back( nullptr );
            }
            if ( hits_near_disk2.size() == 1 ) {
                hits_to_add.push_back( hits_near_disk2[0] );
                nSiHitsFound++;
            } else {
                hits_to_add.push_back( nullptr );
            }

            if (nSiHitsFound >= 1) {
                if ( mGenHistograms ){
                    mHist["FitStatus"]->Fill("AttemptReFit", 1);
                }
                // LOG_INFO << "Fitting on GlobalTrack : " << mGlobalTracks[i] << " with " << nSiHitsFound << " si hits" << endm;
                TVector3 p = mTrackFitter->refitTrackWithSiHits(mGlobalTracks[i], hits_to_add);

                if ( mGenHistograms ){
                    if (p.Perp() == mFitMoms[i].Perp()) {
                        mHist["FitStatus"]->Fill("BadReFit", 1);
                    } else {
                        mHist["FitStatus"]->Fill("GoodReFit", 1);
                    }
                }

                // mGlobalTracks[i] = mTrackFitter->getTrack();
                mNumFstHits[i] = nSiHitsFound;
                mFitMoms[i] = p;

            } else {
                // unable to refit
            }

            if ( mGenHistograms ){
                mHist["FitStatus"]->Fill( TString::Format( "w%uSi", nSiHitsFound ).Data(), 1 );
            }

        } // loop on globals
    }     // addSiHits

    Seed_t findSiHitsNearMe(Seed_t &available_hits, genfit::MeasuredStateOnPlane &msp, double dphi = 0.004 * 9.5, double dr = 2.75) {
        double probe_phi = TMath::ATan2(msp.getPos().Y(), msp.getPos().X());
        double probe_r = sqrt(pow(msp.getPos().X(), 2) + pow(msp.getPos().Y(), 2));

        Seed_t found_hits;

        for (auto h : available_hits) {
            double h_phi = TMath::ATan2(h->getY(), h->getX());
            double h_r = sqrt(pow(h->getX(), 2) + pow(h->getY(), 2));
            double mdphi = fabs(h_phi - probe_phi);
            
            if ( mdphi < dphi && fabs( h_r - probe_r ) < dr) { // handle 2pi edge
                found_hits.push_back(h);
            }
        }

        return found_hits;
    }

    bool getSaveCriteriaValues() { return mSaveCriteriaValues; }
    std::vector<KiTrack::ICriterion *> getTwoHitCriteria() { return mTwoHitCrit; }
    std::vector<KiTrack::ICriterion *> getThreeHitCriteria() { return mThreeHitCrit; }

    TrackFitter *getTrackFitter() { return mTrackFitter; }
    void setEventVertex( TVector3 v ) { mEventVertex = v; }

  protected:
    unsigned long long int nEvents;

    bool mDoTrackFitting = true;
    bool mSaveCriteriaValues = true;

    FwdTrackerConfig mConfig;
    std::string mConfigFile;
    size_t mTotalHitsRemoved;
    

    std::vector<Seed_t> mRecoTracks; // the tracks recod from all iterations
    std::vector<Seed_t> mRecoTracksThisItertion;

    // Set to the Primary vertex for the event
    TVector3 mEventVertex;
    
    // These are vectors with info about each track / fit
    // they should all have the same length
    std::vector<float> mRecoTrackQuality;
    std::vector<int> mRecoTrackIdTruth;
    std::vector<TVector3> mFitMoms;
    std::vector<unsigned short> mNumFstHits;
    std::vector<genfit::FitStatus> mFitStatus;
    std::vector<genfit::AbsTrackRep *> mGlobalTrackReps;
    std::vector<genfit::Track *> mGlobalTracks;

    QualityPlotter *mQualityPlotter;
    std::shared_ptr<FwdDataSource> mDataSource;

    TrackFitter *mTrackFitter = nullptr;

    std::vector<KiTrack::ICriterion *> mTwoHitCrit;
    std::vector<KiTrack::ICriterion *> mThreeHitCrit;

    // histograms of the raw input data
    bool mGenHistograms = false; // controls these histograms and use of QualityPlotter
    std::map<std::string, TH1 *> mHist;
    

    
};

#endif
