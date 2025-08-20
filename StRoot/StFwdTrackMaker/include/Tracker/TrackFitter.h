#ifndef TRACK_FITTER_H
#define TRACK_FITTER_H

#include "GenFit/ConstField.h"
#include "GenFit/EventDisplay.h"
#include "GenFit/Exception.h"
#include "GenFit/FieldManager.h"
#include "GenFit/KalmanFitStatus.h"
#include "GenFit/GblFitter.h"
#include "GenFit/ProlateSpacepointMeasurement.h"

#include "TDatabasePDG.h"
#include "TGeoManager.h"
#include "TMath.h"
#include "TRandom.h"
#include "TRandom3.h"
#include "TVector3.h"

#include <vector>
#include <memory>
#include "malloc.h"

#include "StFwdTrackMaker/Common.h"

#include "StFwdTrackMaker/include/Tracker/FwdHit.h"
#include "StFwdTrackMaker/include/Tracker/TrackFitter.h"
#include "StFwdTrackMaker/include/Tracker/STARField.h"
#include "StFwdTrackMaker/include/Tracker/FwdGeomUtils.h"

#include "StarGenerator/UTIL/StarRandom.h"
#include "FitterUtils.h"

#include "StMemStat.h"

/* Class for interfacing with GenFit for fitting tracks 
 *
 */
class TrackFitter {
// Accessors and options
  public:
    std::shared_ptr<genfit::Track> getTrack() { return mFitTrack; }
    int getCurrentSeedCharge() const { return mCurrentSeedCharge; }
    TVector3 getCurrentSeedMomentum() const { return mCurrentSeedMomentum; }
    TVector3 getCurrentSeedPosition() const { return mCurrentSeedPosition; }

    // this is used rarely for debugging purposes, especially to check/compare plane misalignment
    static constexpr bool kUseSpacePoints = true; // use spacepoints instead of planar measurements
    static constexpr int kVerbose = 1; // verbosity level for debugging

    void clear(){
        LOG_DEBUG << "TrackFitter::clear() called" << endm;
        mFitTrack.reset();
        for (auto &track : mFitTracks) {
            if (track) {
                track->Clear();
                track.reset();
            }
        }
    }

  public:

    /**
     * @brief Construct a new Track Fitter object
     *
     * @param _mConfig : Config object
     * @param geoCache : Geometry cache filename
     */
    TrackFitter(FwdTrackerConfig _mConfig, TString geoCache) : mConfig(_mConfig), mGeoCache(geoCache), mFitTrack(nullptr) {}

    /**
     * @brief Setup the tracker object
     * Load geometry
     * Setup Material Effects
     * Setup the magnetic field
     * Setup the fitter
     * Setup the fit planes
     */
    void setup() {

        // the geometry manager that GenFit will use
        TGeoManager * gMan = nullptr;

        // Setup the Geometry used by GENFIT
        LOG_INFO << "StFwdTrackMaker is loading the geometry cache: " << mConfig.get<string>("Geometry", mGeoCache.Data()).c_str() << endm;
        TGeoManager::Import(mConfig.get<string>("Geometry", mGeoCache.Data()).c_str());
        gMan = gGeoManager;
        // Set up the material interface and set material effects on/off from the config
        genfit::MaterialEffects::getInstance()->init(new genfit::TGeoMaterialInterface());

        // Set Material Stepper debug level
        genfit::MaterialEffects::getInstance()->setDebugLvl( mConfig.get<int>("TrackFitter.MaterialEffects:DebugLvl", 0) );

        genfit::MaterialEffects::getInstance()->setEnergyLossBetheBloch( mConfig.get<int>("TrackFitter.MaterialEffects.EnergyLossBetheBloch", true) );
        genfit::MaterialEffects::getInstance()->setNoiseBetheBloch( mConfig.get<int>("TrackFitter.MaterialEffects.NoiseBetheBloch", true) );
        genfit::MaterialEffects::getInstance()->setNoiseCoulomb( mConfig.get<int>("TrackFitter.MaterialEffects.NoiseCoulomb", true) );
        genfit::MaterialEffects::getInstance()->setEnergyLossBrems( mConfig.get<int>("TrackFitter.MaterialEffects.EnergyLossBrems", true) );
        genfit::MaterialEffects::getInstance()->setNoiseBrems( mConfig.get<int>("TrackFitter.MaterialEffects.NoiseBrems", true) );
        genfit::MaterialEffects::getInstance()->ignoreBoundariesBetweenEqualMaterials( mConfig.get<int>("TrackFitter.MaterialEffects.ignoreBoundariesBetweenEqualMaterials", true) );

        // do this last to override
        genfit::MaterialEffects::getInstance()->setNoEffects( !mConfig.get<bool>("TrackFitter:MaterialEffects", false)); // negated, true means defaul is all effects on (noEffects off)
        if (!mConfig.get<bool>("TrackFitter:MaterialEffects", false)){
            LOG_INFO << "Turning OFF GenFit Material Effects in stepper" << endm;
        }

        // Determine which Magnetic field to use
        // Either constant field or real field from StarFieldAdaptor
        if (mConfig.get<bool>("TrackFitter:constB", false)) {
            mBField = std::unique_ptr<genfit::AbsBField>(new genfit::ConstField(0., 0., 5.0)); // 0.5 T Bz
            LOG_INFO << "StFwdTrackMaker: Tracking with constant magnetic field" << endm;
        } else if (mConfig.get<bool>("TrackFitter:zeroB", false)) {
            mBField = std::unique_ptr<genfit::AbsBField>(new genfit::ConstField(0., 0., 0.)); // ZERO FIELD
            LOG_INFO << "StFwdTrackMaker: Tracking with ZERO magnetic field" << endm;
        } else {
            mBField = std::unique_ptr<genfit::AbsBField>(new StarFieldAdaptor());
            LOG_INFO << "StFwdTrackMaker: Tracking with StarFieldAdapter" << endm;
        }
        // we must have one of the two available fields at this point
        // note, the pointer is still bound to the lifetime of the TackFitter
        genfit::FieldManager::getInstance()->init(mBField.get());

        setupGenfitKalmanFitter();

        // FwdGeomUtils looks into the loaded geometry and gets detector z locations if present
        FwdGeomUtils fwdGeoUtils( gMan );

        // Create the genfit Planes for the FST sensors
        createAllFstPlanes( fwdGeoUtils );
        createAllFttPlanes( fwdGeoUtils );
        // create the EPD plane at z=375
        mEpdPlane = genfit::SharedPlanePtr(
            new genfit::DetPlane( TVector3(0, 0, 375), TVector3(0, 0, 1) )
        );
        LOG_DEBUG << "Created all FST and FTT planes" << endm;

        if (kVerbose > 0) {
            // report all fitter options
            LOG_INFO << "Fitter options: " << endm;
            LOG_INFO << "\tMaxFailedHits: " << mFitter->getMaxFailedHits() << endm;
            LOG_INFO << "\tMaxIterations: " << mFitter->getMaxIterations() << endm;
            LOG_INFO << "\tMinIterations: " << mFitter->getMinIterations() << endm;
            LOG_INFO << "\tRelChi2Change: " << mFitter->getRelChi2Change() << endm;
            // LOG_INFO << "\tAbsChi2Change: " << mFitter->getAbsChi2Change() << endm;
            LOG_INFO << "\tDeltaPval: " << mFitter->getDeltaPval() << endm;
            LOG_INFO << "\tBlowUpFactor: " << mFitter->getBlowUpFactor() << endm;
            LOG_INFO << "\tBlowUpMaxVal: " << mFitter->getBlowUpMaxVal() << endm;
        }
        
    }

    /**
     * @brief Setup the fitter
     *
     * This is called in the setup() method but could be called again to remake the fitter
     */
    void setupGenfitKalmanFitter(){
        // initialize the main mFitter using a KalmanFitter with reference tracks
        mFitter = std::unique_ptr<genfit::AbsKalmanFitter>(new genfit::KalmanFitterRefTrack(
            4, 1e-3, 1e3, true /*sqrt Formalism, must be set in ctor*/
        ));
        // Note: sqrt formalism is very helpful/necessary in fwd region, to keep cov pos def. 
        // We also benefit from the ref track implementation since FST seeds can be consistent with zero pT

        // Here we load several options from the config,
        // to customize the mFitter behavior
        mFitter->setMaxFailedHits(mConfig.get<int>("TrackFitter.KalmanFitterRefTrack:MaxFailedHits", -1)); // default -1, no limit
        mFitter->setDebugLvl(mConfig.get<int>("TrackFitter.KalmanFitterRefTrack:DebugLvl", 0)); // default 0, no output
        mFitter->setMaxIterations(mConfig.get<int>("TrackFitter.KalmanFitterRefTrack:MaxIterations", 100)); // default 4 iterations
        mFitter->setMinIterations(mConfig.get<int>("TrackFitter.KalmanFitterRefTrack:MinIterations", 10)); // default 0 iterations

        // Set the fit convergence paramters
        mFitter->setRelChi2Change( mConfig.get<double>("TrackFitter.KalmanFitterRefTrack:RelChi2Change", 1e-1) );
        mFitter->setDeltaPval( mConfig.get<double>("TrackFitter.KalmanFitterRefTrack:DeltaPval", 1e-1) );
        mFitter->setBlowUpMaxVal( 1e9 );
        mFitter->setBlowUpFactor( mConfig.get<double>("TrackFitter.KalmanFitterRefTrack:BlowUpFactor", 1e9) );

        double deltaChi2Ref = mConfig.get<double>("TrackFitter.KalmanFitterRefTrack:DeltaChi2Ref", 1e-1);
        if ( static_cast<genfit::KalmanFitterRefTrack*>(mFitter.get()) != nullptr ) {
            LOG_INFO << "Setting the KalmanFitterRefTrack to update ref track Chi2Ref" << endm;
            static_cast<genfit::KalmanFitterRefTrack*>(mFitter.get())->setDeltaChi2Ref( 
                deltaChi2Ref 
            );
        }

        // Report all the parameters
        if (kVerbose > 0) {
            LOG_INFO << "TrackFitter::setupGenfitKalmanFitter() called" << endm;
            LOG_INFO << "\tMaxFailedHits: " << mFitter->getMaxFailedHits() << endm;
            LOG_INFO << "\tMaxIterations: " << mFitter->getMaxIterations() << endm;
            LOG_INFO << "\tMinIterations: " << mFitter->getMinIterations() << endm;
            LOG_INFO << "\tRelChi2Change: " << mFitter->getRelChi2Change() << endm;
            LOG_INFO << "\tDeltaPval: " << mFitter->getDeltaPval() << endm;
            LOG_INFO << "\tBlowUpFactor: " << mFitter->getBlowUpFactor() << endm;
            LOG_INFO << "\tBlowUpMaxVal: " << mFitter->getBlowUpMaxVal() << endm;
            if ( static_cast<genfit::KalmanFitterRefTrack*>(mFitter.get()) != nullptr ) {
                LOG_INFO << "Using **KalmanFitterRefTrack** with Chi2Ref update @ " << deltaChi2Ref << endm;
            }
        }


    } // setupGenfitKalmanFitter

    /**
     * @brief Convert the 3x3 covmat to 2x2 by dropping z
     *
     * @param h : hit with cov matrix
     * @return TMatrixDSym : cov matrix 2x2
     */
    TMatrixDSym CovMatPlane(KiTrack::IHit *h){
        TMatrixDSym cm(2);
        cm(0, 0) = static_cast<FwdHit*>(h)->_covmat(0, 0);
        cm(1, 1) = static_cast<FwdHit*>(h)->_covmat(1, 1);
        cm(0, 1) = static_cast<FwdHit*>(h)->_covmat(0, 1);
        return cm;
    }

    /**
     * @brief Get projection to a given plane
     *
     * @param fstPlane : plane index
     * @param fitTrack : track to project
     * @return genfit::MeasuredStateOnPlane
     */
    genfit::MeasuredStateOnPlane projectToPlane(genfit::SharedPlanePtr plane, std::shared_ptr<genfit::Track> fitTrack, int iState = 0) {
        genfit::MeasuredStateOnPlane nil;
        if (plane == nullptr) {
            LOG_ERROR << "Plane is null, cannot project" << endm;
            return nil;
        }
        if (fitTrack == nullptr) {
            LOG_ERROR << "Track is null, cannot project" << endm;
            return nil;
        }

        genfit::MeasuredStateOnPlane tst = fitTrack->getFittedState(iState);
        fitTrack->getCardinalRep()->extrapolateToPlane(tst, plane);
        return tst;   
    }

    /**
     * @brief Get projection to the EPD
     *
     * @param fitTrack : track to project
     * @return genfit::MeasuredStateOnPlane
     */
    genfit::MeasuredStateOnPlane projectToEpd(std::shared_ptr<genfit::Track> fitTrack) {
        return projectToPlane(mEpdPlane, fitTrack);
    }

    /**
     * @brief Get projection to given FST plane
     *
     * @param fstPlane : plane index
     * @param fitTrack : track to project
     * @return genfit::MeasuredStateOnPlane
     */
    genfit::MeasuredStateOnPlane projectToFst(size_t fstSensorPlaneIndex, std::shared_ptr<genfit::Track> fitTrack) {
        if (fstSensorPlaneIndex > mFstSensorPlanes.size()) {
            genfit::MeasuredStateOnPlane nil;
            LOG_ERROR << "FST plane index out of range: " << fstSensorPlaneIndex << endm;
            return nil;
        }
        return projectToPlane(mFstSensorPlanes[fstSensorPlaneIndex], fitTrack);
    }

    /**
     * @brief Get projection to given FTT plane
     *
     * @param iFttPlane : plane index
     * @param fitTrack : track to project
     * @return genfit::MeasuredStateOnPlane
     */
    genfit::MeasuredStateOnPlane projectToFtt(size_t iFttPlane, std::shared_ptr<genfit::Track> fitTrack) {
        if (iFttPlane > mFttPlanes.size()) {
            LOG_ERROR << "FTT plane index out of range: " << iFttPlane << endm;
            genfit::MeasuredStateOnPlane nil;
            return nil;
        }
        return projectToPlane(mFttPlanes[iFttPlane], fitTrack);
    }

    vector<genfit::Track*> pTracks;
    vector<FwdHit> stressHits;
    void stressTest(){
        pTracks.reserve(10000); // reserve space for 1000 tracks

        double memStart = StMemStat::Used();
        double memEndInside = 0;
        StMemStat::PrintMem("TrackFitter::stressTest START");
        LOG_INFO << "TrackFitter::stressTest" << endm;
        
        { // scope the stack variables
            
            // generate some random hits
            if (stressHits.size() > 0) {
                LOG_INFO << "Stress hits already generated, skipping generation" << endm;
            } else {
                LOG_INFO << "Generating stress hits" << endm;
                for ( size_t i = 0; i < 1000; i++ ) {
                    FwdHit hit;
                    hit.setXYZDetId( 
                        gRandom->Uniform(-500, 500), // random x between -500 and 500
                        gRandom->Uniform(-500, 500), // random y between -500 and 500
                        gRandom->Uniform(-500, 500), // random z between -500 and 500
                        kFstId           // random detector id between 0 and 2
                    );
                    
                    hit._covmat.ResizeTo( 3, 3 );
                    hit._covmat(0, 0) = 1; // set covariance matrix to identity
                    hit._covmat(1, 1) = 1;
                    hit._covmat(2, 2) = 1;
                    hit._covmat(0, 1) = 0;
                    hit._covmat(1, 0) = 0;
                    hit._covmat(0, 2) = 0;
                    hit._covmat(2, 0) = 0;
                    hit._covmat(1, 2) = 0;
                    hit._covmat(2, 1) = 0;
                    stressHits.push_back(hit);
                }
            }
        
            const int n = 10000; // number of seeds to fit
            // loop on the track seeds and fit them
            genfit::Track* fitTrack = new genfit::Track();
            for ( size_t i = 0; i < n; i++ ) {
                // generate a random seed by sampling the hits
                Seed_t seed;
                for ( int j = 0; j < rand() % 7 + 3; j++ ) { // 2 - 9 hits per seed
                    size_t hitIndex = rand() % stressHits.size();
                    seed.push_back( &stressHits[hitIndex] );    

                    int hitId = 0;
                    
                    fitTrack->insertPoint(
                        createTrackSpacepointFromMeasurement(nullptr, &stressHits[hitIndex], hitId)
                    );
                } // loop of seed generation     
            } // loop tracks
            
            // Test fit
            performFit(fitTrack);

            fitTrack->Clear();
            delete fitTrack; // delete the track
            fitTrack = nullptr; // clear the pointer

            StMemStat::PrintMem("TrackFitter::stressTest END (inside of scope)");
            memEndInside = StMemStat::Used();
        }
        StMemStat::PrintMem("TrackFitter::stressTest END (out of scope)");
        double memEndOutside = StMemStat::Used();

        malloc_trim(0);

        StMemStat::PrintMem("TrackFitter::stressTest END (clear HEAP)");
        double memEndFinal= StMemStat::Used();

        printf("Memory used in TrackFitter::stressTest: %.2f MB (start) -> %.2f MB (end inside scope) -> %.2f MB (end outside scope) -> %.2f MB (final clear)\n",
            memStart, memEndInside, memEndOutside, memEndFinal);
        printf("Memory difference: %.2f MB (inside scope) -> %.2f MB (outside scope) -> %.2f MB (final clear)\n",
            memEndInside - memStart, memEndOutside - memEndInside, memEndFinal - memEndOutside);
        printf("Total memory used: %.2f MB\n", memEndFinal - memStart);


        auto mi = mallinfo();
        printf("Heap in use: %.2f MB | Free blocks: %.2f MB | Total arena: %.2f MB\n",
            mi.uordblks / 1024.0 / 1024.0,
            mi.fordblks / 1024.0 / 1024.0,
            mi.arena / 1024.0 / 1024.0);

    }

    genfit::TrackPoint* createTrackPointFromPlanarMeasurement(std::shared_ptr<genfit::Track> fitTrack, FwdHit *fh, int &hitId){
        assert( fh != nullptr && "FwdHit pointer is null, cannot create planar measurement" );
        TVectorD hitOnPlane(2);
        hitOnPlane[0] = fh->getX();
        hitOnPlane[1] = fh->getY();
        auto tp = new genfit::TrackPoint();
        genfit::PlanarMeasurement *measurement = new genfit::PlanarMeasurement(hitOnPlane, CovMatPlane(fh), fh->_detid, ++hitId, tp);
        genfit::SharedPlanePtr plane = getPlaneFor( fh );
        int planeId = fh->_genfit_plane_index;
        
        // I do this to make the planeId unique between FST and FTT
        if (fh->isFtt()) {
            planeId = kFstNumSensors + fh->_genfit_plane_index;
        }          
        measurement->setPlane(plane, planeId);

        tp->addRawMeasurement(measurement);
        tp->setTrack(fitTrack.get());
        tp->setSortingParameter(planeId); // or use the hitId?
        if (fitTrack)
            fitTrack->insertPoint( tp );
        return tp;
    }


    genfit::TrackPoint* createTrackSpacepointFromMeasurement( std::shared_ptr<genfit::Track> fitTrack, FwdHit *fh, int &hitId ) {
        assert( fh != nullptr && "FwdHit pointer is null, cannot create space point" );

        TVectorD pv(3);
        pv[0] = fh->getX();
        pv[1] = fh->getY();
        pv[2] = fh->getZ();
        LOG_DEBUG << "x = " << pv[0] << "+/- " << fh->_covmat(0,0) << ", y = " << pv[1] << " +/- " << fh->_covmat(1,1) << ", z = " << pv[2] << " +/- " << fh->_covmat(2,2) << endm;

        // TMatrixDSym _covmat = TMatrixDSym(3);
        // _covmat(0, 0) = 0.5;
        // _covmat(1, 1) = 0.5;
        // _covmat(2, 2) = 0.5;
        // LOG_INFO << "FAKE COVARIANCE MATRIX: " << endm;
        auto tp = new genfit::TrackPoint();

        genfit::SpacepointMeasurement *measurement = nullptr;
        if (fh->isFst() ){
            // largest error direction is in the radial direction, compute from cartesian coordinates
            TVector3 led( fh->getX(), fh->getY(), 0 );
            led.SetMag(1.0);
            measurement = new genfit::ProlateSpacepointMeasurement(pv, fh->_covmat, fh->_detid, ++hitId, tp);
            static_cast<genfit::ProlateSpacepointMeasurement*>(measurement)->setLargestErrorDirection( led );
        } else if (fh->isFtt() ){
            // largest error direction is in the radial direction, compute from cartesian coordinates
            TVector3 led( fh->getX(), fh->getY(), 0 );
            if ( fh->_covmat(0,0) > fh->_covmat(1,1))
                led.SetXYZ( 1.0, 0, 0 );
            else 
                led.SetXYZ( 0.0, 1.0, 0 );
            measurement = new genfit::ProlateSpacepointMeasurement(pv, fh->_covmat, fh->_detid, ++hitId, tp);
            static_cast<genfit::ProlateSpacepointMeasurement*>(measurement)->setLargestErrorDirection( led );
        } else {
            measurement = new genfit::SpacepointMeasurement(pv, fh->_covmat, fh->_detid, ++hitId, tp);
        }
        
        tp->addRawMeasurement(measurement);
        if ( fitTrack ){
            tp->setTrack(fitTrack.get());
        }
        return tp;
    }

    void setSortingParameter( FwdHit*fh, genfit::TrackPoint*tp, size_t &idxFtt, size_t &idxFst ) {
        // Set the sorting parameter
        if ( fh->isPV() ){
            tp->setSortingParameter(0);
        }
        // These below are only used if kUseSpacePoints is true
        if ( fh->isFtt() ){
            tp->setSortingParameter(4 + idxFtt);
            idxFtt++;
        }
        if ( fh->isFst() ){
            tp->setSortingParameter(1 + idxFst);
            idxFst++;
        }
    }

    void summarizeTrackReps() {
        LOG_INFO << "Track Reps: " << mFitTrack->getNumReps() << endm;
        for (size_t i = 0; i < mFitTrack->getNumReps(); i++) {
            auto tr = mFitTrack->getTrackRep(i);
            // LOG_INFO << "Track Rep " << i << ": " << tr->getName() << endm;
            LOG_INFO << "Track Rep " << i << " " << endm;
            tr->Print();
            auto fs = mFitTrack->getFitStatus(tr);
            if (fs != nullptr) {
                fs->Print();
            } else {
                LOG_ERROR << "Track Rep " << i << " has no fit status" << endm;
            }
        }
    }

    /**
     * @brief setup the track from the given seed and optional primary vertex
     * @param trackSeed : seed points
     * @param seedMom : seed momentum
     * @param seedPos : seed position
     * @param Vertex : primary vertex
     */
    bool setupTrack(Seed_t trackSeed, TVector3 *externalSeedMom = nullptr ) {
        
        mCurrentTrackSeed = trackSeed;
        // setup the track fit seed parameters
        GenericFitSeeder gfs;
        mCurrentSeedCharge = 0; // explicitly reset because a zero charge indicates a failed seed
        gfs.makeSeed(   trackSeed, 
                        mCurrentSeedPosition, 
                        mCurrentSeedMomentum, 
                        mCurrentSeedCharge 
                    );
        if ( mCurrentSeedMomentum.Perp() > 1000 ) {
            LOG_WARN << "Seed momentum is too high, setting to (0,0,1)" << endm;
            mCurrentSeedMomentum.SetXYZ(0, 0, 1);
        }

        if ( externalSeedMom != nullptr ) {
            LOG_INFO << "Note: Using externally provided seed momentum" << endm;
            mCurrentSeedMomentum = *externalSeedMom;
        } else {
            // mCurrentSeedMomentum.SetXYZ(0, 0, 10);
        }

        LOG_DEBUG << "Setting track fit seed position = " << TString::Format( "(px=%f, py=%f, pz=%f)", mCurrentSeedPosition.X(), mCurrentSeedPosition.Y(), mCurrentSeedPosition.Z() )  << endm; 
        LOG_DEBUG << "Setting track fit seed momentum = " << TString::Format( "(%f, %f, %f)", mCurrentSeedMomentum.X(), mCurrentSeedMomentum.Y(), mCurrentSeedMomentum.Z() ) << endm;
        if ( mCurrentSeedMomentum.Perp() > 1e-5 && (mCurrentSeedMomentum.Perp() / mCurrentSeedMomentum.Pz()) > 1e-5 ) {
            LOG_DEBUG << "\t" << TString::Format( "(pT=%f, eta=%f, phi=%f)", mCurrentSeedMomentum.Perp(), mCurrentSeedMomentum.Eta(), mCurrentSeedMomentum.Phi() ) << endm;
        }
        LOG_DEBUG << "Setting track fit seed charge = " << mCurrentSeedCharge << endm;

        if ( mCurrentSeedCharge == 0 ) {
            LOG_ERROR << "Seed charge is zero, skipping track -> usually means collinear points" << endm;
            return false;
        }

        // create the track representations
        // Note that multiple track reps differing only by charge results in a silent failure of GenFit
        auto pionRep = new genfit::RKTrackRep(mPdgPiPlus * mCurrentSeedCharge);

        // Create the track
        mFitTrack = std::make_shared<genfit::Track>(pionRep , mCurrentSeedPosition, mCurrentSeedMomentum);
        mFitTracks.push_back(mFitTrack);
        // now add the points to the track

        int hitId(0);       // hit ID
        
        /******************************************************************************************************************
		 * loop over the hits, add them to the track
		 ******************************************************************************************************************/
        // use these to enforce our sorting parameters
        size_t idxFst = 0; // index of the FST hit
        size_t idxFtt = 0; // index of the FTT hit
        for (auto h : trackSeed) {
            auto fh = dynamic_cast<FwdHit*>(h);
            if (fh == nullptr) {
                LOG_ERROR << "Hit is not a FwdHit, cannot add to track" << endm;
                continue;
            }

            TString hitType = "Unknown";
            if ( fh->isPV() ) hitType = "PV";
            else if ( fh->isFst() ) hitType = "FST";
            else if ( fh->isFtt() ) hitType = "FTT";
            else if ( fh->isEpd() ) hitType = "EPD";
            else {
                LOG_ERROR << "Hit is not a valid FwdHit, cannot add to track" << endm;
                continue;
            }
            

            /******************************************************************************************************************
            * If the Primary vertex is included
            ******************************************************************************************************************/
            if ( kUseSpacePoints || fh->isPV() ) {
                LOG_DEBUG << "Treating " << hitType << " hit as a spacepoint" << endm;
                auto tp = createTrackSpacepointFromMeasurement( mFitTrack, fh, hitId );
                setSortingParameter(fh, tp, idxFtt, idxFst);
                // add the spacepoint to the track
                mFitTrack->insertPoint( tp );
                continue;
            } else {
                createTrackPointFromPlanarMeasurement( mFitTrack, fh, hitId );
            }
        } // loop on trackSeed

        if ( mFitTrack == nullptr ) {
            LOG_ERROR << "Track is null, cannot setup track" << endm;
            return false;
        }
        if (kVerbose){
            LOG_INFO << "Track Setup complete, track has " << mFitTrack->getNumPoints() << " points" << endm;
            LOG_INFO << " sorting changed track points: "  << mFitTrack->sort() << endm;
        }
        return true;
    } // setupTrack

    /** @brief performs the fit on a track
     *  @param t : track to fit
    */
    void performFit( genfit::Track* trackPointer ){
        assert( trackPointer != nullptr && "Track pointer is null, cannot perform fit" );
        /******************************************************************************************************************
		 * Do the fit
		 ******************************************************************************************************************/
        try {
            // check the track for consistency
            trackPointer->checkConsistency();
            // do the fit
            mFitter->processTrack(trackPointer);
            
            // check the track for consistency
            trackPointer->checkConsistency();

            // find track rep with smallest chi2
            trackPointer->determineCardinalRep();
            
            auto status = trackPointer->getFitStatus();
            if ( status == nullptr ) {
                LOG_ERROR << "Track fit status is null" << endm;
                return;   
            }
            if ( kVerbose > 0 ) {
                LOG_INFO << "Fit status:  " << status->isFitConverged() << endm;
                LOG_INFO << "-Fit pvalue: " << status->getPVal() << endm;
                LOG_INFO << "-Fit Chi2:   " << status->getChi2() << endm;
                LOG_INFO << "-Fit Chi2:   " << status->getChi2() << endm;
            }

            if ( status->isFitConverged() && kVerbose > 0 ){
             
                auto cr = trackPointer->getCardinalRep();
                auto p = cr->getMom( trackPointer->getFittedState( 0, cr ));
                
                LOG_INFO << "Track fit charge: " << status->getCharge();
                LOG_INFO << "Fit momentum: " << p.X() << ", " << p.Y() << ", " << p.Z() << endm;
                LOG_INFO << "\tFit Pt: " << p.Pt() << ", eta: " << p.Eta() << ", phi: " << p.Phi() << endm;
            }


        } catch (genfit::Exception &e) {
            LOG_ERROR << "Exception on fit update" << e.what() << endm;
        }
        if ( kVerbose > 0 ) {
            LOG_INFO << "Track fit update complete!" << endm;
        }
    }

    /*
     * @brief Get all FST planes
     *
     * @return std::vector<genfit::SharedPlanePtr>
     */
    void createAllFstPlanes( FwdGeomUtils &fwdGeoUtils )
    {
        // create FWD GeomUtils to get the plane locations
        for (int globalSensorIndex = 0; globalSensorIndex < kFstNumSensors; globalSensorIndex++)
        {
            TVector3 u(1, 0, 0); 
            TVector3 v(0, 1, 0);
            TVector3 o = fwdGeoUtils.getFstSensorOrigin(globalSensorIndex, u, v);
            if (kVerbose > 1) {
                LOG_INFO << "Adding FST Sensor " << globalSensorIndex << " at " << o.X() << ", " << o.Y() << ", " << o.Z() << endm;
                LOG_INFO << "\tSensor " << globalSensorIndex << " U = " << u.X() << ", " << u.Y() << ", " << u.Z() << endm;
                LOG_INFO << "\tSensor " << globalSensorIndex << " V = " << v.X() << ", " << v.Y() << ", " << v.Z() << endm;
            }
            mFstSensorPlanes.push_back(
                genfit::SharedPlanePtr(
                    new genfit::DetPlane(o, u, v)));
        }
    }

    /*
     * @brief Get all FTT planes
     *
     * @return std::vector<genfit::SharedPlanePtr>
     */
    void createAllFttPlanes( FwdGeomUtils &fwdGeoUtils ) {
        // create FWD GeomUtils to get the plane locations
        // 4 planes, 4 quadrants, 2 planes per quadrant
        for (int globalPlaneIndex = 0; globalPlaneIndex < 32; globalPlaneIndex++)
        {
            TVector3 u(1, 0, 0); 
            TVector3 v(0, 1, 0);
            TVector3 o = fwdGeoUtils.getFttQuadrant(globalPlaneIndex, u, v);
            if (kVerbose > 1) { 
                LOG_INFO << "Adding FTT Plane " << globalPlaneIndex << " at " << o.X() << ", " << o.Y() << ", " << o.Z() << endm;
                LOG_INFO << "\tPlane " << globalPlaneIndex << " U = " << u.X() << ", " << u.Y() << ", " << u.Z() << endm;
                LOG_INFO << "\tPlane " << globalPlaneIndex << " V = " << v.X() << ", " << v.Y() << ", " << v.Z() << endm;
            }
            mFttPlanes.push_back(
                genfit::SharedPlanePtr(
                    new genfit::DetPlane(o, u, v)));
        }
    }
    

    /**
     * @brief Primary track fitting routine
     *
     * @param trackSeed :
     * @param Vertex : Primary Vertex
     * @param seedMomentum : seed momentum (can be from MC)
     * @return void : the results can be accessed via the getTrack() method
     */
    long long fitTrack(Seed_t trackSeed, TVector3 *seedMomentum = 0) {
        long long itStart = FwdTrackerUtils::nowNanoSecond();
        LOG_DEBUG << "Fitting track with " << trackSeed.size() << " FWD Measurements" << endm;

        /******************************************************************************************************************
		 * First sort the seed, bc GENFIT seemingly cannot handle out of order points
		 ******************************************************************************************************************/
        std::sort(trackSeed.begin(), trackSeed.end(), 
            [](KiTrack::IHit *a, KiTrack::IHit *b) 
                { return a->getZ() < b->getZ(); }
        );

        /******************************************************************************************************************
		 * Setup the track fit seed parameters and objects
		 ******************************************************************************************************************/
        bool valid = setupTrack(trackSeed, seedMomentum);
        if ( !valid ){
            LOG_ERROR << "Failed to setup track for fit" << endm;
            return -1;
        }
        LOG_DEBUG << "Ready to fit with " << mFitTrack->getNumPoints() << " track points" << endm;

        /******************************************************************************************************************
		 * Do the fit
		 ******************************************************************************************************************/
        performFit( mFitTrack.get() );
        if ( kVerbose ) summarizeTrackReps();
        long long duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        return duration;
    } // fitTrack

    genfit::SharedPlanePtr getPlaneFor( FwdHit * fh ){
        
        // sTGC
        if ( fh->isFtt() ){
            if ( fh->_genfit_plane_index > mFttPlanes.size() ) {
                LOG_ERROR << "Invalid FTT genfit plane index: " << fh->_genfit_plane_index << endm;
                return nullptr;
            }
            return mFttPlanes[fh->_genfit_plane_index];
        }

        // FST
        if ( fh->isFst() ){
            if ( fh->_genfit_plane_index > mFstSensorPlanes.size() ) {
                LOG_ERROR << "Invalid FST sensor genfit plane index: " << fh->_genfit_plane_index << endm;
                return nullptr;
            }
            return mFstSensorPlanes[ fh->_genfit_plane_index ];
        }
        LOG_ERROR << "Unknown FwdHit type, cannot get plane - if this is a PV then use an abs measurement" << endm;
        return nullptr;
    }

    // Store the planes for FTT and FST
    vector<genfit::SharedPlanePtr> mFttPlanes;
    vector<genfit::SharedPlanePtr> mFstSensorPlanes; // 108 planes, one for each sensor

    genfit::SharedPlanePtr mEpdPlane; // EPD plane

  protected:
    std::unique_ptr<genfit::AbsBField> mBField;

    FwdTrackerConfig mConfig; // main config object
    TString mGeoCache; // the name of the geometry cache file

    // Main GenFit fitter instance
    std::unique_ptr<genfit::AbsKalmanFitter> mFitter = nullptr;

    // PDG codes for the default plc type for fits
    static const int mPdgPiPlus = 211;
    // GenFit state - resused
    std::shared_ptr<genfit::Track> mFitTrack;
    vector<std::shared_ptr<genfit::Track>> mFitTracks; // save all genfitTracks to make sure we clear them properly
    Seed_t mCurrentTrackSeed;
    int mCurrentSeedCharge = 0; // current seed charge, used for debugging
    TVector3 mCurrentSeedMomentum = TVector3(0, 0, 10); // current seed momentum, used for debugging
    TVector3 mCurrentSeedPosition = TVector3(0, 0, 0); // current seed position, used for debugging
};


#endif
