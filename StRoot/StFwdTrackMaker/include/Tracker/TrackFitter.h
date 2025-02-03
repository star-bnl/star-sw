#ifndef TRACK_FITTER_H
#define TRACK_FITTER_H

#include "GenFit/ConstField.h"
#include "GenFit/EventDisplay.h"
#include "GenFit/Exception.h"
#include "GenFit/FieldManager.h"
#include "GenFit/KalmanFitStatus.h"
#include "GenFit/GblFitter.h"

#include "TDatabasePDG.h"
#include "TGeoManager.h"
#include "TMath.h"
#include "TRandom.h"
#include "TRandom3.h"
#include "TVector3.h"

#include <vector>
#include <memory>

#include "StFwdTrackMaker/Common.h"

#include "StFwdTrackMaker/include/Tracker/FwdHit.h"
#include "StFwdTrackMaker/include/Tracker/TrackFitter.h"
#include "StFwdTrackMaker/include/Tracker/STARField.h"
#include "StFwdTrackMaker/include/Tracker/FwdGeomUtils.h"

#include "StarGenerator/UTIL/StarRandom.h"
#include "FitterUtils.h"

/* Class for interfacing with GenFit for fitting tracks 
 *
 */
class TrackFitter {

// Accessors and options
  public:
    std::shared_ptr<genfit::Track> getTrack() { return mFitTrack; }

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

        // initialize the main mFitter using a KalmanFitter with reference tracks
        mFitter = std::unique_ptr<genfit::AbsKalmanFitter>(new genfit::KalmanFitterRefTrack());

        // Here we load several options from the config,
        // to customize the mFitter behavior
        mFitter->setMaxFailedHits(mConfig.get<int>("TrackFitter.KalmanFitterRefTrack:MaxFailedHits", -1)); // default -1, no limit
        mFitter->setDebugLvl(mConfig.get<int>("TrackFitter.KalmanFitterRefTrack:DebugLvl", 0)); // default 0, no output
        mFitter->setMaxIterations(mConfig.get<int>("TrackFitter.KalmanFitterRefTrack:MaxIterations", 40)); // default 4 iterations
        mFitter->setMinIterations(mConfig.get<int>("TrackFitter.KalmanFitterRefTrack:MinIterations", 0)); // default 0 iterations

        // Set the fit convergence paramters
        mFitter->setRelChi2Change( mConfig.get<double>("TrackFitter.KalmanFitterRefTrack:RelChi2Change", 1e-3) );
        // mFitter->setAbsChi2Change( mConfig.get<double>("TrackFitter.KalmanFitterRefTrack:AbsChi2Change", 1e-3) );
        mFitter->setDeltaPval( mConfig.get<double>("TrackFitter.KalmanFitterRefTrack:DeltaPval", 1e-3) );
        mFitter->setBlowUpFactor( mConfig.get<double>("TrackFitter.KalmanFitterRefTrack:BlowUpFactor", 1e3) );

        // FwdGeomUtils looks into the loaded geometry and gets detector z locations if present
        FwdGeomUtils fwdGeoUtils( gMan );

        // these default values are the default if the detector is
        // a) not found in the geometry
        // b) not provided in config

        // NOTE: these defaults are needed since the geometry file might not include FST (bug being worked on separately)
        mFSTZLocations = fwdGeoUtils.fstZ(
            mConfig.getVector<double>("TrackFitter.Geometry:fst",
                {140.286011, 154.286011, 168.286011 }
                // 144.633,158.204,171.271
            )
        );

        if ( fwdGeoUtils.fstZ( 0 ) < 1.0 ) { // returns 0.0 on failure
            LOG_WARN << "Using FST z-locations from config or defautl, may not match hits" << endm;
        }

        const double dzInnerFst = 1.715 + 0.04; // cm relative to "center" of disk + residual...
        const double dzOuterFst = 0.240 + 0.04; // cm relative to "center" of disk

        // Now add the Si detector planes at the desired location
        std::stringstream sstr;
        sstr << "Adding FST Planes at: ";
        string delim = "";
        for (auto z : mFSTZLocations) {
            mFSTPlanes.push_back(
                genfit::SharedPlanePtr(
                    // these normals make the planes face along z-axis
                    new genfit::DetPlane(TVector3(0, 0, z), TVector3(1, 0, 0), TVector3(0, 1, 0) )
                )
            );

            // Inner Module FST planes
            mFSTPlanesInner.push_back(
                genfit::SharedPlanePtr(
                    // these normals make the planes face along z-axis
                    new genfit::DetPlane(TVector3(0, 0, z - dzInnerFst), TVector3(1, 0, 0), TVector3(0, 1, 0) )
                )
            );
            mFSTPlanesInner.push_back(
                genfit::SharedPlanePtr(
                    // these normals make the planes face along z-axis
                    new genfit::DetPlane(TVector3(0, 0, z + dzInnerFst), TVector3(1, 0, 0), TVector3(0, 1, 0) )
                )
            );
            // Outer Module FST planes
            mFSTPlanesOuter.push_back(
                genfit::SharedPlanePtr(
                    // these normals make the planes face along z-axis
                    new genfit::DetPlane(TVector3(0, 0, z - dzOuterFst), TVector3(1, 0, 0), TVector3(0, 1, 0) )
                )
            );
            mFSTPlanesOuter.push_back(
                genfit::SharedPlanePtr(
                    // these normals make the planes face along z-axis
                    new genfit::DetPlane(TVector3(0, 0, z + dzOuterFst), TVector3(1, 0, 0), TVector3(0, 1, 0) )
                )
            );

            sstr << delim << z << " (-dzInner=" << z - dzInnerFst << ", +dzInner=" << z+dzInnerFst << ", -dzOuter=" << z - dzOuterFst << ", +dzOuter=" << z + dzOuterFst << ")";
            delim = ", ";
        }
        LOG_DEBUG  << sstr.str() << endm;

        // Now load FTT
        // mConfig.getVector<>(...) requires a default, hence the
        mFTTZLocations = fwdGeoUtils.fttZ(
            mConfig.getVector<double>("TrackFitter.Geometry:ftt", {281.082,304.062,325.058,348.068})
            );

        if ( fwdGeoUtils.fttZ( 0 ) < 1.0 ) { // returns 0.0 on failure
            LOG_WARN << "Using FTT z-locations from config or default, may not match hits" << endm;
        }

        if ( mFTTZLocations.size() != 4 ){
            LOG_ERROR << "Wrong number of FTT layers, got " << mFTTZLocations.size() << " but expected 4" << endm;
        }

        sstr.str("");
        sstr.clear();
        sstr << "Adding FTT Planes at: ";
        delim = "";
        for (auto z : mFTTZLocations) {
            mFTTPlanes.push_back(
                genfit::SharedPlanePtr(
                    // these normals make the planes face along z-axis
                    new genfit::DetPlane(TVector3(0, 0, z), TVector3(1, 0, 0), TVector3(0, 1, 0))
                )
            );
            sstr << delim << z;
            delim = ", ";
        }
        LOG_DEBUG << sstr.str() << endm;
    }

    /**
     * @brief Get the Fst Plane object for a given hit
     *
     * @param h : hit
     * @return genfit::SharedPlanePtr
     */
    genfit::SharedPlanePtr getFstPlane( FwdHit * h ){

        size_t planeId = h->getSector();

        const TVector3 hitXYZ( h->getX(), h->getY(), h->getZ() );

        double phi = hitXYZ.Phi();
        if ( phi < 0 ) phi = TMath::Pi() * 2 + phi;
        const double phi_slice = phi / (TMath::Pi() / 6.0); // 2pi/12
        const int phi_index = ((int)phi_slice);
        const double r  =sqrt( pow(hitXYZ.x(), 2) + pow(hitXYZ.y(), 2) );

        const size_t idx = phi_index % 2;
        auto planeCorr = mFSTPlanesInner[planeId*2 + idx];
        if ( r > 16 ){
            planeCorr = mFSTPlanesOuter[planeId*2 + idx];
        }
        double cdz = (h->getZ() - planeCorr->getO().Z());

        if ( cdz > 0.010 ) {
            LOG_WARN << "FST Z =" << h->getZ() << " vs CORR Plane Z = " << planeCorr->getO().Z() << " DIFF: " << cdz << " phi_slice = " << phi_slice << ", phi_index = " << phi_index << " R=" << hitXYZ.Pt() << " idx=" << idx << endm;
        }

        return planeCorr;
    } // GetFST PLANE

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
     * @brief Get projection to given FST plane
     *
     * @param fstPlane : plane index
     * @param fitTrack : track to project
     * @return genfit::MeasuredStateOnPlane
     */
    genfit::MeasuredStateOnPlane projectToFst(size_t fstPlane, std::shared_ptr<genfit::Track> fitTrack) {
        if (fstPlane > 2) {
            genfit::MeasuredStateOnPlane nil;
            return nil;
        }

        auto detFst = mFSTPlanes[fstPlane];
        // TODO: Why use 1 here?
        genfit::MeasuredStateOnPlane tst = fitTrack->getFittedState(1);
        // NOTE: this returns the track length if needed
        fitTrack->getCardinalRep()->extrapolateToPlane(tst, detFst);

        return tst;
    }

    /**
     * @brief Get projection to given FTT plane
     *
     * @param fttPlane : plane index
     * @param fitTrack : track to project
     * @return genfit::MeasuredStateOnPlane
     */
    genfit::MeasuredStateOnPlane projectToFtt(size_t iFttPlane, std::shared_ptr<genfit::Track> fitTrack) {
        if (iFttPlane > 3) {
            genfit::MeasuredStateOnPlane nil;
            return nil;
        }
        auto fttPlane = mFTTPlanes[iFttPlane];
        // TODO: why use 1 here?
        genfit::MeasuredStateOnPlane tst = fitTrack->getFittedState(1);
        // NOTE: this returns the track length if needed
        fitTrack->getCardinalRep()->extrapolateToPlane(tst, fttPlane);
        return tst;
    }

    /**
     * @brief setup the track from the given seed and optional primary vertex
     * @param trackSeed : seed points
     * @param seedMom : seed momentum
     * @param seedPos : seed position
     * @param Vertex : primary vertex
     */
    bool setupTrack(Seed_t trackSeed ) {
        
        // setup the track fit seed parameters
        GenericFitSeeder gfs;
        int seedQ = 1;
        TVector3 seedPos(0, 0, 0);
        TVector3 seedMom(0, 0, 10); // this default seed actually works better than a half-bad guess
        gfs.makeSeed( trackSeed, seedPos, seedMom, seedQ );
        LOG_DEBUG << "Setting track fit seed position = " << TString::Format( "(%f, %f, %f)", seedPos.X(), seedPos.Y(), seedPos.Z() ) << endm; 
        LOG_DEBUG << "Setting track fit seed momentum = " << TString::Format( "(%f, %f, %f)", seedMom.X(), seedMom.Y(), seedMom.Z() ) << endm;

        LOG_DEBUG << "Setting track fit seed charge = " << seedQ << endm;

        if ( seedQ == 0 ) {
            LOG_ERROR << "Seed charge is zero, skipping track -> usually means collinear points" << endm;
            return false;
        }

        // create the track representations
        // Note that multiple track reps differing only by charge results in a silent failure of GenFit
        auto theTrackRep = new genfit::RKTrackRep(mPdgMuon * -1 * seedQ); // bc pos PDG codes are for neg particles

        // Create the track
        mFitTrack = std::make_shared<genfit::Track>(theTrackRep, seedPos, seedMom);
        // now add the points to the track

        int hitId(0);       // hit ID
        size_t planeId(0);     // detector plane ID

        // initialize the hit coords on plane
        TVectorD hitCoords(2);
        hitCoords[0] = 0;
        hitCoords[1] = 0;

        /******************************************************************************************************************
		 * loop over the hits, add them to the track
		 ******************************************************************************************************************/
        // use these to enforce our sorting parameters
        size_t idxFst = 0; // index of the FST hit
        size_t idxFtt = 0; // index of the FTT hit
        for (auto h : trackSeed) {
            auto fh = dynamic_cast<FwdHit*>(h);
            hitCoords[0] = h->getX();
            hitCoords[1] = h->getY();

            /******************************************************************************************************************
            * If the Primary vertex is included
            ******************************************************************************************************************/
            if ( true ) {
                LOG_DEBUG << "Treating hit as a spacepoint" << endm;
                if ( fh->isPV() ){
                    LOG_DEBUG << "Including primary vertex in fit" << endm;
                }
                TVectorD pv(3);
                pv[0] = h->getX();
                pv[1] = h->getY();
                pv[2] = h->getZ();
                LOG_INFO << "x = " << pv[0] << "+/- " << fh->_covmat(0,0) << ", y = " << pv[1] << " +/- " << fh->_covmat(1,1) << ", z = " << pv[2] << " +/- " << fh->_covmat(2,2) << endm;
                auto tp = new genfit::TrackPoint();
                genfit::SpacepointMeasurement *measurement = new genfit::SpacepointMeasurement(pv, fh->_covmat, fh->_detid, ++hitId, tp);
                tp->addRawMeasurement(measurement);
                tp->setTrack(mFitTrack.get());
                if ( fh->isPV() ){
                    tp->setSortingParameter(0);
                }
                if ( fh->isFtt() ){
                    tp->setSortingParameter(4 + idxFtt);
                    idxFtt++;
                }
                if ( fh->isFst() ){
                    tp->setSortingParameter(1 + idxFst);
                    idxFst++;
                }

                mFitTrack->insertPoint( tp );
                continue;
            }

            genfit::PlanarMeasurement *measurement = new genfit::PlanarMeasurement(hitCoords, CovMatPlane(h), fh->_detid, ++hitId, nullptr);

            planeId = h->getSector();
            genfit::SharedPlanePtr plane;
            if ( fh->isFtt() ){
                planeId = fh->_vid - 9;
            }
            LOG_INFO << "planeId = " << planeId << ", sector " << h->getSector() << ", vid = " << fh->_vid << endm;
            if (fh->isFtt() && mFTTPlanes.size() <= planeId) {
                LOG_ERROR << "invalid VolumId -> out of bounds DetPlane, vid = " << dynamic_cast<FwdHit*>(h)->_vid << " vs. planeId = " << planeId << endm;
                delete measurement;
                continue;
            }

            if (fh->isFtt())
                plane = mFTTPlanes[planeId];
            else if (fh->isFst())
                plane = getFstPlane( fh );

            measurement->setPlane(plane, planeId);
            
            mFitTrack->insertPoint(new genfit::TrackPoint(measurement, mFitTrack.get()));
            LOG_INFO << "\tsetupTrack: Hit at Z = " << h->getZ() << " with plane at Z = " << plane->getO().Z() << endm;

            if (abs(h->getZ() - plane->getO().Z()) > 0.05) {
                LOG_WARN << "Z Mismatch h->z = " << h->getZ() << ", plane->z = "<< plane->getO().Z() <<", diff = " << abs(h->getZ() - plane->getO().Z()) << endm;
            }
        } // loop on trackSeed
        return true;
    } // setupTrack

    /** @brief performs the fit on a track
     *  @param t : track to fit
    */
    void performFit( std::shared_ptr<genfit::Track> t ){
        /******************************************************************************************************************
		 * Do the fit
		 ******************************************************************************************************************/
        try {

            // prepare the track for fitting
            // int nFailedPoints = 0;
            // bool changed = false;
            // changed = dynamic_cast<genfit::KalmanFitterRefTrack*>( mFitter.get() )->prepareTrack( mFitTrack.get(), mFitTrack->getCardinalRep(), false, nFailedPoints);
            // LOG_DEBUG << "Track prepared for fit with " << nFailedPoints << " failed points, changed? = " << changed << endm;

            // check the track for consistency
            mFitTrack->checkConsistency();
            // do the fit
            mFitter->processTrack(t.get());

            // check the track for consistency
            t->checkConsistency();

            // find track rep with smallest chi2
            t->determineCardinalRep();
            // update the seed
            // t->udpateSeed();

            auto status = t->getFitStatus();
            LOG_INFO << "Fit status: " << status->isFitConverged() << endm;
            LOG_INFO << "-Fit pvalue: " << status->getPVal() << endm;
            LOG_INFO << "-Fit Chi2: " << status->getChi2() << endm;

            if ( status->isFitConverged() ){
             
                auto cr = t->getCardinalRep();
                auto p = cr->getMom( t->getFittedState( 0, cr ));
                int rcQ = status->getCharge();  
                LOG_INFO << "Fit momentum: " << p.X() << ", " << p.Y() << ", " << p.Z() << endm;
                LOG_INFO << "\tFit Pt: " << p.Pt() << ", eta: " << p.Eta() << ", phi: " << p.Phi() << endm;
                // LOG_INFO << "\tMc  Pt: " << mcMom.Pt() << ", eta: " << mcMom.Eta() << ", phi: " << mcMom.Phi() << endm;
            }


        } catch (genfit::Exception &e) {
            LOG_ERROR << "Exception on fit update" << e.what() << endm;
        }
        LOG_INFO << "Track fit update complete!" << endm;
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
        bool valid = setupTrack(trackSeed);
        if ( !valid ){
            LOG_ERROR << "Failed to setup track for fit" << endm;
            return -1;
        }
        LOG_DEBUG << "Ready to fit with " << mFitTrack->getNumPoints() << " track points" << endm;

        /******************************************************************************************************************
		 * Do the fit
		 ******************************************************************************************************************/
        performFit( mFitTrack );
        long long duration = (FwdTrackerUtils::nowNanoSecond() - itStart) * 1e-6; // milliseconds
        return duration;
    } // fitTrack

    // Store the planes for FTT and FST
    vector<genfit::SharedPlanePtr> mFTTPlanes;
    vector<genfit::SharedPlanePtr> mFSTPlanes;
    vector<genfit::SharedPlanePtr> mFSTPlanesInner;
    vector<genfit::SharedPlanePtr> mFSTPlanesOuter;

  protected:
    std::unique_ptr<genfit::AbsBField> mBField;

    FwdTrackerConfig mConfig; // main config object
    TString mGeoCache;

    // Main GenFit fitter instance
    std::unique_ptr<genfit::AbsKalmanFitter> mFitter = nullptr;

    // PDG codes for the default plc type for fits
    const int mPdgPiPlus = 211;
    const int mPdgPiMinus = -211;
    const int mPdgPositron = 11;
    const int mPdgElectron = -11;
    const int mPdgMuon = 13;
    const int mPdgAntiMuon = -13;


    // det z locations loaded from geom or config
    vector<double> mFSTZLocations, mFTTZLocations;

    // GenFit state - resused
    std::shared_ptr<genfit::Track> mFitTrack;
};

#endif
