#ifndef TRACK_FITTER_H
#define TRACK_FITTER_H

#include "GenFit/ConstField.h"
#include "GenFit/EventDisplay.h"
#include "GenFit/Exception.h"
#include "GenFit/FieldManager.h"
#include "GenFit/KalmanFitStatus.h"
#include "GenFit/GblFitter.h"

#include "GenFit/KalmanFitter.h"
#include "GenFit/KalmanFitterInfo.h"
#include "GenFit/KalmanFitterRefTrack.h"
#include "GenFit/MaterialEffects.h"
#include "GenFit/PlanarMeasurement.h"
#include "GenFit/RKTrackRep.h"
#include "GenFit/SpacepointMeasurement.h"
#include "GenFit/StateOnPlane.h"
#include "GenFit/TGeoMaterialInterface.h"
#include "GenFit/Track.h"
#include "GenFit/TrackPoint.h"

#include "Criteria/SimpleCircle.h"

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

/* Cass for fitting tracks(space points) with GenFit
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
            mBField = std::unique_ptr<genfit::AbsBField>(new genfit::ConstField(0., 0., 5.)); // 0.5 T Bz
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
        mFitter->setMinIterations(mConfig.get<int>("TrackFitter.KalmanFitterRefTrack:MinIterations", 3)); // default 0 iterations

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

        // get default vertex values used in simulation from the config
        mVertexSigmaXY = mConfig.get<double>("TrackFitter.Vertex:sigmaXY", 1.0);
        mVertexSigmaZ = mConfig.get<double>("TrackFitter.Vertex:sigmaZ", 30.0);
        mIncludeVertexInFit = mConfig.get<bool>("TrackFitter.Vertex:includeInFit", false);
    }

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
     * @brief Fit points to a simple circle
     *
     * @param trackSeed : seed points to fit
     * @param i0 : index of the first hit
     * @param i1 : index of the second hit
     * @param i2 : index of the third hit
     * @return float : curvature
     */
    float fitSimpleCircle(Seed_t trackSeed, size_t i0, size_t i1, size_t i2) {
        float curv = 0;

        // ensure that no index is outside of range for FST or FTT volumes
        if (i0 > 12 || i1 > 12 || i2 > 12)
            return 0;

        try {
            KiTrack::SimpleCircle sc(trackSeed[i0]->getX(), trackSeed[i0]->getY(), trackSeed[i1]->getX(), trackSeed[i1]->getY(), trackSeed[i2]->getX(), trackSeed[i2]->getY());
            curv = sc.getRadius();
        } catch (KiTrack::InvalidParameter &e) {
            // if we got here we failed to get  a valid seed. We will still try to move forward but the fit will probably fail
            LOG_WARN << "Circle fit failed, FWD track fit will likely fail also" << endm;
        }

        //  make sure the curv is valid
        if (isinf(curv)){
            curv = 0.0;
        }

        return curv;
    } // fitSimpleCircle

    /**
     * @brief Determines the seed state to start the fit
     *
     * @param trackSeed : seed points
     * @param seedPos : position seed (return)
     * @param seedMom : momenum seed (return)
     * @return float : curvature
     */
    float seedState(Seed_t trackSeed, TVector3 &seedPos, TVector3 &seedMom) {
        // we require at least 3 hits, so this should be gauranteed
        LOG_DEBUG << "Seed state with " << trackSeed.size() << " seed points " << endm;
        if(trackSeed.size() < 3){
            // failure
            return 0.0;
        }

        // we want to use the LAST 3 hits, since silicon doesnt have R information
        TVector3 p0, p1, p2;
        // use the closest hit to the interaction point for the seed pos
        FwdHit *hit_closest_to_IP = static_cast<FwdHit *>(trackSeed[0]);

        // maps from <key=vol_id> to <value=index in trackSeed>
        std::map<size_t, int> vol_map;

        // init the map
        for (size_t i = 0; i < 13; i++)
            vol_map[i] = -1;

        vector<size_t> idx;
        for (size_t i = 0; i < trackSeed.size(); i++) {
            auto fwdHit = static_cast<FwdHit *>(trackSeed[i]);
            if ( !fwdHit ) continue;
            if (vol_map[ abs(fwdHit->_vid) ] == -1)
                idx.push_back(fwdHit->_vid);
            vol_map[abs(fwdHit->_vid)] = (int)i;

            // find the hit closest to IP for the initial position seed
            if (hit_closest_to_IP == nullptr || hit_closest_to_IP->getZ() > fwdHit->getZ())
                hit_closest_to_IP = fwdHit;
        }

        // now get an estimate of the pT from several overlapping simple circle fits
        // enumerate the available partitions (example for FTT)
        // 12 11 10
        // 12 11 9
        // 12 10 9
        // 11 10 9
        vector<float> curvs;

        if (idx.size() < 3){
            return 0.0;
        }

        if ( idx.size() == 3 ){
            curvs.push_back(fitSimpleCircle(trackSeed, vol_map[idx[0]], vol_map[idx[1]], vol_map[idx[2]]));
        } else if ( idx.size() >= 4 ){
            curvs.push_back(fitSimpleCircle(trackSeed, vol_map[idx[0]], vol_map[idx[1]], vol_map[idx[2]]));
            curvs.push_back(fitSimpleCircle(trackSeed, vol_map[idx[0]], vol_map[idx[1]], vol_map[idx[3]]));
            curvs.push_back(fitSimpleCircle(trackSeed, vol_map[idx[0]], vol_map[idx[2]], vol_map[idx[3]]));
            curvs.push_back(fitSimpleCircle(trackSeed, vol_map[idx[1]], vol_map[idx[2]], vol_map[idx[3]]));
        }

        // average them and exclude failed fits
        float mcurv = 0;
        float nmeas = 0;

        for (size_t i = 0; i < curvs.size(); i++) {
            if (curvs[i] > 10) {
                mcurv += curvs[i];
                nmeas += 1.0;
            }
        }

        if (nmeas >= 1)
            mcurv = mcurv / nmeas;
        else
            mcurv = 10000;

        // Now lets get eta information
        p0.SetXYZ(trackSeed[vol_map[idx[0]]]->getX(), trackSeed[vol_map[idx[0]]]->getY(), trackSeed[vol_map[idx[0]]]->getZ());
        p1.SetXYZ(trackSeed[vol_map[idx[1]]]->getX(), trackSeed[vol_map[idx[1]]]->getY(), trackSeed[vol_map[idx[1]]]->getZ());
        if ( abs(p0.X() - p1.X()) < 1e-6 ){
            p1.SetXYZ(trackSeed[vol_map[idx[2]]]->getX(), trackSeed[vol_map[idx[2]]]->getY(), trackSeed[vol_map[idx[2]]]->getZ());
        }

        LOG_DEBUG << TString::Format( "Fwd SeedState: p0 (%f, %f, %f), p1 (%f, %f, %f)", p0.X(), p0.Y(), p0.Z(), p1.X(), p1.Y(), p1.Z() ) << endm;

        const double K = 0.00029979; //K depends on the units used for Bfield
        double pt = mcurv * K * 5; // pT from average measured curv
        double dx = (p1.X() - p0.X());
        double dy = (p1.Y() - p0.Y());
        double dz = (p1.Z() - p0.Z());
        double phi = TMath::ATan2(dy, dx);
        double Rxy = sqrt(dx * dx + dy * dy);
        double theta = TMath::ATan2(Rxy, dz);
        if (abs(dx) < 1e-6 || abs(dy) < 1e-6){
            phi = TMath::ATan2( p1.Y(), p1.X() );
        }
        Rxy = sqrt( p0.X()*p0.X() + p0.Y()*p0.Y() );
        theta = TMath::ATan2(Rxy, p0.Z());

        LOG_DEBUG << TString::Format( "pt=%f, dx=%f, dy=%f, dz=%f, phi=%f, theta=%f", pt, dx, dy, dz, phi, theta ) << endm;
        // double eta = -log( tantheta / 2.0 );
        // these starting conditions can probably be improvd, good study for students

        seedMom.SetPtThetaPhi(pt, theta, phi);
        seedPos.SetXYZ(hit_closest_to_IP->getX(), hit_closest_to_IP->getY(), hit_closest_to_IP->getZ());

        return mcurv;
    }//seedState

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
     * @brief Get the Fst Plane object for a given hit
     *
     * @param h : hit
     * @return genfit::SharedPlanePtr
     */
    genfit::SharedPlanePtr getFstPlane( FwdHit * h ){

        size_t planeId = h->getSector();

        static const TVector3 hitXYZ( h->getX(), h->getY(), h->getZ() );

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
     * @brief Refit a track with additional FST hits
     *
     * Takes a previously fit track re-fits it with the newly added silicon hits
     * @param originalTrack : original fit track
     * @param fstHits : new FST hits to add
     * @return TVector3 : momentum
     */
    TVector3 refitTrackWithFstHits(genfit::Track *originalTrack, Seed_t fstHits) {
        static const TVector3 pOrig = originalTrack->getCardinalRep()->getMom(originalTrack->getFittedState(1, originalTrack->getCardinalRep()));

        if (originalTrack->getFitStatus(originalTrack->getCardinalRep())->isFitConverged() == false) {
            // in this case the original track did not converge so we should not refit.
            // probably never get here due to previous checks
            return pOrig;
        }

        // Setup the Track Reps
        auto trackRepPos = new genfit::RKTrackRep(mPdgPositron);
        auto trackRepNeg = new genfit::RKTrackRep(mPdgElectron);

        // get the space points on the original track
        auto trackPoints = originalTrack->getPointsWithMeasurement();

        if ((trackPoints.size() < (mFTTZLocations.size() +1) && mIncludeVertexInFit) || trackPoints.size() < mFTTZLocations.size() ) {
            // we didnt get enough points for a refit
            return pOrig;
        }

        TVectorD rawCoords = trackPoints[0]->getRawMeasurement()->getRawHitCoords();
        double z = mFSTZLocations[0]; //first FTT plane, used if we dont have PV in fit
        if (mIncludeVertexInFit)
            z = rawCoords(2);

        TVector3 seedPos(rawCoords(0), rawCoords(1), z);
        TVector3 seedMom = pOrig;

        // Create the ref track using the seed state
        auto mFitTrack = new genfit::Track(trackRepPos, seedPos, seedMom);
        mFitTrack->addTrackRep(trackRepNeg);

        size_t firstFTTIndex = 0;
        if (mIncludeVertexInFit) {
            // clone the PRIMARY VERTEX into this track
            mFitTrack->insertPoint(new genfit::TrackPoint(trackPoints[0]->getRawMeasurement(), mFitTrack));
            firstFTTIndex = 1; // start on hit index 1 below
        }

        // initialize the hit coords on plane
        TVectorD hitCoords(2);
        hitCoords[0] = 0;
        hitCoords[1] = 0;

        size_t planeId(0);
        int hitId(5);

        // add the hits to the track
        for (auto h : fstHits) {
            if ( nullptr == h ) continue; // if no Si hit in this plane, skip

            hitCoords[0] = h->getX();
            hitCoords[1] = h->getY();
            genfit::PlanarMeasurement *measurement = new genfit::PlanarMeasurement(hitCoords, CovMatPlane(h), h->getSector(), ++hitId, nullptr);

            planeId = h->getSector();

            if (mFSTPlanes.size() <= planeId) {
                LOG_WARN << "invalid VolumId -> out of bounds DetPlane, vid = " << planeId << endm;
                return pOrig;
            }

            auto plane = getFstPlane( static_cast<FwdHit*>(h) );

            measurement->setPlane(plane, planeId);
            mFitTrack->insertPoint(new genfit::TrackPoint(measurement, mFitTrack));


            static const TVector3 hitXYZ( h->getX(), h->getY(), h->getZ() );
            float phi = hitXYZ.Phi();
            if ( phi < 0 ) phi = TMath::Pi() * 2 + phi;
            double phi_slice = phi / (TMath::Pi() / 6.0); // 2pi/12
            int phi_index = ((int)phi_slice);
            double dz = (h->getZ() - plane->getO().Z());

            double r  =sqrt( pow(hitXYZ.x(), 2) + pow(hitXYZ.y(), 2) );

            size_t idx = phi_index % 2;
            auto planeCorr = mFSTPlanesInner[planeId + idx];
            if ( r > 16 ){
                planeCorr = mFSTPlanesOuter[planeId + idx];
            }
            double cdz = (h->getZ() - planeCorr->getO().Z());
        } // for fstHits
        // start at 0 if PV not included, 1 otherwise
        for (size_t i = firstFTTIndex; i < trackPoints.size(); i++) {
            // clone the track points into this track
            mFitTrack->insertPoint(new genfit::TrackPoint(trackPoints[i]->getRawMeasurement(), mFitTrack));
        }

        try {
            //Track RE-Fit with GENFIT2
            // check consistency of all points
            mFitTrack->checkConsistency();

            // do the actual track fit
            mFitter->processTrack(mFitTrack);

            mFitTrack->checkConsistency();

            // this chooses the lowest chi2 fit result as cardinal
            mFitTrack->determineCardinalRep();

        } catch (genfit::Exception &e) {
            // will be caught below by converge check
            LOG_WARN << "Track fit exception : " << e.what() << endm;
        }

        if (mFitTrack->getFitStatus(mFitTrack->getCardinalRep())->isFitConverged() == false) {
            // Did not converge
            return pOrig;
        } else { // we did converge, return new momentum

            try {
                // causes seg fault
                // auto cardinalRep = mFitTrack->getCardinalRep();
                // auto cardinalStatus = mFitTrack->getFitStatus(cardinalRep);
                // mFitStatus = *cardinalStatus; // save the status of last fit
            } catch (genfit::Exception &e) {
            }

            static const TVector3 p = mFitTrack->getCardinalRep()->getMom(mFitTrack->getFittedState(1, mFitTrack->getCardinalRep()));
            return p;
        }
        return pOrig;
    } // refit with Si hits

    void fitSpacePoints( Seed_t trackSeed, double *Vertex = 0, TVector3 *seedMomentum = 0 ){

        auto seedPos = TVector3(0, 0, 0);
        auto seedMom = TVector3(0, 0, 10);
        auto theTrackRep = new genfit::RKTrackRep(mPdgMuon);
        // setup track for fit with positive and negative reps
        // mFitTrack = new genfit::Track(trackRepPos, seedPos, seedMom);
        mFitTrack = std::make_shared<genfit::Track>(theTrackRep, seedPos, seedMom);

        // Create Spaces points from each seed
        vector<genfit::SpacepointMeasurement*> spoints;
        for (size_t i = 0; i < trackSeed.size(); i++) {
            auto seed = trackSeed[i];
            TMatrixDSym cm(3);
            cm(0, 0) = 0.01;
            cm(1, 1) = 0.01;
            cm(2, 2) = 0.01;
            auto rhc = TVectorD( 3 );
            rhc[0] = seed->getX();
            rhc[1] = seed->getY();
            rhc[2] = seed->getZ();
            auto spoint = new genfit::SpacepointMeasurement(rhc, cm, 0, i, nullptr);
            spoints.push_back(spoint);
        }

        LOG_INFO << "Fitting track with " << spoints.size() << " space points" << endm;
        // try adding the points to track and fitting
        try {
            for ( size_t i = 0; i < spoints.size(); i++ ){
                mFitTrack->insertPoint(new genfit::TrackPoint(spoints[i], mFitTrack.get()));
            }

            mFitter->processTrack(mFitTrack.get());
            mFitTrack->determineCardinalRep();

        } catch (genfit::Exception &e) {
            LOG_ERROR << "GenFit failed to fit track with: " << e.what() << endm;
        }

        try {
            mFitTrack->checkConsistency();

            mFitTrack->determineCardinalRep();
            auto cardinalRep = mFitTrack->getCardinalRep();
            // static const TVector3 p = cardinalRep->getMom(mFitTrack->getFittedState(1, cardinalRep));
            // sucess, return momentum
            LOG_INFO << "Fit status: " << mFitTrack->getFitStatus(cardinalRep)->isFitConverged() << endm;
            return;
        } catch (genfit::Exception &e) {
            LOG_ERROR << "GenFit failed to fit track consistency with: " << e.what() << endm;
        }
        return ;
    }

    /**
     * @brief Generic method for fitting space points with GenFit
     *
     * @param spoints : spacepoints
     * @param seedPos : seed position
     * @param seedMom : seed momentum
     * @return TVector3 : momentum from fit
     */
    TVector3 fitSpacePoints( vector<genfit::SpacepointMeasurement*> spoints, TVector3 &seedPos, TVector3 &seedMom ){

        // setup track reps
        // auto trackRepPos = new genfit::RKTrackRep(mPdgPositron);
        // auto trackRepNeg = new genfit::RKTrackRep(mPdgElectron);


        // return TVector3(0, 0, 0);
        auto theTrackRep = new genfit::RKTrackRep(mPdgMuon);
        // setup track for fit with positive and negative reps
        // mFitTrack = new genfit::Track(trackRepPos, seedPos, seedMom);
        mFitTrack = std::make_shared<genfit::Track>(theTrackRep, seedPos, seedMom);
        // mFitTrack->addTrackRep(trackRepNeg);
        //

        LOG_INFO << "Fitting track with " << spoints.size() << " space points" << endm;
        // try adding the points to track and fitting
        try {
            for ( size_t i = 0; i < spoints.size(); i++ ){
                mFitTrack->insertPoint(new genfit::TrackPoint(spoints[i], mFitTrack.get()));
            }
            // do the fit against the two possible fits
            // mFitter->processTrackWithRep(mFitTrack, trackRepPos);
            // mFitter->processTrackWithRep(mFitTrack, trackRepNeg);
            mFitter->processTrack(mFitTrack.get());
            mFitTrack->determineCardinalRep();

        } catch (genfit::Exception &e) {
            LOG_ERROR << "GenFit failed to fit track with: " << e.what() << endm;
        }

        try {
            mFitTrack->checkConsistency();

            mFitTrack->determineCardinalRep();
            auto cardinalRep = mFitTrack->getCardinalRep();
            static const TVector3 p = cardinalRep->getMom(mFitTrack->getFittedState(1, cardinalRep));
            // sucess, return momentum
            return p;
        } catch (genfit::Exception &e) {
            LOG_ERROR << "GenFit failed to fit track with: " << e.what() << endm;
        }
        return TVector3(0, 0, 0);
    }


    /**
     * @brief setup the track from the given seed and optional primary vertex
     * @param trackSeed : seed points
     * @param seedMom : seed momentum
     * @param seedPos : seed position
     * @param Vertex : primary vertex
     */
    void setupTrack(Seed_t trackSeed, TVector3 seedMom, TVector3 seedPos, double *Vertex = 0) {
        // If we use the PV, use that as the start pos for the track
        TVectorD pv(3);
        if (Vertex != 0) {
            seedPos.SetXYZ(Vertex[0], Vertex[1], Vertex[2]);
            pv[0] = Vertex[0];
            pv[1] = Vertex[1];
            pv[2] = Vertex[2];
        }

        // create the track representations
        // Note that multiple track reps differing only by charge results in a silent failure of GenFit
        auto theTrackRep = new genfit::RKTrackRep(mPdgMuon);

        // Create the track
        mFitTrack = std::make_shared<genfit::Track>(theTrackRep, seedPos, seedMom);
        // now add the points to the track

        int hitId(0);       // hit ID
        /******************************************************************************************************************
        * Include the Primary vertex if desired
        ******************************************************************************************************************/
        if (mIncludeVertexInFit) {
            LOG_DEBUG << "Including vertex in fit" << endm;
            TMatrixDSym hitCov3(3);
            hitCov3(0, 0) = mVertexSigmaXY * mVertexSigmaXY;
            hitCov3(1, 1) = mVertexSigmaXY * mVertexSigmaXY;
            hitCov3(2, 2) = mVertexSigmaZ * mVertexSigmaZ;

            genfit::SpacepointMeasurement *measurement = new genfit::SpacepointMeasurement(pv, hitCov3, 0, ++hitId, nullptr);
            mFitTrack->insertPoint(new genfit::TrackPoint(measurement, mFitTrack.get()));
        }

        size_t planeId(0);     // detector plane ID

        // initialize the hit coords on plane
        TVectorD hitCoords(2);
        hitCoords[0] = 0;
        hitCoords[1] = 0;

        /******************************************************************************************************************
		 * loop over the hits, add them to the track
		 ******************************************************************************************************************/
        for (auto h : trackSeed) {
            auto fh = dynamic_cast<FwdHit*>(h);
            hitCoords[0] = h->getX();
            hitCoords[1] = h->getY();

            genfit::PlanarMeasurement *measurement = new genfit::PlanarMeasurement(hitCoords, CovMatPlane(h), h->getSector(), ++hitId, nullptr);

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
            else
                plane = getFstPlane( fh );

            measurement->setPlane(plane, planeId);
            mFitTrack->insertPoint(new genfit::TrackPoint(measurement, mFitTrack.get()));

            if (abs(h->getZ() - plane->getO().Z()) > 0.05) {
                LOG_WARN << "Z Mismatch h->z = " << h->getZ() << ", plane->z = "<< plane->getO().Z() <<", diff = " << abs(h->getZ() - plane->getO().Z()) << endm;
            }
        } // loop on trackSeed
    } // setupTrack

    /** @brief performs the fit on a track
     *  @param t : track to fit
    */
    void performFit( std::shared_ptr<genfit::Track> t ){
        /******************************************************************************************************************
		 * Do the fit
		 ******************************************************************************************************************/
        try {
            // do the fit
            mFitter->processTrack(t.get());
            // find track rep with smallest chi2
            t->determineCardinalRep();
            // update the seed
            t->udpateSeed();
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
    long long fitTrack(Seed_t trackSeed, double *Vertex = 0, TVector3 *seedMomentum = 0) {
        long long itStart = FwdTrackerUtils::nowNanoSecond();
        LOG_DEBUG << "Fitting track with " << trackSeed.size() << " seed points" << endm;
        
        // get the seed info from our hits
        static TVector3 seedMom, seedPos;
        LOG_DEBUG << "Getting seed state" << endm;
        // returns track curvature if needed
        seedState(trackSeed, seedPos, seedMom);

        if (seedMomentum != nullptr) {
            seedMom = *seedMomentum;
        }

        setupTrack(trackSeed, seedMom, seedPos, Vertex);
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

    void SetIncludeVertex( bool vert ) { mIncludeVertexInFit = vert; }

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

    // parameter ALIASED from mConfig wrt PV vertex
    double mVertexSigmaXY = 1;
    double mVertexSigmaZ = 30;
    vector<double> mVertexPos;
    bool mIncludeVertexInFit = false;

    // GenFit state - resused
    std::shared_ptr<genfit::Track> mFitTrack;
};

#endif
