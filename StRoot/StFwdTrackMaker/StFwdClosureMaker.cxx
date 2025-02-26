#include "StFwdTrackMaker/StFwdClosureMaker.h"
#include "St_base/StMessMgr.h"
#include "StBFChain/StBFChain.h"
#include "StFwdTrackMaker/StFwdTrackMaker.h"

#include "StFwdTrackMaker/include/Tracker/FwdTracker.h"
#include "StFwdTrackMaker/include/Tracker/ObjExporter.h"
// StEvent includes
#include "StEvent/StBTofCollection.h"
#include "StEvent/StBTofHeader.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFttCluster.h"
#include "StEvent/StFttCollection.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFcsCollection.h"
#include "StFcsDbMaker/StFcsDb.h"
#include "StRoot/StEpdUtil/StEpdGeom.h"
#include "StEvent/StFwdTrackCollection.h"
#include "StEvent/StFwdTrack.h"
#include "TPad.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuFstCollection.h"
#include "StMuDSTMaker/COMMON/StMuFstHit.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuFwdTrack.h"
#include "StMuDSTMaker/COMMON/StMuFwdTrackCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsCollection.h"
#include "StMuDSTMaker/COMMON/StMuFcsCluster.h"
#include "StMuDSTMaker/COMMON/StMuFcsHit.h"
#include "StMuDSTMaker/COMMON/StMuFttCluster.h"
#include "StMuDSTMaker/COMMON/StMuFttPoint.h"
#include "StMuDSTMaker/COMMON/StMuMcTrack.h"
#include "StMuDSTMaker/COMMON/StMuFstHit.h"

#include "tables/St_g2t_fts_hit_Table.h"
#include "tables/St_g2t_track_Table.h"
#include "tables/St_g2t_vertex_Table.h"
#include "tables/St_g2t_event_Table.h"

/* StFwdClosureMaker.cxx 
 * This maker is a simple example of how to use the genfit package to fit tracks in the FWD
 * It is a simple maker that uses the GEANT information to fit tracks in the FWD detectors
 * The goal is to provide closure tests tracking 
 */

TVector3 StFwdClosureMaker::raster(TVector3 p0) {
    
    TVector3 p = p0;
    double r = p.Perp();
    double phi = p.Phi();
    const double minR = 5.0;
    // 5.0 is the r minimum of the Si
    p.SetPerp(minR + (std::floor((r - minR) / mRasterR) * mRasterR + mRasterR / 2.0));
    p.SetPhi(-TMath::Pi() + (std::floor((phi + TMath::Pi()) / mRasterPhi) * mRasterPhi + mRasterPhi / 2.0));
    return p;
}

TMatrixDSym StFwdClosureMaker::makeSiCovMat(TVector3 hit) {
    // we can calculate the CovMat since we know the det info, but in future we should probably keep this info in the hit itself
    // measurements on a plane only need 2x2
    // for Si geom we need to convert from cylindrical to cartesian coords
    TMatrixDSym cm(2);
    TMatrixD T(2, 2);
    TMatrixD J(2, 2);
    const float x = hit.X();
    const float y = hit.Y();
    const float R = sqrt(x * x + y * y);
    const float cosphi = x / R;
    const float sinphi = y / R;
    const float sqrt12 = sqrt(12.);

    const float dr = mRasterR / sqrt12;
    const float dphi = (mRasterPhi) / sqrt12;

    // Setup the Transposed and normal Jacobian transform matrix;
    // note, the si fast sim did this wrong
    // row col
    T(0, 0) = cosphi;
    T(0, 1) = -R * sinphi;
    T(1, 0) = sinphi;
    T(1, 1) = R * cosphi;

    J(0, 0) = cosphi;
    J(0, 1) = sinphi;
    J(1, 0) = -R * sinphi;
    J(1, 1) = R * cosphi;

    TMatrixD cmcyl(2, 2);
    cmcyl(0, 0) = dr * dr;
    cmcyl(1, 1) = dphi * dphi;

    TMatrixD r = T * cmcyl * J;

    // note: float sigmaX = sqrt(r(0, 0));
    // note: float sigmaY = sqrt(r(1, 1));

    cm(0, 0) = r(0, 0);
    cm(1, 1) = r(1, 1);
    cm(0, 1) = r(0, 1);
    cm(1, 0) = r(1, 0);

    TMatrixDSym tamvoc(3);
    tamvoc( 0, 0 ) = cm(0, 0); tamvoc( 0, 1 ) = cm(0, 1); tamvoc( 0, 2 ) = 0.0;
    tamvoc( 1, 0 ) = cm(1, 0); tamvoc( 1, 1 ) = cm(1, 1); tamvoc( 1, 2 ) = 0.0;
    tamvoc( 2, 0 ) = 0.0;      tamvoc( 2, 1 ) = 0.0; tamvoc( 2, 2 )      = 0.01*0.01;


    return tamvoc;
}

StFwdClosureMaker::StFwdClosureMaker() : StMaker("fwdClosureMaker") {}

TGeoManager * gMan = nullptr;
std::unique_ptr<genfit::AbsKalmanFitter> mFitter = nullptr;
std::unique_ptr<genfit::AbsBField> mBField;
std::map< string, TH1* > mHistograms;

TH1 * addHistogram1D( string name, string title, int nbins, double min, double max ){
    TH1 * h = new TH1F( name.c_str(), title.c_str(), nbins, min, max );
    mHistograms[name] = h;
    return h;
}

TH2 * addHistogram2D( string name, string title, int nbinsX, double minX, double maxX, int nbinsY, double minY, double maxY ){
    TH2 * h = new TH2F( name.c_str(), title.c_str(), nbinsX, minX, maxX, nbinsY, minY, maxY );
    mHistograms[name] = h;
    return h;
}

TH1 * getHistogram1D( string name ){
    if ( mHistograms.count(name) )
        return mHistograms[name];
    assert( false && "Histogram not found" );
    return nullptr;
}
TH2 * getHistogram2D( string name ){
    return dynamic_cast<TH2*>( getHistogram1D(name) );
}

struct Point {
    double x, y;
};

// Function to calculate the determinant of a 2x2 matrix
double determinant(double a, double b, double c, double d) {
    return a * d - b * c;
}

// Function to compute the curvature of a circle given 3 points
double computeCurvature(const Point& p1, const Point& p2, const Point& p3) {
    // Calculate the lengths of the sides of the triangle
    double A = std::sqrt(std::pow(p2.x - p1.x, 2) + std::pow(p2.y - p1.y, 2));
    double B = std::sqrt(std::pow(p3.x - p2.x, 2) + std::pow(p3.y - p2.y, 2));
    double C = std::sqrt(std::pow(p1.x - p3.x, 2) + std::pow(p1.y - p3.y, 2));

    // Calculate the determinant of the matrix formed by the points
    double det = determinant(p2.x - p1.x, p2.y - p1.y, p3.x - p1.x, p3.y - p1.y);
    // LOG_INFO << "Det: " << det << endm;
    double charge = det > 0 ? -1 : 1;
    // Area of the triangle formed by the three points
    double area = std::abs(det) / 2.0;

    if (area == 0) {
        std::cerr << "The points are collinear, curvature is undefined." << std::endl;
        return -1; // Curvature is undefined for collinear points
    }

    // Calculate the radius of the circumcircle using the formula:
    // R = (A * B * C) / (4 * area)
    double radius = (A * B * C) / (4 * area);
    // LOG_INFO << "Radius: " << radius << endm;
    // Curvature is the inverse of the radius
    return charge / radius;
}

int StFwdClosureMaker::Init() {

    /****************************************************** 
     * Setup GenFit
    */
    // Setup the Geometry used by GENFIT
    TGeoManager::Import("fGeom.root");
    gMan = gGeoManager;
    // Set up the material interface and set material effects on/off from the config
    genfit::MaterialEffects::getInstance()->init(new genfit::TGeoMaterialInterface());
    genfit::MaterialEffects::getInstance()->setNoEffects( true );
    // Setup the BField to use
    // mBField = std::unique_ptr<genfit::AbsBField>(new genfit::ConstField(0., 0., 5)); // 0.5 T Bz
    mBField = std::unique_ptr<genfit::AbsBField>(new StarFieldAdaptor());
    genfit::FieldManager::getInstance()->init(mBField.get());
    
    // initialize the main mFitter using a KalmanFitter with reference tracks
    mFitter = std::unique_ptr<genfit::AbsKalmanFitter>(new genfit::KalmanFitterRefTrack(
        /*maxIterations = */ mMaxIt,
        /*deltaPval = */ mPVal,
        /*blowUpFactor = */ mBlowUp
    ));

    mFitter->setRelChi2Change( mRelChi2 );

    // Setup the histograms
    addHistogram2D( "PtCorrelation", "PtCorrelation; MC; RC;", 100, 0, 1, 100, 0, 1 );
    addHistogram1D( "PtResolution", "PtResolution; (p_{T}^{MC} - p_{T}^{RC}) / p_{T}^{MC};", 100, -5, 5 );
    addHistogram1D( "CurveResolution", "CurveResolution; (1/p_{T}^{MC} - 1/p_{T}^{RC}) / 1/p_{T}^{MC};", 100, -5, 5 );
    addHistogram2D( "CurveResolutionVsPt", "CurveResolution; Pt (GeV/c); (1/p_{T}^{MC} - 1/p_{T}^{RC}) / 1/p_{T}^{MC};", 100, 0, 2.0, 100, -5, 5 );
    addHistogram2D( "PtResolutionVsPt", "PtResolution; Pt (GeV/c); (p_{T}^{MC} - p_{T}^{RC}) / p_{T}^{MC};", 100, 0, 2.0, 100, -5, 5 );
    addHistogram1D( "PointCurvePtResolution", "PointCurve PtResolution; (p_{T}^{MC} - p_{T}^{RC}) / p_{T}^{MC};", 100, -5, 5 );
    addHistogram1D( "PointCurveCurveResolution", "PointCurve CurveResolution; (1/p_{T}^{MC} - 1/p_{T}^{RC}) / 1/p_{T}^{MC};", 100, -5, 5 );

    addHistogram1D( "QCurve", "QCurve; q/Pt;", 100, -5, 5 );
    addHistogram2D( "QMatrix", "QMatrix; MC; RC;", 4, -2, 2, 4, -2, 2 );
    addHistogram2D( "QidVsPt", "QMatrix; Pt; Qid;", 100, 0, 2.0, 2, -0.5, 1.5 );
    return kStOk;
}



int StFwdClosureMaker::Finish() {

    getHistogram1D("PtResolution")->Draw();
    gPad->Print( "ClosurePtResolution.pdf" );
    
    getHistogram1D("CurveResolution")->Draw();
    gPad->Print( "ClosureCurveResolution.pdf" );

    getHistogram1D("PointCurvePtResolution")->Draw();
    gPad->Print( "ClosurePointCurvePtResolution.pdf" );
    
    getHistogram1D("PointCurveCurveResolution")->Draw();
    gPad->Print( "ClosurePointCurveCurveResolution.pdf" );

    TFile * fOut = new TFile(mOutFile, "RECREATE");
    fOut->cd();
    for ( auto h : mHistograms ){
        h.second->Write();
    }

    return kStOk;
}


int StFwdClosureMaker::Make() {
    LOG_INFO << "Make (StFwdClosureMaker)" << endm;
    
    /*****************************************************
     * Load the MC Vertex
     */
    St_g2t_vertex *g2t_vertex = (St_g2t_vertex *)GetDataSet("geant/g2t_vertex");
    if (!g2t_vertex)
        return 0;
    
    /*****************************************************
     * Load the MC Tracks
     */
    // Store the McMomentum for the first track (assuming single particle gun)
    TVector3 mcMom(0,0,0);
    int mcQ = 0;
    // Get geant tracks
    St_g2t_track *g2t_track = (St_g2t_track *)GetDataSet("geant/g2t_track");
    if (!g2t_track)
        return 0;

    LOG_DEBUG << g2t_track->GetNRows() << " mc tracks in geant/g2t_track " << endm;
    // Load the MC track info
    for (int irow = 0; irow < g2t_track->GetNRows(); irow++) {
        g2t_track_st *track = (g2t_track_st *)g2t_track->At(irow);

        if (0 == track)
            continue;

        int track_id = track->id;
        float pt2 = track->p[0] * track->p[0] + track->p[1] * track->p[1];
        float pt = std::sqrt(pt2);
        float eta = track->eta;
        TVector3 pp( track->p[0], track->p[1], track->p[2] );
        if ( track_id == 1 ){
            mcMom.SetXYZ( track->p[0], track->p[1], track->p[2] );
            mcQ = track->charge;
        }
        float phi = std::atan2(track->p[1], track->p[0]); //track->phi;
        int q = track->charge;
        LOG_INFO << "McTrack: " << track_id << ", pt = " << pt << ", eta = " << eta << ", phi = " << phi << ", q = " << q << endm;
    }
    
    

    vector<genfit::SpacepointMeasurement*> spoints;

    /*****************************************************
     * Load the FST hits
     */
    St_g2t_fts_hit *g2t_fsi_hits = (St_g2t_fts_hit *)GetDataSet("geant/g2t_fsi_hit");
    if ( !g2t_fsi_hits ){
        LOG_DEBUG << "No g2t_fts_hits, cannot load FST hits from GEANT" << endm;
        return 0;
    }

    // reuse this to store cov mat
    TMatrixDSym hitCov3(3);
    const double sigXY = 0.01;
    hitCov3(0, 0) = sigXY * sigXY;
    hitCov3(1, 1) = sigXY * sigXY;
    hitCov3(2, 2) = 0.1;

    TMatrixDSym vStripCov3(3);
    vStripCov3(0, 0) = sigXY * sigXY;
    vStripCov3(1, 1) = 50;
    vStripCov3(2, 2) = 0.1;

    TMatrixDSym hStripCov3(3);
    hStripCov3(0, 0) = 50;
    hStripCov3(1, 1) = sigXY * sigXY;
    hStripCov3(2, 2) = 0.1;

    /*****************************************************
     * Add Primary Vertex to the track
     */
    if ( g2t_vertex != nullptr ) {
        // Set the MC Vertex for track fitting
        g2t_vertex_st *vert = (g2t_vertex_st*)g2t_vertex->At(0);
        TMatrixDSym cov;
        cov.ResizeTo(3, 3);
        cov(0, 0) = pow(mPrimaryVertexSigXY,2);
        cov(1, 1) = pow(mPrimaryVertexSigXY,2);
        cov(2, 2) = pow(mPrimaryVertexSigZ, 2);
        auto rhc = TVectorD( 3 );
        rhc[0] = vert->ge_x[0];
        rhc[1] = vert->ge_x[1];
        rhc[2] = vert->ge_x[2];
        auto spoint = new genfit::SpacepointMeasurement(rhc, cov, 0, 0, nullptr);
        spoints.push_back(spoint);
        // mForwardTracker->setEventVertex( TVector3( vert->ge_x[0], vert->ge_x[1], vert->ge_x[2] ), cov );
    }

    /*****************************************************
     * Add FST hits to the track
     */
    for (int i = 0; i < g2t_fsi_hits->GetNRows(); i++) {

        g2t_fts_hit_st *git = (g2t_fts_hit_st *)g2t_fsi_hits->At(i);
        if (0 == git)
            continue; // geant hit

        // int track_id = git->track_p;
        int volume_id = git->volume_id;  // 4, 5, 6
        int d = volume_id / 1000;        // disk id

        // int plane_id = d - 4;
        float x = git->x[0];
        float y = git->x[1];
        float z = git->x[2];

        auto rhc = TVectorD( 3 );
        TVector3 rastered = raster( TVector3( x, y, z ) );
        rhc[0] = rastered.X();
        rhc[1] = rastered.Y();
        rhc[2] = rastered.Z();
        auto spoint = new genfit::SpacepointMeasurement(rhc, makeSiCovMat(TVector3( x, y, z )), 0, i+1, nullptr);
        spoints.push_back(spoint);
        LOG_INFO << "FST HIT: d = " << d << ", x=" << x << ", y=" << y << ", z=" << z << endm;
    }

    St_g2t_fts_hit *g2t_stg_hits = (St_g2t_fts_hit *)GetDataSet("geant/g2t_stg_hit");
    if (!g2t_stg_hits){
        LOG_WARN << "geant/g2t_stg_hit is empty" << endm;
        return kStOk;
    }
    int nstg = g2t_stg_hits->GetNRows();

    LOG_DEBUG << "This event has " << nstg << " stg hits in geant/g2t_stg_hit " << endm;
    int nFttHits = 0;
    for (int i = 0; i < nstg; i++) {

        g2t_fts_hit_st *git = (g2t_fts_hit_st *)g2t_stg_hits->At(i);
        if (0 == git)
            continue; // geant hit
        // int track_id = git->track_p;
        int volume_id = git->volume_id;
        int plane_id = (volume_id - 1) / 100;           // from 1 - 16. four chambers per station

        // only use the hits on the front modules
        if ( mFttMode == kPoint && volume_id % 2 ==0 )
            continue;

        float x = git->x[0];
        float y = git->x[1];
        float z = git->x[2];

        if (plane_id < 0 || plane_id >= 4) {
            continue;
        }

        auto rhc = TVectorD( 3 );
        rhc[0] = x;
        rhc[1] = y;
        rhc[2] = z;
        LOG_INFO << "FTT HIT: plane_id = " << plane_id << ", volume_d = " << volume_id << " x=" << x << ", y=" << y << ", z=" << z << endm;

        if ( kPoint == mFttMode ){
            auto spoint = new genfit::SpacepointMeasurement(rhc, hitCov3, 0, i+4, nullptr);
            if ( nFttHits < mNumFttToUse )
                spoints.push_back(spoint);
        } else {
            if ( volume_id % 2 == 0 ){
                auto spoint = new genfit::SpacepointMeasurement(rhc, vStripCov3, 0, i+4, nullptr);
                if ( nFttHits < mNumFttToUse )
                    spoints.push_back(spoint);
            } else {
                auto spoint = new genfit::SpacepointMeasurement(rhc, hStripCov3, 0, i+4, nullptr);
                if ( nFttHits < mNumFttToUse )
                    spoints.push_back(spoint);
            }
        }
            
        nFttHits++;
    }

    float ptCurve = 9999.0;
    float qCurve = 1.0;
    if ( spoints.size() >= 3 ){
        double curve = computeCurvature( {spoints[0]->getRawHitCoords()[0], spoints[0]->getRawHitCoords()[1]}, 
                                            {spoints[1]->getRawHitCoords()[0], spoints[1]->getRawHitCoords()[1]}, 
                                            {spoints[2]->getRawHitCoords()[0], spoints[2]->getRawHitCoords()[1]} );
        const double K = 0.00029979; //K depends on the units used for Bfield
        const double BStrength = 5; // 0.5 T
        double pt = fabs((K*BStrength)/curve); // pT from average measured curv
        qCurve = curve > 0 ? 1 : -1;
        ptCurve = pt;
        LOG_INFO << "Curve: " << curve << ", Pt: " << pt << endm;
    }


    /*****************************************************
     * Setup the Genfit Fit Track
     */
    auto theTrackRep = new genfit::RKTrackRep(-13 * qCurve);
    auto seedPos = TVector3(0, 0, 0);
    auto seedMom = TVector3(0, 0, 10);
    // seedMom.SetPtEtaPhi( ptCurve, 3.0, 0 );
    auto mFitTrack = std::make_shared<genfit::Track>(theTrackRep, seedPos, seedMom);

    LOG_INFO << "Track fit with " << spoints.size() << " space points" << endm;
    try {
        for ( size_t i = 0; i < spoints.size(); i++ ){
            mFitTrack->insertPoint(new genfit::TrackPoint(spoints[i], mFitTrack.get()));
        }

        LOG_INFO << "Track prep = " << mFitter->isTrackPrepared( mFitTrack.get(), theTrackRep ) << endm; 

        // mFitter->checkConsistency();
        mFitTrack->checkConsistency();
        // mFitter->processTrack(mFitTrack.get());
        mFitter->processTrack(mFitTrack.get());
        
        mFitTrack->checkConsistency();
        mFitTrack->determineCardinalRep();

        // mFitter->processTrack(mFitTrack.get());
        

        auto status = mFitTrack->getFitStatus();
        LOG_INFO << "Fit status: " << status->isFitConverged() << endm;
        LOG_INFO << "-Fit pvalue: " << status->getPVal() << endm;
        LOG_INFO << "-Fit Chi2: " << status->getChi2() << endm;

        

        auto cr = mFitTrack->getCardinalRep();
        auto p = cr->getMom( mFitTrack->getFittedState( 0, cr ));
        int rcQ = status->getCharge();  
        LOG_INFO << "Fit momentum: " << p.X() << ", " << p.Y() << ", " << p.Z() << endm;
        LOG_INFO << "\tFit Pt: " << p.Pt() << ", eta: " << p.Eta() << ", phi: " << p.Phi() << endm;
        LOG_INFO << "\tMc  Pt: " << mcMom.Pt() << ", eta: " << mcMom.Eta() << ", phi: " << mcMom.Phi() << endm;

        
        if (status->isFitConvergedPartially()){
        getHistogram1D("PtResolution")->Fill( (p.Pt() - mcMom.Pt()) / mcMom.Pt() );
        getHistogram1D("CurveResolution")->Fill( (1/p.Pt() - 1/mcMom.Pt()) / (1/mcMom.Pt()) );
        getHistogram1D("PtCorrelation")->Fill( mcMom.Pt(), p.Pt() );
        getHistogram1D("QCurve")->Fill( rcQ / p.Pt() );

        getHistogram1D("PointCurvePtResolution")->Fill( (ptCurve - mcMom.Pt()) / mcMom.Pt() );
        getHistogram1D("PointCurveCurveResolution")->Fill( (1/ptCurve - 1/mcMom.Pt()) / (1/mcMom.Pt()) );

        getHistogram2D("PtResolutionVsPt")->Fill( mcMom.Pt(), (p.Pt() - mcMom.Pt()) / mcMom.Pt() );
        getHistogram2D("CurveResolutionVsPt")->Fill( mcMom.Pt(), (1/p.Pt() - 1/mcMom.Pt()) / (1/mcMom.Pt()) );
        
        getHistogram2D("QMatrix")->Fill( mcQ, rcQ );
        getHistogram2D("QidVsPt")->Fill( mcMom.Pt(), mcQ == rcQ ? 1 : 0 );
        }
        else {
            LOG_INFO << "Fit did not converge" << endm;
        }


    } catch (genfit::Exception &e) {
        LOG_ERROR << "GenFit failed to fit track with: " << e.what() << endm;
    }

    // load the space points from fst geant hits


    // for (size_t i = 0; i < trackSeed.size(); i++) {
    //     auto seed = trackSeed[i];
    //     TMatrixDSym cm(3);
    //     cm(0, 0) = 0.01;
    //     cm(1, 1) = 0.01;
    //     cm(2, 2) = 0.01;
    //     auto rhc = TVectorD( 3 );
    //     rhc[0] = seed->getX();
    //     rhc[1] = seed->getY();
    //     rhc[2] = seed->getZ();
    //     auto spoint = new genfit::SpacepointMeasurement(rhc, cm, 0, i, nullptr);
    //     spoints.push_back(spoint);
    // }

    return kStOk;
}
void StFwdClosureMaker::Clear(const Option_t *opts) {
    return;
}