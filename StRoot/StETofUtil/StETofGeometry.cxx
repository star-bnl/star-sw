/***************************************************************************
 *
 * $Id: StETofGeometry.cxx,v 1.1 2018/07/25 14:34:40 jeromel Exp $
 *
 * Author: Florian Seck, April 2018
 ***************************************************************************
 *
 * Description: Collection of geometry classes for the eTOF:
 *              - eTOF geometry constants
 *              - StETofNode: generic eTOF geometry object initialized via
 *                TGeoManager
 *              - StETofModule, StETofCounter inherit from StETofNode
 *              - StETofGeometry builds the geometry and features all
 *                necessary methods to match track helices with eTOF hits
 *
 ***************************************************************************
 *
 * $Log: StETofGeometry.cxx,v $
 * Revision 1.1  2018/07/25 14:34:40  jeromel
 * First version, reviewed Raghav+Jerome
 *
 *
 ***************************************************************************/
#include <algorithm>

#include "TGeoManager.h"
#include "TGeoVolume.h" 
#include "TGeoPhysicalNode.h"
#include "TGeoMatrix.h"
#include "TGeoBBox.h"

#include "StETofUtil/StETofGeometry.h"
#include "StMaker.h"


///////////////////////////////////////////////////////////////////////////////////////////////////
//
// StETofNode
// ==========
//
///////////////////////////////////////////////////////////////////////////////////////////////////

bool StETofNode::mDebug = false;

StETofNode::StETofNode( const TGeoPhysicalNode& gpNode )
{
    mGeoMatrix = static_cast< TGeoHMatrix* > ( gpNode.GetMatrix() );
    /*  
    double* trans  = mGeoMatrix->GetTranslation();
    double* rot    = mGeoMatrix->GetRotationMatrix();

    LOG_INFO << trans[0] << "  " << trans[1] << "  " << trans[2] << endm;   
    
    LOG_INFO << rot[0] << "  " << rot[1] << "  " << rot[2] << endm; 
    LOG_INFO << rot[3] << "  " << rot[4] << "  " << rot[5] << endm; 
    LOG_INFO << rot[6] << "  " << rot[7] << "  " << rot[8] << endm; 
    */
    mBox = static_cast< TGeoBBox* > ( gpNode.GetShape() );

    buildMembers();
}

StETofNode::StETofNode( const TGeoPhysicalNode& gpNode, const float& dx, const float& dy )
{
    mGeoMatrix = static_cast< TGeoHMatrix* > ( gpNode.GetMatrix() );
    /*
    double* trans  = mGeoMatrix->GetTranslation();
    double* rot    = mGeoMatrix->GetRotationMatrix();

    LOG_INFO << trans[0] << "  " << trans[1] << "  " << trans[2] << endm;
    
    LOG_INFO << rot[0] << "  " << rot[1] << "  " << rot[2] << endm;
    LOG_INFO << rot[3] << "  " << rot[4] << "  " << rot[5] << endm;
    LOG_INFO << rot[6] << "  " << rot[7] << "  " << rot[8] << endm;
    */
    mBox = static_cast< TGeoBBox* > ( gpNode.GetShape() );
    
    // resize mBox with dx and dy
    float dz = mBox->GetDZ();
    mBox->SetBoxDimensions( dx, dy, dz );
        
    buildMembers();
}


void
StETofNode::convertPos( StETofNode* from, const double* pos_from, StETofNode* to, double* pos_to )
{
    if( to == 0 ) {
        from->local2Master( pos_from, pos_to );
    }
    else {
        double xg[ 3 ];
        from->local2Master( pos_from, xg );
        to->master2Local( xg, pos_to );
    }
}


void
StETofNode::local2Master( const double* local, double* master )
{
    // transformation from local cooridinates into global coordinates
    mGeoMatrix->LocalToMaster( local, master );
}


void
StETofNode::master2Local( const double* master, double* local )
{
    // transformation from global cooridinates into local coordinates
    mGeoMatrix->MasterToLocal( master, local );
}


StThreeVectorD
StETofNode::calcCenterPos()
{
    // calculate center position of the node in global coordinates
    double xl[3] = { 0, 0, 0 };
    double xm[3];

    local2Master( xl, xm );

    return StThreeVectorD( xm[ 0 ], xm[ 1 ], xm[ 2 ] );
}


StThreeVectorD
StETofNode::calcXYPlaneNormal()
{
    // calculate the normal vector to the local XY-plane
    // i.e. the global representation of the local unit vector (0,0,1)
    
    double xl[ 3 ] = { 0, 0, 1 };
    double xm[ 3 ];
    
    // transform to global coordinates
    local2Master( xl, xm );
    
    // subtract vector pointing to the center
    /*
    xm[ 0 ] -= mGeoMatrix->GetTranslation()[ 0 ];
    xm[ 1 ] -= mGeoMatrix->GetTranslation()[ 1 ];
    xm[ 2 ] -= mGeoMatrix->GetTranslation()[ 2 ];

    return StThreeVectorD( xm[ 0 ], xm[ 1 ], xm[ 2 ] );
    */

    StThreeVectorD norm( xm[ 0 ], xm[ 1 ], xm[ 2 ] );

    // subtract vector pointing to the center
    norm -= mCenter;
    
    return norm;
}


double
StETofNode::calcEta( const double& rel_local_x )
{
    // calculate min (relative local x = -1) / max (relative local x = 1)  eta at the edges of the node
    double xl[3] = { 0, 0, 0 };
    double xm[3];
    
    if( fabs( rel_local_x ) <= 1. && rel_local_x != 0. ) { 
        double dx = mBox->GetDX();  
        xl[ 0 ] = dx * rel_local_x;
    }

    local2Master( xl, xm );
    return StThreeVectorD( xm[ 0 ], xm[ 1 ], xm[ 2 ] ).pseudoRapidity();
}


double
StETofNode::calcPhi( const double& rel_local_x, const double& rel_local_y )
{
    // calculate min (relative local y = -1) / max (relative local y = 1)  phi at the edges of the node
    // set rel_local_x to -1 to get the lower edge (largest span of the node in phi)
    double xl[3] = { 0, 0, 0 };
    double xm[3];
        
    if( fabs( rel_local_x ) <= 1. && rel_local_x != 0. ) { 
        double dx = mBox->GetDX(); 
        xl[ 0 ] = dx * rel_local_x;
    }
    if( fabs( rel_local_y ) <= 1. && rel_local_y != 0. ) { 
        double dy = mBox->GetDY(); 
        xl[ 1 ] = dy * rel_local_y;
    }

    local2Master( xl, xm );
    return StThreeVectorD( xm[ 0 ], xm[ 1 ], xm[ 2 ] ).phi();
}


void
StETofNode::buildMembers()
{
    // build member variables: mMinEta, mMaxEta, mMinPhi, mMaxPhi, mCenter, mNormal 
    mCenter = calcCenterPos();
    mNormal = calcXYPlaneNormal();
    
    mEtaMin = calcEta( -1. );
    mEtaMax = calcEta(  1. );

    mPhiMin = calcPhi( -1, -1. );
    mPhiMax = calcPhi( -1,  1. );
    
    if( mDebug ) print();
}


bool
StETofNode::isLocalPointIn( const double* local )
{
    // returns true if point in local coordinates is inside the node's volume
    return mBox->Contains( local );
}


bool
StETofNode::isGlobalPointIn( const StThreeVectorD& global )
{
    // returns true if point in global coordinates is inside the node's volume
    double xm[ 3 ] = { global.x(), global.y(), global.z() };
    double xl[ 3 ];

    master2Local( xm, xl );

    return mBox->Contains( xl );
}


bool
StETofNode::helixCross( const StHelixD& helix, double& pathLength, StThreeVectorD& cross, double& theta )
{
    // check if helix goes through this node
    // and return path length of helix before crossing this node
    float maxPathLength = 1000;
    
    bool isInside = false;
    pathLength = 0;
    
    // find intersection between helix & the node's XY-plane
    pathLength = helix.pathLength( mCenter, mNormal );
    
    if( pathLength > 0 && pathLength < maxPathLength ) {
        cross = helix.at( pathLength );
        theta = mNormal.angle( helix.cat( pathLength ) );
    }
    
    // check if the intersection point is really inside the node
    isInside = isGlobalPointIn( cross );
    
    return isInside;
}


void
StETofNode::print( const Option_t* opt ) const
{
    double* trans  = mGeoMatrix->GetTranslation();
    double* rotMat = mGeoMatrix->GetRotationMatrix();

    LOG_INFO << " -------- "
         << "\nBox dimension: "  << mBox->GetDX() << " : " << mBox->GetDY()  << " : " << mBox->GetDZ()
         << "\ncenter pos: "     << mCenter.x()   << " : " << mCenter.y()    << " : " << mCenter.z()
         << "\ncenter phi: "     << mCenter.phi() << ", eta: " << mCenter.pseudoRapidity()
         << "\nphi range: "      << mPhiMin << " : " << mPhiMax
         << "\teta range: "      << mEtaMin << " : " << mEtaMax
         << "\nXYplane normal: " << mNormal.x()   << " : " << mNormal.y()    << " : " << mNormal.z()
         << "\ntrans [0-2] = "   << trans[ 0 ]  << "  " << trans[ 1 ]  << "  " << trans[ 2 ] 
         << "\nrotMat[0-2] = "   << rotMat[ 0 ] << "  " << rotMat[ 1 ] << "  " << rotMat[ 2 ]
         << "\nrotMat[3-5] = "   << rotMat[ 3 ] << "  " << rotMat[ 4 ] << "  " << rotMat[ 5 ]
         << "\nrotMat[6-8] = "   << rotMat[ 6 ] << "  " << rotMat[ 7 ] << "  " << rotMat[ 8 ]
         << "\n ------------------------------------------------ " << endm;
}





///////////////////////////////////////////////////////////////////////////////////////////////////
//
// StETofGeomModule
// ================
//
///////////////////////////////////////////////////////////////////////////////////////////////////

bool StETofGeomModule::mDebug = true; //false;

StETofGeomModule::StETofGeomModule( const TGeoPhysicalNode& gpNode, const int moduleId )
: StETofNode( gpNode ),
  mModuleIndex( moduleId ) 
{
    mSector = calcSector( moduleId );
    mPlane  = calcPlane(  moduleId );

    mETofCounter.reserve( ETofGeomConst::nCounters );

    if( mDebug ) print();
}


void
StETofGeomModule::addCounter( const TGeoPhysicalNode& gpNode, const int moduleId, const int counterId )
{   
    StETofGeomCounter* counter = new StETofGeomCounter( gpNode, moduleId, counterId );
    
    mETofCounter.push_back( counter );
}


void
StETofGeomModule::addCounter( const TGeoPhysicalNode& gpNode, const float& dx, const float& dy, const int moduleId, const int counterId )
{  
    StETofGeomCounter* counter = new StETofGeomCounter( gpNode, dx, dy, moduleId, counterId );
    
    mETofCounter.push_back( counter );
}


StETofGeomCounter*
StETofGeomModule::counter( const  unsigned int i ) const
{
    if( mETofCounter.size() <= i || i < 0 ) {
        LOG_ERROR << "Counter not defined" << endm;
        return nullptr;
    }
    
    return mETofCounter[ i ];
}


int
StETofGeomModule::calcSector( const int moduleId )
{
    // calculate sector from moduleId
    // moduleId = (plane - 1) + 3 * (sector - 13)
    return ( moduleId / ETofGeomConst::nPlanes ) + ETofGeomConst::sectorStart;
}


int
StETofGeomModule::calcPlane( const int moduleId )
{
    // calculate plane from moduleId
    // moduleId = (plane - 1) + 3 * (sector - 13)
    return ( moduleId % ETofGeomConst::nPlanes ) + ETofGeomConst::zPlaneStart;
}


void
StETofGeomModule::print( const Option_t* opt ) const
{
    LOG_INFO << "StETofGeomModule, module# = " << mModuleIndex << "  sector = " << mSector << "  plane = " << mPlane << endm;
    StETofNode::print( opt );
}





///////////////////////////////////////////////////////////////////////////////////////////////////
//
// StETofGeomCounter
// =================
//
///////////////////////////////////////////////////////////////////////////////////////////////////

bool StETofGeomCounter::mDebug = true; //false;

StETofGeomCounter::StETofGeomCounter( const TGeoPhysicalNode& gpNode, const int moduleId, const int counterId )
: StETofNode( gpNode ),
  mModuleIndex( moduleId ),
  mCounterIndex( counterId )
{
    mSector = calcSector( moduleId  );
    mPlane  = calcPlane( moduleId );

    createGeomStrips();

    if( mDebug ) print();
}


StETofGeomCounter::StETofGeomCounter( const TGeoPhysicalNode& gpNode, const float& dx, const float& dy, const int moduleId, const int counterId )
: StETofNode( gpNode, dx, dy ),
  mModuleIndex( moduleId ),
  mCounterIndex( counterId )
{
    mSector = calcSector( moduleId  );
    mPlane  = calcPlane( moduleId );

    createGeomStrips();

    if( mDebug ) print();
}


int
StETofGeomCounter::calcSector( const int moduleId )
{
    // calculate sector from moduleId
    // moduleId = (plane - 1) + 3 * (sector - 13)
    return ( moduleId / ETofGeomConst::nPlanes ) + ETofGeomConst::sectorStart;
}


int
StETofGeomCounter::calcPlane( const int moduleId )
{
    // calculate plane from moduleId
    // moduleId = (plane - 1) + 3 * (sector - 13)
    return ( moduleId % ETofGeomConst::nPlanes ) + ETofGeomConst::zPlaneStart;
}


void
StETofGeomCounter::createGeomStrips()
{
    // divide the counter into strips   
    float counterDx  = this->box()->GetDX();
    float stripWidth = 2 * counterDx / ETofGeomConst::nStrips;

    for( int i=0; i<=ETofGeomConst::nStrips; i++) {
        mStripX[ i ] = stripWidth * i - counterDx;
        //cout << mStripX[ i ] << endl;
    } 
}


int
StETofGeomCounter::findStrip( const double* local )
{
    // look up the strip the local point is in
    
    int iStrip = -1;

    if( isLocalPointIn( local ) ) {
        for( int i=0; i<ETofGeomConst::nStrips; i++ ) {
            if( mStripX[ i ] <= local[ 0 ] && local[ 0 ] <= mStripX[ i+1 ] ) {
                iStrip = i+1;
                break;
            }
        } 
    }

    return iStrip;
}


void
StETofGeomCounter::print( const Option_t* opt ) const
{
    LOG_INFO << "StETofGeomCounter, module# = " << mModuleIndex << "  sector = " << mSector << "  plane = " << mPlane << "  counter = " << mCounterIndex + 1 << endm;
    StETofNode::print( opt );
}





///////////////////////////////////////////////////////////////////////////////////////////////////
//
// StETofGeometry
// ==============
//
///////////////////////////////////////////////////////////////////////////////////////////////////

bool StETofGeometry::mDebug = false;

StETofGeometry::StETofGeometry( const char* name, const char* title )
: TNamed( name, title ),
  mNValidModules( 0 ),
  mInitFlag( false )
{

}


StETofGeometry::~StETofGeometry()
{

}


int
StETofGeometry::calcModuleIndex( const int& sector, const int& plane )
{
    // calculate module index from sector (13 -- 24) and plane (1 -- 3)
    return (plane - ETofGeomConst::zPlaneStart ) + ETofGeomConst::nPlanes * ( sector - ETofGeomConst::sectorStart );
}

int
StETofGeometry::calcVolumeIndex( const int& sector, const int& plane, const int& counter, const int& strip )
{
    // calculate volume Id
    int idMultiplier[ 3 ] = { 10000, 1000, 100 };
    int id = sector * idMultiplier[ 0 ] + plane * idMultiplier[ 1 ] + counter * idMultiplier[ 2 ] + strip;

    return id;
}

void
StETofGeometry::init( StMaker* maker, TGeoManager* geoManager )
{
    //if( maker->debug() ) debugOn();
    debugOn();
    //debugOff();

    if( geoManager == nullptr ) LOG_ERROR << " *** StETofGeometry::Init - Cannot find TGeoManager *** " << endm;

    LOG_INFO << " +++ geoManager :   "  << geoManager << endm;

    mNValidModules = 0;

    //StETofGeomCounter* mCounter[ 36 ][ 3 ];


    // loop over sectors
    for( int sector = ETofGeomConst::sectorStart; sector <= ETofGeomConst::sectorStop; sector++ ) {
        // loop over planes
        for( int plane = ETofGeomConst::zPlaneStart; plane <= ETofGeomConst::zPlaneStop; plane++ ) {
            std::string geoPath( formTGeoPath( geoManager, plane, sector ) );

            if( geoPath.empty() ) {
                LOG_INFO << "StETofGeometry::Init(...) - Cannot find path to ETOF module "
                            "(id " << plane << sector << "). Skipping..." << endm;
                continue;
            }
            mNValidModules++;

            const TGeoPhysicalNode* gpNode = geoManager->MakePhysicalNode( geoPath.c_str() );

            int moduleId = calcModuleIndex( sector, plane );

            mETofModule[ mNValidModules-1 ] = new StETofGeomModule( *gpNode, moduleId );

            
            //load the counters of the modules
            //  StETofGeomCounter* mCounter[ 3 ];

            // loop over counters
            for( int counter = ETofGeomConst::counterStart; counter <= ETofGeomConst::counterStop; counter++ ) {
                std::string geoPath( formTGeoPath( geoManager, plane, sector, counter ) );

                if( geoPath.empty() ) {
                    LOG_INFO << "StETofGeometry::Init(...) - Cannot find path to ETOF counter "
                                "(id " << plane << sector << ", " << counter << "). Skipping..." << endm;
                    continue;
                }

                const TGeoPhysicalNode* gpNode = geoManager->MakePhysicalNode( geoPath.c_str() );

                //mCounter[ moduleId ][ counter-1 ] = new StETofGeomCounter( *gpNode, moduleId, counter );

                //get the gas gap dimensions
                int gap = 1;
                std::string geoPathActiveVolume( formTGeoPath( geoManager, plane, sector, counter, gap ) );

                if( geoPathActiveVolume.empty() ) {
                    LOG_INFO << "StETofGeometry::Init(...) - Cannot find path to ETOF counter gas gap (for active area evaluation)"
                                "(id " << plane << sector << ", " << counter << "). Skipping..." << endm;
                    continue;
                }

                const TGeoPhysicalNode* gpNodeActiveVolume = geoManager->MakePhysicalNode( geoPathActiveVolume.c_str() );

                const TGeoBBox* activeVolume = static_cast< TGeoBBox* > ( gpNodeActiveVolume->GetShape() );

                float dx = activeVolume->GetDX();
                float dy = activeVolume->GetDY();

                LOG_INFO << activeVolume->GetDX() << "  " << activeVolume->GetDY() << "  " << activeVolume->GetDZ() << endm;

                int counterId = counter - ETofGeomConst::counterStart;

                mETofModule[ mNValidModules-1 ]->addCounter( *gpNode, dx, dy, moduleId, counterId );

            } // end of loop over counters

        } // end of loop over planes
    } // end of loop over sectors

    LOG_INFO << "amount of valid modules: " << mNValidModules << endm;  

    //cout << " ************************************** " << endl;
    //mETofModule[ 5 ]->counter( 0 )->print();
    //cout << " ************************************** " << endl;


    // finished initializing geometry
    setInitFlag( true );
}


/**
 * FormTGeoPath
 * Returns full path to the eTof module/counter placed at a predefined location in the detector's ROOT geometry.
 * An empty string is returned if the module/counter is not found in the geometry hierarchy (via TGeoManager).
 */
std::string
StETofGeometry::formTGeoPath( const TGeoManager* geoManager, int plane, int sector, int counter )
{
    std::ostringstream geoPath;

    geoPath << "/HALL_1/CAVE_1/MagRefSys_1/ETOF_" << plane << sector;

    bool found = geoManager->CheckPath( geoPath.str().c_str() );

    if( !found ) {
        geoPath.str("");
        geoPath.clear();

        geoPath << "/HALL_1/CAVE_1/ETOF_" << plane << sector;
    }

    // go deeper if counter is requested
    if( counter >= 1 ) {
        geoPath << "/EGAS_1/ECOU_" << counter;
    }

    found = geoManager->CheckPath( geoPath.str().c_str() );

    return found ? geoPath.str() : "";
}


std::string
StETofGeometry::formTGeoPath( const TGeoManager* geoManager, int plane, int sector, int counter, int gap )
{
    std::ostringstream geoPath;

    geoPath << "/HALL_1/CAVE_1/MagRefSys_1/ETOF_" << plane << sector;

    bool found = geoManager->CheckPath( geoPath.str().c_str() );

    if( !found ) {
        geoPath.str("");
        geoPath.clear();

        geoPath << "/HALL_1/CAVE_1/ETOF_" << plane << sector;
    }

    // go deeper if counter is requested
    if( counter >= 1 ) {
        geoPath << "/EGAS_1/ECOU_" << counter;
    }
    if( gap >= 1 ) {
        geoPath << "/EGAP_" << gap;
    }

    found = geoManager->CheckPath( geoPath.str().c_str() );

    return found ? geoPath.str() : "";
}


StETofNode*
StETofGeometry::findETofNode( const int moduleId, const int counterId )
{
    int iModule  = -1;
    int iCounter = -1;

    for( unsigned int i=0; i<mNValidModules; i++ ) {
        if( mETofModule[ i ]->moduleIndex() == moduleId ) {
            iModule = i;
            break;
        }
    }

    if( iModule == -1 ) {
        LOG_ERROR << "ETOF volume for moduleId " << moduleId << " and counter " << counterId << " is not loaded ..." << endm;
        return nullptr;
    }

    int nValidCounters = mETofModule[ iModule ]->numberOfCounters();

    for( int j=0; j<nValidCounters; j++ ) {
        if( mETofModule[ iModule ]->counter( j )->counterIndex() == counterId ) {
            iCounter = j;
            break;
        }
    }
    
    if( iCounter == -1 ) {
        LOG_ERROR << "ETOF volume for moduleId " << moduleId << " and counter " << counterId << " is not loaded ..." << endm;
        return nullptr;
    }

    return mETofModule[ iModule ]->counter( iCounter );
}


void
StETofGeometry::hitLocal2Master( const int moduleId, const int counterId, const double* local,  double* master )
{
    master[ 0 ] = 0;
    master[ 1 ] = 0;
    master[ 2 ] = 0;

    if( !findETofNode( moduleId, counterId ) ) {
        LOG_ERROR << "ETOF volume of a hit is not loaded in the geometry" << endl;
        return;
    }

    findETofNode( moduleId, counterId )->local2Master( local, master );

    return;
}

StThreeVectorD
StETofGeometry::helixCrossETofPlane( const StHelixD& helix )
{
    //if ( IsDebugOn() ) LOG_INFO << "zplane:" << ETofGeomConst::zplanes[ 1 ] << endm;

    // center of ETOF plane
    StThreeVectorD r( 0, 0, ETofGeomConst::zplanes[ 1 ] );

    // Normal to ETOF plane
    StThreeVectorD n( 0, 0, 1 );

    //if ( IsDebugOn() )
    //  logPoint( "( outer- ) helix origin" , helix.origin() );

    double s = helix.pathLength( r, n );
    StThreeVectorD point = helix.at( s );

    //if ( IsDebugOn() ) {
    //  LOG_INFO << "pathLength @ ETOF plane = " << s << endm;
    //  logPoint( "intersection", point );
    //}

    return point;
}


/**
 * HelixCrossSector 
 * Returns a vector of sector ids that the track could intersect with
 */
vector< int >
StETofGeometry::helixCrossSector( const StHelixD& helix )
{
    StThreeVectorD point = helixCrossETofPlane( helix );

    //LOG_INFO << "track phi @ ETOF= " << point.phi() << endm;

    return sectorAtPhi( point.phi() );
}

vector< int >
StETofGeometry::sectorAtPhi( const double& angle )
{
    float phi = angle;
    // make phi bounded by [0, 2pi]
    if ( phi < 0. ) phi += 2. * M_PI;

    // 15 degree slice; half of an ETOF sector
    double slice = M_PI / 12.;

    // sector 15 at phi = 0
    // sector 24 at phi = pi/2
    // sector 21 at phi = pi
    // sector 18 at phi = 3pi/2
    vector< int > sectorId = { 15, 14, 13, 24, 23, 22, 21, 20, 19, 18, 17, 16 };

    double iSlice = phi / slice;
    int sectorA = -1;
    int sectorB = -1;
    int intSlice = ( int ) iSlice;

    int indexA = ( intSlice / 2 );

    // in this case the track falls into a 15 degree slice in the center of the sector, only matches with this sector
    if ( intSlice % 2 == 0 && intSlice < 24 ){
        sectorA = sectorId[ indexA ];
    } else {
        // in this case the track is in the 15 degree slice overlap of two sector
        int indexB = indexA + 1;
        if( indexB >= ( int )
            sectorId.size() ) indexB = 0; // prevent index out of range
        
        sectorA = sectorId[ indexA ];
        sectorB = sectorId[ indexB ];
    }
    //if ( IsDebugOn() ) LOG_INFO << "phi = " << phi << ", iSlice = " << iSlice << ", SectorA: " << sectorA << ", SectorB: " << sectorB <<  endm;

    vector< int > r = { sectorA };
    if ( sectorB >= 13 )
        r.push_back( sectorB );

    return r;
}


/**
 * HelixCrossCounter( 
 * Returns true if a counter is crossed by a helix
**/
bool
StETofGeometry::helixCrossCounter( const StHelixD& helix, vector< int >& idVec )
{
    // estimate which sector(s) the track crossed
    vector< int > sectorsCrossed = helixCrossSector( helix ); 
    
    if( sectorsCrossed.size() == 1 ) LOG_INFO << "sector crossed: "  << sectorsCrossed[ 0 ] << endm;
    if( sectorsCrossed.size() == 2 ) LOG_INFO << "sectors crossed: " << sectorsCrossed[ 0 ] << ", " << sectorsCrossed[ 1 ] << endm;
    
    // loop over all modules
    for( unsigned int i=0; i<mNValidModules; i++ ) {
        if( !mETofModule[ i ] ) continue;
        
        // only search in modules of crossed sectors
        int iSector = mETofModule[ i ]->sector();        
        auto found = std::find( std::begin( sectorsCrossed ), std::end( sectorsCrossed ), iSector );

        if( found == std::end( sectorsCrossed ) ) continue;
            
        //LOG_INFO << iSector << "  " << mETofModule[i]->GetPlane() << endm;
    
        double module_pathLen;
        double module_theta;
        StThreeVectorD module_cross;

        bool helixCrossedModule = mETofModule[ i ]->helixCross( helix, module_pathLen, module_cross, module_theta );
        
        if( module_theta > 0.5 * M_PI ) module_theta -= M_PI;
        module_theta = fabs( module_theta * 180. / M_PI );

        /*
        if( helixCrossedModule ) {
            LOG_INFO << " -----------" << "\nmoduleId:"<< mETofModule[ i ]->moduleIndex() << "  helix_crossed: " << helixCrossedModule
                 << "  sector: " << mETofModule[ i ]->sector() << " plane: " << mETofModule[ i ]->plane() << endm;
            LOG_INFO << "pathLength: " << module_pathLen << "   absolute impact angle: " << module_theta << " degree" << endm;
            logPoint( "crossing point" , module_cross );
            LOG_INFO << "cross.eta: " << module_cross.pseudoRapidity() << endm;
        }
        */


        // only search for intersections of counters with the helix if the module was crossed
        if( !helixCrossedModule ) continue;
        
        int nValidCounters = mETofModule[ i ]->numberOfCounters();
        
        // loop over counters
        for( int j=0; j<nValidCounters; j++ ) { 
            double pathLen;
            double theta;
            StThreeVectorD cross;

            bool helixCrossedCounter = mETofModule[ i ]->counter( j )->helixCross( helix, pathLen, cross, theta );
            
            if( theta > 0.5 * M_PI ) theta -= M_PI;
            theta = fabs( theta * 180. / M_PI );

            if( helixCrossedCounter ) {
                double global[ 3 ];
                double local [ 3 ];

                global[ 0 ] = cross.x();
                global[ 1 ] = cross.y();
                global[ 2 ] = cross.z();

                mETofModule[ i ]->counter( j )->master2Local( global, local );
                int strip = mETofModule[ i ]->counter( j )->findStrip( local );

                int sector  = mETofModule[ i ]->sector();
                int plane   = mETofModule[ i ]->plane();
                int counter = mETofModule[ i ]->counter( j )->counterIndex();

                int volumeIndex = calcVolumeIndex( sector, plane, counter, strip );
                //LOG_INFO << " &&&&& volume index: " << volumeIndex << endm;

                idVec.push_back( volumeIndex );

                /*
                LOG_INFO << " -----------" << "\ncounterId: " << mETofModule[ i ]->counter( j )->counterIndex() << endm;
                LOG_INFO << "pathLength: " << pathLen << "   absolute impact angle: " << theta << " degree" << endm;
                logPoint( "crossing point" , cross );
                LOG_INFO << "cross.eta: " << cross.pseudoRapidity() << "\n" << endm;
                LOG_INFO << "localX: " << local[ 0 ] << "  localY: " << local[ 1 ] << "  localZ: " << local[ 2 ] << endm;
                LOG_INFO << "Strip: " << strip << " * * * " << endm;
                */
            }

        } // end loop over counters
    } // end loop over modules

    return true;
}


void
StETofGeometry::logPoint( const char* text, const StThreeVectorD& point )
{
    LOG_INFO << text << " at (" << point.x() << ", " << point.y() << ", " << point.z() << ")" << endm;
}


StETofGeomModule* 
StETofGeometry::module( const unsigned int i )
{
    if( isInitDone() && i < mNValidModules ) {
        return mETofModule[ i ];
    }
    else return nullptr;
}
