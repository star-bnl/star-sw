#include <cmath>

#include "StETofUtil/etofHelperFunctions.h"

#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

#include "StMessMgr.h"
#include "StBTofHeader.h"


// *******************************************************************************************************
//
double
calculateVpdTstart( const StBTofHeader* btofHeader, const double& pVtxZ, const double& bTofClockRange )
{
    if( !btofHeader ) {
        LOG_WARN << "calculateVpdTstart():  -- no bTof header. Skip start time calculation." << endm;
        return 0.;
    }


    const int nVpd = 19; // number of VPD tubes on each side of STAR


    int nWest = btofHeader->numberOfVpdHits( west );
    int nEast = btofHeader->numberOfVpdHits( east );

    double vpdLeTime[ 2 * nVpd ];

    // check if bTof header is filled with useful information
    if( fabs( btofHeader->vpdVz() ) > 200. ) {
        LOG_DEBUG << "calculateVpdTstart(): no valid Vpd data in the bTOF header " << endm;
        return 0.;
    }
    else {
        LOG_DEBUG << "calculateVpdTstart(): Vpd vertex is at: " << btofHeader->vpdVz() << endm;
    }


    double tMean   = 0.;
    int nTubes     = 0;
    int nWestTubes = 0;
    int nEastTubes = 0;

    // west side
    for( int i=0; i< nVpd; i++ ) {
        vpdLeTime[ i ] = btofHeader->vpdTime( west, i+1 );
        if( vpdLeTime[ i ] > 0. ) {
            tMean = updateCyclicRunningMean( tMean, vpdLeTime[ i ], nTubes, bTofClockRange );
            nWestTubes++;
            LOG_DEBUG << "calculateVpdTstart(): loading VPD west tubeId = " << i+1 << " time " << vpdLeTime[ i ] << endm;
        }
    }

    // east side
    for( int i=0; i< nVpd; i++ ) {
        vpdLeTime[ i + nVpd ] = btofHeader->vpdTime( east, i+1 );
        if( vpdLeTime[ i + nVpd ] > 0. ) {
            tMean = updateCyclicRunningMean( tMean, vpdLeTime[ i + nVpd ], nTubes, bTofClockRange );
            nEastTubes++;
            LOG_DEBUG << "calculateVpdTstart(): loading VPD east tubeId = " << i+1 << " time " << vpdLeTime[ i + nVpd ] << endm;
        }
    }


    if( nWest != nWestTubes || nEast != nEastTubes ) {
        LOG_ERROR << "calculateVpdTstart(): number of fired Vpd tubes not correct ... " << endm;
        //return 0.;
    }

    
    double tstart = 0.;
    if( nWestTubes + nEastTubes ) {
        tstart = tMean;

        if( fabs( pVtxZ ) < 200. ) {
            LOG_DEBUG << "calculateVpdTstart():  --  Vpd start time before pVtxZ ( " << pVtxZ << " ) correction: " << tstart << endm;            

            tstart -= ( ( nEastTubes - nWestTubes ) * pVtxZ / ( c_light * nanosecond ) ) / ( nWestTubes + nEastTubes );
        }

    }


    if( tstart > bTofClockRange ) {
        tstart -= bTofClockRange;
    }
    else if( tstart < 0. ) {
        tstart += bTofClockRange;
    }

    LOG_DEBUG << "calculateVpdTstart(): -- sum: " << tMean << " nWest: " << nWest << " nEast: " << nEast << " Vpd start time: " << tstart << endm;

    return tstart;
}    
// *******************************************************************************************************

double
updateCyclicRunningMean( double mean, double value, int& count, double bTofClockRange )
{
    double valIn = value;
    if( mean - value < -0.9 * bTofClockRange ) {
        valIn -= bTofClockRange;  
    } 
    else if( mean - value > 0.9 * bTofClockRange ) {
        valIn += bTofClockRange;
    }


    double scaling = 1. / ( count + 1. );

    double valOut  = valIn * scaling + mean * ( 1. - scaling );
    count++;

    //LOG_INFO << "old mean: " << mean << "  scaling: " << scaling << " value in: " << valIn << "  new mean: " << valOut << endm;
    
    return valOut;
}



// *******************************************************************************************************
// calculate pathlength of a helix between two points 
double
tofPathLength( const StThreeVectorD&  beginPoint, const StThreeVectorD& endPoint, const double& curvature )
{

  StThreeVectorD vdif = endPoint - beginPoint;
  double C = vdif.perp();  
  double s_perp = C;
  if ( curvature ) {
    double R = 1 / curvature;
    s_perp = 2 * R * asin( C / ( 2 * R ) );
  }

  double s_z = fabs( endPoint.z() - beginPoint.z() );
  double value = sqrt( s_perp * s_perp + s_z * s_z );

  return value;
}
// *******************************************************************************************************
// calculate expected time of flight of particle hypothesis
double
expectedTimeOfFlight( const double pathLength, const double momentum, const double mass )
{
    double inverseBeta = sqrt( 1 + mass * mass / ( momentum * momentum ) );

    return pathLength * centimeter * ( 1. / c_light ) * inverseBeta / nanosecond;
}
// *******************************************************************************************************
