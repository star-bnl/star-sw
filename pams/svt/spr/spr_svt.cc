/***************************************************************************
 *
 * $Id: spr_svt.cc,v 1.3 1998/10/08 20:35:45 kathy Exp $
 *
 * Author: blasiuk and nyfeiman 
 ***************************************************************************
 *
 * Description:  This calculates the de/dx for SVT tracks
 *              
 ***************************************************************************
 *
 * $Log: spr_svt.cc,v $
 * Revision 1.3  1998/10/08 20:35:45  kathy
 * Brian Lasiuk's new spr
 *
 **************************************************************************/
#include "spr_svt.h"

#define   MAXROWS         10000
#define   MAXPOINTS       10
#include <iostream.h>
#include <math.h>
//#include <vector>

// SCL

// PAM

  long spr_svt_(
  TABLE_HEAD_ST *sprParameterH,       SPR_SPRPAR_ST    *sprParameter,
  TABLE_HEAD_ST *svtTrackKinematicsH, STK_KINE_ST      *svtTrackKinematics,
  TABLE_HEAD_ST *svtGeometryH,        SVG_GEOM_ST      *svtGeometry,
  TABLE_HEAD_ST *svtClusterH,         SCS_SPT_ST       *svtCluster,
  TABLE_HEAD_ST *svtGroupsH,          SGR_GROUPS_ST    *svtGroups,
  TABLE_HEAD_ST *svtTrackH,           STK_TRACK_ST     *svtTrack)

{
    //
    // define counter variables
    //
    
    int ii, jj,kk;
    
    cout << "*****************************************************" << endl;
    cout << "*            dE/dx and pid for the SVT              *" << endl;
    cout << "*---------------------------------------------------*" << endl;
    cout << "*  Hit data               : svt_spt_direct module   *" << endl;
    cout << "*  Wafer orientation      : svg module, svt_geom    *" << endl;
    cout << "*  Momentum reconstruction: sgr or stk              *" << endl;
    cout << "*  dE/dx - pid            : spr module              *" << endl;
    cout << "*  Hypothetical Pid is set to zero                  *" << endl; 
    cout << "*****************************************************" << endl;
    
    // used to check global tracks here!
    
    //
    // Check to make sure there are tracks
    //
    
    if (svtTrackH->nok == 0) {
	cerr << "svt_dedx::spr_am ==> svtTrack->nok is 0" << endl;
	cerr << "No SVT tracks, aborting..." << endl;
	return 1;
    }   
    
    //
    // Read in sprParameter if necessary
    // No input parameters are required currently!
    //

    //
    // fill in global track pointers (if necessary)
    //

    // When STL becomes available, use it
    // The arrays will not have to be statically declared
    // vector<int> globalTrackRow;
    // vector<int> spacePoints;
    // vector<float> spacePointsCharge;
    // vector<float> pathLength;
    // vector<float> ionization;

    int   numberOfTracks = (svtTrackH->nok);

    int   globalTrackRow[MAXROWS];    
    int   spacePoints[MAXPOINTS];
    float spacePointsCharge[MAXPOINTS];
    float pathLength[MAXPOINTS];
    float ionization[MAXPOINTS];
    
    //
    // calculate de/dx for tracks:
    //
    
    for (ii = 0; ii<<numberOfTracks; ii++) {
	cout << "Track: " << ii;
	cout << " # of points on track " << (svtTrack[ii].nspt) << endl;
	int numberOfPointsOnTrack = svtTrack[ii].nspt;
	int trackId=(svtTrack[ii].id);  // needed as an index
	
	// zero arrays (not necessary with STL--use clear() )
	for(kk=0; kk<MAXPOINTS; kk++) {
	    spacePoints[kk]       = 0;
	    spacePointsCharge[kk] = 0;
	    pathLength[kk]        = 0;
	    ionization[kk]        = 0;
	}
	
	//
	// get id's of points for the iith track
	// To gain access, must loop over "groups" of space points
	//

	//
	// Match the group and track ID numbers!
	//
	
	int index = 0;
	for (jj=0; jj<(svtGroupsH->nok); jj++) {
	    if (trackId == svtGroups[jj].id1) {
		spacePoints[index] = svtGroups[jj].id2;
		index++; // can incorporate this into the spacePoints[index]?
	    }
	    if (index == numberOfPointsOnTrack)
		break;  // get out if I found all the points!
	}

	//
	// reset and get the ionization from the clusters
	// using the indices above
	//
	index = 0;
	for (jj=0; jj<numberOfPointsOnTrack; jj++) {
	    ionization[index]        = svtCluster[spacePoints[jj]].de[0];
	    //
	    // A pathLength correction will be necessary
	    // in the future, for now let us allow the machinery
	    // for it
	    //
	    pathLength[index]        = 1.0;
	    spacePointsCharge[index] = (ionization[index]/pathLength[index]);
	    index++;
	}

	//
	// The dE/dx calculation
	// For now, only use the logmean
	// a truncated mean will require a sort!
	//
	float dEdx=0;
	float dEdxError=0;
	for (jj=0; jj<numberOfPointsOnTrack; jj++) {
	    dEdx      += log(spacePointsCharge[jj]);
	    dEdxError += 2*log(spacePointsCharge[jj]);
	}

	//
	// Normalize to the number of Points
	//
	dEdx      /= numberOfPointsOnTrack;
	dEdxError /= numberOfPointsOnTrack;      

	dEdx = exp(dEdx);
	
	//
	// Assign to tables
	//
      
	dEdxError = exp(dEdxError) - (dEdx * dEdx);
	svtTrack[ii].dedx[0] = dEdx;
	svtTrack[ii].dedx[1] = dEdxError;
    }
    
    return STAFCV_OK;
}

