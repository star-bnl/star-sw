/***************************************************************************
 *
 * $Id: spr_svt.cc,v 1.7 1999/02/15 03:32:34 caines Exp $
 *
 * Author: blasiuk and nyfeiman 
 ***************************************************************************
 *
 * Description:  This calculates the de/dx for SVT tracks
 *              
 ***************************************************************************
 *
 * $Log: spr_svt.cc,v $
 * Revision 1.7  1999/02/15 03:32:34  caines
 * Fixed bug so now looks at correct hits
 *
 * Revision 1.6  1999/02/14 21:44:14  caines
 * Code altered to include SSD
 *
 * Revision 1.5  1998/12/03 21:05:31  fisyak
 * Add type_of_call for NT
 *
 * Revision 1.4  1998/10/15 18:42:48  lasiuk
 * Initial Revision
 *
 * Revision 1.3  1998/10/08 20:35:45  kathy
 * Brian Lasiuk's new spr
 *
 **************************************************************************/
#include "spr_svt.h"

#define   MAXROWS         10000
#define   MAXPOINTS       50
#include <iostream.h>
#include <math.h>
//#include <vector>

#define MYDEBUG 0
#define idb if(MYDEBUG) cout
// SCL

// PAM

  long type_of_call spr_svt_(
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
    
    int ii=0;
    int jj=0;
    int kk=0;
    
    float u_ab_x, u_ab_y, u_ab_z;
    float norm, cos_theta;
    int wafer_no;
    
    idb << "*****************************************************" << endl;
    idb << "*            dE/dx and pid for the SVT              *" << endl;
    idb << "*---------------------------------------------------*" << endl;
    idb << "*  Hit data               : svt_spt_direct module   *" << endl;
    idb << "*  Wafer orientation      : svg module, svt_geom    *" << endl;
    idb << "*  Momentum reconstruction: sgr or stk              *" << endl;
    idb << "*  dE/dx - pid            : spr module              *" << endl;
    idb << "*  Hypothetical Pid is set to zero                  *" << endl; 
    idb << "*****************************************************" << endl;
   
    
    // used to check global tracks here!
    
    //
    // Check to make sure there are tracks
    //

    idb << "***svtTrackH->nok "  << (svtTrackH->nok)  << endl;
    idb << "*svtGroupsH->nok " << (svtGroupsH->nok) << endl;

	    
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

    idb << "numberOfTracks " << numberOfTracks << endl;
    idb << "MAXROWS " << MAXROWS << endl;
    idb << "MAXPOINTS " << MAXPOINTS << endl;
    
    int   globalTrackRow[MAXROWS];    
    int   spacePoints[MAXPOINTS];
    float spacePointsCharge[MAXPOINTS];
    float pathLength[MAXPOINTS];
    float ionization[MAXPOINTS];
    
    //
    // calculate de/dx for tracks:
    //

    idb << "numberOfTracks " << numberOfTracks << " ii " << ii << endl;
    idb << "(svtTrack[ii].nspt) " << (svtTrack[ii].nspt) << endl;
    
    for (ii = 0; ii<numberOfTracks; ii++) {
      svtTrack[ii].dedx[0] = 0.;
      svtTrack[ii].dedx[1] = 0.;
      if ( svtTrack[ii].flag > 0 ){
	idb << ii << " # of points on track " << (svtTrack[ii].nspt) << endl;
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
	    spacePoints[index] = svtGroups[jj].id2-1;
	    index++;    // can incorporate this into the spacePoints[index]?
	  }
	  if (index == numberOfPointsOnTrack)
	    break;      // get out if I found all the points!
	}
	
	// Calc unit vector for first point
  	u_ab_x = (svtCluster[spacePoints[1]].x[0] - 
		  svtCluster[spacePoints[0]].x[0]);
	u_ab_y = (svtCluster[spacePoints[1]].x[1] - 
		  svtCluster[spacePoints[0]].x[1]); 
	u_ab_z = (svtCluster[spacePoints[1]].x[2] - 
		  svtCluster[spacePoints[0]].x[2]); 
	norm = sqrt((u_ab_x*u_ab_x)+(u_ab_y*u_ab_y)+(u_ab_z*u_ab_z));
	
	u_ab_x /= norm;
	u_ab_y /= norm;
	u_ab_z /= norm; 
	
	//
	// reset and get the ionization from the clusters
	// using the indices above
	//
	index = 0;
	idb << "numberOfPointsOnTrack " << numberOfPointsOnTrack << endl;
	for (jj=0; jj<numberOfPointsOnTrack; jj++) {
	  
	  ionization[index]        = svtCluster[spacePoints[jj]].de[0];
	  
	  // Path length correction
	  
	  
	  for ( wafer_no=0; wafer_no<svtGeometryH->nok; wafer_no++){
	    if( svtCluster[spacePoints[jj]].id_wafer == 
		svtGeometry[wafer_no].id) break;
	  }
	  cos_theta = u_ab_x*svtGeometry[wafer_no].n[0] +
	    u_ab_y*svtGeometry[wafer_no].n[1] +
	    u_ab_z*svtGeometry[wafer_no].n[2] ;
	  
	  
	  pathLength[index]        = fabs(0.03/cos_theta);
	  
	  spacePointsCharge[index] = (ionization[index]/pathLength[index]);
	  
	  idb << "PathLength= " << pathLength[index] << endl;
	  
	  index++;
	  
	  // unit vector for next point
	  
	  u_ab_x = (svtCluster[spacePoints[jj+1]].x[0] - 
		    svtCluster[spacePoints[jj]].x[0]);
	  u_ab_y = (svtCluster[spacePoints[jj+1]].x[1] - 
		    svtCluster[spacePoints[jj]].x[1]); 
	  u_ab_z = (svtCluster[spacePoints[jj+1]].x[2] - 
		    svtCluster[spacePoints[jj]].x[2]); 
	  norm = sqrt((u_ab_x*u_ab_x)+(u_ab_y*u_ab_y)+(u_ab_z*u_ab_z));
	  
	  u_ab_x /= norm;
	  u_ab_y /= norm;
	  u_ab_z /= norm;
	}
	
	//
	// The dE/dx calculation
	// For now, only use the logmean
	// a truncated mean will require a sort!
	//
	float dEdx=0;
	float dEdxError=0;
	for (jj=0; jj<numberOfPointsOnTrack; jj++) {
	  idb << "spacePointsCharge " << (spacePointsCharge[jj]) << endl;
	  dEdx      += log(spacePointsCharge[jj]);
	  dEdx  += spacePointsCharge[jj];
	  dEdxError += log(spacePointsCharge[jj]);
	}
	
	//
	// Normalize to the number of Points
	//
	dEdx      /= numberOfPointsOnTrack;
	dEdxError /= numberOfPointsOnTrack;      
	
	dEdx = exp(dEdx);
	dEdxError = sqrt(exp(dEdxError));
	//dEdxError = exp(dEdxError) - dEdx*dEdx;
	//cout << "dEdx " << dEdx << " +/- " << dEdxError << endl;
	
	//
	// Assign to tables
	//
	
	dEdxError = exp(dEdxError) - (dEdx * dEdx);
	svtTrack[ii].dedx[0] = dEdx;
	svtTrack[ii].dedx[1] = dEdxError;
	
      }
    }
      
      idb << "End of spr_svt" << endl;
      return STAFCV_OK;
}

