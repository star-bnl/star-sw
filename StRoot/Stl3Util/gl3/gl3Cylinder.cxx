#include "gl3Cylinder.h"

#include "Stl3Util/base/FtfLog.h"

gl3Cylinder::gl3Cylinder() : minNoOfHitsOnTrack(10)
{
}


//####################################################################
// 
//####################################################################
int gl3Cylinder::setParameters(int GI1_in, int GI2_in, 
			       int GI3_in, int GI4_in, 
			       int GI5_in, 
			       float GF1_in, float GF2_in, 
			       float GF3_in, float GF4_in, 
			       float GF5_in)
{
    bool errorFlag = false;

    gl3Algorithm::setParameters(GI1_in, GI2_in, GI3_in, GI4_in, GI5_in, 
				GF1_in, GF2_in, GF3_in, GF4_in, GF5_in);

    float & minZ   = GF1;
    float & maxZ   = GF2;
    float & radius = GF3;
    
    int & minTracksInCylinder = GI1;


    if (minZ >= maxZ) {
	ftfLog("%s: invalid parameters: minZ >= maxZ (%f >= %f)\n",
	       getAlgorithmName(), minZ, maxZ);
	errorFlag = true;
    }
    
    if (radius < 0) {
	ftfLog("%s: invalid parameters: negative radius (%f)\n",
	       getAlgorithmName(), radius);
	errorFlag = true;
    }

    if (minTracksInCylinder < 1) {
	ftfLog("%s: invalid parameters: invalid min no. of tracks in cylinder (%f)\n",
	       getAlgorithmName(), minTracksInCylinder);
	errorFlag = true;
    }

    return errorFlag;
}

//####################################################################
// 
//####################################################################
int gl3Cylinder::decide ( )
{
    int nTracksInCyl = 0;

    float & minZ   = GF1;
    float & maxZ   = GF2;
    float & radius = GF3;
    
    int & minTracksInCylinder = GI1;
    
    
    // loop over global tracks
    for (int trkcnt = 0 ; trkcnt<event->getNTracks(); trkcnt++ ) {
	gl3Track *gTrack = event->getTrack(trkcnt);
	
	// acept only tracks with nHits more then minNoOfHitsOnTrack
	if(gTrack->nHits > minNoOfHitsOnTrack) {
	    Ftf3DHit hit = gTrack->extraRadius(radius);
	    // fill histos with the coordinates of the closest point to x=y=0

	    if ( hit.x != 0. || hit.y != 0. || hit.z != 0. ) {
		//ftfLog("Extrapolated point: %d %d %d\n",
		//     hit.x, hit.y, hit.z);

		if ( hit.z >= minZ && hit.z <= maxZ) {
		    //ftfLog("Triggered!!!");

		    // the track hits the cylinder
		    nTracksInCyl++;
		}
	    }
	}
    }

    // DECISION
    if( nTracksInCyl >= minTracksInCylinder )
	return 1;
    else
	return 0;
}

