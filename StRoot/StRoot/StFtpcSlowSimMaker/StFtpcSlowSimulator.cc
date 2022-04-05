// $Id: StFtpcSlowSimulator.cc,v 1.19 2009/11/14 13:18:33 jcs Exp $
// $Log: StFtpcSlowSimulator.cc,v $
// Revision 1.19  2009/11/14 13:18:33  jcs
// change LOG_INFO messages to LOG_DEBUG messages
//
// Revision 1.18  2007/01/15 15:02:20  jcs
// replace printf, cout and gMesMgr with Logger
//
// Revision 1.17  2003/10/21 19:14:46  jcs
// use geantPlane function in debug printout
//
// Revision 1.16  2003/10/07 12:43:07  jcs
// use StFtpcGeantReader member function to extract FTPC plane number from GEANT volumeID
//
// Revision 1.15  2003/09/02 17:58:16  perev
// gcc 3.2 updates + WarnOff
//
// Revision 1.14  2003/07/04 14:06:43  fsimon
// Now rotating hits from global GEANT coordinates into local FTPC coordinates.
// This uses the instance of StFtpcTrackingParams defined in StFtpcSlowSimMaker.
//
// Revision 1.13  2002/09/13 13:46:41  fsimon
// Include functionality for smearing and kicking out hits,
// uncomment if needed
//
// Revision 1.12  2002/06/07 10:35:31  fsimon
// Additional debug info (tracing of hits)
//
// Revision 1.11  2002/04/19 22:24:13  perev
// fixes for ROOT/3.02.07
//
// Revision 1.10  2001/04/27 13:18:19  jcs
// cleanup comments, use SystemOfUnits for conversion
//
// Revision 1.9  2001/04/25 13:38:00  jcs
// remove obsolete comment
//
// Revision 1.8  2001/04/20 13:04:25  jcs
// this is the routine in which I changed the if/else statements for
// calculating the polar coordinates to avoid problems with optimizing
//
// Revision 1.7  2001/04/20 12:50:29  jcs
// cleanup comments
//
// Revision 1.6  2001/04/02 12:04:39  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters from StarDb/ftpc
//
// Revision 1.5  2001/03/19 15:53:10  jcs
// use ftpcDimensions from database
//
// Revision 1.4  2001/03/06 23:36:24  jcs
// use database instead of params
//
// Revision 1.3  2001/01/11 17:37:25  jcs
// replace math.h with PhysicalConstants.h
//
// Revision 1.2  2000/11/27 14:08:08  hummler
// inplement tzero and lorentz angle correction factor
//
// Revision 1.1  2000/11/23 10:16:44  hummler
// New FTPC slow simulator in pure maker form
//
//
///////////////////////////////////////////////////////////////////////////
//
// This is the main routine  for the FTPC Simulator
//
#include <Stiostream.h>
#include "PhysicalConstants.h"

#include "StFtpcSlowSimulator.hh"
#include "StFtpcSlowSimField.hh"
#include "StFtpcSlowSimCluster.hh"
#include "StFtpcSlowSimReadout.hh"
#include "StFtpcRawWriter.hh"
#include "StFtpcClusterMaker/StFtpcGeantReader.hh"
#include "StFtpcClusterMaker/StFtpcParamReader.hh"
#include "StFtpcClusterMaker/StFtpcDbReader.hh"

// include for Detector Rotations
#include "StFtpcTrackMaker/StFtpcTrackingParams.hh" 


#include "TF1.h"

#ifndef DEBUG
#define DEBUG 0
#endif

StFtpcSlowSimulator::StFtpcSlowSimulator(StFtpcGeantReader *geantReader,
					 StFtpcParamReader *paramReader,
                                         StFtpcDbReader *dbReader,
					 StFtpcRawWriter *rawWriter)
{
  mGeant = geantReader;
  mParam = paramReader;
  mDb    = dbReader;
  mWriter = rawWriter;
}

int StFtpcSlowSimulator::simulate()
{
    int i;

    if(DEBUG) {LOG_DEBUG << "Begin Initialization..." << endm;}

    // setup fields
    StFtpcSlowSimField *field = new StFtpcSlowSimField(mParam, mDb); // define the field
    if(DEBUG) field->Output();                  // write out field 

    // setup readout
    float *ADC = new float[mDb->numberOfPadrows()
			  *mDb->numberOfSectors()
			  *mDb->numberOfPads()
			  *mDb->numberOfTimebins()];      
    
    StFtpcSlowSimReadout *rdout = new StFtpcSlowSimReadout(mParam, mDb, ADC, field);
    // create readout
    if(DEBUG) rdout->Print();

    /////////////////////////////////////
    // big loop over all the hit points
    /////////////////////////////////////
    if (DEBUG) {LOG_DEBUG << "Looping over points..." << endm;}

    // tmp variables
    float electron;      
    float rad_off;
    float pad_off;
    float rad;
    float phi=0.;
    float drift_time;

    float aip = mDb->gasIonizationPotential()*eV;         // convert to GeV
    float px, py, pz, pp;
    float xx, yy, zz;
    float de;
    float r_min    = mDb->sensitiveVolumeInnerRadius();
    float r_max    = mDb->sensitiveVolumeOuterRadius();
    float dip_ang, cross_ang;

    int number_hits = mGeant->numberOfHits();
    int rad_rej = 0;
    int de_zero = 0;
    int n_cross_ang_max = 0;
    int counter=0;
    
    //create smearing function
    //TF1* noise = new TF1("noise","gaus",-2,2);
    //noise->SetParameters(1,0,0.035);
    //LOG_DEBUG << "Using gaussian smearing of GEANT hits  with a sigma of 350 um" << endm;

    //create smearing function
    //TF1* kickout = new TF1("kickout","1",0,1);
    //LOG_DEBUG << "Using Probability Function to throw out a certain percentage of all hits" << endm;


    for ( i=0; i<number_hits; ++i ) 
      {
      

	//          px = mGeant->pLocalX(i);
	//          py = mGeant->pLocalY(i);
	//          pz = mGeant->pLocalZ(i);

	// as local momenta from gstar are garbage:
	// use gstar vertex momenta instead.
	// errors should be small, except for shower tracks,
	// where track_p points to the shower generating track...
	px = mGeant->pVertexX(i);
	py = mGeant->pVertexY(i);
	pz = mGeant->pVertexZ(i);
	pp = sqrt ( px*px + py*py );
	
	xx = mGeant->x(i);
	yy = mGeant->y(i);
	zz = mGeant->z(i);

	// Add gaussian smearing to x and y coordinates
       
	//xx += noise->GetRandom();
	//yy += noise->GetRandom();


	// Throw out a given percentage of hits
	//if (kickout->GetRandom() < 0.2) continue;

       // Convert geant volume number into FTPC row number
	int irow = mGeant->geantPlane(mGeant->geantVolume(i)) -1 ; // Geant goes from 1-20 and C++ counter uses  0-19

	
	// Rotate the hit into FTPC - internal coordinate system (GEANT hits are always in global coords)
	// This function is "stolen" from StFtpcPoint::TransformGlobal2Ftpc()

	StThreeVectorD org(xx, yy, zz);
	StThreeVectorD transform = StFtpcTrackingParams::Instance()->GlobalToTpcRotation() * (org - StFtpcTrackingParams::Instance()->TpcPositionInGlobal());
	
	// internal FTPC rotation
	Int_t whichFtpc = (transform.z() < 0) ? 0 : 1; // east or west
	
	// first tranformation to new origin (FTPC installation point)
	transform.setY(transform.y() - StFtpcTrackingParams::Instance()->InstallationPointY(whichFtpc));
	transform.setZ(transform.z() - StFtpcTrackingParams::Instance()->InstallationPointZ(whichFtpc));
    
	// actual rotation
	transform = StFtpcTrackingParams::Instance()->FtpcRotationInverse(whichFtpc) * transform;
    
	// set z-position back to original value
	transform.setY(transform.y() + StFtpcTrackingParams::Instance()->InstallationPointY(whichFtpc));
	transform.setZ(transform.z() + StFtpcTrackingParams::Instance()->InstallationPointZ(whichFtpc));
    
	xx = transform.x();
	yy = transform.y();
	// zz = transform.z(); //not used since z - coordinate is given by the rows, z-rotation is not taken into account in simulations,
	// but the influence should be quite small



	//   Test that current point is within chamber          
         rad = sqrt ( xx*xx + yy*yy );
        if(rad < r_min || rad > r_max) {
	  ++rad_rej;
	  continue ;
        }

	de = mGeant->energyLoss(i);
	if ( de == 0 ) {
	  ++de_zero;
	}

	if(DEBUG) {
	  LOG_DEBUG << "Now processing hit " << i << " with xx;yy;zz;px;py;pz :"<< xx << "; " <<yy <<"; "<< zz <<"; "<< px <<"; "<< py <<"; "<< pz << endm;
        }

 
	// angle between r and p vectors in xy plane:
        float fpp=0; if (pp>0) fpp=atan2(py,px);
	float alpha = fpp - atan2(yy,xx);
	
	
	// momentum components with respect to r in xy plane:
	float p_perp = pp * sin(alpha);
	float p_rad = pp * cos(alpha);
	if(DEBUG) {
	  LOG_DEBUG << "alpha=" << alpha 
                    << " pperp=" << p_perp 
                    << " prad=" << p_rad 
                    << " xx = "<<xx
                    << " yy = "<<yy
                    << " zz = "<<zz
                    << " px = "<<px
                    << " py = "<<py
                    << " pz=" << pz << endm;
	}

	//  dip angle with respect to plane defined by z- and phi- axes	
	dip_ang   = atan(p_perp / pz);
        //  cross angle with respect to plane defined by z- and r- axes
	cross_ang = atan(p_rad / pz);
	if(cross_ang>halfpi) cross_ang = cross_ang - pi;

	//   Limit cross_ang and dip_angle to 1.5 to avoid nonsensical results
        //   Holm has inactivated these tests
 	float ang_max = 0;
 	if ( fabs( cross_ang) > ang_max ) {
 	  cross_ang = ang_max*cross_ang/fabs(cross_ang) ;
 	  ++n_cross_ang_max;
 	}
 	if ( fabs( dip_ang) > ang_max ) {
 	  dip_ang = ang_max*dip_ang/fabs(dip_ang) ;
 	  ++n_cross_ang_max;
 	}

        // calculate the polar coordinates
        //  so that phi-phi_min >= 0.0  (phi_min=halfpi)

        if (xx > 0.0) {
                phi = twopi + atan(yy/xx); 
        }
        else if (xx < 0.0) {
                phi = pi + atan(yy/xx);
        }
        else {
            if (yy >= 0.0)
                phi = halfpi;
            else if (yy < 0.0) 
                phi = 1.5*pi ;
        }
	
	if(DEBUG) {
	  LOG_DEBUG << i << " " << xx << " " << yy << " " << zz << " " << rad << " " << phi << endm;
        }
	
        // define cluster for each accepted hit point
        ++counter;
        drift_time  = -mDb->tZero();                          
        electron    = de / aip;
	
        rad_off     = rdout->GetPadLength() * tan(dip_ang);
        pad_off     = rdout->GetPadLength() * tan(cross_ang); // in cm


        if (DEBUG) {
            LOG_DEBUG << " ##### Point i= " << i
                      << " counter=" << counter
                      << " nel=" << electron
                      << " rad=" << rad
                      << " phi=" << phi
                      << endm;
            LOG_DEBUG << " #####            "
                      << " rad_off=" << rad_off
                      << " pad_off=" << pad_off
                      << endm;
            LOG_DEBUG << " x = " << xx
                      << " y = " << yy
                      << " z = " << zz << endm;
            LOG_DEBUG << " px = " << px
                      << " py = " << py
                      << " pz = " << pz
                      << " pp = " << pp
                      << " de = " << de << endm;
            LOG_DEBUG << " dip_angle = " << dip_ang
                      << " cross_angle = " << cross_ang
                      << " row_id = " << mGeant->geantPlane(mGeant->geantVolume(i))-1
                      << " track_id = " << mGeant->track(i)+1
                      << " ge_pid = " << mGeant->trackPid(i)
                      << endm;
	   }
	StFtpcSlowSimCluster *clus = 
	  new StFtpcSlowSimCluster(mParam, mDb, field, electron, rad_off, pad_off, 
				   rad, phi, drift_time, irow);

	clus->DriftDiffuse(field); // transport the cluster

        rdout->Avalanche(clus); // multiply electrons

        rdout->PadResponse(clus);            // response on pad 

        rdout->ShaperResponse(clus);  // response on shaper 

        rdout->Digitize(clus, irow);         // digitize signal

       	delete clus;

    } // end of loop over hit points
    
    if (DEBUG) {
       LOG_DEBUG << "Total number of hit points tested = " << number_hits << endm;
       LOG_DEBUG << "Number of hit points accepted = " << counter << endm;
       LOG_DEBUG << "Number of hit points rejected (radius test) = " << rad_rej << endm;
       LOG_DEBUG << "Number of hit points rejected (de=0 test) = " << de_zero << endm;
       LOG_DEBUG << "Number of hit points with cross_ang > cross_ang_max  = " << n_cross_ang_max << endm;
       LOG_DEBUG << "Writing out ADC array in raw data structure." << endm;
    }

   rdout->OutputADC();

   mWriter->writeArray(ADC,
		       mDb->numberOfPadrows(),
		       mDb->numberOfSectors(),
		       mDb->numberOfPads(),
		       mDb->numberOfTimebins(),
		       mParam->zeroSuppressThreshold());

    delete rdout;
    delete field;
    delete[] ADC;

    //delete noise;
    //delete kickout;

    return 1;
}

StFtpcSlowSimulator::~StFtpcSlowSimulator()
{

}
