/*:>-------------------------------------------------------------------
**: FILE:     FtfDedx.cxx
**: HISTORY:
**:<------------------------------------------------------------------*/
#include "Stl3Util/ftf/FtfDedx.h"


//***********************************************************************
// constructor
//***********************************************************************
FtfDedx::FtfDedx(St_l3_Coordinate_Transformer *trafo, float cutLow, float cutHigh, float driftLoss)
{
  fCoordTransformer = trafo;
  fCutLow = cutLow;
  fCutHigh = cutHigh;
  fNTruncate = 0;
  fDriftLoss = driftLoss;
  // initialize unit vector perpendicular to rows
  // for each sector
  for (int sector=0; sector<24; sector++) {
        // caution: sector>12 needs x->-x and y->y (east side!)
        fUnitVec[sector].x = fCoordTransformer->GetSectorSin(sector);
	if (sector+1>12) fUnitVec[sector].x = -1 * fUnitVec[sector].x;
	fUnitVec[sector].y = fCoordTransformer->GetSectorCos(sector);
  }
}


//***********************************************************************
//   dEdx calculation using Truncated Mean algorithm
//   averaging method: dE/dx = (sum of q/s)/(# of points)
//
//   author: christof
//***********************************************************************
int FtfDedx::TruncatedMean(FtfTrack *track) {

  double padLength;
  int nPointsInArray;

  // dx calculation:
  //   straight-line approx. in x-y-plane
  //   assume perfect hit position on track
  //   :=> don't calculate intersection of circle and row
  //   1/cosl corr. factor for dip angle

//   printf("Track: id %i, pt %f, nHits %i, q %i, tanl %f\n",
// 	 fTrack->id, fTrack->pt, fTrack->nHits, fTrack->q, fTrack->tanl);

  // calculate dip angle correction factor 
  double cosl = 1 / (::sqrt( 1 + (double) track->tanl*track->tanl));

//   printf("   cosl %f\n", cosl);
  if ( cosl==0 ) return 0;

  // calculate center of circle
  double tPhi0 = track->psi + track->q * pi/2;
  if ( tPhi0 > 2*pi ) tPhi0 = tPhi0 - 2*pi;
  else if ( tPhi0 < 0. ) tPhi0 = tPhi0 + 2*pi;

  double x0 = track->r0 * cos(track->phi0);
  double y0 = track->r0 * sin(track->phi0);
  double rr = track->pt / ( bFactor *track->getPara()->bField );
  double xc = x0 - rr * cos(tPhi0);
  double yc = y0 - rr * sin(tPhi0);

//   printf("   xc %f  yc %f\n", xc, yc);

  nPointsInArray = 0;

  //now loop over hits
  for ( FtfHit *ihit = (FtfHit *)track->firstHit ; 
	ihit != 0 ;
	ihit = (FtfHit *)ihit->nextTrackHit) {

        // discard hits with wrong charge information
        // i.e. hits of one-pad cluster or merged cluster
        if (ihit->flags) continue;

        // calculate direction of the tangent of circle at position
        // of given hit:
        //     - rotate radius vector to hit by -90 degrees
        //       matrix (  0   1 )
        //              ( -1   0 )
        //     - divide by length to get unity vector
        struct christofs_2d_vector tangent;
	tangent.x = (ihit->y - yc)/rr;
	tangent.y = -(ihit->x - xc)/rr;
	//printf("   tangent x %f y %f\n", tangent.x, tangent.y);
  
	// get crossing angle by calculating dot product of 
	// tanget and unity vector perpendicular to row (look-up table)
	double cosCrossing = fabs(tangent.x * fUnitVec[ihit->sector-1].x + tangent.y * fUnitVec[ihit->sector-1].y);

	// padlength
	if (ihit->row<14) padLength = padLengthInnerSector;
	else padLength = padLengthOuterSector;

	// ==> finally dx:
	if ( cosCrossing==0 ) continue;
	double dx = padLength / (cosCrossing * cosl);

	// drift length correctrion: d_loss [m^-1], l_drift [cm]
	// q_corr = q_meas / ( 1 - l_drift * d_loss/100 )
	double scaleDrift = 1 - fabs(ihit->z) * fDriftLoss/100;


	// first shot: de_scale as implemented in tph.F,
	// i.e. gas gain, wire-to-pad coupling, etc.
	// !! hard coded, has to come from the db somewhere sometime !!
	double deScale;
	if (ihit->row<14) deScale = 5.0345021e-9;
	else deScale = 1.4234702e-8;

	fDedxArray[nPointsInArray] = ihit->q * deScale / ( dx * scaleDrift );
	nPointsInArray++;
	
//         printf("  ihit row %i x %f y %f z %f  q %d  q_scaled %e scale %e flags %d\n",
// 	       ihit->row, ihit->x, ihit->y, ihit->z, ihit->q, ihit->q*deScale, deScale, ihit->flags);
// 	printf("    cosCros: %f, cosl: %f ===>> dx %f\n", cosCrossing, cosl, dx);

  }

  // sort dEdxArray
  sort( fDedxArray, fDedxArray+nPointsInArray );

  // calculate absolute cuts
  int cLow  = (int) floor(nPointsInArray * fCutLow);
  int cHigh = (int) floor(nPointsInArray * fCutHigh);

  track->nDedx = 0;
  track->dedx  = 0;

  for (int i=cLow; i<cHigh; i++) {
        track->nDedx++;
	track->dedx += fDedxArray[i];
  }
  
  if (track->nDedx>0) track->dedx = track->dedx/track->nDedx;

//   printf("  %e   %i\n", track->dedx, track->nDedx);

  return 0;
}
