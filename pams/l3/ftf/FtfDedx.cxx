/*:>-------------------------------------------------------------------
**: FILE:     FtfDedx.cxx
**: HISTORY:
**:<------------------------------------------------------------------*/
#include "FtfDedx.h"
//***********************************************************************
// constructors
//***********************************************************************
FtfDedx::FtfDedx(FtfTrack *track) {

  fTrack = track;
  fCutLow = 0;
  fCutHigh = 0.7;
  fNTruncate = 0;
  fDriftLoss = 0;
  // initialize unit vector perpendicular to rows
  // for each sector
  for (int sector=0; sector<24; sector++) {
    // caution: sector>12 needs x->-x and y->y (east side!)
    fUnitVec[sector].x = SectorSin[sector];
    if (sector+1>12) fUnitVec[sector].x = -1 * fUnitVec[sector].x;
    fUnitVec[sector].y = SectorCos[sector];
  } 
}

FtfDedx::FtfDedx (FtfTrack *track, float cutLow, float cutHigh) {

  fTrack = track;
  fCutLow = cutLow;
  fCutHigh = cutHigh;
  fNTruncate = 0;
  fDriftLoss = 0;
  // initialize unit vector perpendicular to rows
  // for each sector
  for (int sector=0; sector<24; sector++) {
    // caution: sector>12 needs x->-x and y->y (east side!)
    fUnitVec[sector].x = SectorSin[sector];
    if (sector+1>12) fUnitVec[sector].x = -1 * fUnitVec[sector].x;
    fUnitVec[sector].y = SectorCos[sector];
  } 
}

FtfDedx::FtfDedx (FtfTrack *track, float cutLow, float cutHigh, float driftLoss) {

  fTrack = track;
  fCutLow = cutLow;
  fCutHigh = cutHigh;
  fNTruncate = 0;
  fDriftLoss = driftLoss;
  // initialize unit vector perpendicular to rows
  // for each sector
  for (int sector=0; sector<24; sector++) {
    // caution: sector>12 needs x->-x and y->y (east side!)
    fUnitVec[sector].x = SectorSin[sector];
    if (sector+1>12) fUnitVec[sector].x = -1 * fUnitVec[sector].x;
    fUnitVec[sector].y = SectorCos[sector];
  } 
}

//***********************************************************************
//   dEdx calculation using Truncated Mean algorithm
//   averaging method: dE/dx = (sum of q/s)/(# of points)
//
//   author: christof
//***********************************************************************
int FtfDedx::TruncatedMean() {

  double padLength;
  int nPointsInArray;

  // dx calculation:
  //   straight-line approx. in x-y-plane
  //   assume perfect hit position on track
  //   :=> don't calculation intersection of circle and row
  //   1/cosl corr. factor for dip angle

//   printf("Track: id %i, pt %f, nHits %i, q %i, tanl %f\n",
// 	 fTrack->id, fTrack->pt, fTrack->nHits, fTrack->q, fTrack->tanl);

  // calculate dip angle correction factor 
  double cosl = 1 / (sqrt( 1 + (double) fTrack->tanl*fTrack->tanl));

//   printf("   cosl %f\n", cosl);
  if ( cosl==0 ) return 0;

  // calculate center of circle
  double tPhi0 = fTrack->psi + fTrack->q * pi/2;
  if ( tPhi0 > 2*pi ) tPhi0 = tPhi0 - 2*pi;
  else if ( tPhi0 < 0. ) tPhi0 = tPhi0 + 2*pi;

  double x0 = fTrack->r0 * cos(fTrack->phi0);
  double y0 = fTrack->r0 * sin(fTrack->phi0);
  double rr = fTrack->pt / ( bFactor *fTrack->getPara()->bField );
  double xc = x0 - rr * cos(tPhi0);
  double yc = y0 - rr * sin(tPhi0);

//   printf("   xc %f  yc %f\n", xc, yc);

  nPointsInArray = 0;

  //now loop over hits
  for ( FtfHit *ihit = (FtfHit *)fTrack->firstHit ; 
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
        struct vector tangent;
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

  fTrack->nDedx = 0;
  fTrack->dedx  = 0;

  for (int i=cLow; i<cHigh; i++) {
        fTrack->nDedx++;
	fTrack->dedx += fDedxArray[i];
  }
  
  if (fTrack->nDedx>0) fTrack->dedx = fTrack->dedx/fTrack->nDedx;

//   printf("  %e   %i\n", fTrack->dedx, fTrack->nDedx);

  return 0;
}


//***************************************************************************
//   Pablo: Calculates dEdx
//***************************************************************************
void FtfDedx::PabloDedx ( ){
   int i, j ;
   FtfHit *nextHit ;
   int nTruncate = max(1, fNTruncate*fTrack->nHits/100) ;
   nTruncate = min(fTrack->nHits/2,nTruncate) ;
//
//   Define array to keep largest de's
//
   double *de = new double[nTruncate] ;
//
//    Reset
//
   fTrack->dedx = 0.F ;
   memset ( de, 0, nTruncate*sizeof(double) ) ;
//
//
//
   for  ( nextHit = (FtfHit *)fTrack->firstHit ; 
          nextHit != 0 ;
          nextHit = (FtfHit *)nextHit->nextTrackHit) { 
    
      fTrack->dedx += nextHit->q ;
	 
      if ( nextHit->q < de[0] ) continue ;

      for ( i = nTruncate-1 ; i>=0 ; i-- ){
         if ( nextHit->q > de[i] ){
            for ( j=0 ; j<i ; j++ ) de[j] = de[j+1] ;
            de[i] = nextHit->q ;
            break ;
	 }
      }
   }
//
//    Subtract largest de
//
   for ( i=0 ; i<nTruncate ; i++ ) fTrack->dedx -= de[i] ;
   fTrack->dedx = fTrack->dedx / fTrack->length ;
/*   End track in required volume condition */
      
}
