/* ------- System includes -------------- */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>


/* ------- STAF generated includes ------- */

#include "tkf.h"
#include "tkf_utilities.h"

#define		kaonMass		.493677		/*	Kaon (+ or -) mass in GeV	*/
#define		pionMass		.139569		/*	Pi (+ or -) mass in GeV		*/
#define		muonMass		.105658		/*	Muon (+ or -) mass in GeV	*/
#define		pi0Mass			.134976		/*	Pi (neutral) mass in GeV	*/

#define		KaonToMuonQ		.236		/*	Kaon CM p when decaying to muoon and neutrino     */
#define		KaonToPionQ		.205		/*	Kaon CM p when decaying to pi+ and pi0	 */
#define		PionToMuonQ		.030		/*	Kaon CM p when decaying to pi+ and pi0	 */
										
#define		arraySize		10000

#define		radToDeg		57.2957795131
#define		degToRad		0.0174532925199			
	
/************************************************************/			
/*					      		*/
/*	tkf_						*/
/*					       		*/
/*					       		*/
/*	Functional Description				*/
/*		This is the main routine of tkf.c which is called	*/
/*		by the outside world.  After finding all tracks and	*/
/*		converting them to local coordinates the function	*/
/*		cycles through all unique track pairings and 		*/
/*		looks for kinks.  This is done by projecting the 	*/
/*		track helix parameters onto the bend plane 			*/
/*		resulting in a circle. Then the pairs of circles 	*/
/*		are checked to see if they intersect.  If two 		*/
/*		intersections occur then the one that has the least	*/
/*		"error" in fitting is used.  Then the z-seperation 	*/
/*		is checked to make sure the tracks actually 		*/
/*		intersect in 3-D.  Finally, the information 		*/
/*		detailed in the tkf_out structure is calculated and	*/
/*		stored.  						        */
/*										*/
/*	C code created 11/95 by Scott Tooker					*/
/*	C code heavily modified 12/95 by S. Margetis to make it actually work	*/
/*	C code ported to STAF 8/97 by S. Margetis				*/
/* 									*/
/************************************************************/
long type_of_call tkf_(
  TABLE_HEAD_ST         *tkfpar_h,     TKF_TKFPAR_ST           *tkfpar ,
  TABLE_HEAD_ST         *tkfout_h,     TKF_TKFOUT_ST           *tkfout ,
  TABLE_HEAD_ST      *g2t_track_h,      G2T_TRACK_ST        *g2t_track ,
  TABLE_HEAD_ST     *g2t_vertex_h,     G2T_VERTEX_ST       *g2t_vertex ,
  TABLE_HEAD_ST           *tpch_h,    G2T_TPC_HIT_ST             *tpch ,
  TABLE_HEAD_ST        *tptrack_h,    TPT_TRACK_ST          *tptrack ,
  TABLE_HEAD_ST         *tpeval_h,      TTE_MCTRK_ST           *tpeval ,
  TABLE_HEAD_ST        *globtrk_h,    DST_TRACK_ST          *globtrk ,
  TABLE_HEAD_ST       *gpideval_h, EPI_GLOB_PID_EVAL_ST         *gpideval )
{
	int							returnCode;
	static G2T_TPC_HIT_ST		*trackList[arraySize];
	static localTrack			*localTracks[arraySize];
	int						tracksFound;
	int						ii, i, j, k, l, m;
	localTrack					*trackOne;
	localTrack					*trackTwo;
	localTrack					*newTrack1;
	localTrack					*newTrack2;
	localTrack					*newTrackOne[2];
	localTrack					*newTrackTwo[2];
	localTrack					*parentTrack;
	localTrack					*daughterTrack;
	double						coordsOne[2];
	double						radiusOne;
	double						coordsTwo[2];
	double						radiusTwo;
	double						xIntersect[2];
	double						yIntersect[2];
	int							iflag;
	double						r[2];
	double						newCoords1[2];
	double						newCoords2[2];
	double						targetCoords[2];
	int							numberOfSolutions;
	double						dca[2];	
	int							foundVertexCandidate;
	double						MVCoords[3];
	double						trackOneMomentum[3];
	double						trackTwoMomentum[3];
	double						dca1;
	double						dca2;
	double						deltaPionMuon;
	double						deltaKaonMuon;
	double						deltaKaonPion;
	double						p, pd, pDotpd;
	double						pn;
	double						trackPos[3];
	double						theta;
	int						aux[10000], iv1, iv2;

	const double				alpha = 0.299792458;
	const double				magnetic_field = 5.0;

		
	/*	Set counters for number of rows of good data to zero	*/
	
	tkfout_h->nok = 0;
	
	/*	Set return code to normal success	*/
	
	returnCode = STAFCV_OK;
	
	for (i=0; i<arraySize;i++)
	{
		localTracks[i] = NULL;
	}
		
	/*	Get acceptable tracks from tpc hit table	*/
	
/*  sort tpeval (tp_mctrk) table to recid   */

        for (i=0; i<tpeval_h->nok;i++)
          {
            aux[tpeval[i].recid]=tpeval[i].mcid;
          }   


	tracksFound = 0;
        ii = 0;
        if((globtrk_h->nok) == 0) return returnCode;
        for (i=0;i< globtrk_h->nok;i++)
          {
                 if (localTracks[ii] = (localTrack *) malloc(sizeof(localTrack)))
                  {
                    if( globtrk[i].iflag > 0 ) 
                      {
                        localTracks[ii]->id    = 0;
                        localTracks[ii]->mc_id = 0;
                        localTracks[ii]->nhits = 0;
                        localTracks[ii]->row   = 0;
                        localTracks[ii]->lr    = 0.;
                        localTracks[ii]->r     = 0.;
                        localTracks[ii]->rphi  = 0.;
                        localTracks[ii]->z     = 0.;
                        localTracks[ii]->psi   = 0.;
                        localTracks[ii]->tanl  = 0.;
                        localTracks[ii]->qR    = 0.;
                
                
                        localTracks[ii]->id    = globtrk[i].id;
                        localTracks[ii]->mc_id = aux[globtrk[i].id];
                        localTracks[ii]->nhits = globtrk[i].ndegf;
			/*                        localTracks[ii]->row   = globtrk[i].last_row; */
                        localTracks[ii]->lr    = sqrt(globtrk[i].x_last[0]*globtrk[i].x_last[0] +
                                                     globtrk[i].x_last[1]*globtrk[i].x_last[1]);
                        localTracks[ii]->r     = sqrt(globtrk[i].x0*globtrk[i].x0 +
                                                      globtrk[i].y0*globtrk[i].y0);
                        localTracks[ii]->rphi  = localTracks[ii]->r * 
			                         atan2(globtrk[i].y0,globtrk[i].x0);
                        localTracks[ii]->z     = globtrk[i].z0;
                        localTracks[ii]->psi   = globtrk[i].psi * (degToRad);
                        localTracks[ii]->tanl  = globtrk[i].tanl;
                        localTracks[ii]->qR    = ((double) globtrk[i].icharge) * (alpha*magnetic_field) * 
			                                  (globtrk[i].invpt/1000.0);
                        tracksFound++;
                        ii++;
                      }
                  }
                else
                  {
                        printf("Out of memory error in transformToLocalTracks. Aborting.");
                        abort();
                      }

               } /* i=1 to globtrk_h.nok */

	
	/*	Cycle through pairs of tracks looking for ones that intersect in bend plane	*/
	i = 0;
	j = 0;
	k = 0;
	l = -1;
	m = 0;
	
	foundVertexCandidate = 0;
	for (i=0;i<tracksFound;i++)
	{
		/*	Convert helix parameters of first track to x-y circle coordinates	*/
		trackOne = NULL;
		trackOne = localTracks[i];

/*
		if ( trackOne->lr > tkfpar->r_max )
                  continue;
*/
		
		radiusOne = 0;
		radiusOne = tkf_circle_param(trackOne, coordsOne); 
		
		targetCoords[0] = g2t_vertex[0].ge_x[0];
		targetCoords[1] =  g2t_vertex[0].ge_x[1];
       		tkf_project_track(coordsOne, radiusOne, targetCoords, newCoords1);
		dca1 = fabs(tkf_dca_track(coordsOne, radiusOne, newCoords1, trackOne)- g2t_vertex[0].ge_x[2]);


		for (j=i+1;j<tracksFound;j++)
		{
			/*	Convert helix parameters of second track to x-y circle coordinates	*/
			trackTwo = NULL;
			trackTwo = localTracks[j];

                        if ( (trackOne->r) < (trackTwo->r) )
                          {
                            if( (trackOne->lr < 135.) || (trackOne->lr > 180.) )
                              continue;
/*
                            if( (trackTwo->r < 130.) )
                              continue;
*/
                          }
                        else
                          {
                            if( (trackTwo->lr < 135.) || (trackTwo->lr > 180.) )
                              continue;
/*
                            if( (trackOne->r < 130.) )
                              continue;
*/
                          }
	
			radiusTwo = 0;
			radiusTwo = tkf_circle_param(trackTwo, coordsTwo);
			
                        targetCoords[0] = g2t_vertex[0].ge_x[0];
                        targetCoords[1] = g2t_vertex[0].ge_x[1];
                        tkf_project_track(coordsTwo, radiusTwo, targetCoords, newCoords2);
                        dca2 = fabs(tkf_dca_track(coordsTwo, radiusTwo, newCoords2, trackTwo)- g2t_vertex[0].ge_x[2]);


			/*	Find all possible intersections of the two circles	*/
			iflag = 0;
			iflag = tkf_vzero_geom(tkfpar->max_dist, coordsOne, coordsTwo, 
									radiusOne, radiusTwo, xIntersect, yIntersect);
			
			numberOfSolutions = 0;
			switch (iflag)
			{
				case 5:		/*	No solution	*/
					numberOfSolutions = 0;
					break;
				
				case 3:		/*	One solution	*/
					numberOfSolutions = 1;
					break;
				
				default:	/*	Two solutions	*/
					numberOfSolutions = 2;
					break;
			}
			
			newTrackOne[0] = NULL;
			newTrackOne[1] = NULL;
			newTrackTwo[0] = NULL;
			newTrackTwo[1] = NULL;
			
			
			for (k=0;k<numberOfSolutions;k++)
			{
				r[k] = sqrt(xIntersect[k]*xIntersect[k] + yIntersect[k]*yIntersect[k]);
				dca[k] = -1;	
				
				/*	If the intersection falls within the radial cuts we check the z-axis	*/
				if ((r[k] > tkfpar->r_min) && (r[k] < tkfpar->r_max))
				{
					/*	Get helix parameters at the first intersection point	*/
					targetCoords[0] = xIntersect[k];
					targetCoords[1] = yIntersect[k];
					
					tkf_project_track(coordsOne, radiusOne, targetCoords, newCoords1);
					newTrackOne[k] = tkf_update_track_param(coordsOne, radiusOne, 
															newCoords1, trackOne);
					
					tkf_project_track(coordsTwo, radiusTwo, targetCoords, newCoords2);
					newTrackTwo[k] = tkf_update_track_param(coordsTwo, radiusTwo, 
															newCoords2, trackTwo);
					
					dca[k] = sqrt( pow((newCoords2[0] - newCoords1[0]),2) 
								+ pow((newCoords2[1] - newCoords1[1]),2) 
								+ pow((newTrackTwo[k]->z - newTrackOne[k]->z),2));
								
					if (dca[k] <= tkfpar->max_dist)
					{
						foundVertexCandidate++;
					}
					
				}
			}
			
			/*	Choose the track intersection that minimizes dca	*/

			switch (foundVertexCandidate)
			{
				case 0:
					l = -1;
					break;
				
				case 1:
					if (dca[0] == -1)
					{
						l = 1;
					}
					else
					{
						l = 0;
					}
					break;
				
				case 2:
					if (dca[0] <= dca[1])
					{
						l = 0;
					}
					else
					{
						l = 1;
					}
					break;
			}
			
			foundVertexCandidate = 0;
			
			if (l != -1) 
			{
				tkf_track_mom(newTrackOne[l], 0, trackOneMomentum);
				tkf_track_mom(newTrackTwo[l], 0, trackTwoMomentum);
				
				p = sqrt(trackOneMomentum[0]*trackOneMomentum[0] 
							+ trackOneMomentum[1]*trackOneMomentum[1] 
							+ trackOneMomentum[2]*trackOneMomentum[2]);
				
				pd = sqrt(trackTwoMomentum[0]*trackTwoMomentum[0] 
							+ trackTwoMomentum[1]*trackTwoMomentum[1] 
							+ trackTwoMomentum[2]*trackTwoMomentum[2]);
				
				pDotpd = trackOneMomentum[0]*trackTwoMomentum[0] 
							+ trackOneMomentum[1]*trackTwoMomentum[1] 
							+ trackOneMomentum[2]*trackTwoMomentum[2];
				if( (p*pd)!= 0. )
                                  theta = acos(pDotpd/(p*pd))*radToDeg;
				else
                                  theta = 0.;

				if ((theta > tkfpar->theta_min) && (tkfout_h->nok < tkfout_h->maxlen) )
				{
			

                                        tkfout_h->nok++;
					m = tkfout_h->nok - 1;			/* 	Number of tracks currently */
					tkfout[m].id = m + 1;	
					tkfout[m].dca = dca[l];			/*	DCA value	*/

				
					tkfout[m].x[0] = xIntersect[l];	/*	Vertex Coordinates	*/
					tkfout[m].x[1] = yIntersect[l];
					tkfout[m].x[2] = (newTrackTwo[l]->z + newTrackOne[l]->z)/2;
				
					tkfout[m].r = r[l];

					parentTrack = NULL;
					daughterTrack = NULL;
				
										
					if ( (trackOne->r) < (trackTwo->r) )
					{
						parentTrack = newTrackOne[l];
						daughterTrack = newTrackTwo[l];
					
						tkfout[m].itrk_p = parentTrack->id;
						tkfout[m].itrk_d = daughterTrack->id;
						tkfout[m].p[0] = trackOneMomentum[0];
						tkfout[m].p[1] = trackOneMomentum[1];
						tkfout[m].p[2] = trackOneMomentum[2];
						tkfout[m].pd[0] = trackTwoMomentum[0];
						tkfout[m].pd[1] = trackTwoMomentum[1];
						tkfout[m].pd[2] = trackTwoMomentum[2];
						tkfout[m].dcap  = dca1;
						tkfout[m].dcad  = dca2;
                                                tkfout[m].npointp = parentTrack->nhits;	
                                                tkfout[m].npointd = daughterTrack->nhits;	
                                                tkfout[m].sp = trackOne->r;
                                                tkfout[m].sd = trackTwo->r;

      					}
					else
					{
						daughterTrack = newTrackOne[l];
						parentTrack = newTrackTwo[l];
					
						tkfout[m].itrk_p = parentTrack->id;
						tkfout[m].itrk_d = daughterTrack->id;
						tkfout[m].pd[0] = trackOneMomentum[0];
						tkfout[m].pd[1] = trackOneMomentum[1];
						tkfout[m].pd[2] = trackOneMomentum[2];
						tkfout[m].p[0] = trackTwoMomentum[0];
						tkfout[m].p[1] = trackTwoMomentum[1];
						tkfout[m].p[2] = trackTwoMomentum[2];
						tkfout[m].dcap  = dca2;
						tkfout[m].dcad  = dca1;
                                                tkfout[m].npointp = parentTrack->nhits;	
                                                tkfout[m].npointd = daughterTrack->nhits;	
                                                tkfout[m].sd = trackOne->r;
                                                tkfout[m].sp = trackTwo->r;
					}
					tkfout[m].mcid_p = parentTrack->mc_id;
					tkfout[m].mcid_d = daughterTrack->mc_id;
                                        tkfout[m].lp = parentTrack->lr;
					tkfout[m].ld = daughterTrack->lr;
                                        tkfout[m].dlf= fabs(tkfout[m].lp - tkfout[m].sd);
                                        tkfout[m].dlv= fabs(tkfout[m].lp - tkfout[m].r);

                                        if (daughterTrack->mc_id == 0 )
                                          {
                                            tkfout[m].itrue = -5;
                                            tkfout[m].x_true[0] = 0.;
                                            tkfout[m].x_true[1] = 0.;
                                            tkfout[m].x_true[2] = 0.;
                                          }
                                        else
                                          {
                                            tkfout[m].itrue = 0;
                                            iv1 = g2t_track[daughterTrack->mc_id-1].start_vertex_p;  /* old .ivor */
                                            iv2 = g2t_vertex[iv1-1].parent_p;  /* old .orgtrk */
                                            if ( (parentTrack->mc_id == iv2) && (iv1 != 1) )
                                              {
                                                tkfout[m].itrue = 1;
                                                tkfout[m].x_true[0] = g2t_vertex[iv1-1].ge_x[0];
                                                tkfout[m].x_true[1] = g2t_vertex[iv1-1].ge_x[1];
                                                tkfout[m].x_true[2] = g2t_vertex[iv1-1].ge_x[2];
                                                tkfout[m].mcpidp =  g2t_track[parentTrack->mc_id-1].ge_pid; /* old .ipart */
                                                tkfout[m].mcpidd =  g2t_track[daughterTrack->mc_id-1].ge_pid;
                                              }
                                          }
				




					/*	Calculate pt and pz for the parent and daughter particles	*/
				
					tkfout[m].pt = sqrt(tkfout[m].p[0]*tkfout[m].p[0] + 
										tkfout[m].p[1]*tkfout[m].p[1]);
					tkfout[m].ptd = sqrt(tkfout[m].pd[0]*tkfout[m].pd[0] + 
										tkfout[m].pd[1]*tkfout[m].pd[1]);
				
					tkfout[m].pz = tkfout[m].p[2];
					tkfout[m].pzd = tkfout[m].pd[2];
				
					/* Calculate the theta decay angle (lab frame)	*/
				
					tkfout[m].theta = theta;
				
					/* calculate daughter mass	*/
				
                                        p = sqrt(tkfout[m].p[0]*tkfout[m].p[0] + tkfout[m].p[1]*tkfout[m].p[1]
                                               + tkfout[m].p[2]*tkfout[m].p[2]);
				        pd =  sqrt(tkfout[m].pd[0]*tkfout[m].pd[0] + tkfout[m].pd[1]*tkfout[m].pd[1]
                                               +   tkfout[m].pd[2]*tkfout[m].pd[2]);

					pn = sqrt(p*p + pd*pd - 2.*pDotpd);	/* neutrino etc. p*/
				
					deltaKaonMuon = fabs(sqrt(p*p + kaonMass*kaonMass)   -
                                                             sqrt(pd*pd + muonMass*muonMass) - pn);
					deltaKaonPion = fabs(sqrt(p*p + kaonMass*kaonMass)   -
                                                             sqrt(pd*pd + pionMass*pionMass) - 
                                                             sqrt(pn*pn + pi0Mass*pi0Mass));
                                        deltaPionMuon = fabs(sqrt(p*p + pionMass*pionMass)   -
                                                             sqrt(pd*pd + muonMass*muonMass) - pn);
                                        tkfout[m].hypo[0] = deltaKaonMuon;
                                        tkfout[m].hypo[1] = deltaKaonPion;
                                        tkfout[m].hypo[2] = deltaPionMuon;

				
					/*	Assign Particle ID to daughter and calculate theta_cm	*/
					if( (deltaKaonPion < deltaKaonMuon) && (deltaKaonPion < deltaPionMuon) )
					{
						tkfout[m].theta_cm = radToDeg*asin((pd/KaonToPionQ)*sin(tkfout[m].theta*degToRad));
                                                if( (parentTrack->qR) > 0. )
                                                  {
                                                    tkfout[m].pidd = 8; 	/*	Pion +	*/
                                                    tkfout[m].pidp = 11;
                                                  }
                                                else
                                                  {
                                                    tkfout[m].pidd = 9;         /*      Pion -  */
                                                    tkfout[m].pidp = 12;
                                                  }
					}
					else if( (deltaKaonMuon < deltaKaonPion) && (deltaKaonMuon < deltaPionMuon) )
                                        {
						tkfout[m].theta_cm = radToDeg*asin((pd/KaonToMuonQ)*sin(tkfout[m].theta*degToRad));
                                                if( (parentTrack->qR) > 0. )
                                                  {
                                                    tkfout[m].pidd = 5;         /*      Muon +  */
                                                    tkfout[m].pidp = 11;
                                                  }
                                                else
                                                  {
                                                    tkfout[m].pidd = 6;         /*      Muon -  */
                                                    tkfout[m].pidp = 12;
                                                  }
 					}
                                        else
                                        {
                                                tkfout[m].theta_cm = radToDeg*asin((pd/PionToMuonQ)*sin(tkfout[m].theta*degToRad));
                                                if( (parentTrack->qR) > 0. )
                                                  {
                                                    tkfout[m].pidd = 5;         /*      Muon +  */
                                                    tkfout[m].pidp = 8;
                                                  }
                                                else
                                                  {
                                                    tkfout[m].pidd = 6;         /*      Muon -  */
                                                    tkfout[m].pidp = 9;
                                                  }
                                         }

                                  } /*  theta>min */
				free((void *) newTrackOne);
				free((void *) newTrackTwo);   
                              } /* l != -1   */
                      }  /* j=i+1 to tracksfound   */
              }  /* i=1 to tracksfound   */
      
	free((void *) localTracks);
	/*	Return a normal sucess code	*/
	
	return returnCode;
}
