/*------------------------------------------------------------------
FILE:         scs_fitter.c
DESCRIPTION:  scs - cluster finder - determine space points
AUTHOR:       Dave Read, Claude Pruneau
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      4/18/95 created  by Dave Read 
              8/13/95 Update with moment-analysis code by Dave Read 
              8/15/96 adapted to STAF by C.Pruneau
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long scs_fitter
DESCRIPTION:  determine space points

 scs_fitter take clusters from the svt_clus table and turn them into entries in the
 spt table.  
 
 scs_finder first calls a routine which calculates the first and
 second moments of the charge distribution in a cluster.  These moments
 are used to determine whether the cluster should be processed as an
 isolated hit or not.

ARGUMENTS:    par   : scs parameters
ARGUMENTS:    map   : 8 to 10 bit map - expansion map
ARGUMENTS:    geom  : geom of the detector
ARGUMENTS:    seq   : sequence table from DAQ or SSS
ARGUMENTS:    adc   : actual ADC alues from DAQ or SSS
ARGUMENTS:    clus  : cluster list produced by scs_finder
ARGUMENTS:    merge : merged cluster list
ARGUMENTS:    spt   : space point table

RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include "scs_fitter.h"

#define ANALYSIS_OK      0
#define ANALYSIS_FAILED  1
#define DAQ_INPUT        0
#define MONTECARLO_INPUT 1

#define SQR(a)  ((a)*(a))
#define CUBE(a) ((a)*(a)*(a))
#define PI 3.1415926535
/******************************************************************/
/*   Prototype Function Definition                                */
/******************************************************************/
int Is_Merged_Cluster (SCS_CLUSTER_ST   *, SCS_PAR_ST  *par);
int Deconvolve_Cluster (SCS_CLUSTER_ST  *clus, 
			SVG_GEOM_ST     *geom,
			SVG_SHAPE_ST    *shape,
			SCS_PAR_ST      *par,
			TABLE_HEAD_ST *seq_h,     SSF_SEQ_ST   *seq ,
			TABLE_HEAD_ST *mv_h,       SSF_MV_ST   *mv ,
			TABLE_HEAD_ST *adc_h,     SSF_ADC_ST   *adc ,
			TABLE_HEAD_ST *spt_h,     SCS_SPT_ST   *spt,
			TABLE_HEAD_ST *merge_h, SCS_MERGE_ST   *merge );
int Find_Spt_MC_Hit (SCS_CLUSTER_ST  *clus,
		     TABLE_HEAD_ST *seq_h,   SSF_SEQ_ST   *seq ,
		     TABLE_HEAD_ST *mv_h,    SSF_MV_ST    *mv);
int Fill_Spt (SCS_PAR_ST   *par,
	      SVG_GEOM_ST  *geom,
	      SVG_SHAPE_ST *shape,
	      SCS_CLUSTER_ST  *clus, 
	      TABLE_HEAD_ST *spt_h,   SCS_SPT_ST   *spt);

int Calculate_Cluster_Moments (TABLE_HEAD_ST  *seq_h,       SSF_SEQ_ST  *seq,
			       TABLE_HEAD_ST  *adc_h,       SSF_ADC_ST  *adc,
			       TABLE_HEAD_ST  *map_h,  SSF_8TO10MAP_ST  *map,
			       SCS_CLUSTER_ST    *clus);

/***************************************************************************/

long type_of_call scs_fitter_(
		 TABLE_HEAD_ST            *par_h,        SCS_PAR_ST              *par ,
		 TABLE_HEAD_ST            *map_h,   SSF_8TO10MAP_ST              *map ,
		 TABLE_HEAD_ST           *geom_h,       SVG_GEOM_ST             *geom ,
		 TABLE_HEAD_ST          *shape_h,      SVG_SHAPE_ST            *shape ,
		 TABLE_HEAD_ST            *seq_h,        SSF_SEQ_ST              *seq ,
		 TABLE_HEAD_ST             *mv_h,         SSF_MV_ST               *mv ,
		 TABLE_HEAD_ST            *adc_h,        SSF_ADC_ST              *adc ,
		 TABLE_HEAD_ST           *clus_h,    SCS_CLUSTER_ST             *clus ,
		 TABLE_HEAD_ST          *merge_h,      SCS_MERGE_ST            *merge ,
		 TABLE_HEAD_ST            *spt_h,        SCS_SPT_ST              *spt ) 
{
   int         iCluster, iStatus;
   SCS_CLUSTER_ST *theClus;

   /* Loop on all clusters in the table */

   for (iCluster = 0; iCluster < clus_h->nok; iCluster++)
     {
       theClus = clus + iCluster;

       /* calculate the moments, if succesful, proceed to 
	  analyse the cluster as a merge on non merge cluster */

       if (Calculate_Cluster_Moments(seq_h,seq, adc_h,adc, map_h,map, theClus))
	 {
	   if (Is_Merged_Cluster(theClus, par))
	     {
	       Deconvolve_Cluster (theClus, geom, shape, par,
				   seq_h,   seq,
				   mv_h,    mv,
				   adc_h,   adc,
				   spt_h,   spt,
				   merge_h, merge);
	     }
	   else
	     {
	       if (par[1].input==MONTECARLO_INPUT) 
		 Find_Spt_MC_Hit (theClus, seq_h, seq, mv_h, mv);
	       Fill_Spt (par, geom, shape, theClus, spt_h, spt);
	     }
	 }
     }
   return STAFCV_OK;
   }

/*------------------------------------------------------------------
ROUTINE:      long Find_Spt_Mhit
DESCRIPTION:  Find the MonteCarlo hit associated with a space point.

Running this routine is obviously meaningful only while processing
monte carlo data. 

ARGUMENTS:    clus  : pointer to the current cluster
ARGUMENTS:    seq   : sequence table
ARGUMENTS:    mv    : analog table produced by sss


RETURN VALUE: 
------------------------------------------------------------------*/
int Find_Spt_MC_Hit (SCS_CLUSTER_ST *clus,
		     TABLE_HEAD_ST *seq_h,  SSF_SEQ_ST  *seq,
		     TABLE_HEAD_ST *mv_h,   SSF_MV_ST   *mv)
{
  
  /* this routine is not ready - it will require some thinking
     so right now I feed "0" as a key to indicate that it is 
     not known.
     */
  
  clus->key_mchit = 0;
  
  return 0;
}


/*------------------------------------------------------------------
ROUTINE:      long Calculate_Cluster_Moments
DESCRIPTION:  calculates the zeroth, first and second moments of the charge 
              distribution of a cluster.
ARGUMENTS:    clus  : pointer to the current cluster record
              seq   : table of sequences output by DAQ or SSF
	      adc   : table of ADC values output by DAQ or SSF
RETURN VALUE: 
------------------------------------------------------------------*/
int Calculate_Cluster_Moments (TABLE_HEAD_ST  *seq_h,       SSF_SEQ_ST  *seq,
			       TABLE_HEAD_ST  *adc_h,       SSF_ADC_ST  *adc,
			       TABLE_HEAD_ST  *map_h,  SSF_8TO10MAP_ST  *map,
			       SCS_CLUSTER_ST    *clus)
{
  int    i, 
    j,
    iSum       = 0,
    iDriftMom1 = 0,
    iAnodeMom1 = 0,
    iNumPixels = 0,
    iStartTime, iStartAnode, iLen, iADC, iKey,  iSeq;
  int    iDrift, iAnode;
  float  fMom0=0;
  float  fDriftMom1 = 0;
  float  fDriftMom2 = 0;
  float  fAnodeMom1 = 0;
  float  fAnodeMom2 = 0;
  float  fFittedCharge;
  float fAmp, fSigmaA2, fSigmaD2;
  float fDiffA, fDiffD, fExp;
  
  SSF_SEQ_ST *theSeq;
  
  /*  Start by calculating the 1st moment.  */
  /*  Get the first sequence in the cluster */

  iSeq = clus->key_first;             
  
  /*  Now record where this cluster started so all sequences
   *  in the cluster can be referenced to this position
   */ 
  
  iStartTime  = seq[iSeq].t_beg;
  iStartAnode = seq[iSeq].anode_id;
  
  for (i = 0; i < clus->n_seq; i++)
    {
      theSeq = seq + iSeq;
      iLen = theSeq->t_end - theSeq->t_beg + 1;
      iNumPixels += iLen;
      
      iKey = theSeq->key_adc;
      for (j = 0; j < iLen; j++)
	{
	  /* First, convert the ADC value back to a 10-bit value */
	  iADC         = map[adc[iKey].adc].adc10;
	  iDrift       = theSeq->t_beg + j - iStartTime;
	  iAnode       = theSeq->anode_id  - iStartAnode;
	  iSum        += iADC;
	  iDriftMom1  += iADC * iDrift;
	  iAnodeMom1  += iADC * iAnode;
	  fDriftMom2  += iADC * SQR(iDrift);
	  fAnodeMom2  += iADC * SQR(iAnode);
	  iKey++;
	}
      iSeq = seq[iSeq].key_next;
    }
  
  /*  Calculate the 0th, 1st, and 2nd moments     */
  /*   remembering to include the cluster         */
  /*  position offset (which was removed earlier) */
  
  fMom0      = (float) iSum;
  fDriftMom1 = (float) iDriftMom1/fMom0;
  fAnodeMom1 = (float) iAnodeMom1/fMom0;
  fDriftMom2 = fDriftMom2/fMom0 - SQR(fDriftMom1);
  fAnodeMom2 = fDriftMom2/fMom0 - SQR(fAnodeMom1);
  fDriftMom1 += iStartTime;
  fAnodeMom1 += iStartAnode;
  
  /* Try for a quick estimate of charge                          */
  /* This next loop finds the ADC value for the pixel closest    */
  /* to the cluster centroid.  Start by looping over all anodes. */
  
  iSeq = clus->key_first;
  for (i = 0; i < clus->n_seq; i++)
    {
      theSeq = seq + iSeq;
      
      /* Is this the central anode ? */
      if (theSeq->anode_id > fAnodeMom1 - 0.5 &&
	  theSeq->anode_id < fAnodeMom1 + 0.5)
	{
	  iKey = theSeq->key_adc + ((int) fDriftMom1 - theSeq->t_beg);
	  
	  /* Convert the ADC value back to a 10-bit value */
	  iADC = map[adc[iKey].adc].adc10;
	  
	  /* Calculate differences between centroid position and actual
	     positions of the anode & time bin */
	  
	  fDiffA = theSeq->anode_id - fAnodeMom1;
	  fDiffD = fDriftMom1 - (int) fDriftMom1;
	  
	  break;
	}
      
      iSeq = seq[iSeq].key_next;
    }
  
  /* Q & D estimate for amplitude */
  fAmp = iADC;
  
  /* Assume gaussian form; use 2nd moment to get rough
     values for the sigmas */
  fSigmaA2 = fAnodeMom2;
  fSigmaD2 = fDriftMom2;
  
  /* Now we can calculate a better amplitude */
  fExp  = exp(-0.5 * SQR(fDiffA) / fSigmaA2);
  fExp *= exp(-0.5 * SQR(fDiffD) / fSigmaD2);
  fAmp  = (float) iADC / fExp;
  
  /* Use better amplitude to get better sigma  */
  
  fSigmaA2 = fAnodeMom2;
  fSigmaD2 = fDriftMom2;
  
  /* Finally calculate total charge under the gaussian */
  fFittedCharge = 2 * PI * fAmp * sqrt((double)fSigmaA2) * 
                        sqrt ((double)fSigmaD2);
  
  clus->x[0]    = fDriftMom1;
  clus->x[1]    = fAnodeMom1;
  clus->x[2]    = 0.0229859;
  clus->dx[0]   = 0.0;
  clus->dx[1]   = 0.0;
  clus->dx[2]   = 0.0;
  clus->de[0]   = fFittedCharge;
  clus->de[1]   = (int) sqrt ((double) iNumPixels);
  clus->mom0    = fMom0;
  clus->mom2[0] = fDriftMom2;
  clus->mom2[1] = fAnodeMom2;
  
  return ANALYSIS_OK;
}

/*------------------------------------------------------------------
ROUTINE:      long Is_Merged_Cluster
DESCRIPTION:  determines whether a cluster is suspected to contain
  more than one hit by looking at the second moments. This routine
  is called by the fitter and should NOT be called by the shell
  as it is not strictly compliant with STAF calling structure.
  Clus is meant to point a specific (current) cluster rather than
  the top of the table. Same goes with "par" which passes the cut
  parameter used for the definition of merged on non merged.

ARGUMENTS:    Clus : pointer to a cluster record (not a table)
              par  : pointer to a scs parameter record (not table)
RETURN VALUE: 
------------------------------------------------------------------*/
int Is_Merged_Cluster (SCS_CLUSTER_ST *Clus, SCS_PAR_ST *par)
{
  float fCut;
  
  fCut = fabs(par->cuts[1] * Clus->mom2[1] - Clus->mom2[0] - par->cuts[0]);
  
  /*  Does it make the cut for being a single hit? */
  
  if ( (fCut <= par->cuts[2] && Clus->mom2[0] < par->cuts[3] && Clus->mom2[1] < par->cuts[4]) ||
       (Clus->mom2[0] < par->cuts[5]  && Clus->mom2[1] < par->cuts[6]) )
    return  0;      /* It made the cuts; return 0 */
  else
    /* It failed the cut, so return -1  */
    return -1;
}


/*------------------------------------------------------------------
ROUTINE:      long fill_spt
DESCRIPTION:  the routine which actually does the conversion from
 centroids (in drift and transverse direction) to coordinates on the
 wafer face, and then to coordinates in global STAR units.
 
 This routine is called from scs_fitter and should not be called
 from the shell directly as it does not use the full calling
 structures.

 It is assumed that the proper "geom" record is passed to the routine
 so there is no need to pass the full table but only the active
 record. Same applies for the shape table.

ARGUMENTS:    Cl           (pointer to CLUSTER struct)
             iCluster     (integer number of cluster in cluster table)
             geom     (SVT geometry table)
             shape    (SVT shape table (SDD shapes))
 
   Output:   spt      (SVT Space Point table)
RETURN VALUE: 
------------------------------------------------------------------*/
int Fill_Spt (SCS_PAR_ST     *par,
	      SVG_GEOM_ST    *geom,
	      SVG_SHAPE_ST   *shape,
	      SCS_CLUSTER_ST *clus,
	      TABLE_HEAD_ST  *spt_h, SCS_SPT_ST    *spt)
{
  int    iSpt, iCluster;
  float  zero[3], x[3], distance;
  float  fCorrection;
  
  /*  Make sure there's room in the spt table! */
  
  iSpt = spt_h->nok;
  if (iSpt >= spt_h->maxlen)
    return ANALYSIS_FAILED;

  /* convert time buckets to cm and subtract guard region 
     Use the anode index to determine side of the wafer
     is being dealt with and if the offset has to be
     added or subtracted
     */
  
  if (clus->anode <= 240)
    distance  = shape->shape[0] - 0.1*clus->x[0]*par->v_drift/par->freq;
  else
    distance  =  0.1*clus->x[0]*par->v_drift/par->freq - shape->shape[0];
  
  
  /*  This correction accounts for the PASA response time,
   *  and is a purely empirical correction based on plots of
   *  the drift position errors.  As the fit was done in micron
   *  the value is multiplied by 1.9e-4 to convert in "cm"
   */
  
  fCorrection = 1.0e-4 * (par->drift_corr[0]
			  + par->drift_corr[1] * distance
			  + par->drift_corr[2] * SQR(distance)
			  + par->drift_corr[3] * CUBE(distance) );
  
  clus->x[0] += fCorrection;  
  
  /*  Adjust the anode position by 0.5 anodes  
      and convert to cm
      */
  
  clus->x[1] = shape->shape[1] - (clus->x[1]+0.5) * par->anode_pitch / 10.0;

  /* fill the spt record/table*/

  spt[iSpt].id         = iSpt;
  spt[iSpt].id_cluster = clus->id;
  spt[iSpt].de[0]      = clus->de[0];
  spt[iSpt].de[1]      = clus->de[1];
  spt[iSpt].id_globtrk = -1;
  spt[iSpt].id_match   = -1;
  spt[iSpt].id_mchit   = clus->key_mchit;
  spt[iSpt].id_mctrack = -1;
  spt[iSpt].id_track   = -1;
  spt[iSpt].id_wafer   = geom->id;
  spt[iSpt].xl[0]      = clus->x[0];  /* local coordinates */
  spt[iSpt].xl[1]      = clus->x[1];
  spt[iSpt].xl[2]      = clus->x[2];
  spt[iSpt].cov[0]     = 0.;
  spt[iSpt].cov[1]     = 0.;
  spt[iSpt].cov[2]     = 0.;
  /*spt[iSpt].iAnodes    = clus->iAnodes;*/
  spt[iSpt].mom2[0]    = clus->mom2[0];
  spt[iSpt].mom2[1]    = clus->mom2[1];
  
  /*  Convert local wafer coordinates to 
   *  STAR by calling the svtltog() routine.
   *  Do both position "x" and error "dx"
   */
  
  /*  zero[0]=zero[1]=zero[2]=0.; */
  /*svtltog_ (clus->x,   spt[iSpt].x,   geom->x, geom->d, geom->t, geom->n); */
  /*svtltog_ (clus->dx, spt[iSpt].cov, zero,    geom->d, geom->t, geom->n); */
  
  /* update the number of OK records */

  spt_h->nok++; 
  
  return ANALYSIS_OK;
}

typedef struct POINT_TYPE
{
  long  x,y;
  float val;
} scsPOINT;


void      free_matrix ();
int     **malloc_matrix (int iRows, int iCols);
scsPOINT    *Find_Peaks ();
int       Compare_Point ();
int       IsValidPeak (int **Pixels, int iRows, int iCols, scsPOINT  *Peaks, int iNumPeaks);


/*------------------------------------------------------------------
ROUTINE:      long 
DESCRIPTION:  
ARGUMENTS:
RETURN VALUE: 
------------------------------------------------------------------*/
int Deconvolve_Cluster (SCS_CLUSTER_ST  *clus, 
			SVG_GEOM_ST     *geom,
			SVG_SHAPE_ST    *shape,
			SCS_PAR_ST      *par,
			TABLE_HEAD_ST *seq_h,   SSF_SEQ_ST   *seq ,
			TABLE_HEAD_ST *mv_h,    SSF_MV_ST    *mv ,
			TABLE_HEAD_ST *adc_h,   SSF_ADC_ST   *adc ,
			TABLE_HEAD_ST *spt_h,   SCS_SPT_ST   *spt,
			TABLE_HEAD_ST *merge_h, SCS_MERGE_ST *merge )
{
  int   i, iSeq, iRows, iFirstTime, iLastTime, iFirstAnode, iLastAnode, iCols, iNumPeaks;
  
  scsPOINT         *Peaks;
  SSF_SEQ_ST    *theSeq;
  int **Pixels,   **Shadow;
  long iCluster;


  /* How big an array do we need in order to process this puppy? */
  
  iRows    = clus->n_seq;
  iSeq     = clus->key_first;
  iCluster = clus->id;

  theSeq = seq + iSeq;
  iFirstTime    = theSeq->t_beg;
  iLastTime     = theSeq->t_end;
  iFirstAnode   = theSeq->anode_id;
  iLastAnode    = theSeq->anode_id;
  
  for (i = 0; i < clus->n_seq; i++)
    {
      theSeq = seq + iSeq;
      if (theSeq->t_beg < iFirstTime)
	iFirstTime = theSeq->t_beg;
      if (theSeq->t_end > iLastTime)
	iLastTime = theSeq->t_end;
      if (theSeq->anode_id < iFirstAnode)
	iFirstAnode = theSeq->anode_id;
      if (theSeq->anode_id > iLastAnode)
	iLastAnode = theSeq->anode_id;
      iSeq = theSeq->key_next;
    }
  
  /*  OK, we're ready:  Enlarge the array by one bin all around
   *  so we don't have to worry about over-writing the edges...
   */
  
  iFirstAnode -= 1;
  iLastAnode  += 1;
  iFirstTime  -= 1;
  iLastTime   += 1;
  
  iCols = iLastTime  - iFirstTime  + 1;
  iRows = iLastAnode - iFirstAnode + 1;
  
  /* Allocate memory for the arrays */
  
  Pixels = malloc_matrix (iRows, iCols);
  Shadow = malloc_matrix (iRows, iCols);
  
  /* Check that we got the memory we wanted... */
  
  if (Pixels == NULL || Shadow == NULL)
    {
      if (Pixels)
	free_matrix (Pixels, iRows);
      if (Shadow)
	free_matrix (Shadow, iRows);
      return 0;
    }
  
  /* Now fill the pixel array with pixels from the cluster */
  
  Fill_Pixel_Array (Pixels, iFirstTime, iFirstAnode, clus, seq, adc);
  
  /*
    PrintPixels (Pixels, iRows, iCols);
    */
  
  /* Count how many peaks there are */
  
  iNumPeaks = 0;
  Peaks = Find_Peaks (Pixels, Shadow, iRows, iCols, &iNumPeaks);
  
  /* If there is only one peak, just place it in the space-point table */
  
  if (iNumPeaks == 1)
    {
      if (par[1].input==MONTECARLO_INPUT) 
	Find_Spt_MC_Hit (clus, seq_h, seq, mv_h, mv);
      Fill_Spt (par, geom, shape, clus, spt_h, spt);
    }
  
  /* Otherwise try to deconvolve these */
  
  else if (Peaks && iNumPeaks > 1)
    Fit_Peaks (Pixels, iRows, iCols, iFirstAnode, iFirstTime, 
	      iNumPeaks, Peaks, geom, shape, par, clus,
	      spt_h,   spt,
	      merge_h, merge);
  
  /* Now release all memory allocated during all of this...*/
  
  free_matrix (Pixels, iRows);
  free_matrix (Shadow, iRows);
  
  return 0;
}

/*------------------------------------------------------------------
ROUTINE:      scsPOINT * Find_peaks
DESCRIPTION:  find peaks within the local array
RETURN VALUE: 
------------------------------------------------------------------*/
#define MAX_PEAKS 10
scsPOINT * Find_Peaks (int  **Pixels,
		   int  **Shadow,
		   int    iRows,
		   int    iCols,
		   int   *iNumPeaks)
{
  scsPOINT *Array;
  static scsPOINT Peaks[MAX_PEAKS];
  int i, j, k;
  
  Array = (scsPOINT *) malloc (iRows * iCols * sizeof (scsPOINT));
  if (Array == NULL)
    {
      *iNumPeaks = 0;
      return NULL;
    }
  
  k = 0;
  for (i = 1; i < iRows - 1; i++)
    for (j = 1; j < iCols - 1; j++)
      {
	Array[k].x = i;
	Array[k].y = j;
	Array[k].val = Pixels[i][j];
	k++;
      }
  qsort (Array, k, sizeof (scsPOINT), Compare_Point);
  
  /* Now lets go find the peaks */
  
  *iNumPeaks = 0;
  for (i = 0; i < k && *iNumPeaks < MAX_PEAKS; i++)
    {
      int x = Array[i].x;
      int y = Array[i].y;
      
      if (Shadow[x][y] == 0 && Pixels[x][y] > 20)
	{
	  Peaks[*iNumPeaks].x = x;
	  Peaks[*iNumPeaks].y = y;
	  Peaks[*iNumPeaks].val = Array[i].val;
	  
	  if (IsValidPeak (Pixels, iRows, iCols, Peaks, *iNumPeaks))
            *iNumPeaks += 1;
	}
      
      /* Make sure that these pixels never get used again */
      
      BlockOut (Shadow, x, y);
    }
  
  free (Array);
  
  return Peaks;
}

/*------------------------------------------------------------------
ROUTINE:      int Compare_Point
DESCRIPTION:  evaluate the diff between two points
RETURN VALUE: the amplitude difference
------------------------------------------------------------------*/
int Compare_Point (scsPOINT *a, scsPOINT *b)
{
  return b->val - a->val;
}

#define PEAK_TO_VALLEY 2.0
/*------------------------------------------------------------------
  ROUTINE:      long IsValidPeak 
  DESCRIPTION:  tries to determine if a candidate peak is a "real"
  one.  The cut is a peak-to-valley ratio cut.  The p/v ratio is 
  determined between each candidate peak and
  all previously-found peaks by walking the 2-D line between each
  combination of peaks, looking for the 'valley' along the way.
  If a candidate peak passes the p/v cut against all previous peaks
  then it is accepted.
  ARGUMENTS:
  RETURN VALUE: 
  ------------------------------------------------------------------*/
int IsValidPeak (int    **Pixels, 
		 int      iRows, 
		 int      iCols, 
		 scsPOINT   *Peaks, 
		 int      iNumPeaks)
{
  int Px = Peaks[iNumPeaks].x,
    Py = Peaks[iNumPeaks].y,
    i;
  
  for (i = 0; i < iNumPeaks; i++)
    {
      int    dx, dy, val, peak, valley;
      float  fSlope, fRatio;
      
      dx = Peaks[i].x - Px;
      dy = Peaks[i].y - Py;
      
      fSlope = (float) dx / (float) dy;
      
      peak = valley = Pixels[Px][Py];
      for (; dx > 0; dx--)
	{
	  dy = fSlope * dx;
	  val = Pixels[Px + dx][Py + dy];
	  if (val < valley)
	    valley = val;
	}
      
      fRatio = (float) peak / (float) val;
      if (fRatio < PEAK_TO_VALLEY)
	return 0;
    }
  
  return 1;
}
/*------------------------------------------------------------------
  ROUTINE:      long Fit_Peaks
  DESCRIPTION:  Fit the peaks in a merge configuration
  ARGUMENTS:
  RETURN VALUE: 
  ------------------------------------------------------------------*/

int Fit_Peaks (int             *Pixels[],
	       int             iRows,
	       int             iCols,
	       int             iFirstAnode,
	       int             iFirstTime,
	       int             iNumPeaks,
	       scsPOINT           *Peaks,
	       SVG_GEOM_ST     *geom,
	       SVG_SHAPE_ST    *shape,
	       SCS_PAR_ST      *par,
	       SCS_CLUSTER_ST  *clus,
	       TABLE_HEAD_ST   *spt_h,         SCS_SPT_ST *spt,
	       TABLE_HEAD_ST   *merge_h,     SCS_MERGE_ST *merge)
{
  SCS_CLUSTER_ST Cl2;
  
  int i, j, k, iSum, iX, iY;
  float fX, fY;
  float x[2][2];
  int iAmp[2];
  
  for (i = 0; i < iNumPeaks; i++)
    {
      if (Peaks[i].x < 0 || Peaks[i].x > iRows)
	continue;
      if (Peaks[i].y < 0 || Peaks[i].y > iCols)
	continue;
      
      iSum = iX = iY = 0;
      for (j = Peaks[i].x - 1; j <= Peaks[i].x + 1; j++)
	for (k = Peaks[i].y - 1; k <= Peaks[i].y + 1; k++)
	  {
            iSum += Pixels[j][k];
            iX   += j * Pixels[j][k];
            iY   += k * Pixels[j][k];
	  }
      fX = (float) iX / iSum;
      fY = (float) iY / iSum;
      
      Cl2.x[0] = fY + iFirstTime;
      Cl2.x[1] = fX + iFirstAnode;
      Cl2.x[2] = 0.0229859;
      /*Cl2.id_sdd = Cl->id_sdd;*/
      Cl2.anode = iRows - 2;
      Cl2.mom2[0]    = -1;
      Cl2.mom2[1]    = -1;
      Cl2.dx[0]      = 0.0;
      Cl2.dx[1]      = 0.0;
      Cl2.dx[2]      = 0.0;
      Cl2.key_mchit  = -1;
      Cl2.de[0]      = -1;
      Cl2.de[1]      = -1;
      
      Fill_Spt(par, geom, shape, &Cl2, spt_h, spt);
      
      /*  Now sock away the centroids for later use in evaluation.
       *  Doing this after Fill_Spt() gets us coordinates in cm 
       *  because it leaves converted coords in the Cl struct.
       */
      
      if (i < 2)
	{
	  x[i][0] = Cl2.x[0];
	  x[i][1] = Cl2.x[1];
	  iAmp[i] = Pixels[Peaks[i].x][Peaks[i].y];
	}
    }
  
  /*  Finally, fill a merge struct for this cluster.
   *  The values for the "h1_dx" fields are large so that screwed-up
   *  entries (i.e. unmatched space spt) don't leave bogus peaks at 0 !
   */
  
  k = merge_h->nok;
  merge[k].numpeaks = iNumPeaks;
  merge[k].h1_x[0]     = x[0][0];
  merge[k].h1_x[1]     = x[0][1];
  merge[k].h1_dx[0]    = 10000;
  merge[k].h1_dx[1]    = 10000;
  merge[k].h2_x[0]     = x[1][0];
  merge[k].h2_x[1]     = x[1][1];
  merge[k].h2_dx[0]    = 10000;
  merge[k].h2_dx[1]    = 10000;
  merge[k].h1_amp      = iAmp[0];
  merge[k].h2_amp      = iAmp[1];
  merge[k].id_clus     = clus->id;
  merge[k].x2mom       = clus->mom2[0];
  merge[k].y2mom       = clus->mom2[1];
  merge[k].clus_x[0]   = clus->x[0];
  merge[k].clus_x[1]   = clus->x[1];
  merge_h->nok += 1;
}

/*------------------------------------------------------------------
ROUTINE:      int BlockOut
DESCRIPTION:  mark pixel position as blocked
RETURN VALUE: 0
------------------------------------------------------------------*/
int BlockOut (int   *Shadow[], int x, int y)
{
  Shadow[x-1][y-1] = 1;
  Shadow[x-1][ y ] = 1;
  Shadow[x-1][y+1] = 1;
  
  Shadow[ x ][y-1] = 1;
  Shadow[ x ][ y ] = 1;
  Shadow[ x ][y+1] = 1;
  
  Shadow[x+1][y-1] = 1;
  Shadow[x+1][ y ] = 1;
  Shadow[x+1][y+1] = 1;
  
  return 0;
}

/*------------------------------------------------------------------
ROUTINE:      long Print_Pixels
DESCRIPTION:  debugging tool to print pixel information
RETURN VALUE: 
------------------------------------------------------------------*/
int Print_Pixels (int   *Pixels[], int iRows, int iCols)
{
  int i, j;
  
  for (i = 0; i < iRows; i++)
    {
      for (j = 0; j < iCols; j++)
	printf (" %3i", (int) Pixels[i][j]);
      printf ("\n");
    }
  printf ("\n");
  
  return 0;
}
/*------------------------------------------------------------------
ROUTINE:      int Fill_Pixel_Array
DESCRIPTION:  fill the pixel array with ADC values
RETURN VALUE: 0
------------------------------------------------------------------*/
int Fill_Pixel_Array (int   *Pixels[], 
		      int   iFirstTime,
		      int   iFirstAnode,
		      SCS_CLUSTER_ST   *clus,
		      SSF_8TO10MAP_ST  *map,
		      SSF_SEQ_ST       *seq,
		      SSF_ADC_ST       *adc)
{
  int    i,  j, iSeq, iRow, iCol, iADC, iBins;
  
  SSF_SEQ_ST *theSeq;
  
  iSeq = clus->key_first;
  for (i = 0; i < clus->n_seq; i++)
    {
      theSeq   = seq + iSeq;
      iBins = theSeq->t_end     - theSeq->t_beg + 1;
      iRow  = theSeq->anode_id  - iFirstAnode;
      iCol  = theSeq->t_beg     - iFirstTime;
      iADC  = theSeq->key_adc;
      for (j = 0; j < iBins; j++)
	Pixels[iRow][iCol + j] = map[adc[iADC + j].adc].adc10;
      iSeq = theSeq->key_next;
    }
  
  return 0;
}

/*------------------------------------------------------------------
ROUTINE:      int **malloc_matrix
DESCRIPTION:  allocates memory for matrix to do deconvolution
RETURN VALUE: pointer to the array
ARGUMENTS: iRows, iCols  : size of the array needed 
------------------------------------------------------------------*/
int **malloc_matrix (int iRows, int iCols)
{
  int     **Array;
  int       i, j;
  
  Array = (int **) malloc (iRows * sizeof (int *));
  if (Array == NULL)
    return NULL;
  
  for (i = 0; i < iRows; i++)
    {
      Array[i] = (int *) malloc (iCols * sizeof (int));
      if (Array[i] == NULL)
	{
	  printf ("Error allocating memory for pixel array!");
	  while (--i >= 0)
            free (Array[i]);
	  free (Array);
	  break;
	}
      
      for (j = 0; j < iCols; j++)
	Array[i][j] = 0;
    }
  
  return Array;
}


/*------------------------------------------------------------------
ROUTINE:      void free_matrix
DESCRIPTION:  deallocates memory used by matrix
RETURN VALUE: none
ARGUMENTS:    *Array[] pointer to the array
ARGUMENTS:    iRows    number of rows in the array
------------------------------------------------------------------*/
void free_matrix (void *Array[], int iRows)
{
  int i;
  for (i = iRows - 1; i >= 0; i--)
    if (Array[i])
      free (Array[i]);
  free (Array);
}

