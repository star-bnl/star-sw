/*------------------------------------------------------------------
FILE:         tpt_sts.c
DESCRIPTION:  Straight line tracking.
AUTHOR:       ims - Iwona Sakrejda, isakrejda@lbl.gov
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      00aug96-v000a-hpl- Creation.
              18dec96-v001a-ims- Bug's found by Eric Hjort fixed
              (sqrt was missing in the residual calculation)
              14mar97-v100a-ims-bugs in skipping found by Bill Love 
              corrected by I Sakrejda.
              17 March 1997 WAL kludges in the rest of the sector nums.
               8 April 1997 WAL puts in Iwona's cross_and_dip calls.
               9 April 1997 WAL tries to put in some error calculations.
	      19  June 1997 PGJ added reverse loop over rows.
              26  June 1997 WAL re-adds C' error term and dy sign correction
              14 Novem 1997 RRB
                            * Pull some comparisons out of loops
                            * Pull some calculations out of loops
                            * Treat "done" as logical, instead of integer
                            * Define additional useful constants
                            * Assign hit closest to track, rather than
                              keeping the first allowed hit, if it is closest
                              in x-y OR closest in z.
                            * Outlier rejection loop was able to reject
                              an outlier iff it was the *first* point; fixed
                            * Reformat code for legibility (to me; sorry...)
              25 Novem 1997 RRB
                            * Integrate WAL's dip, crossing-angle fixes.
                            * Integrate WAL's bug fix for lost tracks.
               1 Decem 1997 RRB
                            * Consider nmin when rejecting outliers.
                            * Cut on x-y dist. before calc. z dist. for effic.
			    * Do not check row for a 3rd or subsequent hit
			      if one x,y,z range for the hits in the row
			      indicate that one will not be found;
                              tpt speedup is ~20% for laser events in one sect.
               4 Decem 1997 RRB
                            * Incorporate WAL's nrow/2 -> nrow/3 change
                              to help track-finding efficiency
                            * Apply WAL's elimination of the gap check in a
			      track after it is found (lhole_2).
                            * Apply WAL's elimination of the gap check in a
			      track after deciding to output it (lhole_3).
                            * Apply WAL's az, bz algorithms
			    * Add 1 to npntl*, changing convention
			      to simplify program logic
			    * Walk through hits in a row in increasing order
                              for simplicity/to address memory in order
	                    * Replace checks on "done" with a goto;
                              (simple, but offensive to purists)
               9 Decem 1997 IS, RRB
	                    * Re-do nmin logic: calculate nmin after track
                              is found, applying cut on the larger
			      of par->nmin and (spanned rows)/2.
			    * Require mrow>1 at start of loop for 2nd hit,
			      so that a third hit is possible.
			    * Save pointers for closest hits for efficiency
			    * More if/for/do end-of-structure comments added
              10 Decem 1997 RRB
	                    * Replace float's with double's where round-off
			      error may be a problem.
			    * Change:
			       abs(strack[ntrack].azx+999.0)<0.0001)  -->
			      fabs(strack[ntrack].azx+999.0)<0.0001)

------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      tpt_sts_
DESCRIPTION:  Main straight line tracking routine
ARGUMENTS:    
**:       IN:
**:                par    - Tracking parameters
**:               par_    - 
**:    INOUT:
**:              tphit    - table of TPC coordinate measurements
**:             tphit_    - header Structure for tphit
**:      OUT:
**:                res    - Iwona's residual table
**:               res_    - header Structure for res
**:           diff_res    - Another of Iwona's residual table.
**:          diff_res_    - header Structure for test_res
**:             strack    - straight track fit parametrization.
**:            strack_    - header Structure for strack
**:    RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "tpt_sts.h"
#
/* RRB: Add additional useful constants: */
#define MAX_SECS 24
#define MAX_ROWS 45
#define MAX_HITS 100000
#ifndef TRUE
#define TRUE  1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#
#define tgc_cross_and_dip_ F77_NAME(tgc_cross_and_dip,TGC_CROSS_AND_DIP)
extern void type_of_call tgc_cross_and_dip_(long *,float *,float *,float *);

#define tls_index_sort_i_ F77_NAME(tls_index_sort_i,TLS_INDEX_SORT_I)
extern void type_of_call tls_index_sort_i_(long *, long *,long *,long *,long *);

/* Underscore on next line is necessary to link with FORTRAN. */
long type_of_call tpt_sts_(
  TABLE_HEAD_ST            *par_h,      TPT_SPARS_ST              *par ,
  TABLE_HEAD_ST          *tphit_h,      TCL_TPHIT_ST            *tphit ,
  TABLE_HEAD_ST            *res_h,        TPT_RES_ST              *res ,
  TABLE_HEAD_ST       *diff_res_h,        TPT_RES_ST         *diff_res ,
  TABLE_HEAD_ST         *strack_h,     TPT_STRACK_ST            *strack)
{
/*
 * Here are examples of how you should access table members:
 * *tptpar_h.nok, tptpar[1007]->colName.
 */
    int drow;                          /* Row number increment (+1 or -1) */
    int ldel;                          /* Index of outlier to delete */
    int lkeep;                         /* Counter for kept outliers */
    int l_outlier_keep;                /* Index for kept outliers */
    int max_hole;                      /* par->hole = max row sep. of hits */
    int nshift[MAX_ROWS];              /* Shift needed to delete outliers */
    int outlier[MAX_ROWS];             /* List of outliers */
    int outlier_keep[MAX_ROWS];        /* List of outliers to keep */
    int par_ilimit;                    /* par->ilimit = outlier loop limit */
    int par_nmin;                      /* */
    int row_inner;                     /* Innermost row to analyze */
    int row_outer;                     /* Outermost row to analyze */
    int row_range;                     /* Number of allowed rows */
    int rowa, rowb;                    /* First, last allowed row */
    int sector_prev;                   /* Sector ID in previous loop */
    int use_dety, use_dety_n;          /* TRUE iff using 1/dety, 1/dety_n */

    long dkl, dkl_2, dkl_3;            /* Temp. indices for hits in row */
    long i;                            /* General-purpose index */
    long isector;                      /* Sector index for arrays (0--23) */
    long j, jl, jm, jn, jn_2, jn_3;    /* Offsets by row number for hits */
    long k, kl, km, kn, kn_2, kn_3;    /* Offsets from row offset for hits */
    long len=MAX_HITS;                 /* Dimension of hit arrays */
    long lhole;                        /* Row separation of hits */
    long l, ll;                        /* Loop counters */
    long loc_hit[MAX_HITS];            /* Hit indices, sorted by sector,row */
    long lrow;                         /* Row counter */
    long lrow_last_hit;                /* lrow for last hit found on track */
    long m;                            /* Row index */
    long mrow;                         /* Row counter */
    long n, nn, n_2, n_3;              /* */
    long ndel;                         /* # hits killed in outlier rejection */
    long ndel_try;                     /* First ndel count */
    long nkeep;                        /* # outliers that have to be kept */
    long nmin;                         /* */
    long npntl, npntl_2, npntl_3;      /* Number of points */
    long nres;                         /* Array entry counter for residuals */
    long nrow, nrow_2, nrow_3;         /* Row counter */
    long ntrack;                       /* Track counter */
    long ntrack_p1;                    /* Track counter + 1 */
    long row_count[MAX_ROWS];          /* Hit count by row */
    long row_pnt[MAX_ROWS];            /* Offset by row number for hits */
    long sector;                       /* Sector number (1--24) */
    long skip_row[MAX_SECS][MAX_ROWS]; /* TRUE iff row is to be skipped */

    float A, B, C, D;
    float Az, Bz;
    float Az_norm;                     /* 1.0/sqrt(Az*Az) */
    float a_ddy[MAX_ROWS];             /* Absolute distance to track in xy */
    float a_ddz[MAX_ROWS];             /* Absolute distance to track in z */
    float alpha;                       /* Pad-row crossing angle (radians) */
    float ax, bx;
    float ax_norm;                     /* 1.0/sqrt(ax*ax) */
    float ay, by;
    float ay_norm;                     /* 1.0/sqrt(ay*ay) */
    float az, bz;
    float az_norm;                     /* 1.0/sqrt(az*az) */
    float chisqx, chisqz;
    float cosa;                        /* cosine for local->global rotation */
    float cos_alpha;                   /* cos(alpha) */
    float cross;                       /* Pad-row crossing angle (radians) */
    float d_2[MAX_ROWS];               /* (Distance to track)^2 */
    float d_2_lim;                     /* Dist.^2 to closest rejected hit */
    float dA, dB, dC, dD;
    float daz, dbz;
    float dclosest_2;                  /* (Distance)^2 to closest hit */
    double detx, detx_n;
    double dety, dety_n;
    double detz_n;
    float dip;                         /* Track dip angle (radians) */
    float dmaxy;                       /* (Max. prelim. xy res.) allowed */
    float dmaxy_2;                     /* (Max. prelim. xy res.)^2 allowed */
    float dmaxz;                       /* (Max. prelim.  z res.) allowed */
    float dmaxz_2;                     /* (Max. prelim.  z res.)^2 allowed */
    float dx, dy, dy0, dxy_2;          /* xy residuals */
    float dy_lim;                      /* Limit on xy residual (cm) */
    float dz, dz0, dz_2;               /*  z residuals */
    float dz_lim;                      /* Limit on  z residual (cm) */
    float lambda;                      /* Polar angle of track */
    float par_outlimit;                /* Local value of par->outlimit */
    float row_max_x[MAX_ROWS];         /* Max. x for hit in row */
    float row_min_x[MAX_ROWS];         /* Min. x for hit in row */
    float row_max_y[MAX_ROWS];         /* Max. y for hit in row */
    float row_min_y[MAX_ROWS];         /* Min. y for hit in row */
    float row_max_z[MAX_ROWS];         /* Max. z for hit in row */
    float row_min_z[MAX_ROWS];         /* Min. z for hit in row */
    float sina;                        /* sine for local->global rotation */
    float sumy, sumz;                  /* Temp. summing variables */
    double sx, sy, sz;
    double sxy, sxz, syz;
    double sxx, syy, sw;
    float track_dx,track_dy,track_dz;  /* Temp. values of track[]->dx,dy,dz */
    float vtracky[3];
    float xm[MAX_ROWS], ym[MAX_ROWS];  /* Global x,y position of hit */
    float xc, x1, x2, x3;
    float x_proj_min_y;                /* x corresponding to row_min_y */
    float x_proj_max_y;                /* x corresponding to row_max_y */
    float yc, y1, y2, y3;
    float y_proj_min_x;                /* y corresponding to row_min_x */
    float y_proj_max_x;                /* y corresponding to row_max_x */
    float zc, z1, z2, z3;
    float z_proj_min;                  /* z corresponding to row_min_x,y */
    float z_proj_max;                  /* z corresponding to row_max_x,y */

#define SIGMAB 0.0                     /* Not used here yet... */
#define SIGMAC 30.0                    /* Not used here yet... */
#define SIGMAF 0.0                     /* Not used here yet... */
#define SIGMAG 0.0                     /* Not used here yet... */
#define RADTODEG 57.295780             /* 360/(2*pi)=57.2957795132 */
#define sigma_xy 0.05                  /* Error in xy; (0.05 cm) */
#define sigma_z  0.05                  /* Error in  z; (0.05 cm) */
#define sigma2y sigma_xy*sigma_xy      /* Error^2 in xy */
#define sigma2z sigma_z*sigma_z        /* Error^2 in  z */

    FILE *dataf;
    TCL_TPHIT_ST *track[MAX_ROWS];
    TCL_TPHIT_ST *track_del[MAX_ROWS]; /* List of hits to delete */
    TCL_TPHIT_ST *pjjp;
    TCL_TPHIT_ST *pjlp;
    TCL_TPHIT_ST *pjp, *pjp_2, *pjp_3;
    TCL_TPHIT_ST *pjlp_best, *pjp_2_best, *pjp_3_best;

/* set all the counters to 0 */
    res_h[0].nok = 0;
    diff_res_h[0].nok = 0;
    strack_h[0].nok = 0;

/* reset the track flag, tfs does not do it.....*/
    for (l=0; l<tphit_h[0].nok; l++) tphit[l].track=0;

/* Open the file for the residuals*/
    dataf=fopen("mydata","w");

/* Set up a matrix listing rows to skip */
    reject(par_h, par, skip_row);

/* Setup pointers to rows and count hit/row */
    for (k=0; k<MAX_ROWS; k++) row_count[k]=row_pnt[k]=0;

/* Sort all the hits according to sector/row*/
    tls_index_sort_i_(&tphit_h[0].nok, &tphit[0].row,
		      &tphit[1].row,loc_hit,&len);
/*
 * loc_hit was sorted for fortran, so the index starts at 1; fix it,
 * then load the row pointers
 */
    for (i=0; i<tphit_h[0].nok; i++){
	loc_hit[i] -= 1;
	j = loc_hit[i];
	k = tphit[j].row%100 - 1;
	if (row_count[k]++ == 0) row_pnt[k]=i;
    }

    nres = 0;
    ntrack = 0;
/*
 * Do setup before looping; pull out some calculations and lookups
 * either for simplicity, or because compiler may fail to optimize "properly"
 * (as does gcc for i960, at least).
 */
    max_hole = par->hole;
    /*
     * We cannot find a track if max_hole<1:
     */
    if (max_hole<1) max_hole=1;

    par_nmin = par->nmin;
    /*
     * We need *at least* 3 hits to find a track:
     */
    if (par_nmin<3) par_nmin=3;

    dmaxy = fabs(par->oy);
    dmaxy_2 = dmaxy * dmaxy;
    dmaxz = fabs(par->oz);
    dmaxz_2 = dmaxz * dmaxz;

    par_outlimit = par->outlimit;
    par_ilimit = par->ilimit;

    rowa = par[0].first_row;
    rowb = par[0].last_row;
    row_range = abs(rowb-rowa) + 1;
    if (rowa<rowb) {
	row_inner = rowa;
	row_outer = rowb;
	drow =  1;
    }
    else{
	row_inner = rowb;
	row_outer = rowa;
	drow = -1;
    }

    /*
     * Find 1-D x,y,z limits for each pad row with hits, so we can skip
     * detailed searches for hits that cannot be there.  Better approaches
     * are possible, but this is simple to implement for now.
     */
    for (n=row_inner-1; n<row_outer; n++) {
	if (row_count[n] > 0) {
	    jn = row_pnt[n];
	    /*
	     * Load first hit (kn=0) for comparison:
	     */
	    pjp = &(tphit[loc_hit[jn]]);
	    row_min_x[n] = row_max_x[n] = pjp->x;
	    row_min_y[n] = row_max_y[n] = pjp->y;
	    row_min_z[n] = row_max_z[n] = pjp->z;
	    for (kn=1; kn<row_count[n]; kn++) {
		pjp = &(tphit[loc_hit[jn+kn]]);

		/* Min, max in x: */
		xc = pjp->x;
		if (xc<row_min_x[n])
		    row_min_x[n] = xc;
		else if (xc>row_max_x[n])
		    row_max_x[n] = xc;

		/* Min, max in y: */
		yc = pjp->y;
		if (yc<row_min_y[n])
		    row_min_y[n] = yc;
		else if (yc>row_max_y[n])
		    row_max_y[n] = yc;

		/* Min, max in z: */
		zc = pjp->z;
		if (zc<row_min_z[n])
		    row_min_z[n] = zc;
		else if (zc>row_max_z[n])
		    row_max_z[n] = zc;

	    }   /*
		 * for(kn=0;kn<row_count[n];kn++)
		 */
	}   /*
	     * if(row_count[n]>0)
	     */
    }   /*
	 * for(n=row_inner;n<=row_outer;n++)
	 */

    /*
     * Loop over rows for the first hit:
     */
    n = rowa - 1;
    nrow = row_range;
    while (nrow-- >= par_nmin) {
	jn = row_pnt[n];
	/* loop over hits in one row */
	for (kn=0; kn<row_count[n]; kn++) {
	    pjp = &(tphit[loc_hit[jn+kn]]);
	    if (!pjp->track) {
		isector = pjp->row/100 - 1;
		if (!skip_row[isector][n]) {
		    x1 = pjp->x;
		    y1 = pjp->y;
		    z1 = pjp->z;

		    m = n;
		    mrow = nrow;

		    /*
		     * Loop over rows for the second hit;
		     * mrow>1 required to allow a third hit.
		     */
		    while(mrow-- >1 && (nrow-mrow)<=max_hole) { 
			m += drow;
			if (!skip_row[isector][m]) {
			    jm = row_pnt[m];
			    /*
			     * Loop over hits in the second row:
			     */
			    for (km=0; km<row_count[m]; km++) {
				pjjp = &(tphit[loc_hit[jm+km]]);

    /* Renormalize indentation until end of routine */
    if (!pjjp->track) {
	x2 = pjjp->x;
	y2 = pjjp->y;
	z2 = pjjp->z;
 
	sx = x1 + x2;
	sy = y1 + y2;
	sz = z1 + z2;

	sxy = x1*y1 + x2*y2;
	sxz = x1*z1 + x2*z2;
	syz = y1*z1 + y2*z2;

	sxx = x1*x1 + x2*x2;
	syy = y1*y1 + y2*y2;
	sw  = 2.0;

	dety = sw*sxx - sx*sx;
	detx = sw*syy - sy*sy;
	use_dety = fabs(dety)>fabs(detx);
	if (use_dety) {
	    ay = (sxy*sw-sy*sx)/dety;
	    by = (sy*sxx-sxy*sx)/dety;
	    ay_norm = 1.0/sqrt(1.0+ay*ay);
	    az = (sxz*sw-sz*sx)/dety;
	    bz = (sz*sxx-sxz*sx)/dety;
	}
	else {
	    ax = (sxy*sw-sx*sy)/detx;
	    bx = (sx*syy-sxy*sy)/detx;
	    ax_norm = 1.0/sqrt(1.0+ax*ax);
	    az = (syz*sw-sz*sy)/detx;
	    bz = (sz*syy-syz*sy)/detx;
	}
	az_norm = 1.0/sqrt(1.0+az*az);

	l = m;
	lrow = mrow;
	npntl_2 = 2;
	lhole = 0;
	/*
	 * Loop over rows for the subsequent candidates:
	 */
	while (lrow-- >0 && lhole<=max_hole) { 
	    l += drow;
	    dkl = -1;
	    
	    /* check whether to skip that row */
	    if (!skip_row[isector][l] && row_count[l]>0) {
		/*
		 * Determine whether any candidates *might* exist;
		 * check closest possible distance in x-y:
		 */
		if (use_dety) {
		    y_proj_min_x = by + ay*row_min_x[l];
		    y_proj_max_x = by + ay*row_max_x[l];
		    if ((y_proj_max_x-row_max_y[l] > dmaxy/ay_norm &&
			 y_proj_min_x-row_max_y[l] > dmaxy/ay_norm) ||
			(row_min_y[l]-y_proj_max_x > dmaxy/ay_norm &&
			 row_min_y[l]-y_proj_min_x > dmaxy/ay_norm))
			goto ROW_CHECKED_1;
		}
		else {
		    x_proj_max_y = bx + ax*row_max_y[l];
		    x_proj_min_y = bx + ax*row_min_y[l];
		    if ((x_proj_max_y-row_max_x[l] > dmaxy/ax_norm &&
			 x_proj_min_y-row_max_x[l] > dmaxy/ax_norm) ||
			(row_min_x[l]-x_proj_max_y > dmaxy/ax_norm &&
			 row_min_x[l]-x_proj_min_y > dmaxy/ax_norm))
			goto ROW_CHECKED_1;
		}

		/*
		 * Check closest possible distance in z:
		 */
		if (use_dety) {
		    z_proj_min = bz + az*row_min_x[l];
		    z_proj_max = bz + az*row_max_x[l];
		}
		else {
		    z_proj_min = bz + az*row_min_y[l];
		    z_proj_max = bz + az*row_max_y[l];
		}
		if ((z_proj_max-row_max_z[l] > dmaxz/az_norm &&
		     z_proj_min-row_max_z[l] > dmaxz/az_norm) ||
		    (row_min_z[l]-z_proj_max > dmaxz/az_norm &&
		     row_min_z[l]-z_proj_min > dmaxz/az_norm))
		    goto ROW_CHECKED_1;

		/*
		 * Loop over hits in one row for third (or later) candidates:
		 */
		jl = row_pnt[l];
		dclosest_2 = dmaxy_2 + dmaxz_2;
		for (kl=0; kl<row_count[l]; kl++) {
		    pjlp = &(tphit[loc_hit[jl+kl]]);
		    if (!pjlp->track) {
			/*
			 * Check distance in x-y:
			 */
			xc = pjlp->x;
			yc = pjlp->y;
			if (use_dety) {
			    dy = yc - (by+ay*xc);
			    dxy_2 = (dy*ay_norm)*(dy*ay_norm);
			}
			else {
			    dx = xc - (bx+ax*yc); 
			    dxy_2 = (dx*ax_norm)*(dx*ax_norm);
			}
			if (dxy_2>dmaxy_2) continue;

			/*
			 * Check distance in z:
			 */
			zc = pjlp->z;
			if (use_dety)
			    dz0 = zc - (bz+az*xc);
			else
			    dz0 = zc - (bz+az*yc);  
			dz_2 = (dz0*az_norm)*(dz0*az_norm);
			if (dz_2>dmaxz_2) continue;

			/*
			 * Find the closest candidate:
			 */
			if (dxy_2+dz_2<dclosest_2) {
			    dclosest_2 = dxy_2 + dz_2;
			    dkl = kl;
			    pjlp_best = pjlp;
			}
		    }   /*
			 * if(!pjlp->track)
			 */
		}   /*
		     * for(kl=0;kl<row_count[l];kl++)
		     * Done looping for 3rd and later hits in one row.
		     */
	    }   /*
		 * if(!skip_row[isector][l]&&row_count[l]>0)
		 * Done selecting best hit from among all hits in one row.
		 */

	ROW_CHECKED_1:
	    if (dkl>=0) {
		npntl_2 += 1;
		lhole = 0;
		lrow_last_hit = lrow;
		x3 = pjlp_best->x;
		y3 = pjlp_best->y;
		z3 = pjlp_best->z;

		sx  += x3;
		sy  += y3;
		sz  += z3;
		sxy += x3*y3;
		sxz += x3*z3;
		syz += y3*z3;
		sxx += x3*x3;
		syy += y3*y3;
		sw  += 1.0;
		
		dety = sw*sxx - sx*sx;
		detx = sw*syy - sy*sy;
		use_dety = fabs(dety)>fabs(detx);
		if (use_dety) {
		    ay = (sxy*sw-sy*sx)/dety;
		    by = (sy*sxx-sxy*sx)/dety;
		    ay_norm = 1.0/sqrt(1.0+ay*ay);
		    az = (sxz*sw-sz*sx)/dety;
		    bz = (sz*sxx-sxz*sx)/dety;
		}
		else {
		    ax = (sxy*sw-sx*sy)/detx;
		    bx = (sx*syy-sxy*sy)/detx;
		    ax_norm = 1.0/sqrt(1.0+ax*ax);
		    az = (syz*sw-sz*sy)/detx;
		    bz = (sz*syy-syz*sy)/detx;
		}
		az_norm = 1.0/sqrt(1.0+az*az);
	    }   /*
		 * if(dkl>=0)
		 */
	    else {
		lhole++;
	    }   /*
		 * if(dkl>=0)else
		 * End of track update for the best candidate from this row.
		 */
	}   /*
	     * while(lrow-->0&&lhole<=max_hole)
	     * end of loop over all these other rows for the subsequent hits
	     */
	/*
	 * Skip subsequent logic if not enough hits within extreme
	 * limits of the track:
	 */
	nmin = ceil((nrow-lrow_last_hit+1)/2);
	if (nmin<par_nmin) nmin=par_nmin;
	if (npntl_2<nmin) goto TOO_SHORT;

	/*
	 * To avoid choosing wrong points on the track, especially the
         * first one or two points before the track is well-defined,
	 * re-find the hits using the global track parameters;
	 * to include other hits, increase par-> oy/oz/outlimit.
	 */
	ll = 0;
	sx = sy = sz = sxy = sxz = syz = sxx = syy = sw = 0.0;
	do {
	    npntl = 0;
	    nrow_2 = row_range;
	    n_2 = rowa - 1;
	    while (nrow_2-->0){
		dkl_2 = -1;
		if (!skip_row[isector][n_2] && row_count[n_2]>0) {
		    /*
		     * Determine whether any candidates *might* exist;
		     * check closest possible distance in x-y:
		     */
		    if (use_dety) {
			y_proj_min_x = by + ay*row_min_x[n_2];
			y_proj_max_x = by + ay*row_max_x[n_2];
			if ((y_proj_max_x-row_max_y[n_2] > dmaxy/ay_norm &&
			     y_proj_min_x-row_max_y[n_2] > dmaxy/ay_norm) ||
			    (row_min_y[n_2]-y_proj_max_x > dmaxy/ay_norm &&
			     row_min_y[n_2]-y_proj_min_x > dmaxy/ay_norm))
			    goto ROW_CHECKED_2;
		    }
		    else {
			x_proj_max_y = bx + ax*row_max_y[n_2];
			x_proj_min_y = bx + ax*row_min_y[n_2];
			if ((x_proj_max_y-row_max_x[n_2] > dmaxy/ax_norm &&
			     x_proj_min_y-row_max_x[n_2] > dmaxy/ax_norm) ||
			    (row_min_x[n_2]-x_proj_max_y > dmaxy/ax_norm &&
			     row_min_x[n_2]-x_proj_min_y > dmaxy/ax_norm))
			    goto ROW_CHECKED_2;
		    }
		    
		    /*
		     * Check closest possible distance in z:
		     */
		    if (use_dety) {
			z_proj_min = bz + az*row_min_x[n_2];
			z_proj_max = bz + az*row_max_x[n_2];
		    }
		    else {
			z_proj_min = bz + az*row_min_y[n_2];
			z_proj_max = bz + az*row_max_y[n_2];
		    }
		    if ((z_proj_max-row_max_z[n_2] > dmaxz/az_norm &&
			 z_proj_min-row_max_z[n_2] > dmaxz/az_norm) ||
			(row_min_z[n_2]-z_proj_max > dmaxz/az_norm &&
			 row_min_z[n_2]-z_proj_min > dmaxz/az_norm))
			goto ROW_CHECKED_2;

		    /*
		     * Loop over hits in one row:
		     */
		    jn_2 = row_pnt[n_2];
		    dclosest_2 = dmaxy_2 + dmaxz_2;
		    for (kn_2=0; kn_2<row_count[n_2]; kn_2++){
			pjp_2 = &(tphit[loc_hit[jn_2+kn_2]]);
			if (!pjp_2->track) {
			    /*
			     * Check distance in x-y:
			     */
			    xc = pjp_2->x;
			    yc = pjp_2->y;
			    if (use_dety) {
				dy = yc - (by+ay*xc);
				dxy_2 = (dy*ay_norm)*(dy*ay_norm);
			    }
			    else {
				dx = xc - (bx+ax*yc); 
				dxy_2 = (dx*ax_norm)*(dx*ax_norm);
			    }
			    if (dxy_2>dmaxy_2) continue;

			    /*
			     * Check distance in z:
			     */
			    zc = pjp_2->z;
			    if (use_dety)
				dz0 = zc - (bz+az*xc);
			    else
				dz0 = zc - (bz+az*yc);  
			    dz_2 = (dz0*az_norm)*(dz0*az_norm);
			    if (dz_2>dmaxz_2) continue;

			    /*
			     * Find the closest candidate:
			     */
			    if (dxy_2+dz_2<dclosest_2) {
				dclosest_2 = dxy_2 + dz_2;
				dkl_2 = kn_2;
				pjp_2_best = pjp_2;
			    }
			}   /*
			     * if (!pjp_2->track)
			     */
		    }   /*
			 * for (kn_2=0;kn_2<row_count[n_2];kn_2++)
			 * Done looping over hits in this row.
			 */
		}   /*
		     * if(!skip_row[isector][n_2])&&row_count[n_2]>0
		     * Done skipping empty and unwanted rows.
		     */

	    ROW_CHECKED_2:
		if (dkl_2>=0) {
		    track[npntl++] = pjp_2_best;
		    xc = pjp_2_best->x;
		    yc = pjp_2_best->y;
		    zc = pjp_2_best->z;
		    sx  += xc;
		    sy  += yc;
		    sz  += zc;
		    sxy += xc*yc;
		    sxz += xc*zc;
		    syz += yc*zc;
		    sxx += xc*xc;
		    syy += yc*yc;
		    sw  += 1.0;
		}   /*
		     * if(dkl_2>=0)
		     * Done adding hit to track.
		     */
		n_2 += drow;
	    }   /*
		 * while(nrow_2-->0)
		 * Done re-finding hits for this track.
		 */
	    if (npntl<nmin) goto TOO_SHORT;
/*
 * If enough points exist, re-find global track parameters:
 */
	    dety = sw*sxx - sx*sx;
	    detx = sw*syy - sy*sy;
	    use_dety = fabs(dety)>fabs(detx);
	    if (use_dety) {
		ay = (sxy*sw-sy*sx)/dety;
		by = (sy*sxx-sxy*sx)/dety;
		ay_norm = 1.0/sqrt(1.0+ay*ay);
		az = (sxz*sw-sz*sx)/dety;
		bz = (sz*sxx-sxz*sx)/dety;
	    }
	    else {
		ax = (sxy*sw-sx*sy)/detx;
		bx = (sx*syy-sxy*sy)/detx;
		ax_norm = 1.0/sqrt(1.0+ax*ax);
		az = (syz*sw-sz*sy)/detx;
		bz = (sz*syy-syz*sy)/detx;
	    }
	    az_norm = 1.0/sqrt(1.0+az*az);
/*
 * If any "extra" points exist, check for outliers to reject:
 */
	    ndel = 0;
	    if (npntl>nmin) {
		sumy = 0;
		sumz = 0;
		/*
		 * Calculate and sum distance of hits to track in x-y and z:
		 */
		for (l=0; l<npntl; l++) {
		    xc = track[l]->x;
		    yc = track[l]->y;
		    zc = track[l]->z;
		    if (use_dety) {
			a_ddy[l] = fabs((yc-(by+ay*xc))*ay_norm);
			a_ddz[l] = fabs((zc-(bz+az*xc))*az_norm);
		    }
		    else {
			a_ddy[l] = fabs((xc-(bx+ax*yc))*ax_norm);
			a_ddz[l] = fabs((zc-(bz+az*yc))*az_norm);
		    }
		    sumy += a_ddy[l];
		    sumz += a_ddz[l];
		    d_2[l] = a_ddy[l]*a_ddy[l] + a_ddz[l]*a_ddz[l];
		}   /*
		     * for(l=0;l<npntl;l++)
		     * Done summing distances to hits on track.
		     */
		/*
		 * Define outliers, based on the average absolute deviation:
		 */
		dy_lim = sumy*(par_outlimit/npntl);
		dz_lim = sumz*(par_outlimit/npntl);

		/*
		 * Find the outliers:
		 */
		ndel_try = 0;
		for (l=0; l<npntl; l++) {
		    if (a_ddy[l]>dy_lim || a_ddz[l]>dz_lim) {
			outlier[ndel_try++] = l;
		    }
		}

		/*
		 * If there are too many outliers, keep those closest
		 * to the track and only reject the allowed maximum;
		 * the requirement is npntl-ndel>=nmin:
		 */
		nkeep = nmin - (npntl-ndel_try);
		if (nkeep>0) {
		    /*
		     * Mark all outliers as not to be kept:
		     */
		    for (l=0; l<ndel_try; l++) outlier_keep[l]=FALSE;

		    /*
		     * Make list of the "nkeep" smallest outliers:
		     */
		    for (lkeep=0; lkeep<nkeep; lkeep++) {
			d_2_lim = dy_lim*dy_lim + dz_lim*dz_lim;
			for (l=0; l<ndel_try; l++) {
			    ldel = outlier[l];
			    if (d_2[ldel]<d_2_lim && !outlier_keep[l]) {
				d_2_lim = d_2[ldel];
				l_outlier_keep = l;
			    }
			}
			outlier_keep[l_outlier_keep] = TRUE;
		    }   /*
			 * for(lkeep=0;lkeep<nkeep;lkeep++)
			 */
		    /*
		     * Condense the outlier list to non-kept outliers:
		     */
		    for (l=0, ldel=0; l<ndel_try; l++) {
			if (!outlier_keep[l]) outlier[ldel++]=outlier_keep[l];
		    }
		    ndel_try = ldel;
		}   /*
		     * if (nkeep>0)
		     */
		/*
		 * Strike the outliers by overwriting them in the list
		 * of good hits (same ordering in outlier and track list);
		 * keep count of deleted points in "ndel":
		 */
		if (ndel_try>0) {
		    ldel = outlier[0];
		    for (l=0, nkeep=0; l<npntl; l++) {
			if (l==ldel) {
			    track_del[ndel++] = track[l];
			    if (ndel<ndel_try) ldel=outlier[ndel];
			}
			else {
			    if (ndel>0) track[nkeep]=track[l];
			    nkeep++;
			}
		    }   /*
			 * for(l=0,nkeep=0;l<npntl;l++)
			 */
		}   /*
		     * if (ndel_try>0)
		     */
	    }   /*
		 * if(npntl>nmin)
		 * Done checking for possible outliers.
		 */
	    /*
	     * Subtract contributions from any deleted points:
	     */
	    npntl = npntl - ndel;
	    if (ndel>0) {
		for (l=0; l<ndel; l++) {
		    x3 = track_del[l]->x;
		    y3 = track_del[l]->y;
		    z3 = track_del[l]->z;
		    sx-=x3;     sy-=y3;     sz-=z3;
		    sxy-=x3*y3; sxz-=x3*z3; syz-=y3*z3;
		    sxx-=x3*x3; syy-=y3*y3; sw-=1.0;
		}
		dety = sw*sxx - sx*sx;
		detx = sw*syy - sy*sy;
		use_dety = fabs(dety)>fabs(detx);
		if (use_dety) {
		    ay = (sxy*sw-sy*sx)/dety;
		    by = (sy*sxx-sxy*sx)/dety;
		    ay_norm = 1.0/sqrt(1.0+ay*ay);
		    az = (sxz*sw-sz*sx)/dety;
		    bz = (sz*sxx-sxz*sx)/dety;
		}
		else {
		    ax = (sxy*sw-sx*sy)/detx;
		    bx = (sx*syy-sxy*sy)/detx;
		    ax_norm = 1.0/sqrt(1.0+ax*ax);
		    az = (syz*sw-sz*sy)/detx;
		    bz = (sz*syy-syz*sy)/detx;
		}
		az_norm = 1.0/sqrt(1.0+az*az);
	    }   /*
		 * if(ndel>0
		 * Find track param. w/o outliers.
		 */
	    ll++;
	} while(ndel>0 && ll<par_ilimit);   /* do()
                                             * Done iterating par->ilimit times
					     * over track param. refinding
					     * and outlier rejection.
					     */

	/* Let's fill the output track table */
	if (npntl>=par_nmin) {
	    /*
	     * Refit the track with errors;
	     * sigma2y, sigma2z taken as CONSTANT for the moment...
	     */
	    sx = sy = sxy = sxx = syy = sw = 0.0;
	    for (l=0; l<npntl; l++) {
		x3 = track[l]->x;
		y3 = track[l]->y;

		sx  += x3/sigma2y;
		sy  += y3/sigma2y;
		sxy += x3*(y3/sigma2y);
		sxx += x3*(x3/sigma2y);
		syy += y3*(y3/sigma2y);
		sw  += 1.0/sigma2y;
	    }   /*
		 * for(l=0;l<npntl;l++)
		 */
	    dety_n = sw*sxx - sx*sx;
	    detx_n = sw*syy - sy*sy;
	    use_dety_n = fabs(dety_n)>fabs(detx_n);

	    if (use_dety_n) {                      /* y=A+B*x fit */
		A = (sy*sxx-sxy*sx)/dety_n;
		B = (sxy*sw-sx*sy)/dety_n;
		dA = sqrt(sxx/dety_n);
		dB = sqrt(sw/dety_n);
		if (B>=0)
		    cosa =  1.0/sqrt(1.0+B*B);
		else
		    cosa = -1.0/sqrt(1.0+B*B);
		sina = B*cosa;
	    }
	    else {                                 /* x=C+D*y fit */
		C = (sx*syy-sxy*sy)/detx_n;
		D = (sxy*sw-sx*sy)/detx_n;
		dC = sqrt(syy/detx_n);
		dD = sqrt(sw/detx_n);
		if (D>=0)
		    sina = sqrt(1.0/(1.0+D*D));
		else
		    sina = -sqrt(1.0/(1.0+D*D));
		cosa = D*sina;
	    }
		  
	    /* rotate Cartesian coordinates, and fit z versus xm's */
	    sx = sz = sxz = sxx = sw = 0.0;
	    for (l=0; l<npntl; l++) {
		x3 = track[l]->x;
		y3 = track[l]->y;
		z3 = track[l]->z;

		xm[l] = y3*sina + x3*cosa;
		ym[l] = y3*cosa - x3*sina;
		
		sx  += xm[l]/sigma2z;
		sz  += z3/sigma2z;
		sxz += xm[l]*(z3/sigma2z);
		sxx += xm[l]*(xm[l]/sigma2z);
		sw  += 1.0/sigma2z;
	    }   /*
		 * for(l=0;l<npntl;l++)
		 */

            /* z=Bz + Az*xm fit */
	    detz_n = sw*sxx - sx*sx;
	    Az = (sxz*sw-sz*sx)/detz_n;
	    Bz = (sz*sxx-sxz*sx)/detz_n;
	    daz = sqrt(sw/detz_n);
	    dbz = sqrt(sxx/detz_n);

	    Az_norm = 1.0/sqrt(1.0+Az*Az);

	    ntrack_p1 = ntrack + 1;

	    strack[ntrack].trk = ntrack_p1;
	    strack[ntrack].npnt = npntl;
	    strack[ntrack].az = Az;
	    strack[ntrack].daz = daz;

	    /* - recast the xy fits to ax*x +ay*y =1 */
	    if (use_dety_n) {
		/* z=a*x+b case  also y = A +B*x fit  */
		strack[ntrack].ax = -B/A;
		strack[ntrack].ay = 1.0/A;
		strack[ntrack].dax = fabs((B/A*dA-dB)/A);
		strack[ntrack].day = fabs(dA/A/A);
		if (fabs(sina)>0.01) {
		    strack[ntrack].azy = Az/sina;   /* z=bzy+azy*y fit */
		    strack[ntrack].bzy = -Az*A/B*cosa + Bz;
		    strack[ntrack].dazy = daz/sina;
		    strack[ntrack].dbzy = daz*A/B*cosa + dbz;
		}
		else {
		    strack[ntrack].azy = -999.0;
		    strack[ntrack].bzy = -999.0;
		}
		strack[ntrack].azx = Az/cosa;       /* z=bzx+azx*x fit */
		strack[ntrack].bzx = Az*A*sina + Bz;
		strack[ntrack].dazx = daz/cosa;
		strack[ntrack].dbzx = daz*B*sina + dbz;
	    }
	    else {
		/* z=a*y+b case  (and x = C + D*y fit)  */
		strack[ntrack].ax = 1.0/C;
		strack[ntrack].ay = -D/C;
		strack[ntrack].dax = fabs(dC/C/C);
		strack[ntrack].day = fabs((D/C*dC - dD)/C);

		strack[ntrack].azy = Az/sina;
		strack[ntrack].bzy = Az*(C*cosa) + Bz;
		strack[ntrack].dazy = daz/sina;
		strack[ntrack].dbzy = daz*(C*cosa) + dbz;

		if (fabs(cosa)<0.01) {
		    strack[ntrack].azx = -999.0;
		    strack[ntrack].bzx = -999.0;
		}
		else {
		    strack[ntrack].azx = Az/cosa;
		    strack[ntrack].bzx = -Az*(C/D*sina) + Bz;
		    strack[ntrack].dazx = daz/cosa;
		    strack[ntrack].dbzx = -daz*(C/D*sina) + dbz;
		}
	    }   /*
		 * if(use_dety_n)
		 */
	    /*
	     * Let's calculate residuals, first we loop over ALL rows to get a 
	     * new track including the hits in skip_row[isector][n_2]=TRUE
	     * and the previously rejected outliers; we do not check the
             * hole size, because we have already decided to output the
             * track and do not want to risk inconsistency.
	     */
	    npntl_3 = 0;
	    nrow_3 = row_range;
	    n_3 = rowa - 1;
	    while (nrow_3-- >0) { 
		dkl_3 = -1;
		if (row_count[n_3]<=0) goto ROW_CHECKED_3;
		/*
		 * Determine whether any candidates *might* exist;
		 * check closest possible distance in x-y:
		 */
		if (use_dety) {
		    y_proj_min_x = by + ay*row_min_x[n_3];
		    y_proj_max_x = by + ay*row_max_x[n_3];
		    if ((y_proj_max_x-row_max_y[n_3] > dmaxy/ay_norm &&
			 y_proj_min_x-row_max_y[n_3] > dmaxy/ay_norm) ||
			(row_min_y[n_3]-y_proj_max_x > dmaxy/ay_norm &&
			 row_min_y[n_3]-y_proj_min_x > dmaxy/ay_norm))
			goto ROW_CHECKED_3;
		}
		else {
		    x_proj_max_y = bx + ax*row_max_y[n_3];
		    x_proj_min_y = bx + ax*row_min_y[n_3];
		    if ((x_proj_max_y-row_max_x[n_3] > dmaxy/ax_norm &&
			 x_proj_min_y-row_max_x[n_3] > dmaxy/ax_norm) ||
			(row_min_x[n_3]-x_proj_max_y > dmaxy/ax_norm &&
			 row_min_x[n_3]-x_proj_min_y > dmaxy/ax_norm))
			goto ROW_CHECKED_3;
		}
		    
		/*
		 * Check closest possible distance in z:
		 */
		if (use_dety) {
		    z_proj_min = bz + az*row_min_x[n_3];
		    z_proj_max = bz + az*row_max_x[n_3];
		}
		else {
		    z_proj_min = bz + az*row_min_y[n_3];
		    z_proj_max = bz + az*row_max_y[n_3];
		}
		if ((z_proj_max-row_max_z[n_3] > dmaxz/az_norm &&
		     z_proj_min-row_max_z[n_3] > dmaxz/az_norm) ||
		    (row_min_z[n_3]-z_proj_max > dmaxz/az_norm &&
		     row_min_z[n_3]-z_proj_min > dmaxz/az_norm))
		    goto ROW_CHECKED_3;

		/*
		 * Loop over hits in one row:
		 */
		jn_3 = row_pnt[n_3];
		dclosest_2 = dmaxy_2 + dmaxz_2;
		for (kn_3=0; kn_3<row_count[n_3]; kn_3++){
		    pjp_3 = &(tphit[loc_hit[jn_3+kn_3]]);
		    if (!pjp_3->track) {
			/*
			 * Check distance in x-y:
			 */
			xc = pjp_3->x;
			yc = pjp_3->y;
			if (use_dety) {
			    dy = yc - (by+ay*xc);
			    dxy_2 = (dy*ay_norm)*(dy*ay_norm);
			}
			else {
			    dx = xc - (bx+ax*yc); 
			    dxy_2 = (dx*ax_norm)*(dx*ax_norm);
			}
			if (dxy_2>dmaxy_2) continue;

			/*
			 * Check distance in z:
			 */
			zc = pjp_3->z;
			if (use_dety)
			    dz0 = zc - (bz+az*xc);
			else
			    dz0 = zc - (bz+az*yc);  
			dz_2 = (dz0*az_norm)*(dz0*az_norm);
			if (dz_2>dmaxz_2) continue;

			/*
			 * Find the closest candidate:
			 */
			if (dxy_2+dz_2<dclosest_2) {
			    dclosest_2 = dxy_2 + dz_2;
			    dkl_3 = kn_3;
			    pjp_3_best = pjp_3;
			}
		    }   /*
			 * if(!pjp_3->track)
			 */
		}   /*
		     * for(kn_3=0;kn_3<row_count[n_3];kn_3++)
		     * End of loop finding (final) closest hit in row.
		     */

	    ROW_CHECKED_3:
		if (dkl_3>=0) track[npntl_3++]=pjp_3_best;
		n_3 += drow;
	    }   /*
		 * while(nrow_3-->0)
		 * End of absolute residual calculation for this track.
		 */

	    /* 
	     * Now we recalculate residuals for this track, as well as chisq.
	     * The parameters used are from the loop with restriction that
	     * skip_row[isector][n_2]=FALSE .
	     */
	    chisqx = 0;
	    chisqz = 0;

	    vtracky[0] =  strack[ntrack].ay;
            vtracky[1] = -strack[ntrack].ax;
            vtracky[2] =  strack[ntrack].azx*vtracky[0];
	    if(fabs(strack[ntrack].azx+999.0)<0.0001)
		vtracky[2] = strack[ntrack].azy*vtracky[1];

	    sector_prev = -1;
	    for (l=0; l<npntl_3; l++) {
		/*
		 * Get some needed variables:
		 */
		sector = track[l]->row/100;
		if (sector!=sector_prev) {
		    sector_prev = sector;
		    /*calculate the crossing angle and the dip angle */   
                    tgc_cross_and_dip_(&sector,vtracky,&cross,&dip);
                    cos_alpha = cos(cross);
		    alpha = RADTODEG*cross;
                    lambda = RADTODEG*dip;
		}
		track_dx = sigma_xy;
		track_dy = 0.0;
		track_dz = sigma_z;
		xc = track[l]->x;
		yc = track[l]->y;
		zc = track[l]->z;
/*
 * Find residuals and chi-squared:
 */
		if (use_dety_n)
		    dy = (A+B*xc-yc)*fabs(cosa);
		else
		    dy = (C+D*yc-xc)*fabs(sina);
		dy0 = dy/cos_alpha;
		xc = yc*sina + xc*cosa;
		dz0 = zc - (Bz+Az*xc);
		dz = dz0*Az_norm;
		chisqx += dy0*dy0/(track_dx*track_dx+track_dy*track_dy);
		chisqz += (dz0/track_dz)*(dz0/track_dz);
/*
 * Load TCL_TPHIT_ST *track[MAX_ROWS]:
 */		
		track[l]->track = ntrack_p1;
		track[l]->alpha = alpha;
		track[l]->dx = track_dx;
		track[l]->dy = track_dy;
		track[l]->dz = track_dz;
		track[l]->lambda = lambda;
/*
 * Load TPT_RES_ST *res and TPT_RES_ST *diff_res:
 */
		res_h[0].nok  = diff_res_h[0].nok  = nres + 1;
		res[nres].trk = diff_res[nres].trk = ntrack_p1;
		res[nres].hit = diff_res[nres].hit = track[l]->id;
	
		res[nres].resy = dy;
		res[nres].resz = dz;

		diff_res[nres].resy = dy0;
		diff_res[nres].resz = dz0;
		/*
		 * Increment residual tables counter:
		 */
		nres++;
	    }   /*
		 * for(l=0;l<npntl_3;l++)
		 */
	    strack[ntrack].chisqxy = chisqx;
	    strack[ntrack].chisqz = chisqz;
	    strack_h[0].nok = ntrack_p1;
	    ntrack++;
	    goto FIRST_HIT_USED;
	}   /*
	     * if(npntl>=par_nmin)
	     * Done putting track into output track table.
	     */
    }   /*
	 * if(!pjjp->track)
	 * End of check for unused candidate for 2nd hit.
	 */
			    TOO_SHORT:
				;
			    }   /*
				 * for(km=0;km<row_count[m];km++)
				 * End of loop over candidates for 2nd hit.
				 */
			}   /*
			     * if(!skip_row[isector][m])
			     * End of a check on rows to skip for 2nd hit.
			     */
		    }   /*
			 * while(mrow-->1&&(nrow-mrow)<=max_hole)
			 * End of loop over rows for the 2nd candidate.
			 */
		}   /*
		     * if(!skip_row[isector][n])
		     * End of check on rows to skip for 1st hit.
		     */
	    }   /*
		 * if(!pjp->track)
		 * End of check for unused candidate for 1st hit.
		 */

	FIRST_HIT_USED:
	    ;
	}   /*
	     * for(kn=0;kn<row_count[n];kn++)
	     * End of loop over candidates for the 1st hit.
	     */
	n += drow;
    }   /*
	 * while(nrow-->=par_nmin)
	 * End of loop over rows for the 1st hit.
	 */
    /*
     * the final arrys track[npntl] are from the last loop. All the track 
     * parameters, however, are come from DO loop.
     */
    fclose(dataf);
    return STAFCV_OK;
}

int reject (TABLE_HEAD_ST *par_h,
	    TPT_SPARS_ST *par,
	    long rskip[MAX_SECS][MAX_ROWS])
{
    #include "PAM.h"
    #include "tpt_spars.h"

    long l, k;
    long row;         /* Row array index; here, [0,44] */
    long sector;      /* Sector array index; here, [0,23] */
/*
 * Initialize all rows as "no skip" with FALSE's:
 */
    for (l=0; l<MAX_SECS; l++) {
	for (k=0; k<MAX_ROWS; k++) rskip[l][k]=FALSE;
    }
/*
 * Mark the rows to skip with TRUE's:
 */
    for (l=0; l<par[0].nskip; l++) {  
	sector = par[0].skip[l]/100 - 1;
	row = par[0].skip[l]%100 - 1;
/*
 * Check validity range in case parameters were inconsistent
 */
	if (row<MAX_ROWS && row>=0 && sector>=0 && sector<MAX_SECS)
	    rskip[sector][row] = TRUE;
    }
    return(1);
}
