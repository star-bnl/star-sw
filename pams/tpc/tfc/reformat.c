/*:>--------------------------------------------------------------------
**: FILE:       reformat.c
**: HISTORY:
**:             00jan93-v000a-hpl- Created by stic Version
**:  Id: idl.y,v 1.8 1996/10/15 18:33:35 ward Exp  
**:
**:  Nathan Stone  Feb 1997   Initial "beta" Release
**:  Nathan Stone  05/01/97   Installed first "public" release to CVS
**:  Nathan Stone  05/10/97   Added gain correction to ZP data
**:  Nathan Stone  05/29/97   Added first/last bucket and adc cuts 
**:                           to both ZP and BLK data
**:  R.Bossingham  10/03/97   * Changed nPads from 181 to 182
**:                           * Removed misleading initializations of:
**:                             mapSector, mapRDO, mapGood, mapRow, mapPad,
**:                             counted, used
**:                           * Properly initialize: counted, used, 
**:                             mapGood, mapRow, mapPad
**:                           * Change small, frequently used variables from
**:                             char to int to speed up code:
**:                             counted, used, first_call,
**:                             mapped, warned, zero_suppressed
**:                           * Delete declaration of unused variable: bdptr
**:                           * Misleading line changed in reformat (not a bug)
**:                             bucket = structtbl[sptr++].info; to sptr++;
**:                           * Search for correct pedestal index
**:                             with times_rms option corrected.
**:                           * Limit ADC's to [0,1023], to prevent corruption
**:                             of the time bucket number
**:                           * Kill unreachable "break;"'s after "continue;"'s
**:                           * Kill unused variables in "store":
**:                             more_than_one, group, chan, inok, j,k,l, secID
**:                           * Killed unused variables in "reformat":
**:                             pedval, oldpedval, nchan
**:                           * Use nBuck to tell whether or not a sequence
**:                             is being filled, eliminating the bug of
**:                             checking the old ADC value against threshold
**:                             (which varies, if adjusting with time_rms)
**:                           * Address tppixel[k].datum sequentially when
**:                             adding nBuck*nBfact (seq. length information)
**:                           * Set threshold before inner loop if times_rms!=0
**:                           * Find end of data table before inner loop
**:                           * Increment bucket index over natural 1->512
**:                           * Loop over buckets for zero-suppressed data
**:                             didn't use bucket number consistently; fixed
**:                           * Check for pointer problems *before* inner loops
**:                           * Check for (unlikely) geometry events last
**:                           * Jump to next index entry if dptr or sptr go off
**:                             table end (only way to reset them).
**:                           * Eliminate bucket number check from inner loop
**:  R.Bossingham  10/08/97   * Create local switches for gain, ped correction;
**:                             check existence of ped table *before* looping
**:                           * Check for ped table if using times_rms
**:                             or finding gains
**:                           * Check for gain table if gain correcting
**:                           * Move fmtpar[0].first_bucket, .last_bucket, 
**:                             .thresh checks outside index table loop
**:  R.Bossingham  10/09/97   * Implement indexing for gain and ped tables;
**:                             speed up gain finding, duplicate pad check,
**:                             and pedestal finding.
**:                           * Add 0.5 before truncating "long datum" to
**:                             to reduce the gain-correction bias.
**:                           * Eliminate unitialized ref's to time buckets
**:                             in some printf's.
**:                           * Protect against unitialized RDOid
**:  R.Bossingham  11/07/97   * Time buckets in tppixel changed to start at 0;
**:                             tcl handles offset by 1, changing start to 1.
**:                           * Fix bugs for applying time bucket range limits.
**:                           
**:  R.Bossingham  01/27/98   Fixed two bugs encountered by Ken Barish and
**:                           hunted down by Iwona Sakrejda:
**:                           * Sequence length was not set for the last
**:                             time bucket in a sequence for "normal" data.
**:                           * Threshold was being constrained to be positive;
**:                             while this is *usual*, it is not necessary.
**:                             (A warning is still issued, however.)
**:                           
**:<------------------------------------------------------------------*/
#include "reformat.h"

/* bring in the math library explicitly */
#include <math.h>

/* Include the row and pad number arrays for geometry mapping */
#include "mapping_arrays.h"

/* Include the byte-to-short data non-linear conversion array */
#include "byte2short.h"

/* define some parameters */
#define nSectors 24		/* number of total sectors */
#define nRDOs 6 		/* number of RDOs per super-sector */
#define nGroups 72		/* number of "groups" per RDO */
#define nChans 16		/* number of channels per "group" */
#define nBuckets 512		/* number of buckets per pad */
#define nBoards 6 		/* max # of RDO boards in setup (may vary) */
#define nRows 45		/* number of rows */
#define nPads 182		/* max # of pads per row */
#define nPadsSector 5692	/* # of pads per sector */
#define nPadsTotal  nSectors*nPadsSector
#define nChansTotal nSectors*nRDOs*nGroups*nChans
#ifndef TRUE
#define TRUE  1   		/* Defined for code legibility */
#endif
#ifndef FALSE
#define FALSE 0   		/* Defined for code legibility */
#endif

/* Some pixel-packing parameters */
#define Bfact  1024
#define nBfact 1048576

/* Type definition for indexing the ped/gain tables */
typedef struct offset_mapped {
    long gain;
    long ped;
} INDX_MAPPED;

/* Some global variables for indexing the ped/gain tables */
int           pads_per_row[nRows]={ 88,  96, 104, 112, 118,
				   126, 134, 142, 150, 158,
			           166, 174, 182,  98, 100,
			           102, 104, 106, 106, 108,
			           110, 112, 112, 114, 116,
			           118, 120, 122, 122, 124,
			           126, 128, 128, 130, 132,
			           134, 136, 138, 138, 140,
			           142, 144, 144, 144, 144};
int           pads_before_row[nRows];

/* define some handy macros */
#define nTimesSeen(group,chan) (chan+16*(((group-1)/9)%2))
#define sqr(x) ((x)*(x))

/* declare the functions */
int get_ID
   (const int mode,
    int sector,
    int RDO,
    unsigned char mapIndex[nSectors][nRDOs],
    unsigned char mapSector[nBoards],
    unsigned char mapRDO[nBoards]);

int calculate_gain
   (TABLE_HEAD_ST         *fmtpar_h,     TFC_FMTPAR_ST           *fmtpar ,
    TABLE_HEAD_ST       *pedestal_h, TFC_NATIVE_PEDESTAL_ST    *pedestal ,
    TABLE_HEAD_ST         *pulser_h, TFC_NATIVE_PEDESTAL_ST      *pulser ,
    TABLE_HEAD_ST           *gain_h, TFC_NATIVE_GAIN_ST            *gain ,
    unsigned char mapIndex[nSectors][nRDOs],
    unsigned char mapSector[nBoards],
    unsigned char mapRDO[nBoards]);

long get_gain_indx_mapped
   (INDX_MAPPED indx_mapped[nPadsTotal],
    int sector,
    int row,
    int pad);

long get_ped_indx_mapped
   (INDX_MAPPED indx_mapped[nPadsTotal],
    int sector,
    int row,
    int pad);

int init_indx
   (INDX_MAPPED indx_mapped[nPadsTotal]);

long put_gain_indx_mapped
   (INDX_MAPPED indx_mapped[nPadsTotal],
    long gnok,
    int sector,
    int row,
    int pad);

long put_ped_indx_mapped
   (INDX_MAPPED indx_mapped[nPadsTotal],
    long pnok,
    int sector,
    int row,
    int pad);

int tfc_store
   (TABLE_HEAD_ST         *fmtpar_h,     TFC_FMTPAR_ST           *fmtpar ,
    TABLE_HEAD_ST     *native_map_h, TFC_NATIVE_MAP_ST       *native_map ,
    TABLE_HEAD_ST          *valid_h,      TFC_VALID_ST            *valid ,
    unsigned char mapIndex[nSectors][nRDOs],
    unsigned char mapSector[nBoards],
    unsigned char mapRDO[nBoards],
    unsigned char mapGood[nBoards][nGroups],
    unsigned char mapRow[nBoards][nGroups][nChans],
    unsigned char mapPad[nBoards][nGroups][nChans]);

long type_of_call reformat_
   (TABLE_HEAD_ST         *fmtpar_h,     TFC_FMTPAR_ST           *fmtpar ,
    TABLE_HEAD_ST       *indextbl_h,     TYPE_INDEX_ST         *indextbl ,
    TABLE_HEAD_ST       *bytedata_h,  TYPE_BYTEDATA_ST         *bytedata ,
    TABLE_HEAD_ST      *shortdata_h, TYPE_SHORTDATA_ST        *shortdata ,
    TABLE_HEAD_ST      *structtbl_h, TYPE_STRUCTTBL_ST        *structtbl ,
    TABLE_HEAD_ST       *gain_bad_h,  TYPE_GAIN_BAD_ST         *gain_bad ,
    TABLE_HEAD_ST     *native_map_h, TFC_NATIVE_MAP_ST       *native_map ,
    TABLE_HEAD_ST          *valid_h,      TFC_VALID_ST            *valid ,
    TABLE_HEAD_ST       *pedestal_h, TFC_NATIVE_PEDESTAL_ST    *pedestal ,
    TABLE_HEAD_ST         *pulser_h, TFC_NATIVE_PEDESTAL_ST      *pulser ,
    TABLE_HEAD_ST           *gain_h, TFC_NATIVE_GAIN_ST            *gain ,
    TABLE_HEAD_ST          *tppad_h,      TSS_TPPAD_ST            *tppad ,
    TABLE_HEAD_ST        *tppixel_h,    TSS_TPPIXEL_ST          *tppixel )
{
/*:>--------------------------------------------------------------------
**: ROUTINE:    reformat_
**: DESCRIPTION:
**:             This routine translates data from the "new" raw data
**:             format to tppad/tppixel tables
**: AUTHOR:     N.T.B. Stone
**: ARGUMENTS:
**:       IN:
**:           indextbl    - Index Table
**:          indextbl_    - header Structure for indextbl
**:           bytedata    - Data Table (8-bit)
**:          bytedata_    - header Structure for bytedata
**:          shortdata    - Data Table (10-bit)
**:         shortdata_    - header Structure for shortdata
**:          structtbl    - Structure Table
**:         structtbl_    - header Structure for structtbl
**:           gain_bad    - Online Gains / Status table
**:          gain_bad_    - header Structure for gain_bad
**:    INOUT:
**:             fmtpar    - reformat steering switches, etc.
**:            fmtpar_    - header Structure for fmtpar
**:         native_map    - geometry map (for "native" data)
**:        native_map_    - header Structure for native_map
**:              valid    - list of valid group numbers (for "native" data)
**:             valid_    - header Structure for valid
**:           pedestal    - pedestal event averaged adc values
**:          pedestal_    - header Structure for pedestal
**:             pulser    - pulser event averaged adc values
**:            pulser_    - header Structure for pulser
**:               gain    - gain correction factor
**:              gain_    - header Structure for gain
**:      OUT:
**:              tppad    - TPPad information table
**:             tppad_    - header Structure for tppad
**:            tppixel    - TPPixel data table
**:           tppixel_    - header Structure for tppixel
**:
**: RETURNS:    STAF Condition Value
**:
**:>------------------------------------------------------------------
**: fmtpar switch definitions:
**:
**:   event_type = -3   Extract online gains into gains table
**:   event_type = -1	Geometry data
**:   event_type = 0	"Normal" data
**:              -->	Re-format zero-suppressed data
**:              -->	Ped/Gain-correct AND Re-format "black" data
**:   event_type = 1	Pedestal data (do averaging)
**:   event_type = 2	Pulser data (do averaging)
**:   event_type = 3	Gain Calculation (Subtract Ped from Pulser & integrate)
**:
**:    data_size =-1    Use Byte-Data table (no non-linear conversion)
**:    data_size = 1    Use Byte-Data table (with non-linear conversion)
**:    data_size = 2    Use Short-Data table
**:
**: ped_subtract = 0    Apply NEITHER pedestals NOR gains
**: ped_subtract = 1    Apply pedestals AND gains
**: ped_subtract = 2    Apply gains ONLY
**:
**:>------------------------------------------------------------------*/

/* define some STATIC global maps */
    static unsigned char mapIndex[nSectors][nRDOs]; /* maps sec/RDO to ID */
    static unsigned char mapSector[nBoards];        /* maps ID to sector */
    static unsigned char mapRDO[nBoards];           /* maps ID to RDO */
    static unsigned char mapGood[nBoards][nGroups];	/* 1 or 0 */
    static unsigned char mapRow[nBoards][nGroups][nChans];
    static unsigned char mapPad[nBoards][nGroups][nChans];

    int    bucket_a, bucket_b;
    int    dptr_inc;
    int    i, j, k, l, rdoID, secID;
    long   iptr, sptr, dptr;
    long   adcdata_nok;
    long   g_indx;                      /* Pointer index into gain buffer */
    long   nPad;
    int    pad, row, group, chan, fee, fee0;
    int    nSeq, nSeq0, nSeq1, nBuck, nBuck0, bucket;
    int    l_ped_subtract;                  /* Local switch for ped. sub. */
    int    l_gain_correct;                  /* Local switch for gain corr. */
    int    sector, RDO;
    int    istat;
    int    nPed;                            /* pedestal value plus threshold */
    int    pad_starting;                    /* TRUE if ped/pul. starts on pad*/
    long   inok, tnok;                      /* nok header counters */
    long   pnok, pnok0, gnok;               /* more header counters */
    long   datum;

    int zero_suppressed, mapped, warned;
    int adc_8bit;                           /* Use 8-bit data */
    int adc_nonlin;                         /* 8-bit data is non-linear */
    int adc_10bit;                          /* Use 10-bit data */

    float  oldrms, oldave, oldvar, newvar;
    float  gain_pad;                        /* gain for current pad */
    float  threshold;                       /* thresh + time_rms*RMS */
    double rms2;

    static INDX_MAPPED indx_mapped[nPadsTotal];
    static int first_call=1;

    /* Check all headers first */
    if(( indextbl_h->maxlen < 0 ) ||
       ( indextbl_h->nok > indextbl_h->maxlen ) ||
       ( bytedata_h->maxlen < 0 ) ||
       ( bytedata_h->nok > bytedata_h->maxlen ) ||
       ( shortdata_h->maxlen < 0 ) ||
       ( shortdata_h->nok > shortdata_h->maxlen ) ||
       ( structtbl_h->maxlen < 0 ) ||
       ( structtbl_h->nok > structtbl_h->maxlen ) ||
       ( tppad_h->maxlen < 0 ) ||
       ( tppad_h->nok > tppad_h->maxlen ) ||
       ( tppixel_h->maxlen < 0 ) ||
       ( tppixel_h->nok > tppixel_h->maxlen ) ){
	printf(" <E> reformat: Error in incoming table sizes!!!\n");
	return STAFCV_BAD;
    }

    if (first_call){
	first_call = 0;
	for (i=0; i<nBoards; i++){
	    for (j=0; j<nGroups; j++){
		mapGood[i][j] = 0;
		for (k=0; k<nChans; k++){
		    mapRow[i][j][k] = 0;
		    mapPad[i][j][k] = 0;
		}
	    }
	}
/*
 * Initialize ped and gain access information:
 */
	if (init_indx(indx_mapped)) {
	    printf("main: Fatal error return from init_indx.");
            return STAFCV_BAD;
	}
        istat = tfc_store(
		      fmtpar_h,         fmtpar ,
		      native_map_h,	native_map ,
		      valid_h,		valid ,
		      mapIndex,
		      mapSector,
		      mapRDO,
		      mapGood,
		      mapRow,
		      mapPad);
        if (istat) {
            printf(" <E> reformat: Storage of incoming tables failed!!!\n");
            return STAFCV_BAD;
        } else {
            /* printf(" <I> reformat: "
	       "Storage of incoming tables was successful.\n"); */
        }
    }

    if (fmtpar[0].printf >= 1)
	printf (" <I> reformat: Total IndexTBL entries for this event: %d\n", 
		indextbl_h->nok);

    if (indextbl_h->nok >= 1){
	sptr   = indextbl[0].struct_row;
	dptr   = indextbl[0].data_row;
	sector = indextbl[0].sector;
	RDO    = indextbl[0].rdo_loc;
	if (fmtpar[0].printf >= 1)
	    printf(" <I> reformat: "
		   "Initial sector, RDO = %d, %d\n", sector, RDO);
	rdoID  = get_ID (1, sector, RDO, mapIndex, mapSector, mapRDO);
	if (rdoID < 0){
	    printf(" <E> reformat: Invalid initial sector/RDO combination!\n");
	    return STAFCV_BAD;
	}
    }
    else{
	return STAFCV_BAD;         /* Otherwise, RDOid is used unitialized */
    }



    if (fmtpar[0].event_type == -1){
	if (fmtpar[0].printf >= 1){
	    printf(" <I> reformat: "
		   "Event_Type - Extracting geometry ");
	    if (abs(fmtpar[0].data_size) == 1)
		printf("from bytedata table...\n");
	    else
		printf("from shortdata table...\n");
	}
    } else if (fmtpar[0].event_type == 0) {
	if (fmtpar[0].printf >= 1)
	    printf(" <I> reformat: "
		   "Event_Type - Processing \"normal\" data...\n");
    } else if (fmtpar[0].event_type == 1) {
	if (fmtpar[0].printf >= 1)
	    printf(" <I> reformat: "
		   "Event_Type - Averaging Pedestals...\n");
        nPed = fmtpar[0].nped_samples[rdoID];
	if (fmtpar[0].printf >= 1)
	    printf(" <I> reformat: nped_samples(%d) = %d\n", rdoID, nPed);
        pnok = pedestal_h->nok;
    } else if (fmtpar[0].event_type == 2) {
	if (fmtpar[0].printf >= 1)
	    printf("<I> reformat: Event_Type - Averaging Pulsers...\n");
        nPed = fmtpar[0].npuls_samples[rdoID];
	if (fmtpar[0].printf >= 1)
	    printf(" <I> reformat: npuls_samples(%d) = %d\n", rdoID, nPed);
        pnok = pulser_h->nok;
    } else if (fmtpar[0].event_type == 3) {
	printf(" <I> reformat: Event_Type - Calculating Gains...\n");
	if (pedestal_h->nok == 0) {
	    printf(" <E> reformat: Cannot subtract pedestal from pulser "
		  "tables when they are empty!!\n");
	    return STAFCV_BAD;
	}
	istat = calculate_gain(
			       fmtpar_h,	fmtpar ,
			       pedestal_h,	pedestal ,
			       pulser_h,	pulser ,
			       gain_h,		gain ,
			       mapIndex,
			       mapSector,
			       mapRDO);
	if (istat) {
	    printf(" <E> reformat: Gain calculation failed!!!\n");
	    return STAFCV_BAD;
	} else {
	    printf(" <I> reformat: Gain calculation successful.\n");
	    return STAFCV_OK;
	}
    } else if (fmtpar[0].event_type == -3){
	printf(" <I> reformat: Event_Type - Extracting Online Gains...\n");
    } else {
	printf(" <E> reformat: Event_Type - Unknown (%d)\n",
	       fmtpar[0].event_type);
	return STAFCV_BAD;
    }

/*
 * Define local switches for pedestal subtraction and gain correction
 * to guard against stupid compilers or compilation without optimization.
 */
    if (fmtpar[0].ped_subtract == 1) {
	l_ped_subtract = TRUE;
	l_gain_correct = TRUE;
    }
    else if (fmtpar[0].ped_subtract == 2) {
	l_ped_subtract = FALSE;
	l_gain_correct = TRUE;
    }
    else {
	l_ped_subtract = FALSE;
	l_gain_correct = FALSE;
    }

/* Check that pedestal and gain tables exist, if they are needed */
    if (fmtpar[0].event_type == 0){
	if (l_ped_subtract && pedestal_h->nok == 0) {
	    printf(" <E> reformat: Cannot subtract pedestal tables "
		   "when they are empty!!\n");
	    return STAFCV_BAD;
	}
	if (fmtpar[0].times_rms>0){
	    printf(" <E> reformat: Cannot scale pedestal rms table"
		   "when it is empty!!\n");
	    return STAFCV_BAD;
	}
	if (l_gain_correct && gain_h->nok == 0){
	    printf(" <E> reformat: "
		   "Cannot apply gains when they are empty!!\n");
	    return STAFCV_BAD;
	}
    }

    /* Check some critical switches */
    if (fmtpar[0].thresh < 1){
	printf(" <W> reformat: zero/negative threshold constant used!\n");
    }
    threshold = fmtpar[0].thresh;
    
    if (fmtpar[0].first_bucket < 1){
	printf(" <W> reformat: zero/negative first_bucket is disallowed!\n"
	       "     resetting it to 1...\n");
	fmtpar[0].first_bucket = 1;
    }
    if (fmtpar[0].last_bucket > 512){
	printf(" <W> reformat: last_bucket > 512 is disallowed!\n"
	       "     resetting it to 512...\n");
	fmtpar[0].last_bucket = 512;
    }

    if (fmtpar[0].data_size == 1){
	adc_nonlin = TRUE;
	adc_8bit   = TRUE;
	adc_10bit  = FALSE;
    }
    else if (fmtpar[0].data_size == -1){
	adc_nonlin = FALSE;
	adc_8bit   = TRUE;
	adc_10bit  = FALSE;
    }
    else{
	adc_nonlin = FALSE;
	adc_8bit   = FALSE;
	adc_10bit  = TRUE;
    }

/*****************************************************************************/
/* Loop over all entries in the index table -------------------------------- */

    for (iptr = 0; iptr < indextbl_h->nok; iptr++){
	sptr   = indextbl[iptr].struct_row;
	dptr   = indextbl[iptr].data_row;
	sector = indextbl[iptr].sector;
	RDO    = indextbl[iptr].rdo_loc;

	if (fmtpar[0].printf >= 2)
	    printf(" <I> reformat: sptr   = %d\n"
		   "               dptr   = %d\n"
		   "               sector = %d\n"
		   "               RDO    = %d\n",
		   sptr, dptr, sector, RDO);

	/* check the sector/RDO integrity */
	if (RDO < 1 || RDO > 6){
	    printf(" <E> reformat: Invalid initial RDO number: %d\n", RDO);
	    return STAFCV_BAD;
	}
	if (sector < 1 || sector > 24){
	    printf(" <E> reformat: Invalid initial sector number: %d\n", 
		   sector);
	    return STAFCV_BAD;
	}

	/* Check the structure pointer integrity */
	if (sptr > structtbl_h->nok){
	    printf(" <E> reformat: initial structure pointer out of range!\n"
		   "     sptr, structtbl_h->nok = %d, %d\n", 
		   sptr, structtbl_h->nok);
	    continue;
	}
	
	/* Check the data pointer integrity, if this is not a counter entry */
	if (indextbl[iptr].data_type != 128)
	    if (fmtpar[0].event_type == -3){
		if (dptr > gain_bad_h->nok){
		    printf(" <E> reformat: initial gain data pointer out of "
			   "range!\n"
			   "     dptr, gain_bad_h->nok  = %d, %d\n",
			   dptr, gain_bad_h->nok);
		    continue;
		}
	    } else {
		if (adc_8bit)
		    adcdata_nok = bytedata_h->nok;
		else
		    adcdata_nok = shortdata_h->nok;

		if (dptr > adcdata_nok){
		    printf(" <E> reformat: initial adc data pointer out of "
			   "range!\n"
			   "     dptr, adcdata_nok  = %d, %d\n",
			   dptr, adcdata_nok);
		    continue;
		}
	    }

	/* Obtain an ID for this sector/RDO pair */
	rdoID  = get_ID (1, sector, RDO, mapIndex, mapSector, mapRDO);
	if (rdoID < 0) return STAFCV_BAD;
	secID  = get_ID (0, sector, RDO, mapIndex, mapSector, mapRDO);
	if (secID < 0) return STAFCV_BAD;

	zero_suppressed = 255;
	mapped = 255;

	switch (indextbl[iptr].data_type){

/* Zero-Suppressed Mapped data --------------------------------------------- */
	case 0: case 1:
	    if (fmtpar[0].printf >= 1)
		printf (" <I> reformat: "
			"Data_Type %d - Zero-Suppressed, Mapped\n",
			indextbl[iptr].data_type);
	    if (fmtpar[0].only_type >= 0 &&
		fmtpar[0].only_type != 0 && fmtpar[0].only_type != 1){
		if (fmtpar[0].printf >= 1)
		    printf(" <W> reformat: Not processing this data_type.\n");
		continue;
	    }

	    zero_suppressed = 1;
	    mapped = 1;
	    row    = indextbl[iptr].rgm;
	    nPad  = structtbl[sptr++].info;

	    if (fmtpar[0].printf >= 2)
		printf(" <I> reformat: row, nPad = %d, %d\n", row, nPad);

	    break;

/* Zero-Suppressed Native data --------------------------------------------- */
	case 2: case 3:
	    if (fmtpar[0].printf >= 1)
		printf (" <I> reformat: "
			"Data_Type %d - Zero-Suppressed, Native\n",
			indextbl[iptr].data_type);
	    if (fmtpar[0].only_type >= 0 &&
		fmtpar[0].only_type != 2 && fmtpar[0].only_type != 3){
		if (fmtpar[0].printf >= 1)
		    printf(" <W> reformat: Not processing this data_type.\n");
		continue;
	    }

	    zero_suppressed = 1;
	    mapped = 0;
	    group  = indextbl[iptr].rgm;
	    nPad  = structtbl[sptr++].info;

	    if (fmtpar[0].printf >= 2)
		printf(" <I> reformat: group, nPad = %d, %d\n", group, nPad);

	    break;

/* Black Mapped data ------------------------------------------------------- */
	case 4: case 5:
	    if (fmtpar[0].printf >= 1)
		printf (" <I> reformat: Data_Type %d - Black, Mapped\n", 
			indextbl[iptr].data_type);
	    if (fmtpar[0].only_type >= 0 &&
		fmtpar[0].only_type != 4 && fmtpar[0].only_type != 5){
		if (fmtpar[0].printf >= 1)
		    printf(" <W> reformat: Not processing this data_type.\n");
		continue;
	    }

	    zero_suppressed = 0;
	    mapped = 1;
	    row    = indextbl[iptr].rgm;
	    nPad  = structtbl[sptr++].info;

	    if (fmtpar[0].printf >= 2)
		printf(" <I> reformat: row, nPad = %d, %d\n", row, nPad);

	    break;

/* Black Native data ------------------------------------------------------- */
	case 6: case 7:
	    if (fmtpar[0].printf >= 1)
		printf (" <I> reformat: Data_Type %d - Black, Native\n", 
			indextbl[iptr].data_type);
	    if (fmtpar[0].only_type >= 0 &&
		fmtpar[0].only_type != 6 && fmtpar[0].only_type != 7){
		if (fmtpar[0].printf >= 1)
		    printf(" <W> reformat: Not processing this data_type.\n");
		continue;
	    }

	    zero_suppressed = 0;
	    mapped = 0;
	    group  = indextbl[iptr].rgm;
	    nPad  = structtbl[sptr++].info;

	    if (fmtpar[0].printf >= 2)
		printf(" <I> reformat: group, nPad = %d, %d\n", group, nPad);

	    break;

/* Mapped Gains ------------------------------------------------------------ */
	case 17:
	    if (fmtpar[0].printf >= 1)
		printf (" <I> reformat: Data_Type %d - Mapped Gains\n", 
			indextbl[iptr].data_type);
	    if (fmtpar[0].only_type >= 0 && fmtpar[0].only_type != 17 ){
		if (fmtpar[0].printf >= 1)
		    printf(" <W> reformat: Not processing this data_type.\n");
		continue;
	    }

	    row   = indextbl[iptr].rgm;
	    nPad  = structtbl[sptr++].info;
	    if (fmtpar[0].printf >= 2)
		printf(" <I> reformat: row, nPad = %d, %d\n", row, nPad);

	    /* Process these pads */
	    for (i=0; i<nPad; i++){
		pad = structtbl[sptr++].info;
		if (fmtpar[0].printf >= 2)
		    printf(" <I> reformat: pad = %d\n", pad);

		if (sptr > structtbl_h->nok){
		    printf(" <E> reformat: sptr incremented out of range!\n");
		    break;
		}

		/* Skip un-mapped pads */
		if (row == 0 || pad == 0)
		    continue;

		/* Ensure no duplicates in gain table... */
		g_indx = get_gain_indx_mapped(indx_mapped,
					      sector, row, pad);
		if (g_indx<0) {
		    gnok = gain_h->nok++;
		    put_gain_indx_mapped(indx_mapped, gnok,
					 sector, row, pad);
		}
		else {
		    printf(" <W> reformat: "
			   "Already existing gain replaced for "
			   "sector, RDO, row, pad"
			   " = %d %d %d %d\n",
			   sector, RDO, row, pad);
		    gnok = g_indx;
		}
		gain[gnok].sector = sector;
		gain[gnok].RDO = RDO;
		gain[gnok].row = row;
		gain[gnok].pad = pad;
		gain[gnok].gain = gain_bad[dptr++].gain / fmtpar[0].ave_gain;
	    }
	    continue;

/* Native Gains ------------------------------------------------------------ */
	case 19:
	    if (fmtpar[0].printf >= 1)
		printf (" <I> reformat: Data_Type %d - Native Gains\n", 
			indextbl[iptr].data_type);
	    if (fmtpar[0].only_type >= 0 && fmtpar[0].only_type != 19 ){
		if (fmtpar[0].printf >= 1)
		    printf(" <W> reformat: Not processing this data_type.\n");
		continue;
	    }

	    group = indextbl[iptr].rgm;
	    nPad  = structtbl[sptr++].info;
	    if (fmtpar[0].printf >= 2)
		printf(" <I> reformat: group, nPad = %d, %d\n", group, nPad);

	    /* Process these pads */
	    for (i=0; i<nPad; i++){
		chan = structtbl[sptr++].info;
		if (fmtpar[0].printf >= 2)
		    printf(" <I> reformat: chan = %d\n", chan);

		row = mapRow[rdoID][group-1][chan-1];
		pad = mapPad[rdoID][group-1][chan-1];


		if (sptr > structtbl_h->nok){
		    printf(" <E> reformat: sptr incremented out of range!\n");
		    break;
		}

		/* Skip un-mapped pads */
		if (row == 0 || pad == 0)
		    continue;

		/* Ensure no duplicates in gain table... */
		g_indx = get_gain_indx_mapped(indx_mapped,
					      sector, row, pad);
		if (g_indx<0) {
		    gnok = gain_h->nok++;
		    g_indx = put_gain_indx_mapped(indx_mapped, gnok,
						  sector, row, pad);
		}
		else {
		    printf(" <W> reformat: "
			   "Already existing gain replaced for "
			   "sector, RDO, row, pad"
			   " = %d %d %d %d\n",
			   sector, RDO, row, pad);
		    gnok = g_indx;
		}
		gain[gnok].sector = sector;
		gain[gnok].RDO = RDO;
		gain[gnok].row = row;
		gain[gnok].pad = pad;
		gain[gnok].gain = gain_bad[dptr++].gain / fmtpar[0].ave_gain;
	    }
	    continue;

/* Process Structure-table geometry entries -------------------------------- */
	case 48:
	    if (fmtpar[0].printf >= 1)
		printf(" <I> reformat: Data_Type %d - Geometry data\n", 
		       indextbl[iptr].data_type);
	    if (fmtpar[0].only_type >= 0 && fmtpar[0].only_type != 48){
		if (fmtpar[0].printf >= 1)
		    printf(" <W> reformat: Not processing this data_type.\n");
		continue;
	    }

	    for (i=0 ; i<36; i++, sptr++){
		if (sptr > structtbl_h->nok){
		    printf(" <E> reformat: sptr incremented out of range!\n");
		    break;
		}
		fee = structtbl[sptr].info;
		if (fmtpar[0].printf >= 2)
		    printf(" <I> reformat: new fee = %d\n", fee);

		if (fee <= feeMax){
		    for (j=0; j<2; j++){ /* 2 groups per card */
			group = FEEid2group[i][j];
			if (! mapGood[rdoID][group-1]){
			    /* enable processing of these groups */
			    if (fmtpar[0].printf >= 1){
				printf(" <I> reformat: "
				       "Adding sector, RDO, group = "
				       " %d, %d, %d\n", sector, RDO, group);
			    }
			    inok = valid_h->nok++;
			    valid[inok].group  = group;
			    valid[inok].RDO    = RDO;
			    valid[inok].sector = sector;
			    mapGood[rdoID][group-1] = 1;
			}

			/* Extract row/pad information for this group */
			for (chan=1; chan<=16; chan++){
			    if (mapRow[rdoID][group-1][chan-1]==0){
				row = row_fee[fee][nTimesSeen(group,chan)-1];
				pad = pad_fee[fee][nTimesSeen(group,chan)-1];

				if (row > 0 && row <= 45 &&
				    pad > 0 && pad <= 200){
				    inok = native_map_h->nok++;
				    native_map[inok].sector  = sector;
				    native_map[inok].RDO     = RDO;
				    native_map[inok].group   = group;
				    native_map[inok].channel = chan;
				    native_map[inok].row     = row;
				    native_map[inok].pad     = pad;
			    
				    mapRow[rdoID][group-1][chan-1] = row;
				    mapPad[rdoID][group-1][chan-1] = pad;
				}
			    }
			}
		    }
		} else {
		    /* printf(" <E> reformat: "
		       "Invalid FEE card number: %d\n",fee); */
		}
	    }
	    continue; /* don't go down to data processing */
	    
/* Echo the presence of Header entries ------------------------------------- */
	case 64:
	    if (fmtpar[0].printf >= 1)
		printf(" <I> reformat: Data_Type %d - RDO Header\n",
		       indextbl[iptr].data_type);
	    if (fmtpar[0].only_type >= 0 && fmtpar[0].only_type != 64){
		if (fmtpar[0].printf >= 1)
		    printf(" <W> reformat: Not processing this data_type.\n");
		continue;
	    }

	    continue; /* don't go down to data processing */

/* Event number entry ------------------------------------------------------ */
	case 128:
	    if (fmtpar[0].printf >= 1)
		printf(" <I> reformat: Data_Type %d - Event Number\n", 
		       indextbl[iptr].data_type);
	    if (fmtpar[0].only_type >= 0 && fmtpar[0].only_type != 128){
		if (fmtpar[0].printf >= 1)
		    printf(" <W> reformat: Not processing this data_type.\n");
		continue;
	    }

	    if (fmtpar[0].printf >= 1)
		printf(" <I> reformat: Event number is: %d\n", dptr);

	    continue; /* don't go down to data processing */

	default:
	    printf (" <W> reformat: "
		    "Data_Type %d: Un-known\n", indextbl[iptr].data_type);
	    continue;

	} /* switch on data_type */

	if (zero_suppressed == 255 || mapped == 255){
	    printf(" <E> reformat: Invalid data_type flagging !\n");
	    continue;
	}

/*****************************************************************************/
/* Process data tables ----------------------------------------------------- */

	/* Zero-suppressed data -------------------------------------------- */
	if (zero_suppressed) {
		       
	    /* Skip this channel, if it is not valid */
	    if (!mapped)
		if (!mapGood[rdoID][group-1])
		    continue;

	    /* Process these pads */
	    for (i=0; i<nPad; i++){
		if (mapped) {
		    pad = structtbl[sptr++].info;
		    if (fmtpar[0].printf >= 2)
			printf(" <I> reformat: pad = %d\n", pad);
		} else {
		    chan = structtbl[sptr++].info;
		    if (fmtpar[0].printf >= 2)
			printf(" <I> reformat: chan = %d\n", chan);
		    row = mapRow[rdoID][group-1][chan-1];
		    pad = mapPad[rdoID][group-1][chan-1];
		}

		/* Skip un-mapped pads, but keep the pointers straight */
	        if (row==0 || pad == 0){
		    if (fmtpar[0].printf >= 2)
			printf(" <I> reformat: Un-Mapped pad.  Skipping...\n");
		    if (sptr+2 > structtbl_h->nok){
			printf(" <E> reformat: "
			       "sptr will be incremented out of range!\n");
			goto end_index_entry;
		    }
		    nSeq0 = structtbl[sptr++].info;
		    nSeq1 = structtbl[sptr++].info;
		    if (sptr+2*(nSeq0+nSeq1) > structtbl_h->nok){
			printf(" <E> reformat: "
			       "sptr will be incremented out of range!\n");
			goto end_index_entry;
		    }
		    for (j=0; j<(nSeq0+nSeq1); j++){
			sptr++;
			nBuck0 = structtbl[sptr++].info+1;
			dptr  += nBuck0;
		    }
		    continue;
	        }

		/* Find gain table index */
		if (l_gain_correct) {
		    gnok = get_gain_indx_mapped(indx_mapped,
						sector, row, pad);
		    if (gnok<0) {
			printf(" <E> reformat: "
			       "Could not locate pre-existant "
			       "sector, RDO, row, pad"
			       " = %d %d %d %d\n",
			       sector, RDO, row, pad);
			return STAFCV_BAD;
		    }
		    gain_pad = gain[gnok].gain;
		    /* not a good idea to divide by zero */
                    if(gain_pad<=0.0)gain_pad=1.0;
		}
		else {
		    gain_pad = 1.;
		}

		if (sptr+2 > structtbl_h->nok){
		    printf(" <E> reformat: "
			   "sptr will be incremented out of range!\n");
		    goto end_index_entry;
		}
		nSeq0 = structtbl[sptr++].info;
		nSeq1 = structtbl[sptr++].info;
		if (fmtpar[0].printf >= 2)
		    printf(" <I> reformat: nSeq0 = %d\n"
			   "               nSeq1 = %d\n", nSeq0, nSeq1);

		if (sptr+2*(nSeq0+nSeq1) > structtbl_h->nok){
		    printf(" <E> reformat: "
			   "sptr will be incremented out of range!\n");
		    goto end_index_entry;
		}
		nSeq = 0;  /* the number of sequences above threshold */
		tnok = tppixel_h->nok;

		/* process all sequences on this pad */
		for (j=0; j<(nSeq0+nSeq1); j++){
		    if (j<nSeq0)
			bucket = structtbl[sptr++].info;
		    else
			bucket = structtbl[sptr++].info+256;
		    
		    nBuck = 0;
		    nBuck0 = structtbl[sptr++].info+1;
		    if (fmtpar[0].printf >= 2)
			printf(" <I> reformat: nBuck0 = %d\n", nBuck0);

		    if (dptr+nBuck0 > adcdata_nok){
			printf(" <E> reformat: dptr will be incremented "
			       "beyond end of data table!\n");
			goto end_index_entry;
		    }

/*
 * Time buckets in tppixel are numbered starting at zero;
 * the numbering is then offset in tcl to start at one;
 * however, first_bucket and last_bucket start at one:
 */
		    dptr_inc = (fmtpar[0].first_bucket-1) - bucket;
		    if(dptr_inc<=0)
			bucket_a = bucket;
		    else{
			bucket_a = (fmtpar[0].first_bucket-1);
			dptr = dptr + dptr_inc;
		    }

		    dptr_inc = (bucket+nBuck0-1) - (fmtpar[0].last_bucket-1);
		    if(dptr_inc<=0) {
			bucket_b = (bucket+nBuck0-1);
			dptr_inc = 0;
		    }
		    else{
			bucket_b = (fmtpar[0].last_bucket-1);
		    }

		    /* transfer above-threshold buckets in this sequence */
		    for (k=bucket_a; k<=bucket_b; k++){
			if (fmtpar[0].printf >= 2)
			    printf(" <I> reformat: bucket = %d\n", k);

			if (adc_8bit){
			    if (adc_nonlin)
				datum = byte2short[bytedata[dptr++].data];
			    else
				datum = bytedata[dptr++].data;
			}
			else
			    datum = shortdata[dptr++].data;

			/* do gain correction */
			datum = (datum+0.5)/gain_pad;

			/* Limit datum to 10 bits */
			if (datum<0)
			    datum=0;
			else if (datum>1023)
			    datum=1023;

			if (fmtpar[0].printf >= 2)
			    printf(" <I> reformat: final datum = %d\n", datum);

			/* find pedestal info */
			if (fmtpar[0].times_rms > 0) {
			    /* Ensure proper addressing... */
			    pnok = get_ped_indx_mapped(indx_mapped,
						       sector, row, pad);
			    if (pnok==-1) {
				printf(" <E> reformat: "
				       "Could not locate pre-existant "
				       "sector, RDO, row, pad"
				       " = %d %d %d %d\n",
				       sector, RDO, row, pad);
				return STAFCV_BAD;
			    }
			    pnok += k; 
			    threshold = fmtpar[0].thresh
				+ pedestal[pnok].rms*fmtpar[0].times_rms;
			}

			/* Add this pixel to the pixel table */
			if (datum >= threshold){
			    inok = tppixel_h->nok++;
			    if (tppixel_h->nok > tppixel_h->maxlen){
				printf(" <E> reformat: "
				       "Incr. tppixel out of range!\n");
				return STAFCV_BAD;
			    }
			    tppixel[inok].datum = datum + Bfact*k;
			    nBuck++;
			}

			/* see if we need to close out this sequence */
			if (nBuck > 0 &&
			    (datum < threshold || k == bucket_b)) {
			    for (l=tppixel_h->nok-nBuck;
				 l<=tppixel_h->nok-1;
				 l++)
				tppixel[l].datum += nBuck*nBfact;
			    nSeq++;
			    nBuck = 0;
			}
		    } /* End of loop k over buckets */
		    dptr = dptr + dptr_inc;
		}

		/* Increment tppad, if there were any sequences on this pad */
		if (nSeq > 0){
		    inok = tppad_h->nok++;
		    if (tppad_h->nok > tppad_h->maxlen){
			printf(" <E> reformat: "
			       "Incremented tppad out of range!\n");
			return STAFCV_BAD;
		    }
		    tppad[inok].jpix    = tnok+1;
		    tppad[inok].nseq    = nSeq;
		    tppad[inok].secpad  = pad;
		    tppad[inok].tpc_row = 100*sector + row;
		}
	    }
	}

	/* Black data ----------------------------------------------------- */
	else {

	    /* Skip this channel, if it is not valid */
	    if (!mapped)
		if (!mapGood[rdoID][group-1])
		    continue;

	    /* Process these pads */
	    for (i=0; i<nPad; i++){
		if (mapped) {
		    pad = structtbl[sptr++].info;
		    if (fmtpar[0].printf >= 2)
			printf(" <I> reformat: pad = %d\n", pad);
		}
		else {
		    chan = structtbl[sptr++].info;
		    if (fmtpar[0].printf >= 2)
			printf(" <I> reformat: chan = %d\n", chan);
		    row = mapRow[rdoID][group-1][chan-1];
		    pad = mapPad[rdoID][group-1][chan-1];
		}

		if (sptr > structtbl_h->nok){
		    printf(" <E> reformat: sptr incremented out of range!\n");
		    break;
		}

		/* Skip un-mapped pads, but keep the pointers straight */
		if (row == 0 || pad == 0){
		    if (fmtpar[0].printf >= 2)
			printf(" <I> reformat: Un-Mapped pad.  Skipping...\n");
		    dptr += 512;
		    continue;
		}

		/* Find gain table index */
		if (fmtpar[0].event_type == 0 && l_gain_correct){
		    gnok = get_gain_indx_mapped(indx_mapped,
						sector, row, pad);
		    if (gnok<0) {
			printf(" <E> reformat: "
			       "Could not locate pre-existant "
			       "sector, RDO, row, pad"
			       " = %d %d %d %d\n",
			       sector, RDO, row, pad);
			return STAFCV_BAD;
		    }
		    gain_pad = gain[gnok].gain;
		}
		else {
		    gain_pad = 1.;
		}

		tnok = tppixel_h->nok;

		/* Check that data pointer will remain within bounds */
		if (dptr+512 > adcdata_nok){
		    printf(" <E> reformat: dptr will be incremented "
			   "beyond end of data table!\n");
		    goto end_index_entry;
		}

		/* Initialize; only "normal" data uses these */
		nBuck = 0;
		nSeq = 0;

	/* Process all buckets on this pad --------------------------------- */
		pad_starting = FALSE;
		for (j=0; j<512; j++) {
		    if (fmtpar[0].printf >= 2)
			printf(" <I> reformat: bucket = %d\n", j+1);
		  
		    /* get the adc value for this bucket */
		    if (adc_8bit){
			if (adc_nonlin)
			    datum = byte2short[bytedata[dptr++].data];
			else
			    datum = bytedata[dptr++].data;
		    }
		    else{
			datum = shortdata[dptr++].data;
		    }

	/* "Normal" Data --------------------------------------------------- */
		    if (fmtpar[0].event_type == 0) {
			/* Don't process this bucket if it is out of range */
			if (j < fmtpar[0].first_bucket-1 ||
			    j > fmtpar[0].last_bucket-1) continue;

			/* find pedestal info */
			if (l_ped_subtract) {
			    pnok = get_ped_indx_mapped(indx_mapped,
						       sector, row, pad);
			    if (pnok==-1) {
				printf(" <E> reformat: "
				       "Could not locate pre-existant "
				       "sector, RDO, row, pad, bucket"
				       " = %d %d %d %d %d\n",
				       sector, RDO, row, pad, j+1);
				return STAFCV_BAD;
			    }
			    pnok += j; 
			    
			    datum -= pedestal[pnok].ave;
			    if (datum < 0) datum = 0;

			    threshold = fmtpar[0].thresh
				+ pedestal[pnok].rms*fmtpar[0].times_rms;
			}

			/* do gain correction */
			datum = (datum+0.5)/gain_pad;

			/* Limit datum to 10 bits */
			if (datum<0)
			    datum=0;
			else if (datum>1023)
			    datum=1023;

			if (fmtpar[0].printf >= 2)
			    printf(" <I> reformat: final datum = %d\n", datum);

			/* Add this pixel to the pixel table (maybe) */
			if (datum >= threshold){
			    inok = tppixel_h->nok++;
			    if (tppixel_h->nok > tppixel_h->maxlen){
				printf(" <E> reformat: "
				       "Incremented tppixel out of range!\n");
				return STAFCV_BAD;
			    }
			    tppixel[inok].datum = datum + Bfact*j;
			    nBuck++;
			}
			    
			/* If last pixel in seq, increment counters */
			if (nBuck > 0 && 
			    (j==fmtpar[0].last_bucket-1 || datum<threshold)){
			    nSeq++;
			    for (k=tppixel_h->nok-nBuck;
				 k<=tppixel_h->nok-1;
				 k++) tppixel[k].datum += nBuck*nBfact;
			    nBuck = 0;
			}
		    }

	/* Pedestal / Pulser Event ----------------------------------------- */
		    else if (fmtpar[0].event_type == 1 ||
			     fmtpar[0].event_type == 2) {

			/* Don't process this bucket if it is out of range */
			if (j < fmtpar[0].first_bucket-1 ||
			    j > fmtpar[0].last_bucket-1) continue;

			if (nPed == 0){
			    /* Look for index corresponding to bucket 1 */
			    if(!pad_starting){
				pad_starting = TRUE;
				pnok0 = get_ped_indx_mapped(indx_mapped,
							    sector, row, pad);
				if(pnok0==-1){
				    pnok = pedestal_h->nok++;
				    pnok0 = pnok - j;
				    put_ped_indx_mapped(indx_mapped, pnok0,
							sector, row, pad);
				}
			    }
			    else{
				pnok = pnok0 + j;
				pedestal_h->nok++;
			    }
			    pedestal[pnok].sector = sector;
			    pedestal[pnok].RDO    = RDO;
			    pedestal[pnok].row    = row;
			    pedestal[pnok].pad    = pad;
			    pedestal[pnok].bucket = j+1;
			    pedestal[pnok].rms    = 0;
			    pedestal[pnok].ave    = datum;
			} else {
			    /* Ensure proper addressing... */
			    if(!pad_starting){
				pad_starting = TRUE;
				pnok0 = get_ped_indx_mapped(indx_mapped,
							    sector, row, pad);
				if(pnok0==-1){
				    printf(" <E> reformat: "
					   "Could not locate pre-existant "
					   "sector, RDO, row, pad, bucket"
					   " = %d %d %d %d %d\n",
					   sector, RDO, row, pad, j+1);
				    return STAFCV_BAD;
				}
			    }
			    pnok = pnok0 + j;

			    oldave = pedestal[pnok].ave;
			    oldrms = pedestal[pnok].rms;
			    oldvar = sqr(oldrms) + sqr(oldave);

			    pedestal[pnok].ave=(oldave*nPed+datum)/(nPed+1.0);
			    newvar = (oldvar*nPed + sqr(datum)) / (nPed + 1.0);
			    rms2 = (double) newvar - 
				sqr(pedestal[pnok].ave);
			    if (rms2 > 0)
				pedestal[pnok].rms=(float)sqrt(rms2);
			    else
				pedestal[pnok].rms = 0;
			    
			    if (fmtpar[0].printf >= 2)
				printf(" <I> reformat: pedestal.ave = %f\n"
				       "               pedestal.rms = %f\n",
				       pedestal[pnok].ave, pedestal[pnok].rms);
			}
		    }

	/* "Old" Geometry Event -------------------------------------------- */
		    else if (fmtpar[0].event_type == -1) {
			fee = datum;
			if (j==0) {
			    fee0 = fee;
			    if (fmtpar[0].printf >= 2)
				printf(" <I> reformat: fee0 = %d\n", fee0);

			    row = row_fee[fee][nTimesSeen(group,chan)-1];
			    pad = pad_fee[fee][nTimesSeen(group,chan)-1];
			    warned = 0;
			    if (fmtpar[0].printf >= 1)
				printf(" <I> reformat: new fee = %d\n", fee0);

			} else if (fee != fee0) {
			    if (!warned){
				warned = 1;
				printf(" <W> reformat: "
				       "Non-uniform geometry data! "
				       "sector, RDO, group, chan, FEE0, FEE = "
				       "%2d, %1d, %2d, %2d, %3d, %3d\n"
				       "     Using FEE0.\n", 
				       sector, RDO, group, chan, fee0, fee);
			    }
			}
		    }

	/* Unknown Mode ---------------------------------------------------- */
		    else {
			printf(" <E> reformat: "
			       "Unknown processing mode: event_type = %d\n",
			       fmtpar[0].event_type);
			return STAFCV_BAD;
		    }

	/* Done looping over buckets.  Do some housekeeping for this pad --- */
		} /* j loop, over buckets */
		    
		/* Store extracted geometry values */
		if (fmtpar[0].event_type == -1 &&
		    row > 0 && row <= 45 &&
		    pad > 0 && pad <= feeMax){
		    inok = native_map_h->nok++;
		    native_map[inok].sector  = sector;
		    native_map[inok].RDO     = RDO;
		    native_map[inok].group   = group;
		    native_map[inok].channel = chan;
		    native_map[inok].row     = row;
		    native_map[inok].pad     = pad;
		    
		    /* pack them into the static arrays */
		    mapRow[rdoID][group-1][chan-1] = row;
		    mapPad[rdoID][group-1][chan-1] = pad;
		}
		
		/* increment the tppad table (if any sequences) */
		else if (nSeq > 0){
		    inok = tppad_h->nok++;
		    if (tppad_h->nok > tppad_h->maxlen){
			printf(" <E> reformat: "
			       "Incremented tppad out of range!\n");
			return STAFCV_BAD;
		    }
		    tppad[inok].jpix    = tnok+1;
		    tppad[inok].nseq    = nSeq;
		    tppad[inok].secpad  = pad;
		    tppad[inok].tpc_row = 100*sector + row;
		}
	    }
	}
    end_index_entry:
	continue;
    } /* loop through index table */

    if (fmtpar[0].event_type == 1) fmtpar[0].nped_samples[rdoID]++;
    if (fmtpar[0].event_type == 2) fmtpar[0].npuls_samples[rdoID]++;

    return STAFCV_OK;
}

/*:>--------------------------------------------------------------------
**:  Return the mapIndex for a sector/RDO combination,
**:  and update if it is a new one.
**:<------------------------------------------------------------------*/
int get_ID 
   (const int mode,
    int sector,
    int RDO,
    unsigned char mapIndex[nSectors][nRDOs],
    unsigned char mapSector[nBoards],
    unsigned char mapRDO[nBoards])
{
    int i, j, nSec;
    int counted[nSectors];

    /* Initialize the index reference tables */
    static int first_call=1;
    if (first_call){
	first_call = 0;
	for (i=0; i<nSectors; i++)
	    for (j=0; j<nRDOs; j++)
		mapIndex[i][j] = nBoards;
	for (i=0; i<nBoards; i++){
	    mapSector[i]=0;
	    mapRDO[i]=0;
	}
    }

    /* dont' take any garbage */
    if ( RDO < 1 || RDO > 6 || sector < 1 || sector > 24){
	printf(" <E> reformat: Illegal sector/RDO value(s)! "
	       "sector, RDO = %d, %d\n", sector, RDO);
	return -1;
    }

    /* check first to see if this pair has not been seen */
    if (mapIndex[sector-1][RDO-1] == nBoards){
	/* check to see if there is any more space in the map arrays */
	if (mapSector[nBoards-1] != 0){
	    printf(" <E> reformat: Insufficient mapping array space!\n"
		   "     More than %d boards have been mapped.\n", nBoards);
	    return -1;
	}
	/* Look for the next unused index */
	for (i=0; i<nBoards; i++){
	    if (mapSector[i]==0){
		printf(" <I> reformat: Assigning index %d to "
		       "sector %d, RDO %d\n", i, sector, RDO);
		mapSector[i] = sector;
		mapRDO[i] = RDO;
		mapIndex[sector-1][RDO-1] = i;
		break;
	    }
	}
    }
    if (mode == 1)
	return mapIndex[sector-1][RDO-1];
    else {
	nSec = 0;
	for (i=0; i<nSectors; i++)
	    counted[i]=0;
	for (i=0; i<nBoards; i++){
	    if (sector == mapSector[i])
		return nSec;
	    else if ((counted[mapSector[i]-1]++) == 0)
		nSec++;
	}
    }
    return 0;
}

/*:>--------------------------------------------------------------------
**:  Calculate the gain by subtracting pedestal run from pulser run,
**:  then integrating the values shown within some window
**:
**:  Possible Upgrades (?)
**:	- use global "map" tables to operate on both pedestal and pulser values
**:	  (non-essential while tables have only one sector/RDO combination)
**:<------------------------------------------------------------------*/
int calculate_gain
   (TABLE_HEAD_ST         *fmtpar_h,     TFC_FMTPAR_ST           *fmtpar ,
    TABLE_HEAD_ST       *pedestal_h, TFC_NATIVE_PEDESTAL_ST    *pedestal ,
    TABLE_HEAD_ST         *pulser_h, TFC_NATIVE_PEDESTAL_ST      *pulser ,
    TABLE_HEAD_ST           *gain_h, TFC_NATIVE_GAIN_ST            *gain ,
    unsigned char mapIndex[nSectors][nRDOs],
    unsigned char mapSector[nBoards],
    unsigned char mapRDO[nBoards])
{
    int i, j, k, l, channels;
    int secID, inok;
    int   used[nBoards][nRows][nPads];
    float gain_ave, rawsignal;
    float netADC[nBoards][nRows][nPads][nBuckets];
    float mapPed[nBoards][nRows][nPads][nBuckets];
    float mapRMS[nBoards][nRows][nPads][nBuckets];

    if (fmtpar[0].printf >= 1)
	printf(" <I> reformat: Packing Pedestal table...\n");
    for (i=0; i<pedestal_h->nok; i++){
	secID = get_ID (0, pedestal[i].sector, pedestal[i].RDO,
			mapIndex, mapSector, mapRDO);
	if (secID < 0) return 1;
	mapPed[secID][pedestal[i].row-1][pedestal[i].pad-1]
	    [pedestal[i].bucket-1] = pedestal[i].ave;
	mapRMS[secID][pedestal[i].row-1][pedestal[i].pad-1]
	    [pedestal[i].bucket-1] = pedestal[i].rms;
    }

    if (fmtpar[0].printf >= 1)
	printf(" <I> reformat: Subtracting pedestal baseline...\n");
    for (i=0; i<nBoards; i++) {
	for (j=0; j<nRows; j++) {
	    for (k=0; k<nPads; k++) {
		used[i][j][k] = 0;
	    }
	}
    }
    for (i=0; i<pulser_h->nok; i++) {
	secID = get_ID (0, pulser[i].sector, pulser[i].RDO,
			mapIndex, mapSector, mapRDO);
	if (secID < 0) return 1;
	netADC[secID][pulser[i].row-1][pulser[i].pad-1][pulser[i].bucket-1] =
	    pulser[i].ave - 
	    mapPed[secID][pulser[i].row][pulser[i].pad][pulser[i].bucket] -
	    mapRMS[secID][pulser[i].row][pulser[i].pad][pulser[i].bucket] *
	    fmtpar[0].times_rms;
	if (netADC[secID][pulser[i].row-1][pulser[i].pad-1]
	    [pulser[i].bucket-1] < 0)
	    netADC[secID][pulser[i].row-1][pulser[i].pad-1]
	    [pulser[i].bucket-1] = 0;
	used[secID][pulser[i].row-1][pulser[i].pad-1] = 1;
    }

    if (fmtpar[0].printf >= 1)
	printf(" <I> reformat: Integrating net pulser signals...\n");
    gain_ave = 0;
    channels = 0;
    for (i=0; i<nBoards; i++)
	for (j=0; j<nRows; j++)
	    for (k=0; k<nPads; k++)
		if (used[i][j][k]){
		    rawsignal = 0;
		    for(l=fmtpar[0].first_bucket-1; 
			l<=fmtpar[0].last_bucket-1; l++){
			if (netADC[i][j][k][l] > fmtpar[0].thresh)
			rawsignal = rawsignal + netADC[i][j][k][l];
		    }
		    /* store the integrated signal back in the adc array */
		    netADC[i][j][k][0] = rawsignal;
		    gain_ave += rawsignal;
		    channels++;
		}

    /* Normalize the gains */
    gain_ave = gain_ave / (float) channels;
    fmtpar[0].ave_gain = gain_ave;
    printf(" <I> reformat: Average Raw Gain = %5.2f\n", fmtpar[0].ave_gain);
    if (fmtpar[0].printf >= 1)
	printf(" <I> reformat: Normalizing gains...\n");

    gain_h->nok = 0;
    for (i=0; i<nBoards; i++)
	for (j=0; j<nRows; j++)
	    for (k=0; k<nPads; k++)
		if (used[i][j][k]){
		    inok = gain_h->nok++;
		    gain[inok].sector	= mapSector[i];
		    gain[inok].RDO	= mapRDO[i];
		    gain[inok].row	= i+1;
		    gain[inok].pad	= j+1;
		    gain[inok].gain	= netADC[i][j][k][0]/gain_ave;
		}

    return 0;
}

/*:>--------------------------------------------------------------------
**:  Store native_map and valid tables in static arrays
**:<------------------------------------------------------------------*/
int tfc_store
   (TABLE_HEAD_ST         *fmtpar_h,     TFC_FMTPAR_ST           *fmtpar ,
    TABLE_HEAD_ST     *native_map_h, TFC_NATIVE_MAP_ST       *native_map ,
    TABLE_HEAD_ST          *valid_h,      TFC_VALID_ST            *valid ,
    unsigned char mapIndex[nSectors][nRDOs],
    unsigned char mapSector[nBoards],
    unsigned char mapRDO[nBoards],
    unsigned char mapGood[nBoards][nGroups],
    unsigned char mapRow[nBoards][nGroups][nChans],
    unsigned char mapPad[nBoards][nGroups][nChans])
{
    int i, rdoID;
    
    if (valid_h->nok > 0){
	if (fmtpar[0].printf >= 1)
	    printf(" <I> reformat: Storing Valid (groups) table...\n");
        for (i=0; i<valid_h->nok ; i++){
	    rdoID = get_ID (1, valid[i].sector, valid[i].RDO, 
			    mapIndex, mapSector, mapRDO);
	    if (rdoID < 0) return 1;
            mapGood[rdoID][valid[i].group-1] = 1;
	}
    }

    if (native_map_h->nok > 0){
	if (fmtpar[0].printf >= 1)
	    printf(" <I> reformat: Storing Native_Map (geometry) table...\n");
        for (i=0; i<native_map_h->nok; i++){
	    rdoID = get_ID (1, native_map[i].sector, native_map[i].RDO, 
			       mapIndex, mapSector, mapRDO);
	    if (rdoID < 0) return 1;
            mapRow[rdoID][native_map[i].group-1]
		[native_map[i].channel-1] = native_map[i].row;
            mapPad[rdoID][native_map[i].group-1]
		[native_map[i].channel-1] = native_map[i].pad;
	}
    }

    return 0;
}

/*:>--------------------------------------------------------------------
**: Initializes ped/gain index table.
**: Returns 0, if OK;
**: Returns 1, if parameters are not internally consistent.
**:<------------------------------------------------------------------*/
int init_indx(INDX_MAPPED indx_mapped[nPadsTotal])
{
    unsigned long i;               /* General-purpose indices */

/*
 * Sum to find the number of pads preceding the start of a given row,
 * starting from row 1.  Then, check for consistency.
 */
    pads_before_row[0] = 0;
    for (i=1; i<nRows; i++) {
	pads_before_row[i] = pads_before_row[i-1] + pads_per_row[i-1];
    }
    if (pads_before_row[nRows-1] + pads_per_row[nRows-1] != nPadsSector)
	return 1;

/* Initialize all indices to -1 (i.e., not yet meaningful) */
    for (i=0; i<nPadsTotal; i++){
	indx_mapped[i].gain = -1;
	indx_mapped[i].ped  = -1;
    }
    return 0;
}

/*:>--------------------------------------------------------------------
**: Returns mapped gain index, if a valid one is defined;
**: Returns -1, otherwise.
**:
**: Assumes validity of sector and row parameters hava already been checked,
**: with sector=[1,24]; row=[1,45].
**:<------------------------------------------------------------------*/
long get_gain_indx_mapped
   (INDX_MAPPED indx_mapped[nPadsTotal],
    int sector,
    int row,
    int pad)
{
    unsigned long i;              /* Index for array */

/* Check that the pad index is legal: */
    if (pad<=0 || pad>pads_per_row[row-1]) return -1;
    i = (sector-1)*nPadsSector + pads_before_row[row-1] + (pad-1);
    return indx_mapped[i].gain;
}

/*:>--------------------------------------------------------------------
**: Returns mapped ped index, if a valid one is defined;
**: Returns -1, otherwise.
**:
**: Assumes validity of sector and row parameters hava already been checked,
**: with sector=[1,24]; row=[1,45].
**:<------------------------------------------------------------------*/
long get_ped_indx_mapped
   (INDX_MAPPED indx_mapped[nPadsTotal],
    int sector,
    int row,
    int pad)
{
    unsigned long i;              /* Index for array */

/* Check that the pad index is legal: */
    if (pad<=0 || pad>pads_per_row[row-1]) return -1;
    i = (sector-1)*nPadsSector + pads_before_row[row-1] + (pad-1);
    return indx_mapped[i].ped;
}

/*:>--------------------------------------------------------------------
**: Sets mapped gain index and returns 0, if parameters are valid;
**: Returns 1, otherwise.
**:
**: Assumes validity of sector and row parameters hava already been checked,
**: with sector=[1,24]; row=[1,45].
**:<------------------------------------------------------------------*/
long put_gain_indx_mapped
   (INDX_MAPPED indx_mapped[nPadsTotal],
    long gnok,
    int sector,
    int row,
    int pad)
{
    unsigned long i;              /* Index for array */

/* Check that the pad index is legal: */
    if (pad<=0 || pad>pads_per_row[row-1] || gnok<0) return 1;
    i = (sector-1)*nPadsSector + pads_before_row[row-1] + (pad-1);
    indx_mapped[i].gain = gnok;
    return 0;
}

/*:>--------------------------------------------------------------------
**: Sets mapped ped index and returns 0, if parameters are valid;
**: Returns 1, otherwise.
**:
**: Assumes validity of sector and row parameters hava already been checked,
**: with sector=[1,24]; row=[1,45].
**:<------------------------------------------------------------------*/
long put_ped_indx_mapped
   (INDX_MAPPED indx_mapped[nPadsTotal],
    long pnok,
    int sector,
    int row,
    int pad)
{
    unsigned long i;              /* Index for array */

/* Check that the pad index is legal: */
    if (pad<=0 || pad>pads_per_row[row-1] || pnok==-1) return 1;
    i = (sector-1)*nPadsSector + pads_before_row[row-1] + (pad-1);
    indx_mapped[i].ped = pnok;
    return 0;
}

