/*:>-------------------------------------------------------------------
**:
**:     program: l3cl_support.cc (StAF version)
**:
**:     clusterfinder support functions
**:     author: dirk schmischke, ikf, kraut@ikf.uni-frankfurt.de
**:     last change: 06/02/98 cs
**:
**:>-----------------------------------------------------------------*/


/* includes */
#include "l3cl_inc.h"
#include <string.h>
#include <stdlib.h>

/* some defines */
#define min(a,b)               (a>b ? b : a)

#if defined(__cplusplus)
extern "C" {
#endif
/* some externals */
extern short row_sizes[45];
/* number of pads in each padrow */
extern unsigned short n_pads[NPADROWS];
extern short padlist[NPADROWS][MAXPADS]; /* contains sequential pad # for each */
                                         /* physical pad in a padrow */
/* list of clusters under construction */
extern TClusterUCList pClustersUnderConstruction[MAX_CLUSTERSUC];
/* index of first and last pad in padrow */
extern int iFirstPad[NPADROWS], iLastPad[NPADROWS];
/* decompression table of hit-data (hit data is compressed from 10 bit to 8 bit) */
extern unsigned short table[256];
extern int clusters;
extern int NPADS;
/* pointers to dynamically allocated arrays... (every array is NPADS wide) */
/* array of pointers to pad-sequence data */
extern CLUSTER **cluster_seq;
/* simulated asic storage for pad-sequence data */
extern CLUSTER *phys_cluster;
/* copy of simulated asic storage for pad-sequence data (not used in this program) */
extern CLUSTER *copy_phys_cluster;
/* pointers to simulated asic storage for number of sequences in each pad */
extern unsigned short *(*n_cluster);
/* pointer to array of formatted output data */
extern PFormattedData pGlobalStore;
extern PFormattedData pGlobalStoreIntern; /* internal use only */
/* simulated asic storage for number of sequences in pad */
extern unsigned short *phys_n_cluster;
extern struct DATA_RAM **data_ram;
extern struct DATA_RAM *phys_data_ram;

#if defined(__cplusplus)
	   }
#endif

void init_phys_map()
{
	int i,j,k=0;

	/*
		Do the mapping from pad sequence numbers to physical pads, padrows.
		Trivial mapping done here. Need table downloaded...
	*/
	for (i=0; i<MAX_PADROW-MIN_PADROW+1; i++) 
	{
		NPADS += n_pads[i] = row_sizes[MIN_PADROW + i - 1]; 
		iFirstPad[i] = 0;
		iLastPad[i] = iFirstPad[i]+n_pads[i]-1;
		for (j=0; j<n_pads[i]; j++) 
		{
			padlist[i][j] = k++;
		}
	}
	/* allocate memory for some dynamic arrays */
	
	if ((cluster_seq = (CLUSTER**) malloc(NPADS * sizeof(CLUSTER*))) == NULL)
  {
    printf("Couldn't allocate memory in init_phys_map!\n");
    exit(-1);
  }
  if ((phys_cluster = (CLUSTER*) malloc(NPADS * sizeof(CLUSTER))) == NULL)
  {
    printf("Couldn't allocate memory in init_phys_map!\n");
    exit(-1);
  }
  if ((copy_phys_cluster = (CLUSTER*) malloc(NPADS * sizeof(CLUSTER))) == NULL)
  {
    printf("Couldn't allocate memory in init_phys_map!\n");
    exit(-1);
  }
  if ((n_cluster = (unsigned short**) malloc(NPADS * sizeof(unsigned short *))) == NULL)
  {
    printf("Couldn't allocate memory in init_phys_map!\n");
    exit(-1);
  }
  if ((phys_n_cluster = (unsigned short*) malloc(NPADS * sizeof(unsigned short))) == NULL)
  {
    printf("Couldn't allocate memory in init_phys_map!\n");
    exit(-1);
  }
  if ((data_ram = (struct DATA_RAM**) malloc(NPADS * sizeof(struct DATA_RAM*))) == NULL)
  {
    printf("Couldn't allocate memory in init_phys_map!\n");
    exit(-1);
  }
  if ((phys_data_ram = (struct DATA_RAM*) malloc(NPADS * sizeof(struct DATA_RAM))) == NULL)
  {
    printf("Couldn't allocate memory in init_phys_map!\n");
    exit(-1);
  }
  /* clear the memory */
  memset(phys_data_ram, 0, NPADS * sizeof(struct DATA_RAM));
  
  /* hardwired locations */
	/*
	phys_cluster = (void*) 0xe0020000; 
	copy_phys_cluster = (void*) 0xe0028000; 
	cluster_seq = (void*) 0xe0030000; 
	n_cluster = (void*) 0xe0030800; 
	phys_n_cluster = (void*) 0xe0031000; 
	data_ram = (void*) 0xe0031400; 
	phys_data_ram = (void*) 0xe0031C00;
	pGlobalStoreIntern = (void*) 0xe0071C00;
	*/
	
  if ((pGlobalStoreIntern = (TFormattedData*) malloc((NPADS+1) * 2 * sizeof(TFormattedData))) == NULL)
  {
      printf("Couldn't allocate memory in init_phys_map!\n");
      exit(-1);
  }
  
  /* align pGlobalStore to a 16 byte boundary, for fast storage (i960) */
  pGlobalStore = (PFormattedData) (((unsigned int)(pGlobalStoreIntern+1)) & 0xfffffff0);
}
//
//   Initialize pointers
//
void init_pointers()
{       
	int i;
	/* set pointers to physical addresses in ASIC */
	for (i=0;i<NPADS; i++) 
	{
		cluster_seq[i] = &phys_cluster[i];
		data_ram[i] = &phys_data_ram[i];
		n_cluster[i] = &phys_n_cluster[i];
	}
}


void init_table()
{
	int i;
	for (i=0;i<256; i++) 
	{
		table[i] = 4*i;       /* just a linear table for now */
	}
}


TSS_TPPAD_ST* init_clusters( int                sector,
                             TSS_TPPAD_ST       *pad_pointer,
                             TABLE_HEAD_ST      *tppad_h,      
        		             TSS_TPPAD_ST       *tppad,        
		                     TSS_TPPIXEL_ST     *tppixel   )
{
	int ipadrow, ipad, i, j, k, start,len, sequence[512];
	int offset;
//	int sec;
//
//    Set cluster counter to zero
//
    for (ipadrow=0; ipadrow<NPADROWS; ipadrow++) {
       for (ipad=iFirstPad[ipadrow]; ipad<=iLastPad[ipadrow]; ipad++)
       {
          i = padlist[ipadrow][ipad];
          (*n_cluster[i]) = 0;
       }
    }
//
// has to be done sector by sector !!!
//
//	sec = pad_pointer->tpc_row / 100;
	while( sector == pad_pointer->tpc_row /100 && pad_pointer <= &(tppad[(tppad_h->nok)-1]) )   
	{
	    offset = 0;
	    // printf("nseq: %d\n", pad_pointer->nseq);
	    for( j=0; j<pad_pointer->nseq; j++ )
	    {
            ipad = pad_pointer->secpad;
            len = tppixel[pad_pointer->jpix+offset-1].datum >> 20;
            start = ( tppixel[pad_pointer->jpix+offset-1].datum >> 10 ) & 0x3FF;
	        ipadrow = pad_pointer->tpc_row % 100;
		    for( k=0; k<len; k++)
		    {
		        sequence[k] = tppixel[pad_pointer->jpix+offset-1].datum & 0x3FF;
		        offset++;
            }

		    make_cluster(ipadrow-MIN_PADROW,ipad,start,len,sequence);
	    }
	    pad_pointer++;
	}

    return pad_pointer ;
//	printf("init_cluster done.\n");

}

void make_cluster(int pr,int off,int mbeg,int mlen, int* seq)
{
	int i,t;
	CLUSTER * cptr;

// 06/10/98 cs: bug in calculating the seq. padnumber
	i = padlist[pr][iFirstPad[pr]+off-1];
    if (*n_cluster[i] >= MAX_CLUSTERSUC ) {
        printf ( "\n Max clusters reached for pad %d ", i ) ;
		return; /* max 31 clusters in list */
    }
	cptr = cluster_seq[i];
	cptr->a_run[*(n_cluster[i])].Part.Begin=mbeg;
	cptr->a_run[*(n_cluster[i])].Part.End=mbeg+mlen-1;
 // printf("clust_seq[%d]->%x:  ", i, cptr);
 // printf("seq: %d . %d; ", cptr->a_run[*(n_cluster[i])].Part.Begin, cptr->a_run[*(n_cluster[i])].Part.End );
 // printf("pSeq->Part.Beg= %d; ", ((PSequence) cluster_seq[i])->Part.Begin );
 // printf("pSeq-> = %x\n", (PSequence) cluster_seq[i] );
	(*n_cluster[i])++;
	/* fill in the data RAM values (not properly mapped to 8 bits )*/
	for (t=mbeg; t<mbeg+mlen;t++)
	{
		data_ram[i]->t_sam[t] = min(seq[t-mbeg] >> 2,255);
		// printf("  >%d", data_ram[i]->t_sam[t]);
	}
	//	printf("\n");
	return;
}


void init_other()
{
	int Index;

	/* initialize clusters under construction array and allocate some memory for sequence list */
	for (Index = 0; Index < MAX_CLUSTERSUC; Index++)
	{
		pClustersUnderConstruction[Index].Sequence.Complete = -1;
		pClustersUnderConstruction[Index].pList = (struct tagClusterUCNode *) allocate_bytes_with_check();
		pClustersUnderConstruction[Index].pList->next = NULL;
		pClustersUnderConstruction[Index].pList->Filling = 0;
	}
	/* initialize prf table */
	SetupLogTable();
}

void WriteDataToTable( int sector, 
                       TABLE_HEAD_ST *hit_h, 
		               TCL_TPHIT_ST  *hit )
{
    int i, nok ;
    float x, y, z ;
    PFormattedData p = pGlobalStore ; 

    nok = hit_h->nok ;
    for ( i = 0 ; i < clusters; i++ )
    {
/*
    Returns coordinates in cm
*/
	   ConvertRawToDetector( sector, (p->PadRow)+1, ((float)p->CenterPad)/64, 
                             ((float)p->CenterTime)/64,
			                  &x, &y, &z );
       hit[nok].id = nok ;
       hit[nok].row = sector * 100 + p->PadRow+1 ;
	   hit[nok].x  = x ;
	   hit[nok].y  = y ;
	   hit[nok].z  = z ;
	   hit[nok].dx = 0.2F ;
	   hit[nok].dy = 0.2F ;
	   hit[nok].dz = 0.2F ;
	   nok++ ;
	   p++;
    }
    hit_h->nok = nok ;
}


/* 
 new: conversion from raw data (sector, padrow, pad, time) to real coordinates in [cm] 
*/
void ConvertRawToDetector(int sector, int padrow, float pad, float timeslice, 
                          float* x, float* y, float* z)
{
	float cart_x, cart_y;
	/* statics
	   padrow-offset (center pad) from detector-center in mm */
	static float PadrowOffset [] = 
	{
	    60.0F, 64.8F, 69.6F, 74.4F, 79.2F, 84.0F, 88.8F, 93.60F, /*  7 * 4.80 cm spacing  */
		98.8F, 104.F, 109.20F, 114.4F, 119.6F, /*  5 * 52 mm spacing  */
		127.195F, 129.195F, 131.195F, 133.195F, 135.195F, /*  32 * 2.0 cm spacing  */
		137.195F, 139.195F, 141.195F, 143.195F, 145.195F,
		147.195F, 149.195F, 151.195F, 153.195F, 155.195F,
		157.195F, 159.195F, 161.195F, 163.195F, 165.195F,
		167.195F, 169.195F, 171.195F, 173.195F, 175.195F,
		177.195F, 179.195F, 181.195F, 183.195F, 185.195F,
		187.195F, 189.195F
	};
	/* cross-spacings between adjacent pads in cm */
	static float PadSpacing [] =
	{
        .335F, .335F, .335F, .335F, .335F, /*  13 * .335 cm  */
		.335F, .335F, .335F, .335F, .335F,
		.335F, .335F, .335F,
		.670F, .670F, .670F, .670F, .670F, /*  32 * .670 cm  */
		.670F, .670F, .670F, .670F, .670F,
		.670F, .670F, .670F, .670F, .670F,
		.670F, .670F, .670F, .670F, .670F,
		.670F, .670F, .670F, .670F, .670F,
		.670F, .670F, .670F, .670F, .670F,
		.670F, .670F
	};
	/* number of pads in padrow */
	static short NumberOfPadsInRow[45] = 
	{
		88,96,104,112,118,126,134,142,150,156,166,174,184,
		98,100,102,104,106,106,108,110,112,112,114,116,
		118,120,122,122,124,126,128,128,130,132,134,136,
		136,138,140,142,144,144,144,144 
	};
	/* sector-rotation factors */
	static float SectorSinus [] =
	{
       	        /*  30 deg each segment */
        0.9659258262891F,  /*  75 deg */
        0.7071067811866F,  /*  45 deg */
	0.2588190451025F,  /*  15 deg */
       -0.2588190451025F,  /* 345 deg */
       -0.7071067811866F,  /* 315 deg */
       -0.9659258262891F,  /* 285 deg */
       -0.9659258262891F,  /* 255 deg */
       -0.7071067811866F,  /* 225 deg */
       -0.2588190451025F,  /* 195 deg */
        0.2588190451025F,  /* 165 deg */
    	0.7071067811866F,  /* 135 deg */
        0.9659258262891F   /* 105 deg */
 	};
	static float SectorCosinus [] =
	{
	    0.2588190451025F,  /* 75 */
            0.7071067811866F,  /* 45 */
            0.9659258262891F,  /* 15 */
            0.9659258262891F,  /* 345 */
            0.7071067811866F,  /* 315 */
            0.2588190451025F,  /* 285 */
	   -0.2588190451025F,  /* 255 */
	   -0.7071067811866F,  /* 225 */
	   -0.9659258262891F,  /* 195 */
	   -0.9659258262891F,  /* 165 */
	   -0.7071067811866F,  /* 135 */
	   -0.2588190451025F   /* 105 */
	};
	/* scaling in time-direction */
	static float TimeScale = 250.0 / 512.0;
    int is ;
/*
     Make sure sector makes sense
*/
    
    if ( sector < 1 || sector > 24 ) {
        printf ( " \n sector out of range %d ", sector ) ;
        *x = *y = *z = 0.F ;
    }
//
//    Get sector index
//
    if ( sector > 12 ) {
        is = 24 - sector - 1 ;
        if ( is < 0 ) is = 11 ;
    }
    else
        is = sector -1 ;

/* calculate unrotated cartesian base-coordinates */
	cart_y = PadrowOffset[padrow-1];
	cart_x = (pad - (NumberOfPadsInRow[padrow-1] / 2)-0.5F) * PadSpacing[padrow-1];

/* rotate these coordinates */
	*x = SectorSinus[is] * cart_x + SectorCosinus[is] * cart_y;
	*y = SectorSinus[is] * cart_y - SectorCosinus[is] * cart_x;

/* calculate time-direction */
	if (sector > 12)
/* negative direction */
		*z = 250. - timeslice * TimeScale;
	else
/* positive direction */
		*z = -250. + timeslice * TimeScale;
}
