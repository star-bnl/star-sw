/*:>-------------------------------------------------------------------
**:
**:     program: l3cl_support.cc (StAF version)
**:
**:     clusterfinder support functions
**:     author: dirk schmischke, ikf, kraut@ikf.uni-frankfurt.de
**:     last change: 06/02/98 cs:
**:                  06/15/98 py: corr. init_cluster and Coord trans
**:                  06/18/98 cs: corr geometry
**:                  09/17/98 cs: new l3 data format
**:                  10/20/98 py: ConvertRawToDetector moved to l3clUtilities.cc 
**:                  10/22/98 py: add l3cl at beginning function names
**:                  10/22/98 py: l3clAllocateMemory and l3clFreeMemory added
**:                  07/06/99 py: l3clWriteDataToTable hit handling commented out, there
**:                               a new sl3Hit format, we need new code to handle it 
**:                  07/07/99 py: SL3HIT replaced with SL3BUFFER
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
  // extern CLUSTER *copy_phys_cluster;
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

int l3clAllocateMemory ( ) {
/*:>--------------------------------------------------------------------
**: FUNCTION:    l3clAllocateMemory
**: DESCRIPTION: Allocates memory for dynamic arrays
**:
**: AUTHOR:     ds   -  Pablo Yepes from Dirk Schimschke's code
**:
**: RETURNS:    <>0 if there was a problem
**:>------------------------------------------------------------------*/
//
// allocate memory for some dynamic arrays
//
  if ((cluster_seq = (CLUSTER**) malloc(NPADS * sizeof(CLUSTER*))) == NULL)
  {
      printf("Couldn't allocate cluster_seq in init_phys_map!\n");
      return 1 ;
  }
//
  if ((phys_cluster = (CLUSTER*) malloc(NPADS * sizeof(CLUSTER))) == NULL)
  {
     printf("Couldn't allocate phys_cluster in init_phys_map!\n");
     return 1 ;
  }
/*
  if ((copy_phys_cluster = (CLUSTER*) malloc(NPADS * sizeof(CLUSTER))) == NULL)
  {
     printf("Couldn't allocate copy_phys_cluster in init_phys_map!\n");
     return 1 ;
  }
*/
  if ((n_cluster = (unsigned short**) malloc(NPADS * sizeof(unsigned short *))) == NULL)
  {
     printf("Couldn't allocate n_cluster in init_phys_map!\n");
     return 1 ;
  }
  if ((phys_n_cluster = (unsigned short*) malloc(NPADS * sizeof(unsigned short))) == NULL)
  {
     printf("Couldn't allocate phys_n_cluster in init_phys_map!\n");
     return 1 ;
  }
  if ((data_ram = (struct DATA_RAM**) malloc(NPADS * sizeof(struct DATA_RAM*))) == NULL)
  {
     printf("Couldn't allocate data_ram in init_phys_map!\n");
     return 1 ;
  }
  if ((phys_data_ram = (struct DATA_RAM*) malloc(NPADS * sizeof(struct DATA_RAM))) == NULL)
  {
     printf("Couldn't allocate phys_data_ram in init_phys_map!\n");
     return 1 ;
  }

  if ((pGlobalStoreIntern = (TFormattedData*) malloc((NPADS+1) * 2 * sizeof(TFormattedData))) == NULL)
  {
     printf("Couldn't allocate pGlobalStoreIntern in init_phys_map!\n");
     return 1 ;
  }
//
// align pGlobalStore to a 16 byte boundary, for fast storage (i960)
//
  pGlobalStore = (PFormattedData) (((unsigned int)(pGlobalStoreIntern+1)) & 0xfffffff0);
//

//
  return 0 ;
}

int l3clFreeMemory ( ) {
/*:>--------------------------------------------------------------------
**: FUNCTION:    l3clFreeMemory  
**: DESCRIPTION: Frees allocated memory   
**:
**: AUTHOR:     py  - Pablo Yepes     
**:
**: RETURNS:    <>0 if there was a problem
**:>------------------------------------------------------------------*/
//
// allocate memory for some dynamic arrays
//
  if ( cluster_seq        != NULL) {
     free (  (void * )cluster_seq        ) ;
     cluster_seq = NULL ;
  }
  if ( phys_cluster       != NULL) {
     free (  (void * )phys_cluster       ) ;
     phys_cluster = NULL ;
  }
  if ( n_cluster          != NULL) {
     free (  (void * )n_cluster          ) ;
     n_cluster = NULL ;
  }
  if ( phys_n_cluster     != NULL) {
     free (  (void * )phys_n_cluster     ) ;
     phys_n_cluster = NULL ;
  }
  if ( data_ram           != NULL) {
     free (  (void * )data_ram           ) ;
     data_ram = NULL ;
  }
  if ( phys_data_ram      != NULL) {
     free (  (void * )phys_data_ram      ) ;
     phys_data_ram = NULL ;
  }
  if ( pGlobalStoreIntern != NULL) {
     free (  (void * )pGlobalStoreIntern ) ;
     pGlobalStoreIntern = NULL ;
  }
  return 0 ;
}

int l3clInitPhysMap()
/*:>--------------------------------------------------------------------
**: FUNCTION:    l3clInitPhysMap
**: DESCRIPTION: Initializes physics maps
**:
**: AUTHOR:     ds  - Dirk Schminschke
**:
**: RETURNS:    <>0 if there was a problem
**:>------------------------------------------------------------------*/
{
  int i,j,k=0;
//
//       Do the mapping from pad sequence numbers to physical pads, padrows.
//       Trivial mapping done here. Need table downloaded...
//
  NPADS = 0 ;
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
//
  return 0 ;
}
/*:>--------------------------------------------------------------------
**: FUNCTION:    l3clInitPointers 
**: DESCRIPTION: Initializes pointers
**:
**: AUTHOR:     ds  - Dirk Schminschke
**:
**:>------------------------------------------------------------------*/
void l3clInitPointers()
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
/*:>--------------------------------------------------------------------
**: FUNCTION:    l3clInitTable    
**: DESCRIPTION: Initializes table    
**:
**: AUTHOR:     ds  - Dirk Schminschke
**:
**:>------------------------------------------------------------------*/
void l3clInitTable()
{
   int i;
   for (i=0;i<256; i++) {
      table[i] = 4*i;       /* just a linear table for now */
   }
}
/*:>--------------------------------------------------------------------
**: FUNCTION:    l3clInitClusters 
**: DESCRIPTION: Initializes clusters 
**:
**: AUTHOR:     ds  - Dirk Schminschke
**:
**:>------------------------------------------------------------------*/
void l3clInitClusters( TABLE_HEAD_ST       *pad_h,
		       L3CLPAD_ST          *pad,
		       TYPE_SHORTDATA_ST   *pixel   )
{
   int i, k;
   int ipadrow, ipad;
   int start, end, sequence[512];
//
// Set cluster counter to zero
//
   for (ipadrow=0; ipadrow<NPADROWS; ipadrow++)
   {
      for (ipad=iFirstPad[ipadrow]; ipad<=iLastPad[ipadrow]; ipad++)
      {
         i = padlist[ipadrow][ipad];
         (*n_cluster[i]) = 0;
      }
   }
//
// read input tables
//
   for ( i = 0; i < pad_h->nok; i++ )
   {
      ipadrow = pad[i].row;
      ipad    = pad[i].pad;
      start   = pad[i].first;
      end     = pad[i].last;
      for ( k = 0; k < (end-start+1); k++) {
          sequence[k] = (pixel + pad[i].jpix + k )->data;
      }
      l3clMakeCluster( ipadrow-1, ipad-1, start, end, sequence);
   }

}
/*:>--------------------------------------------------------------------
**: FUNCTION:    l3clMakecluster 
**: DESCRIPTION: Make clusters
**:
**: AUTHOR:     ds  - Dirk Schminschke
**:             06/10/98 cs: bug in calculating the seq. padnumber
**:
**:>------------------------------------------------------------------*/
void l3clMakeCluster( int pr, int off, int mbeg, int mend, int* seq)
{
   int i,t;
   CLUSTER * cptr;

   i = padlist[pr][iFirstPad[pr]+off];
   if (*n_cluster[i] >= MAX_CLUSTERSUC )
   {
      printf ( "\n Max clusters reached for pad %d ", i ) ;
      return; /* max 31 clusters in list */
   }
   cptr = cluster_seq[i];
   cptr->a_run[*(n_cluster[i])].Part.Begin = mbeg;
   cptr->a_run[*(n_cluster[i])].Part.End   = mend;
   (*n_cluster[i])++;
//
// fill in the data RAM values (not properly mapped to 8 bits )
//
   for ( t = mbeg; t < (mend+1); t++ )
   {
      data_ram[i]->t_sam[t] = min(seq[t-mbeg] >> 2, 255);
   }
   return;
}
/*:>--------------------------------------------------------------------
**: FUNCTION:    l3clInitOther   
**: DESCRIPTION: Other initializations
**:
**: AUTHOR:     ds  - Dirk Schminschke
**:
**:>------------------------------------------------------------------*/
void l3clInitOther()
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
/*:>--------------------------------------------------------------------
**: FUNCTION:    WriteDataToTable
**: DESCRIPTION: writes data to L3 hit table
**:
**: AUTHOR:     ds  - Dirk Schminschke
**:
**:>------------------------------------------------------------------*/
void l3clWriteDataToTable( TABLE_HEAD_ST *hit_h,
                           SL3BUFFER_ST     *hit )
{
   int i, nok ;
   float x, y, z ;
   PFormattedData p = pGlobalStore ; 

   nok = hit_h->nok ;
//
//   sl3Hit format changed
//   We need code to write hit buffer here
//
/*
   for ( i = 0 ; i < clusters; i++ )
   {
      hit[nok].row  = p->PadRow + 1;
      hit[nok].pad  = p->CenterPad;
      hit[nok].time = p->CenterTime;
      nok++ ;
      p++;
   }
   hit_h->nok = nok ;
*/
}
