/*:>-------------------------------------------------------------------
**:
**:     program: l3cl_inc.h (StAF version)
**:
**:     Includefile for level 3 clusterfinder,
**:     includes data structures and defines
**:     author: dirk schmischke, ikf, kraut@ikf.uni-frankfurt.de
**:     last change: 06/02/98 cs
**:
**:>-----------------------------------------------------------------*/


/* includes */
#include <stdio.h>
#include "PAM.h"
#include "l3clPad.h"
#include "type_shortdata.h"
#include "sl3Buffer.h"

/* defines */
#define MAXCLUSTERSIZE 75	/* upper limit to clusters calculated */
#define NTIME	512	/* 512 number of time samples per pad */
#define MAXPADS 184     /* max pads in one padrow */
#define THRESH  4
#define NULLPTR    (struct LINK *)0
#define MAXSEQUENCES 20
#undef  FLOAT
#define FIND_CLUSTERS
//#define MAX_CLUSTERSUC 32  /* maximal 32 cluster under construction at the same time */
#define MAX_CLUSTERSUC 256
#define MAX_SEQUENCES_PER_NODE 6 /* maximum number of sequences per clusteruc node (don't change) */
#define MAXCLUSTERS 30000

#define NewPad(padno) (PSequence) cluster_seq[(padno)]

#define MIN_PADROW 1
#define MAX_PADROW 45
#define NPADROWS (MAX_PADROW-MIN_PADROW+1)

#if defined(__cplusplus)
extern "C" {
#endif

/* typdefs */
typedef struct
{
  unsigned short CenterPad;
  unsigned short CenterTime;
  unsigned short PadRow;
  unsigned short reserved;
} TFormattedData;

typedef TFormattedData* PFormattedData;

struct DATA_RAM {
  unsigned char t_sam[NTIME];
};

typedef union
{
  struct 
  {
    unsigned short Begin;
    unsigned short End;
  } Part;
  unsigned int Complete;
} TSequence;

typedef TSequence* PSequence; 

typedef struct tagClusterUCNode
{
  struct tagClusterUCNode* next;
  unsigned short Filling; /* 0..5 */
  unsigned short reserved;
  TSequence Sequence[MAX_SEQUENCES_PER_NODE];
} TClusterUCNode;

typedef TClusterUCNode* PClusterUCNode;

typedef struct
{
  TSequence Sequence;
  PClusterUCNode pList;
  unsigned char PadRow;
  unsigned char StartPad;
  unsigned short reserved;
} TClusterUCList; 

typedef TClusterUCList* PClusterUCList;

typedef struct
{
  TSequence a_run[MAX_CLUSTERSUC];
} CLUSTER;

/* prototypes */
void* allocate_bytes_with_check();
void deallocate_bytes_with_check(void*);
void l3clFindClusters( int StartRow, int EndRow );
void CalculateMoments(PClusterUCList NewCluster);
int  l3clAllocateMemory();
int  l3clFreeMemory();
int  l3clInitPhysMap();
void l3clInitPointers();
void l3clInitTable();
void l3clInitOther();
void reset_other();
void l3clInitClusters ( TABLE_HEAD_ST        *pad_h,
	                L3CLPAD_ST           *pad,
		        TYPE_SHORTDATA_ST    *pixel );
void l3clMakeCluster     ( int pr,int off,int mbeg,int mend, int *seq );
void l3clWriteDataToTable( TABLE_HEAD_ST *hit_h,
	  	           SL3BUFFER_ST     *hit );
void SetupLogTable();
void l3clSector2GlobalCoordinates ( int sector,
                                    float& xSector, float& ySector, float& zSector,
                                    float& xGlobal, float& yGlobal, float& zGlobal ) ;
void l3clConvertRawToSectorCoordinates( int& padrow, float& pad, float& timeslice,
                                        float& x, float& y, float& z) ;



void* malloc(size_t);
#if defined(__cplusplus)

}
#endif
void memory_kill();
void print_statistics();
