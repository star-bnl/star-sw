/*:>-------------------------------------------------------------------
**:
**:     program: l3cl_findcluster.cc (StAF version)
**:
**:     level 3 clusterfinder FindCluster function
**:     author: dirk schmischke, ikf, kraut@ikf.uni-frankfurt.de
**:     last change: 06/02/98 cs
**:                  06/18/98 cs: corr row_sizes table
**:
**:>-----------------------------------------------------------------*/


#include "l3cl_inc.h"
#include <stdlib.h>


#if defined(__cplusplus)
extern "C" {
#endif

/* globals */
short row_sizes[45] = { 
       88,96,104,112,118,126,134,142,150,158,166,174,182,
       98,100,102,104,106,106,108,110,112,112,114,116,
       118,120,122,122,124,126,128,128,130,132,134,136,
       138,138,140,142,144,144,144,144
};



/* number of pads in each padrow */
unsigned short n_pads[NPADROWS];
short padlist[NPADROWS][MAXPADS]; /* contains sequential pad # for each */
                                  /* physical pad in a padrow */
/* list of clusters under construction */
TClusterUCList pClustersUnderConstruction[MAX_CLUSTERSUC];
/* index of first and last pad in padrow */
int iFirstPad[NPADROWS], iLastPad[NPADROWS];
/* decompression table of hit-data (hit data is compressed from 10 bit to 8 bit) */
unsigned short table[256];
int clusters=0;
int NPADS = 0;
/* pointers to dynamically allocated arrays... (every array is NPADS wide) */
/* array of pointers to pad-sequence data */
CLUSTER **cluster_seq;
/* simulated asic storage for pad-sequence data */
CLUSTER *phys_cluster;
/* copy of simulated asic storage for pad-sequence data (not used in this program) */
CLUSTER *copy_phys_cluster;
/* pointers to simulated asic storage for number of sequences in each pad */
unsigned short *(*n_cluster);
/* pointer to array of formatted output data */
PFormattedData pGlobalStore;
PFormattedData pGlobalStoreIntern; /* internal use only */
/* simulated asic storage for number of sequences in pad */
unsigned short *phys_n_cluster;
struct DATA_RAM **data_ram;
struct DATA_RAM *phys_data_ram;

/*
// find cluster
*/
void FindClusters()
{
/* variables */
   int            iPadRow, iPad;
   PSequence      pSequence1, pSequence2, pSequence1End, pSequence2End;
   PSequence      pSequence2Initial;
   PClusterUCList pClusterUCTemp, pClusterUCNew, pClusterUCEnd, pClusterUCMax;
   PClusterUCList pLastInsert, pTempNow;
   PClusterUCNode pSequenceListTracer;

/* reset the 'found clusters' counter */
   clusters = 0;
 
/* loop over all padrows */
   for (iPadRow = 0; iPadRow < NPADROWS; iPadRow++)
   {
 //printf("clusters: %d\n", clusters);
/* initialize the end of cluster-under-construction list
	at this point there are no clusters under construction, so the end is the beginning */
      pClusterUCEnd = pClustersUnderConstruction;
      pClusterUCMax = &(pClustersUnderConstruction[MAX_CLUSTERSUC]); 

/* initialize sequence pointers to the first and second pad in this padrow */
/* ds980414: major bug!
// index to n_cluster had to be the total number of the pad */
      pSequence1 = NewPad(padlist[iPadRow][iFirstPad[iPadRow]]);
/* pSequence1End = pSequence1 + *(n_cluster[iFirstPad[iPadRow]]); */
      pSequence1End = pSequence1 + *(n_cluster[padlist[iPadRow][iFirstPad[iPadRow]]]);
      pSequence2Initial = pSequence2 = NewPad(padlist[iPadRow][iFirstPad[iPadRow] + 1]);
/* pSequence2End = pSequence2 + *(n_cluster[iFirstPad[iPadRow] + 1]); */
      pSequence2End = pSequence2 + *(n_cluster[padlist[iPadRow][iFirstPad[iPadRow] + 1]]);
  //printf("pSeq1-> %x, pSeq2-> %x\n", pSequence1, pSequence2 );
  //printf("seq1: %d -> %d; seq2: %d -> %d\n", pSequence1->Part.Begin, pSequence1->Part.End, pSequence2->Part.Begin, pSequence2->Part.End );
  //printf("iPadRow: %d, padlist[iPadRow][iFirstPad[iPadRow]]= %d\n", iPadRow, padlist[iPadRow][iFirstPad[iPadRow]] );
/* loop over all but the last pads in this padrow */
      for (iPad = iFirstPad[iPadRow]; iPad < iLastPad[iPadRow]-1; iPad++)
      {
/* loop over all hit-sequences in this pad */
/* to increase efficiency, we loop over all clusters in the cluster-under-construction-list
   and match them with sequences beginning at sequence 0
   setup a pointer to the beginning of the cluster under construction 'list' */
	    pClusterUCTemp = pClustersUnderConstruction;
/* here comes the place, where new clusters under construction are to be temporarily 
    stored, before sorting them in ascending order */
	    pClusterUCNew = pClusterUCEnd;
	/* the loop starts now */
	    for ( ; pClusterUCTemp < pClusterUCEnd; pClusterUCTemp++)
	    {
/* try to find a matching sequence (we compare end-of-sequence1-sequence with 
		begin-of-cluster-under-construction-sequence) 
	NOTE:
      	EVERY cluster under construction MUST have a match! 
	      so, we don't need to test the pSequence1 position */
               while (pSequence1->Part.End < pClusterUCTemp->Sequence.Part.Begin)
               {
/* did NOT match; this could be the start of a new cluster! */
/* start of a new cluster means: there is also a matching cluster in the
	next pad. so, look into the next pad to find a match 
	NOTE:
	at this point need NOT be any matches. so, we must test pSequence2 position */
                  while ((pSequence2 < pSequence2End) && (pSequence2->Part.End < pSequence1->Part.Begin))
                     pSequence2++;
/* did we find a match? */
                  if ((pSequence2 < pSequence2End) && 
                      (pSequence2->Part.Begin < pSequence1->Part.End) &&
                      (pSequence2->Part.End > pSequence1->Part.Begin))
                  {
/* yes ! */
/* start a new cluster. that is: append the sequence-begin-end for both sequences (sequence1, sequence2)
	at the tail of the cluster-under-construction-list (start/stop is for the last sequence(2)) */
			pClusterUCNew->Sequence.Complete = pSequence2->Complete;
						/* store padrow and startpad */
						pClusterUCNew->PadRow = iPadRow;
						pClusterUCNew->StartPad = iPad;

						/* make sure the sequence node is really empty */
						pClusterUCNew->pList->Filling = 0;
						pClusterUCNew->pList->next = NULL;
						/* store the information in this node and increment filling */
						pClusterUCNew->pList->Sequence[pClusterUCNew->pList->Filling++].Complete = pSequence1->Complete;
						/* the same must be done for sequence2 */
						pClusterUCNew->pList->Sequence[pClusterUCNew->pList->Filling++].Complete = pSequence2->Complete;

						/* increment cluster-under-construction pointer to next free location */
					pClusterUCNew++;
                                        if ( pClusterUCNew >= pClusterUCMax ) {
                                           printf ( " \n Maximum number of clusters under construction reached " ) ;
                                           pClusterUCNew-- ;
                                         }

					}
					/* increment sequence2; we have just used it! */
					pSequence2++;
					/* next sequence1 sequence */
					pSequence1++;
				} /* end: sequence1 to cluster-under-construction matching loop */
				/* at this point we MUST have matched a sequence(1) to a cluster-under-construction! */
				/* now check, if it continues in the next pad (sequence2) */
				/* NOTE:
					here need NOT be any matches. so, we must test pSequence2 position */
				while ((pSequence2 < pSequence2End) && (pSequence2->Part.End < pSequence1->Part.Begin))
					pSequence2++;
				/* did we find a match? */
				if ((pSequence2 < pSequence2End) && 
					(pSequence2->Part.Begin < pSequence1->Part.End) &&
					(pSequence2->Part.End > pSequence1->Part.Begin))
				{
					/* yes ! */
					/* plug the new start/end into cluster-under-construction descriptor and append start/end to
						cluster-under-construction-sequence-list */
					pClusterUCTemp->Sequence.Complete = pSequence2->Complete;

					/* setup temporary pointer for tracing the list */
					pSequenceListTracer = pClusterUCTemp->pList;
					/* we assume, that at least one slot is connected to the cluster uc sequence list */
					/* trace the ClusterUC-Sequencelist until a partially empty node appears 
						an empty node is one, which is not overfull (has no next element) */
					while (pSequenceListTracer->next != NULL)
						pSequenceListTracer = pSequenceListTracer->next;
					/* is this slot full? */
					if (pSequenceListTracer->Filling >= MAX_SEQUENCES_PER_NODE)
					{
						/* yes!
							we must add a new node */
						pSequenceListTracer->next = (struct tagClusterUCNode *) allocate_bytes_with_check();
						/* switch to it */
						pSequenceListTracer = pSequenceListTracer->next;
						/* initialize it */
						pSequenceListTracer->Filling = 0;
						pSequenceListTracer->next = NULL;
					}
					/* store the information in this node and increment filling */
					pSequenceListTracer->Sequence[pSequenceListTracer->Filling++].Complete = pSequence2->Complete;
/*ds980415 begin
                                        increment sequence2; we have just used it */
                                        pSequence2++;
				        /* next sequence1 sequence 
				        //pSequence1++;
//ds980415 end */

				}
				else
				{
					PClusterUCNode _stemp;
					/* no match!
						this means, the cluster is finished!
						do the final things with it and delete it from the cluster-under-construction list */
					
					/* DumpCluster(pClusterUCTemp); */
					
	  
					CalculateMoments(pClusterUCTemp);
	  
					clusters++;
                                        if ( clusters >= MAXCLUSTERS ) {
                                           printf ( " \n l3cl: Too many clusters " ) ;
                                           clusters--;
                                        }  
	  
					/* mark as unused */
					pClusterUCTemp->Sequence.Complete = -1; 

					/* release all sequence nodes, except the first */
					pSequenceListTracer = pClusterUCTemp->pList;
					pSequenceListTracer->Filling = 0;
					pSequenceListTracer = pSequenceListTracer->next;
					pClusterUCTemp->pList->next = NULL;
					while (pSequenceListTracer != NULL)
					{
						_stemp = pSequenceListTracer;
						pSequenceListTracer = pSequenceListTracer->next;
						deallocate_bytes_with_check(_stemp);
					}
				}
/* //ds980415: bug, there was no match for this cluster, but maybe for the next
				//increment sequence2; we have just used it 
				pSequence2++;

				//next sequence1 sequence */ 
				pSequence1++;
			} /* end: cluster-under-construction loop */

			/* there might be some additional sequences above the last cluster-under-construction */
			while (pSequence1 < pSequence1End)
			{
				/* this could be the start of a new cluster! */
				/* start of a new cluster means: there is also a matching cluster in the
					next pad. so, look into the next pad to find a match 
					NOTE:
						here need NOT be any matches. so, we must test pSequence2 position */
				while ((pSequence2 < pSequence2End) && (pSequence2->Part.End < pSequence1->Part.Begin))
					pSequence2++;
				/* did we find a match? */
				if ((pSequence2 < pSequence2End) && 
					(pSequence2->Part.Begin < pSequence1->Part.End) &&
					(pSequence2->Part.End > pSequence1->Part.Begin))
				{
					/* yes ! */
					/* start a new cluster. that is: append the sequence-begin-end for both sequences (sequence1, sequence2)
						at the tail of the cluster-under-construction-list (start/stop is for the last sequence(2)) */
					pClusterUCNew->Sequence.Complete = pSequence2->Complete;
					pClusterUCNew->PadRow = iPadRow;
					pClusterUCNew->StartPad = iPad;

					/* make sure node is empty */
					pClusterUCNew->pList->Filling = 0;
					pClusterUCNew->pList->next = NULL;
					/* store the information in this node and increment filling */
					pClusterUCNew->pList->Sequence[pClusterUCNew->pList->Filling++].Complete = pSequence1->Complete;
					/* the same must be done for sequence2 */
					pClusterUCNew->pList->Sequence[pClusterUCNew->pList->Filling++].Complete = pSequence2->Complete;

					/* increment cluster-under-construction pointer to next free location */
					pClusterUCNew++;
                              if ( pClusterUCNew >= pClusterUCMax ) {
                                  printf ( " \n Maximum number of clusters under construction reached " ) ;
                                  pClusterUCNew-- ;
                              }
				}
				/* next sequence1 sequence */
				/* increment sequence2; we have just used it */
				pSequence2++;
				pSequence1++;
			} /* end: sequence1 loop */

			/* delete the finished clusters, if any
				that is: throw out all -1 entries */
			for (pLastInsert = pTempNow = pClustersUnderConstruction; pTempNow < pClusterUCEnd; pTempNow++)
			{
				/* do we have NO -1 here? */
				if (pTempNow->Sequence.Complete != -1)
				{
					/* yes!
						if pTempNow != pLastInsert then we must copy from tempnow to lastinsert 
						and increment lastinsert */
					if (pTempNow != pLastInsert)
					{
						PClusterUCNode _nodetemp;
						/* ATTENTION! very important:
							we must preserve the pList pointer of the destination. otherwise we would have 2 clusters pointing
							to the same clusternode! */
						_nodetemp = pLastInsert->pList;
						*(pLastInsert++) = *pTempNow;
						pTempNow->pList = _nodetemp;
					}
					else
					{
						/* otherwise we must increment lastinsert */
						pLastInsert++;
					}
				}
			} /* end: compaction loop */
			/* NOTE:
			pLastInsert defines now the temporary new end of the cluster-under-construction list */
			/* insert the new clusters (and sort the list), if any */ 
			/* is there something to insert? */
			if (pClusterUCNew != pClusterUCEnd)
			{
				/* yes!
					loop over all sequences, that should be inserted 
					NOTE:
						we reuse pClusterUCTemp!
						the clusters, that should be inserted are already sorted by default  */
				for (pTempNow = pClustersUnderConstruction, pClusterUCTemp = pClusterUCEnd;
					pClusterUCTemp < pClusterUCNew; pClusterUCTemp++)
				{
					/* scan for insertion point 
						NOTE:
							must check for end of list; the insertion point could be at the tail */
					while ((pTempNow < pLastInsert) && 
						(pTempNow->Sequence.Part.End < pClusterUCTemp->Sequence.Part.Begin))
						pTempNow++;
					/* append to tail? */
					if (pTempNow >= pLastInsert)
					{
						PClusterUCNode _nodetemp;
						/* yes, append!
							append here and increment pLastInsert */

						/* ATTENTION! very important:
							we must preserve the pList pointer of the destination. otherwise we would have 2 clusters pointing
							to the same clusternode! */
						_nodetemp = pLastInsert->pList;
						*(pLastInsert++) = *pClusterUCTemp;
						pClusterUCTemp->pList = _nodetemp;
					}
					else
					{
						/* no, insert!
							shift the rest of the list beginning at tempnow one entry upwards 
							reverse order. memcpy not used, should be slower for these small data volumes! 
							NOTE:
								preserve the plist pointer of the destination only, if we don't overwrite any plist
						*/
						/* get some temporary pointers and instances */
						PClusterUCList pTempFrom, pTempTo;
						TClusterUCList Temp;
						PClusterUCNode _nodetemp;

						Temp = *pClusterUCTemp;
						_nodetemp= pLastInsert->pList;
						for (pTempFrom = pLastInsert - 1, pTempTo = pTempFrom + 1; 
							pTempFrom >= pTempNow; pTempFrom--, pTempTo--)
						{
							*pTempTo = *pTempFrom;
						}
						/* preserve plist (with overwrite protection) */
						if(pLastInsert != pClusterUCTemp)
							pClusterUCTemp->pList = _nodetemp;
						/* insert item */

						*pTempNow = Temp;
						/* update pLastInsert to end of list */
						pLastInsert++;;
					}
				}
			}
			/* update pClusterUCEnd */
			pClusterUCEnd = pLastInsert;
			/* swap the sequences and reload the new Sequence2 with the next pad data, if not at the end */
			/* ok, swap */
			pSequence1 = pSequence2Initial;
			pSequence1End = pSequence2End;
			if (iPad < iLastPad[iPadRow]-3)
			{
				/* and reload */
				pSequence2Initial = pSequence2 = NewPad(padlist[iPadRow][iPad + 2]);
/*ds980414: major bug! */
				pSequence2End = pSequence2 + *(n_cluster[padlist[iPadRow][iPad + 2]]);
			}
			else
			{
				/* special handling for the last pad (cheat) 
					suggests an empty sequence list and forces all clusters to be terminated */
				pSequence2End = pSequence2;
			}
		} /* end: pad loop */
	} /* end: padrow loop */	
} /* end: FindClusters */


#if defined(__cplusplus)
}
#endif

