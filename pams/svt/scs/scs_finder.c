/*------------------------------------------------------------------
FILE:         scs_finder.c
DESCRIPTION:  scs - cluster finder 
AUTHOR:       Dave Read, Claude Pruneau
BUGS:         -- STILL IN DEVELOPMENT --
HISTORY:      Created by Dave Read 4/12/95 (read@utpapa.ph.utexas.edu)
              Adapted to STAF by C.Pruneau
------------------------------------------------------------------*/

/*------------------------------------------------------------------
ROUTINE:      long scs_finder
DESCRIPTION:  SVT cluster finder
  svt_scl_ is the actual cluster-finder.  It works by looping through
  all unused sequences until they are all 'used' in a cluster.  When 
  it starts with an unused sequence, it marks the sequence as used, 
  and then initiates a news cluster with this 'seed' sequence.  Then
  it starts looping through the unused clusters, asking for each one
  if it overlaps any sequence in the current cluster.  If it does, it is
  added to the cluster.  

  Note that the scope of this routine is one wafer, i.e. this routine
  addresses one wafer worth of data at a time. 
 
  A cluster consists of a linked list of sequences.  When a sequence is
  'added' to a cluster, it is appended to the end of the linked list.  
  Since it is possible for sequences to arrive out of order (although
  this might not be true in the real DAQ system), a check is made to 
  see if the candidate sequence is adjacent to an 'interior' sequence
  in the cluster.  If it _is_ adjacent to an interior sequence, it
  is still added to the cluster at the end of the chain, but a flag is
  set in the cluster table which marks the cluster as being 'suspicious.'
  This flag is used later in the space-point extraction routines to 
  process the cluster as a possible merged hit.

ARGUMENTS:    seq  : sequence table
ARGUMENTS:    adc  : actual adc values
ARGUMENTS:    par  : parameters of the cluster  finder
ARGUMENTS:    clus : cluster list (output)
RETURN VALUE: STAF Condition Value
------------------------------------------------------------------*/
#include "scs_finder.h"

/* prototype function */
int overlap (int b1, int b2, int e1, int e2);

/* scs_finder */
long type_of_call scs_finder_(
  TABLE_HEAD_ST            *seq_h,        SSF_SEQ_ST              *seq ,
  TABLE_HEAD_ST            *par_h,        SCS_PAR_ST              *par ,
  TABLE_HEAD_ST           *clus_h,    SCS_CLUSTER_ST              *clus ) 
{

{
   int i, j, k,
       iFirst, iLast, iFirstAnode, iLastAnode, iThisAnode,
       iAnode,  iSeq,  iIsMerged,  iNumSeq,
       iNumClusters = 0,
       iClusterChanged;

   /* reset */

   iClusterChanged = 0;

   /* set all sequences to 'unused' */

   for (iSeq = 0; iSeq < seq_h->nok; iSeq++)
      seq[iSeq].key_next = -1;

   /* examine all sequences */

   for (i = 0; i < seq_h->nok; i++)
   {
      /* Has this sequence been used in a cluster already? */

      if (seq[i].key_next != -1)
         continue;

      /* It hasn't been used.  Start a new cluster with it! */

      iFirst = iLast = i;
      iFirstAnode = iLastAnode = seq[i].anode_id;
      iNumSeq = 1;
      iIsMerged = 0;
      seq[i].key_next = 0;

      do
      {
         iClusterChanged = 0;
         for (j = 0; j < seq_h->nok; j++)
         {
            if (j == i || seq[j].key_next != -1)
               continue;

            /*  Loop over all sequences in the cluster; does our 
             *  candidate sequence overlap with any of them? 
             */

            iSeq = iFirst;
            for (k = 0; k < iNumSeq; k++)
            {
               iAnode = seq[j].anode_id;
               iThisAnode = seq[iSeq].anode_id;

               /* Is it on an adjacent anode ? */

               if (iAnode != iThisAnode - 1 && iAnode != iThisAnode + 1)
               {
                  iSeq = seq[iSeq].key_next;
                  continue;
               }

               /* Does it overlap ? */

               if (overlap (seq[j].t_beg,    seq[j].t_end,
                            seq[iSeq].t_beg, seq[iSeq].t_end))
               {
                  if (iAnode == iFirstAnode - 1)
                  {
                     /*  This sequence is before the start of the cluster;
                      *  make this our new first sequence, and make it
                      *  become the new list head 
                      */

                     seq[j].key_next = iFirst;
                     iFirst = j;
                     iFirstAnode = seq[j].anode_id;
                  }
                  else if (iAnode == iLastAnode + 1)
                  {
                     /*  This sequence is after the end of the cluster;
                      *  make this our new last sequence, and add it
                      *  to the end of the list */

                     seq[iLast].key_next = j;
                     seq[j].key_next = 0;
                     iLast = j;
                     iLastAnode = seq[j].anode_id;
                  }
                  else
                  {
                     /*  We found a sequence which overlaps an interior
                      *  sequence in the cluster.  This means we have
                      *  a malformed cluster; set the flag to indicate
                      *  that, and insert the sequence into the list.
                      *  It doesn't matter where we insert it, because
                      *  this cluster will have to be treated specially
                      *  anyway.  Thus, we insert it at any easy place:
                      *  after our current sequence.
                      */

                     iIsMerged = 1;
                     seq[j].key_next = seq[iSeq].key_next;
                     seq[iSeq].key_next = j;
                  }

                  iNumSeq++;
                  iClusterChanged = 1;
                  break;
               }

               /* Follow the link to the next sequence in the cluster */

               iSeq = seq[i].key_next;
            }
         }

      } while (iClusterChanged);

      clus[iNumClusters].key_first   = iFirst;
      clus[iNumClusters].n_seq       = iNumSeq;
      clus[iNumClusters].merge_flag  = iIsMerged;
      iNumClusters++;
   }

   clus_h->nok = iNumClusters;

  return STAFCV_OK;
}
}


/*------------------------------------------------------------------
ROUTINE:      overlap
DESCRIPTION:  

   overlap() decides if two sequences overlap.  Two sequences are considered
   to overlap if their begin or end correspond to one of the
   situations illustrated below:

   1)           B1----E1                
           B2----E2

   2)           B1----E1
                    B1----E2

   3)           B1----E1
            B2--------------E2

   4)         B1---------E1
                 B2----E2

ARGUMENTS:    b1, e1 : begin/end time index of the first sequence
              b2, e2 : begin/end time index of the second sequence
RETURN VALUE:  0 : no overlap
              >0 : overlap    
------------------------------------------------------------------*/
/*  overlap() decides if a two sequences overlap.  It takes the start and
 *  end times of both sequences, and returns non-zero if they overlap.
 */

int overlap (int b1, int e1, int b2, int e2)
{
   if ( (b1 <= b2 && e1 >= b2) || (b1 >= b2 && b1 <= e2) )
     return 1;
   else
     return 0;
}

