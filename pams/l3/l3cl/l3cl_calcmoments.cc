/*:>-------------------------------------------------------------------
**:
**:     program: l3cl_calcmoments.cc (StAF version)
**:
**:     LEVEL 3 clusterfinder CalculateMoments function
**:     author: dirk schmischke, ikf, kraut@ikf.uni-frankfurt.de
**:     last change: 06/07/98 cs
**:
**:>-----------------------------------------------------------------*/


/* includes */
#include <math.h>
#include "l3cl_inc.h"

/* externals */
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
/* simulated asic storage for number of sequences in pad */
extern unsigned short *phys_n_cluster;
extern struct DATA_RAM **data_ram;
extern struct DATA_RAM *phys_data_ram;
/* log-table for prf */
int LogTable[256];



/* all code in pseudocode

   peaks in a sequence
   5 peaks in a sequence should be enough(?) */
#define MAXSEQPEAKS 10
/* 5 peaks at all should be enough */
#define MAXPEAKS 12

typedef struct
{
	int	index;
	int height;
} TSequencePeaks;

/* peaks in cluster */
typedef struct
{
	int pad;
	int pad_in_row;
	int seq_index;
} TPeaks;

/* inlinefunction to find peak(s) in a sequence (timedirection)
   Parameters:
	paddata			- pointer to adc data of one pad
	seq_begin		- begin of sequence (index to paddata)
	seq_end			- end of sequence (index of paddata)
   Returns:
	number_of_peaks	- number of found peaks in this sequence
	seqeunce_peaks	- array of indices where peaks occur (index to paddata) 
	and their heights */

inline void GetSequencePeaks(unsigned char* paddata, int seq_begin, int seq_end,
			     int& number_of_peaks, TSequencePeaks* sequence_peaks)
{
	int index;
	int	slope;
	int	last_adc;
	int temp;

	/* initial slope is falling */
	slope = -1;
	last_adc = 0;
	number_of_peaks = 0;
	/* loop over all adc values in sequence */
	                // printf("seq_begin: %d, seq_end: %d\n", seq_begin, seq_end);
	for(index = seq_begin; index <= seq_end; index++)

	{
	        /* rising slope? */
		if (slope > 0)
		{
			/* yes
			   still rising? */
			if ((temp = paddata[index]) >= last_adc)
			{
				/* yes, remember last adc */
				last_adc = temp;
			}
			else
			{
				/* no, there has been a peak at the last adc index
				   make an entry in peak_indices */
				sequence_peaks[number_of_peaks].index = index-1;
				sequence_peaks[number_of_peaks].height = last_adc;
				if (++number_of_peaks >= MAXSEQPEAKS)
				{
				        /* too many peaks, stop */
					--number_of_peaks;
					break;
				}
				/* remember last adc */
				last_adc = temp;
				/* set slope to falling */
				slope = -1;
			}
		}
		else
		{
		        /* no */
		        /* still falling? */
			if ((temp = paddata[index]) <= last_adc)
			{
				/* yes, remember last adc */
				last_adc = temp;
			}
			else
			{
				/* no, there has been a minimum */
				/* remember last adc */
				last_adc = temp;
				/* set slope to rising */
				slope = 1;
			}
		}
	}
}

/* calculate moments of clusters using prf, with unfolding */
void CalculateMomentsUnfold(PClusterUCList NewCluster)
{
	unsigned short  PadCount, Pad, SequenceCount;
	PClusterUCNode  CurrentNode;
	PFormattedData  pStore;
	int compareouter, compareinner;
	char used[MAXSEQPEAKS];
	int	slopes[2][MAXSEQPEAKS];
	int number_of_sequence_peaks;
	int temp;
	TSequencePeaks sequencepeaks[2][MAXSEQPEAKS];
	/* to avoid confusion... */
	int old_peakstore; /* ds */
	int new_peakstore; /* ds */
	TPeaks peak_indices[MAXPEAKS];
	int number_of_peaks;
	int padcenter, timecenter;
	int padpeak, timepeak;
	int padnext, timenext;
	/* double centerpad, centertime; */
	int c1, c2;
	int dpeakpad, dpeaktime;
	int changepad, changetime;

	/* get a pointer to the current entry in the global store, where the formatted data will be
			written to. clusters is updated by FindClusters each time a new cluster is complete.
			the data is written in the fields SumAdc, SumAdcTimesTime and SumAdcTimesPad of the global array GlobalStore */
	pStore = (PFormattedData)&pGlobalStore[clusters];  

	number_of_sequence_peaks = 0;
	number_of_peaks = 0;
	old_peakstore = 0; /*ds */
	/* ds: new_peakstore is associated with compareouter, old_peakstore with compare inner loop over all nodes in cluster 
			NOTE:
				the first node of a cluster list is allways assigned */
	for(Pad = NewCluster->StartPad, PadCount = padlist[NewCluster->PadRow][NewCluster->StartPad], CurrentNode = NewCluster->pList; CurrentNode != NULL; 
			CurrentNode = CurrentNode->next)
	{
		/* loop over all pads (sequences) */
		for(SequenceCount = 0; SequenceCount < CurrentNode->Filling; SequenceCount++, Pad++, PadCount++)
		{
			new_peakstore = (old_peakstore+1) & 1;
			/* get sequencepeaks */
			GetSequencePeaks(&data_ram[PadCount]->t_sam[0], CurrentNode->Sequence[SequenceCount].Part.Begin, 
				CurrentNode->Sequence[SequenceCount].Part.End,
				temp, sequencepeaks[new_peakstore]);	/*ds */
			/* compare the new peaks with the old ones */
			memset(used, 0, MAXSEQPEAKS);
			/* loop over new peaks */
			for (compareouter = 0; compareouter < temp; compareouter++)
			{
				/* loop over old peaks */
				for (compareinner = 0; compareinner < number_of_sequence_peaks; compareinner++)
				{
				  /* the catchlimit for matching peaks is 1 timebins difference */
					if (!used[compareinner] &&
						(fabs(sequencepeaks[old_peakstore][compareinner].index - 
						     sequencepeaks[new_peakstore][compareouter].index) <= 2))	/*ds a limit of 2 fits much better! */
					{
					        /* found fitting sequence */
					        /* mark as used */
						used[compareinner] = 1;
						/* was the old slope rising? */
						if (slopes[old_peakstore][compareinner] > 0)
						{
							/* yes
							   is the new slope still rising?
							   (from StartPad to the next the slope has to be rising
							    because we don't want peaks on the cluster margin) */
							if ((sequencepeaks[old_peakstore][compareinner].height <= 
								sequencepeaks[new_peakstore][compareouter].height))
							        /*|| (Pad == NewCluster->StartPad + 1)) */
							{
							        /* yes */
							        /* set new slope to rising */
								slopes[new_peakstore][compareouter] = 1;
							}
							else
							{
							        /* no */
							        /* a peak was found */
							        /* store the peak (it was in the last sequence) */

								peak_indices[number_of_peaks].pad = (int)PadCount-1;
	 							peak_indices[number_of_peaks].pad_in_row = (int)Pad-1;
								peak_indices[number_of_peaks].seq_index = sequencepeaks[old_peakstore][compareinner].index;
								number_of_peaks++;
								/* too many peaks? */
								if (number_of_peaks >= MAXPEAKS)
								    goto emergency_exit;	/* sigh! */
								/* set new slope to falling */
								slopes[new_peakstore][compareouter] = -1;
							}	
						}
						else
						{
						        /* no */
						        /* still falling? */
							if (sequencepeaks[old_peakstore][compareinner].height >= 
								sequencepeaks[new_peakstore][compareouter].height)
							{
							        /* yes */
							        /* set new slope to falling */
								slopes[new_peakstore][compareouter] = -1;
							}
							else
							{
							        /* no */
							        /* there would have been a minimum, */
							        /* but that's not likely, however */
							        /* damaged clusters have this behaviour */
								slopes[new_peakstore][compareouter] = 1;
							}
						}
						/* we found a match and don't need to search further */
						break;
					}
				} /* end loop compareinner */
				/* did we find a match? */
				if (compareinner == number_of_sequence_peaks)
				{
				        /* no */
			       	        /* this is a new peakstart */
				        /* set initial slope to falling, to avoid having peaks found at the edge of a damaged cluster */
					slopes[new_peakstore][compareouter] = -1;
				}
			} /* end loop compareouter */
			/* clusters merged at very low distances may have no more than one peak per pad */
			/* but the peaks may have a large shift at a certain point. these clusters can */
			/* be found by scanning all the old peaks and looking for used = 0 and slope = rising. */
			/* loop over old peaks */
			for (compareinner = 0; compareinner < number_of_sequence_peaks; compareinner++)
			{
				if ((!used[compareinner]) && (slopes[old_peakstore][compareinner] == 1))
				{
				        /* should be a peak */
					peak_indices[number_of_peaks].pad = (int)PadCount-1;
               peak_indices[number_of_peaks].pad_in_row = (int)Pad-1;
					peak_indices[number_of_peaks].seq_index = sequencepeaks[old_peakstore][compareinner].index;
					number_of_peaks++;
					/* too many peaks? */
					if (number_of_peaks >= MAXPEAKS)
					    goto emergency_exit;	/* sigh! */
				}
			}

			/* change old and new sequencepeaks */
			old_peakstore = new_peakstore;
			/* remember the number of sequence peaks for the next time */
			number_of_sequence_peaks = temp;
		}
	}
	/* now the pointers needed for fitting are set for all except the last peak. for that, the pointer 
	   to the neighbour in time direction is still missing. */
	/* is there more than one peak? */
emergency_exit:
	/* calculate centroids.... */
	if (number_of_peaks > 0)
	{
	        /* 1. calculate center of all clusterspeaks */
		padcenter = 0;
		timecenter = 0;
		for(temp = 0; temp < number_of_peaks; temp++)
		{
			padcenter += peak_indices[temp].pad_in_row;
			timecenter += peak_indices[temp].seq_index;
		}
		padcenter /= number_of_peaks;

		padcenter = padlist[NewCluster->PadRow][padcenter];
		timecenter /= number_of_peaks;
		/* 2. for each peak: */
		for (temp = 0; temp < number_of_peaks; temp++)
		{
			/*	calculate difference pad_peak - pad_center
			//	if > 0: take pad_peak+1 for prf
			//	else: take pad_peak-1 for prf
			// calculate difference time_peak - time_center
			//	if > 0: take time_peak+1 for prf
			//	else: take time_peak-1 for prf */
			padpeak  = peak_indices[temp].pad;
			timepeak = peak_indices[temp].seq_index;
			dpeaktime = (int) timepeak << 6;
			dpeakpad = (int) peak_indices[temp].pad_in_row << 6;
			if ((padpeak - padcenter) > 0)
			{
				/* take right neighbour */
				padnext = padpeak+1;
				dpeakpad += 32;	/* +0.5 */
				changepad = 1;
			}
			else
			{
				/* take left neighbour */
				padnext = padpeak-1;
				dpeakpad -= 32; /* -0.5 */
				changepad = 0;
			}
			if ((timepeak - timecenter) > 0)
			{
				/* take upper neighbour */
				timenext = timepeak+1;
				dpeaktime += 32; /* +0.5 */
				changetime = 1;
			}
			else
			{
				/* take lower neighbour */
				timenext = timepeak-1;
				dpeaktime -= 32; /* -0.5 */
				changetime = 0;
			}

			/* calculate prf
			// prf:
			// P = (p1+p2)/2 - sigma^2 * ln(c1/c2)
			// where p is the centroid position in pad-units
			// p1, p2 are the pad positions in pad-units
			// c1, c2 are the associated adc values
			// sigma^2 is the gaussian width of the cluster in pad-units
			// sigma has to be taken from measurement or theory
			// sigma^2 could be calculated:
			// sigma^2 = 1/ln(c2^2/(c1*c3))
			// estimated from data:
			// sigma^2 in pad-direction: 0.62 (pad-units)
			// sugma^2 in time-direction: 2.12 (time-units)
			// pad direction: */
			c1 = (int) data_ram[padnext]->t_sam[timepeak];
			c2 = (int) data_ram[padpeak]->t_sam[timepeak];
			c1 = LogTable[c1];
			c2 = LogTable[c2];
			/* centerpad = (double) dpeakpad; */
			/* store centroids as short ints scaled to 1.0 = 64 */
			if (changepad)
				pStore->CenterPad = (unsigned short) dpeakpad + (((int) (0.67*64.0) * (c1 - c2)) / 64);
			else
				pStore->CenterPad = (unsigned short) dpeakpad - (((int) (0.67*64.0) * (c1 - c2)) / 64);
			// time direction:
			c1 = (int) data_ram[padpeak]->t_sam[timenext];
			/*c2 = (int) data_ram[padpeak]->t_sam[timepeak]; */
			c1 = LogTable[c1];
			/* c2 = LogTable[c2]; */
			/* centertime = (double) timepeak; */
			/* store centroids as short ints scaled to 1.0 = 64 */
			if (changetime)
				pStore->CenterTime = (unsigned short) dpeaktime + (((int) (2.12*64.0) * (c1 - c2)) / 64);
			else
				pStore->CenterTime = (unsigned short) dpeaktime - (((int) (2.12*64.0) * (c1 - c2)) / 64);
			/* store padrow */
			pStore->PadRow = NewCluster->PadRow;
			/* next cluster */
			pStore++;
		}
		/* add clusters correct overshoot */
		clusters += number_of_peaks-1;
	}
	else
	  clusters--;	/* there was no cluster */
}

/* calculate moment of cluster using prf, no unfolding */
void CalculateMomentsPrf(PClusterUCList NewCluster)
{
	unsigned short  PadCount, Time, SequenceCount, Pad;
	unsigned int    ui1, SumAdc;
	PClusterUCNode  CurrentNode;
	unsigned char  *MemoryPtr, *MemoryEnd;
	TFormattedData*  pStore;
	unsigned int    PeakIndexPad, PeakIndexTime, MaxValue, PeakIndexPadCount;
	int c1, c2, c3, sigma2, padShift, offPos ;	/* 32 bit! */
   int PadRow ;


  
	/* get a pointer to the current entry in the global store, where the formatted data will be
			written to. clusters is updated by FindClusters each time a new cluster is complete.
			the data is written in the fields SumAdc, SumAdcTimesTime SumAdcTimesPad and reserved 
			of the global array GlobalStore */

	pStore = (TFormattedData*)&pGlobalStore[clusters];
	SumAdc = MaxValue = 0;
	PadCount = PeakIndexPad = PeakIndexPadCount = PeakIndexTime = 0;
   PadRow   = NewCluster->PadRow ;
	/* loop over all nodes in cluster 
			NOTE:
				the first node of a cluster list is allways assigned */
	for( Pad = NewCluster->StartPad, PadCount = padlist[NewCluster->PadRow][NewCluster->StartPad],CurrentNode = NewCluster->pList; 
        CurrentNode != NULL; 
		  CurrentNode = CurrentNode->next)
	{
		/* loop over all pads (sequences) */
		for(SequenceCount = 0; SequenceCount < CurrentNode->Filling; SequenceCount++, Pad++, PadCount++)
		{
			/* loop over all adc values in pad (sequence) */
			for(MemoryPtr = &data_ram[PadCount]->t_sam[Time = CurrentNode->Sequence[SequenceCount].Part.Begin],
				MemoryEnd = &data_ram[PadCount]->t_sam[CurrentNode->Sequence[SequenceCount].Part.End];
				MemoryPtr <= MemoryEnd; MemoryPtr++, Time++)
			{
				/* calculations */
				if (MaxValue < (ui1 = *MemoryPtr))
				{
					/* temporary peak was found */
					/* remember this location */
					PeakIndexTime = Time;
					PeakIndexPadCount = PadCount;
					PeakIndexPad = Pad;
					MaxValue = ui1;
				}
				SumAdc += table[ui1];
			}

		}
		// PadCount++;
	}


	/* process the peak data 
	// if the peak is at an edge, mark cluster as bad cluster
	// 'at an edge' means:
	// PeakIndexTime not in [1..510] or
	// PeakIndexPad not in [1..row_sizes[PadRow]] */
	if ((PeakIndexTime >= 0 ) && (PeakIndexTime <= 511) &&
		 (PeakIndexPad  >= 0 ) && (PeakIndexPad  <= row_sizes[NewCluster->PadRow+MIN_PADROW-1]))
	{
		/* valid peak, calculate prf
		// prf:
		// P = (p1+p2)/2 - sigma^2 * ln(c1/c2)
		// where p is the centroid position in pad-units
		// p1, p2 are the pad positions in pad-units
		// c1, c2 are the associated adc values
		// sigma^2 is the gaussian width of the cluster in pad-units
		// sigma^2 could be calculated:
		// sigma^2 = 1/ln(c2^2/(c1*c3))
		// pad direction: */

//
//   Go up and down two paths for inner sector. It seems
//   to improve resolution even though I don't really
//   understand how the pad is calculated. (PY 7/7/98)
//   I think a better job can be done here.
//
      if ( PadRow < 13 ) {
         offPos   =   2 ;

         c1 = c2 = c3 = 0 ;
//
//   Get values around peak
//
         if ( PeakIndexPad > 1 )
    	      c1 = (int) data_ram[PeakIndexPadCount-2]->t_sam[PeakIndexTime];
         if ( PeakIndexPad > 0 )
	         c1 += (int) data_ram[PeakIndexPadCount-1]->t_sam[PeakIndexTime];
             
            
		  c2 = (int) data_ram[PeakIndexPadCount]->t_sam[PeakIndexTime];
            
         if ( PeakIndexPad < n_pads[PadRow] ) 
            c2 += (int) data_ram[PeakIndexPadCount+1]->t_sam[PeakIndexTime];

         if ( PeakIndexPad < n_pads[PadRow]-1 )
            c3 = (int) data_ram[PeakIndexPadCount+2]->t_sam[PeakIndexTime];
         if ( PeakIndexPad < n_pads[PadRow]-2 )
            c3 += (int) data_ram[PeakIndexPadCount+3]->t_sam[PeakIndexTime];
      }
      else {
         offPos   = 1 ;     
//
//   Get values around peak
//
         if ( PeakIndexPadCount > -1+offPos ) 
		      c1 = (int) data_ram[PeakIndexPadCount-offPos]->t_sam[PeakIndexTime];
         else
            c1 = 0 ;
		   c2 = (int) data_ram[PeakIndexPadCount]->t_sam[PeakIndexTime];
		   if ( PeakIndexPadCount <= NPADS-offPos )
            c3 = (int) data_ram[PeakIndexPadCount+offPos]->t_sam[PeakIndexTime];
         else
            c3 = 0 ;
      }
//
//   Get logs
//
		c1 = LogTable[c1];
		c2 = LogTable[c2];
		c3 = LogTable[c3];

/* store centroids as short ints scaled to 1.0 = 64 */

      sigma2 = 2*c2 - (c1 + c3) ;
      pStore->CenterPad = (unsigned short) ((PeakIndexPad << 6)) ;
      if ( sigma2 !=0 ){
          padShift = - offPos * ((c1 - c2) << 6) / sigma2  ;
          pStore->CenterPad += padShift ;
      } 

		/* time direction: */
      if ( PeakIndexTime > 0 )
		   c1 = (int) data_ram[PeakIndexPadCount]->t_sam[PeakIndexTime-1];
      else
         c1 = 0 ;
		/*c2 = (int) data_ram[PeakIndexPadCount]->t_sam[PeakIndexTime]; */
		if ( PeakIndexTime < NTIME-1 )
         c3 = (int) data_ram[PeakIndexPadCount]->t_sam[PeakIndexTime+1];
      else
         c3 = 0 ;
		c1 = LogTable[c1];
		/* c2 = LogTable[c2]; the same as above */
		c3 = LogTable[c3];
		/* store centroids as short ints scaled to 1.0 = 64 */
		if ( (2*c2 - (c1 + c3)) !=0 )
		    pStore->CenterTime = (unsigned short) ((PeakIndexTime << 6) - ((c1 - c2) << 6) / (2*c2 - (c1 + c3)));
		else pStore->CenterTime = (unsigned short) 0;
		/* store padrow */
		pStore->PadRow = NewCluster->PadRow;
	}
	else
	{
	        /* invalid peak, truncated cluster, mark as bad */
		pStore->CenterPad = 0;
		pStore->CenterTime = 0;
		/* store padrow */
		pStore->PadRow = 0;
	}
}


/* calculate moments of clusters, selects proper method */
void CalculateMoments(PClusterUCList NewCluster)
{
	PClusterUCNode  CurrentNode;
	int SequenceCount;
	int area;
	int UnfoldLimit = 32; /* ~ 4*8 + extra */
   UnfoldLimit = 100 ;

	area = 0;
	/* get total cluster area in hits */
	/* loop over all nodes */

//	printf("calc moments.\n");
	for(CurrentNode = NewCluster->pList; CurrentNode != NULL; CurrentNode = CurrentNode->next)
	{
		/* loop over all pads (sequences) */
		for(SequenceCount = 0; SequenceCount < CurrentNode->Filling; SequenceCount++)
		{
		        /* sum up all sequencewidths */
			area += CurrentNode->Sequence[SequenceCount].Part.End - CurrentNode->Sequence[SequenceCount].Part.Begin;
		}
	}
	/* if area > unfold_limit -> use cluster unfolding */
	/* else use standard method */
	if (area > UnfoldLimit)
		CalculateMomentsUnfold(NewCluster);
	else
		CalculateMomentsPrf(NewCluster);
}


/* setup log-table, scaled by 64; needed for prf */
void SetupLogTable()
{
	int index;

	for (index = 1; index < 256; index++)
	{
		LogTable[index] = (int) (log((double) index) * (double) 64.0);
	}
}

/* notes:
// global data format:
// for each cluster:
// centroid in pad direction: 16 bits, lower 6 bits as fractional part, next 8 bits as padnumber,
//								upper 2 bits as flags
// centroid in time dir: 16 bits, lower 6 bits as fractional part, next 10 bits as timebin
// clustercharge: 16 bits (space for some flagbits) */

#if defined(__cplusplus)
}
#endif
