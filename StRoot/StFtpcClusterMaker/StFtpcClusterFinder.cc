/***************************************************************************
 *
 * $Id: StFtpcClusterFinder.cc,v 1.1 1999/11/02 09:41:26 jcs Exp $
 *
 * Author:   Holm Huemmler  (hummler@mppmu.mpg.de)
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcClusterFinder.cc,v $
 * Revision 1.1  1999/11/02 09:41:26  jcs
 * add source files to empty StFtpcClusterMaker
 *
 **************************************************************************/
#include <iostream.h>
#include <stdlib.h>
#include "StFtpcClusterFinder.hh"
#include "math_constants.h"

StFtpcClusterFinder::StFtpcClusterFinder()
{
  cout << "StFtpcClusterFinder constructed" << endl;
}

StFtpcClusterFinder::~StFtpcClusterFinder()
{
  cout << "StFtpcClusterFinder destructed" << endl;

}

int StFtpcClusterFinder::FindHits(TClusterUC *Cluster, 
				 int iRow, 
				 int iSec, 
				 double *pRadius, 
				 double *pDeflection, 
				 FCL_FTPCADC_ST *ftpcadc, 
				 FCL_DET_ST *det, 
				 FCL_ZROW_ST *zrow, 
				 float fastlog[256],
				 int *point_nok,
				 int *point_maxlen,
				 FCL_FPPOINT_ST *fppoint,
				 FCL_AMPSLOPE_ST *ampslope,
				 FCL_AMPOFF_ST *ampoff,
				 FCL_TIMEOFF_ST *timeoff
				 )
{
  int iPad, iSequence; 
  int iPadPeaks, iThisPadPeaks;
  int iNewPeakLoop, iOldPeakLoop;
  int iNumPeaks, iOldPeakstore, iNewPeakstore;
  int iPeakCompare;
  TPadPeak PadPeaks[2][MAXSEQPEAKS];
  TPeak Peaks[MAXPEAKS];

  iNumPeaks=0;
  iPadPeaks=0;
  iOldPeakstore=0;
/*   printf("starting hitfinder\n"); */

  for(iPad=Cluster->StartPad; iPad<=Cluster->EndPad; iPad++)
    {
      /* set index for alternating use of sequencepeak array */
      iNewPeakstore=(iOldPeakstore+1)&1;
      
      iThisPadPeaks=0;

      /* loop over sequences as they need not be in ascending order, */
      /* if cluster is deformed. */
      for(iSequence = 0; 
          iSequence < Cluster->NumSequences; iSequence++)
	{
	  if(Cluster->SequencePad[iSequence] == iPad)
	    {
	      /* now looping over sequences in pad order */
	      /* get peaks for this sequence */
	      GetSeqPeaksAndCalibAmp(&(Cluster->Sequence[iSequence]), 
				     iRow*det[0].n_sectors*det[0].n_pads,
				     iSec*det[0].n_pads, iPad,
				     PadPeaks[iNewPeakstore], &iThisPadPeaks,
				     ftpcadc, ampslope, ampoff
#ifdef DEBUGFILE
				     , fin
#endif
				     );
	      /* peaks are appended to peak list for this pad */
	    }
	}
      

      /* loop over peaks on this pad */ 
      for(iNewPeakLoop=0; iNewPeakLoop < iThisPadPeaks; iNewPeakLoop++)
	{
	  /* if peak is not matched => slope=0 => reprocessing */
	  PadPeaks[iNewPeakstore][iNewPeakLoop].slope=0;
	  
	  for(iOldPeakLoop=0; iOldPeakLoop < iPadPeaks; iOldPeakLoop++)
	    {
	      iPeakCompare=PadPeaks[iNewPeakstore][iNewPeakLoop].Timebin - 
		PadPeaks[iOldPeakstore][iOldPeakLoop].Timebin;
	      if(   ((iPeakCompare <= 1+PadPeaks[iNewPeakstore][iNewPeakLoop].width) 
		     && (iPeakCompare >= 0)) 
		 || ((iPeakCompare >=-1-PadPeaks[iOldPeakstore][iOldPeakLoop].width) 
		     && (iPeakCompare <= 0)))
		{
		  /* now we found a matching sequence peak  */
		  /* is the local cluster slope rising */
		  if(PadPeaks[iOldPeakstore][iOldPeakLoop].height <= 
		     PadPeaks[iNewPeakstore][iNewPeakLoop].height)
		    {
		      /* if slope is 0 keep the old slope */
		      /* otherwise flat tops will be split in 2 hits */ 
		      if(PadPeaks[iOldPeakstore][iOldPeakLoop].height == 
			 PadPeaks[iNewPeakstore][iNewPeakLoop].height)
			{
			  PadPeaks[iNewPeakstore][iNewPeakLoop].slope = 
			    PadPeaks[iOldPeakstore][iOldPeakLoop].slope;
			}
		      else
			{
			  /* yes, set slope to rising */
			  PadPeaks[iNewPeakstore][iNewPeakLoop].slope=1;
			}
		    }
		  else
		    {
		      /* slope is falling */
		      PadPeaks[iNewPeakstore][iNewPeakLoop].slope=-1;

		      /* was the slope rising before? */
		      if(PadPeaks[iOldPeakstore][iOldPeakLoop].slope > 0)
			{
			  /* yes, peak is found */
			  Peaks[iNumPeaks].pad=iPad-1;
			  Peaks[iNumPeaks].Timebin
			    =PadPeaks[iOldPeakstore][iOldPeakLoop].
			    Timebin;
			  Peaks[iNumPeaks].Sequence.Length = 
			    PadPeaks[iOldPeakstore][iOldPeakLoop].
			    Sequence.Length;
			  Peaks[iNumPeaks].Sequence.StartTimebin = 
			    PadPeaks[iOldPeakstore][iOldPeakLoop].
			    Sequence.StartTimebin;
			  Peaks[iNumPeaks].Sequence.StartADCEntry = 
			    PadPeaks[iOldPeakstore][iOldPeakLoop].
			    Sequence.StartADCEntry;
			  iNumPeaks++;
			  /* check if peak memory full */ 
			  if(iNumPeaks>MAXPEAKS)
			    {
#ifdef DEBUG
			      printf("Sick cluster. %d are too many peaks.\n",
				     iNumPeaks);
#endif
			      return FALSE;
			    }

			}
		    }
		  /* set slope to 0 to mark old peak as used */
		  PadPeaks[iOldPeakstore][iOldPeakLoop].slope=0;
		}
	    } /* end of: for(iOldPeakLoop...) */
	  /* is this peak unmatched, then reprocess */

	  if(PadPeaks[iNewPeakstore][iNewPeakLoop].slope == 0)
	    {
	      /* default for an unmatched peak is rising */
	      PadPeaks[iNewPeakstore][iNewPeakLoop].slope=1;
	      
	      for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
		{
		  if((Cluster->SequencePad[iSequence] == iPad - 1) && 
		     (Cluster->Sequence[iSequence].StartTimebin <= 
		      PadPeaks[iNewPeakstore][iNewPeakLoop].Timebin) && 
		     (Cluster->Sequence[iSequence].StartTimebin + 
		      Cluster->Sequence[iSequence].Length > 
		      PadPeaks[iNewPeakstore][iNewPeakLoop].Timebin) &&
		     ((unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry 
			    - Cluster->Sequence[iSequence].StartTimebin 
			    + PadPeaks[iNewPeakstore][iNewPeakLoop].Timebin].data >= 
		      (unsigned char) PadPeaks[iNewPeakstore][iNewPeakLoop].height))
		    {
		      PadPeaks[iNewPeakstore][iNewPeakLoop].slope=-1;
		    }
		}
	    }
	} /* end of: for(iNewPeakLoop...) */
      /* all new peaks have been matched */
      /* check for unmatched old peaks with rising slope */
      /* make them hits, too, if their right neighbor pixel is lower */
      for(iOldPeakLoop=0; iOldPeakLoop < iPadPeaks; iOldPeakLoop++)
	{
	  if(PadPeaks[iOldPeakstore][iOldPeakLoop].slope > 0)
	    {
	      /* find neighbor pixel */ 
	      for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
		{
		  if((Cluster->SequencePad[iSequence] == iPad) && 
		     (Cluster->Sequence[iSequence].StartTimebin <= 
		      PadPeaks[iOldPeakstore][iOldPeakLoop].Timebin) && 
		     (Cluster->Sequence[iSequence].StartTimebin + 
		      Cluster->Sequence[iSequence].Length > 
		      PadPeaks[iOldPeakstore][iOldPeakLoop].Timebin) &&
		     ((unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry 
			     - Cluster->Sequence[iSequence].StartTimebin 
			     + PadPeaks[iOldPeakstore][iOldPeakLoop].Timebin].data >= 
		      (unsigned char) PadPeaks[iOldPeakstore][iOldPeakLoop].height))
		    {
		      PadPeaks[iOldPeakstore][iOldPeakLoop].slope = -1;
		    }
		}
	    }
	  if(PadPeaks[iOldPeakstore][iOldPeakLoop].slope > 0)
	    {	  
	      Peaks[iNumPeaks].pad = iPad-1;
	      Peaks[iNumPeaks].Timebin = 
		PadPeaks[iOldPeakstore][iOldPeakLoop].Timebin;
	      Peaks[iNumPeaks].Sequence.Length = 
		PadPeaks[iOldPeakstore][iOldPeakLoop].Sequence.Length;
	      Peaks[iNumPeaks].Sequence.StartTimebin = 
		PadPeaks[iOldPeakstore][iOldPeakLoop].Sequence.StartTimebin;
	      Peaks[iNumPeaks].Sequence.StartADCEntry = 
		PadPeaks[iOldPeakstore][iOldPeakLoop].Sequence.StartADCEntry;
	      iNumPeaks++;
	      
	      
	      /* check if peak memory full */ 
	      if(iNumPeaks>MAXPEAKS)
		{
#ifdef DEBUG
		  printf("Sick cluster. %d are too many peaks.\n", iNumPeaks);
#endif
		  return FALSE;
		}
	    }
	}
      iPadPeaks=iThisPadPeaks;
      iOldPeakstore=iNewPeakstore;
    }/* end of: for(iPad...) */
  /* check if there are peaks left over with rising slope */ 
  /* make them hits, too */
  for(iOldPeakLoop=0; iOldPeakLoop < iPadPeaks; iOldPeakLoop++)
    {
      if(PadPeaks[iOldPeakstore][iOldPeakLoop].slope > 0)
	{
	  Peaks[iNumPeaks].pad = iPad-1;
	  Peaks[iNumPeaks].Timebin = 
	    PadPeaks[iOldPeakstore][iOldPeakLoop].Timebin;
	  Peaks[iNumPeaks].Sequence.Length = 
	    PadPeaks[iOldPeakstore][iOldPeakLoop].Sequence.Length;
	  Peaks[iNumPeaks].Sequence.StartTimebin = 
	    PadPeaks[iOldPeakstore][iOldPeakLoop].Sequence.StartTimebin;
	  Peaks[iNumPeaks].Sequence.StartADCEntry = 
	    PadPeaks[iOldPeakstore][iOldPeakLoop].Sequence.StartADCEntry;
	  iNumPeaks++;
	  
	  /* check if peak memory full */ 
	  if(iNumPeaks>MAXPEAKS)
	    {
#ifdef DEBUG
	      printf("Sick cluster. %d are too many peaks.\n", iNumPeaks);
#endif
	      return FALSE;
	    }
	}
    }
  if(!FitPoints(Cluster, iRow, iSec, pRadius, pDeflection, Peaks, 
		iNumPeaks, ftpcadc, det, zrow, fastlog, point_nok,
		point_maxlen, fppoint, timeoff))
    {
#ifdef DEBUG
      printf("Point fitting failed! Cluster is lost.\n");
#endif
      return FALSE;
    }

#ifdef DEBUGFILE
  for(iOldPeakLoop=0; iOldPeakLoop<iNumPeaks; iOldPeakLoop++)
    {
      if(Peaks[iOldPeakLoop].PadPosition>0 && Peaks[iOldPeakLoop].TimePosition>0)
	{
	  fprintf(fpoints, "%f %f %f %f\n", 
		  Peaks[iOldPeakLoop].PadPosition, Peaks[iOldPeakLoop].TimePosition, Peaks[iOldPeakLoop].x, Peaks[iOldPeakLoop].y);
	}
    }
#endif

  return TRUE;
}

int StFtpcClusterFinder::GetSeqPeaksAndCalibAmp(TSequence *Sequence,
					       int iRowTimesSeqsTimesPads,
					       int iSeqTimesPads,
					       int iPad,
					       TPadPeak *Peak, 
					       int *pNumPeaks, 
					       FCL_FTPCADC_ST *ftpcadc,	      
					       FCL_AMPSLOPE_ST *ampslope,
					       FCL_AMPOFF_ST *ampoff
					       )
{
  int bSlope;
  int iIndex, iADCIndex, iPadIndex;
  int iWidth;
  unsigned char cLastADC, cTemp;
  
  bSlope=0;
  cLastADC=0;
  iWidth=0;
  iPadIndex=iRowTimesSeqsTimesPads+iSeqTimesPads+iPad;
  
  for(iIndex=0; iIndex< Sequence->Length; iIndex++)
    {
      iADCIndex=Sequence->StartADCEntry+iIndex;
      cTemp=(unsigned char)((float)(unsigned int)(ftpcadc[iADCIndex].data)
	       *ampslope[iPadIndex].slope + ampoff[iPadIndex].offset);
      ftpcadc[iADCIndex].data=cTemp;

      if(cTemp < cLastADC)
	{
	  if(bSlope > 0)
	    {
	      /* peak is found */

	      Peak[*pNumPeaks].Sequence = *Sequence;
	      Peak[*pNumPeaks].Timebin = Sequence->StartTimebin+iIndex-1;
	      Peak[*pNumPeaks].height = cLastADC;
	      Peak[*pNumPeaks].width = iWidth;
	      if(*pNumPeaks < MAXSEQPEAKS - 1)
		{
		  (*pNumPeaks)++;
		}
	    }
	  bSlope = -1;
	  iWidth=0;
	}
      else
	{
	  if(cTemp != cLastADC)
	    {	     
	      bSlope = 1;
	      iWidth=0;
	    }
	  else
	    {
	      iWidth++;
	    }
	}

#ifdef DEBUGFILE
      fprintf(fin, "%d %d %d\n", iPad, Sequence->StartTimebin+iIndex, (int)cTemp);
#endif

      cLastADC=cTemp;
    }    
  if(bSlope > 0)
    {
	      Peak[*pNumPeaks].Sequence = *Sequence;
	      Peak[*pNumPeaks].Timebin =
		Sequence->StartTimebin+Sequence->Length-1;
	      Peak[*pNumPeaks].height = 
		(unsigned char) ftpcadc[Sequence->StartADCEntry+Sequence->Length-1].data;
	      Peak[*pNumPeaks].width = iWidth;
	      if(*pNumPeaks < MAXSEQPEAKS - 1)
		{
		  (*pNumPeaks)++;
		}
    }
  return TRUE;
}

int StFtpcClusterFinder::FitPoints(TClusterUC* Cluster, 
				  int iRow, 
				  int iSec, 
				  double *pRadius, 
				  double *pDeflection, 
				  TPeak *Peak, 
				  int iNumPeaks, 
				  FCL_FTPCADC_ST *ftpcadc, 
				  FCL_DET_ST *det, 
				  FCL_ZROW_ST *zrow, 
				  float fastlog[256], 
				  int *point_nok,
				  int *point_maxlen,
				  FCL_FPPOINT_ST *fppoint,
				  FCL_TIMEOFF_ST *timeoff)
{
  int iADCValue, iADCPlus, iADCMinus;
  int iADCTimePlus, iADCTimeMinus;
  int iSequence, iBin;
  int ChargeSum, PadSum;
  float TimeSum;
  int iUseGauss;
  int iPeakIndex, iInnerIndex;
  int iNumUnfoldLoops;
  int BadFit, PeakShifted, FailedFit;
  int PadtransPerTimebin, PadtransBin;
  float SumDeltaPos, SumDeltaAmp;
  float NewTimePosition, NewPadPosition;
  float NewPeakHeight, PeakHeightSum;
  float fDeltaADC, fDeltaADCPlus, fDeltaADCMinus;
  float fDeltaADCTimePlus, fDeltaADCTimeMinus;
  float fDriftLength, fRadError, fPhiError;

  PadtransPerTimebin=(int) det[0].n_int_steps / det[0].n_bins;

  if(iNumPeaks == 0)
    {
#ifdef DEBUG
      printf("Cluster starting %d, %d has no peak!\n", Cluster->StartPad, Cluster->Sequence->StartTimebin);
#endif
      return FALSE;
    }

  /* sum up cluster charge */
  ChargeSum=0;
  PadSum=0;
  TimeSum=0;
  for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
    {
      for(iBin = 0; iBin < Cluster->Sequence[iSequence].Length; iBin++)
	{
	  iADCValue = (unsigned char) ftpcadc[Cluster->Sequence[iSequence]
			     .StartADCEntry+iBin].data;
	  ChargeSum += iADCValue;
	  PadSum += Cluster->SequencePad[iSequence] * iADCValue;
	  TimeSum += (Cluster->Sequence[iSequence].StartTimebin + iBin + 
		      timeoff[iRow*det[0].n_sectors*det[0].n_pads+
			     iSec*det[0].n_pads+
			     Cluster->SequencePad[iSequence]].offset) 
	    * iADCValue;
	}
    }

  if(iNumPeaks == 1)
    {  
    
      Peak->PeakHeight = (unsigned char) ftpcadc[Peak->Sequence.StartADCEntry + Peak->Timebin - 
				Peak->Sequence.StartTimebin].data;  

      /* one-peak cluster, fit according to preference from det */
      iUseGauss=det[0].usegauss;


      /* calculate pad position first */
      if((iUseGauss & 1) == 1)
	{
	  /* get values for gaussfit */
	  iADCValue = (int) Peak->PeakHeight;
	  iADCPlus=0;
	  iADCMinus=0;
	  /* find sequences with neighbor pixels */ 
	  for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
	    {
	      if((Cluster->SequencePad[iSequence] == Peak->pad - 1) && 
		 (Cluster->Sequence[iSequence].StartTimebin <= Peak->Timebin) && 
		 (Cluster->Sequence[iSequence].StartTimebin + 
		  Cluster->Sequence[iSequence].Length > Peak->Timebin))
		{
		  iADCMinus = (unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry 
				     - Cluster->Sequence[iSequence].StartTimebin 
				     + Peak->Timebin].data;
		}
	      if((Cluster->SequencePad[iSequence] == Peak->pad + 1) && 
		 (Cluster->Sequence[iSequence].StartTimebin < Peak->Timebin) && 
		 (Cluster->Sequence[iSequence].StartTimebin + 
		  Cluster->Sequence[iSequence].Length > Peak->Timebin))
		{
		  iADCPlus = (unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry 
				     - Cluster->Sequence[iSequence].StartTimebin 
				     + Peak->Timebin].data;
		}
	    }
	  
	  /* check if Gaussfit is possible */
	  if((iADCValue == 0) || (iADCPlus == 0) || (iADCMinus == 0) || 
	     ((iADCValue <= iADCPlus) && (iADCValue <= iADCMinus)))
	    {
	      /* use weighted mean instead */
	      iUseGauss = iUseGauss & 2;
	    }
	  else
	    {
	      /* do gaussfit */
	      Peak->PadSigma = sqrt (1 / 
			       ((2 * fastlog[iADCValue]) -
				    (fastlog[iADCPlus] + fastlog[iADCMinus])));
	      Peak->PadPosition = (float) Peak->pad + 
		sqr(Peak->PadSigma) * (fastlog[iADCPlus] - fastlog[iADCMinus]);
	    }
	} 
      
      if((iUseGauss & 1) == 0)
	{
	  /* calculate pad position by weighted mean */ 
	  Peak->PadPosition = (float) PadSum / (float) ChargeSum;
	  
	  /* calculate pad direction cluster width by weighted mean */
	  Peak->PadSigma=0;
	  for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
	    {
	      for(iBin = 0; iBin < Cluster->Sequence[iSequence].Length; iBin++)
		{
		  Peak->PadSigma+=(sqr((float) Cluster->SequencePad[iSequence] 
				 - Peak->PadPosition)
			     *(unsigned char) ftpcadc[Cluster->Sequence[iSequence]
				     .StartADCEntry+iBin].data);
		}
	    }
	  Peak->PadSigma /= (float) ChargeSum;
	}

      /* now calculate the time position */
      if((iUseGauss & 2) == 1)
	{
	  /* get values for gaussfit */
	  iADCValue = (int) Peak->PeakHeight;
	  iADCPlus=0;
	  iADCMinus=0;

	  if(Peak->Timebin > Peak->Sequence.StartTimebin)
	    {
	      iADCMinus = (unsigned char) ftpcadc[Peak->Sequence.StartADCEntry + Peak->Timebin - 
			     Peak->Sequence.StartTimebin - 1].data;
	    }

	  if(Peak->Timebin + 1 < 
	     Peak->Sequence.StartTimebin + Peak->Sequence.Length)
	    {
	      iADCPlus = (unsigned char) ftpcadc[Peak->Sequence.StartADCEntry + Peak->Timebin - 
			     Peak->Sequence.StartTimebin + 1].data;
	    }
	  
	  /* check if Gaussfit will fail */
	  if((iADCValue == 0) || (iADCPlus == 0) || (iADCMinus == 0) || 
	     ((iADCValue <= iADCPlus) && (iADCValue <= iADCMinus)))
	    {
	      /* use weighted mean instead */
	      iUseGauss = 0;
	    }
	  else
	    {
	      /* do gaussfit */
	      Peak->TimeSigma = sqrt (1 / 
				((2 * fastlog[iADCValue]) -
				 (fastlog[iADCPlus] + fastlog[iADCMinus])));
	      Peak->TimePosition = (float) Peak->Timebin + 
		timeoff[iRow*det[0].n_sectors*det[0].n_pads+
		       iSec*det[0].n_pads+Peak->pad].offset +
		sqr(Peak->TimeSigma) * (fastlog[iADCPlus] - fastlog[iADCMinus]);
	    }
	}

      if((iUseGauss & 2) == 0)
	{
	  /* calculate time position by weighted mean */ 
	  Peak->TimePosition = (float) TimeSum / (float) ChargeSum;
	  
	  /* calculate pad direction cluster width by weighted mean */
	  Peak->TimeSigma=0;
	  for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
	    {
	      for(iBin = 0; iBin < Cluster->Sequence[iSequence].Length; iBin++)
		{
		  Peak->TimeSigma+=(sqr((float) 
				  Cluster->Sequence[iSequence].StartTimebin 
				  + (float) iBin - Peak->TimePosition)
			     *(unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry
                                                      +iBin].data);
		}
	    }
	  Peak->TimeSigma /= (float) ChargeSum;
	}

      /* transform from pad/time to x/y/z */
      if(!Padtrans(Peak, iRow, iSec, det, zrow, 
		   pRadius, pDeflection))
	{
#ifdef DEBUG
	  printf("Peak position can't be transformed!\n");
#endif
	}


      if(*point_nok < *point_maxlen - 1 && !isnan(Peak->x) && !isnan(Peak->y) && !isnan(Peak->PadSigma) && !isnan(Peak->TimeSigma))
	{
	  /* fill point table */
	  fppoint[*point_nok].row=iRow+1;           
	  fppoint[*point_nok].sector=iSec+1;
	  fppoint[*point_nok].n_pads=Cluster->EndPad +1 - Cluster->StartPad;
	  fppoint[*point_nok].n_bins=Peak->Sequence.Length;
	  fppoint[*point_nok].max_adc=(long int) Peak->PeakHeight;
	  fppoint[*point_nok].charge=ChargeSum;
	  fppoint[*point_nok].x=Peak->x;
	  fppoint[*point_nok].y=Peak->y;
	  fppoint[*point_nok].z=Peak->z;
	  fppoint[*point_nok].s_phi=Peak->PadSigma*det[0].rad_per_pad;
	  fppoint[*point_nok].s_r=Peak->TimeSigma*Peak->Rad/Peak->TimePosition;
	  fDriftLength = det[0].r_out - Peak->Rad;
          fPhiError = det[0].pad_err_diff[0] 
	      + fDriftLength*det[0].pad_err_diff[1] 
	      + fDriftLength*fDriftLength*det[0].pad_err_diff[2];
          fRadError = det[0].time_err_diff[0] 
	      + fDriftLength*det[0].time_err_diff[1] 
	      + fDriftLength*fDriftLength*det[0].time_err_diff[2];
	  if(fppoint[*point_nok].n_pads==2)
	    {
	      fPhiError = sqrt(fPhiError * fPhiError
			       + det[0].pad_err_2mean * det[0].pad_err_2mean);
	    }
	  if(fppoint[*point_nok].n_pads==3 && iUseGauss & 1 == 1)
	    {
	      fPhiError = sqrt(fPhiError * fPhiError
			       + det[0].pad_err_3gauss * det[0].pad_err_3gauss);
	    }
	  if(fppoint[*point_nok].n_pads==3 && iUseGauss & 1 == 0)
	    {
	      fPhiError = sqrt(fPhiError * fPhiError
			       + det[0].pad_err_3mean * det[0].pad_err_3mean);
	    }

	  fppoint[*point_nok].flags=0;

	  if(iADCValue>254)
	    {
	      fppoint[*point_nok].flags=4;
	      fPhiError = sqrt(fPhiError * fPhiError
			       + det[0].pad_err_sat * det[0].pad_err_sat);
	      fRadError = sqrt(fRadError * fRadError
			       + det[0].time_err_sat * det[0].time_err_sat);
	    }	  
	  if(Cluster->CutOff==1)
	    {
	      fppoint[*point_nok].flags=fppoint[*point_nok].flags | 16;
	      fPhiError = sqrt(fPhiError * fPhiError
			       + det[0].pad_err_cutoff * det[0].pad_err_cutoff);
	      fRadError = sqrt(fRadError * fRadError
			       + det[0].time_err_cutoff * det[0].time_err_cutoff);
	    }

	  /* transform errors to actual hit position */ 
	  PadtransBin=(int) ((Peak->TimePosition+0.5)*PadtransPerTimebin);
	  fPhiError *= Peak->Rad / det[0].r_out;
	  fRadError *= (pRadius[10*PadtransBin]-pRadius[10*PadtransBin+10])
	    / (pRadius[10]-pRadius[20]);

	  fppoint[*point_nok].x_err = 
	      sqrt(fRadError*cos(Peak->Phi)*fRadError*cos(Peak->Phi) 
		   + fPhiError*sin(Peak->Phi)*fPhiError*sin(Peak->Phi));
	  fppoint[*point_nok].y_err = 
	      sqrt(fRadError*sin(Peak->Phi)*fRadError*sin(Peak->Phi) 
		   + fPhiError*cos(Peak->Phi)*fPhiError*cos(Peak->Phi));
	  fppoint[*point_nok].z_err = det[0].z_err;

	  (*point_nok)++;
	}
      else
	{
#ifdef DEBUG
	  printf("fppoint table is full. Point not stored.\n");
#endif
	}
    } /* end of: if(iNumPeaks == 1) */
  else
    {
      /* multi-peak cluster, unfold */
      /* initialize all peaks for unfolding */
      BadFit=0;
      FailedFit=0;
      for(iPeakIndex=0; iPeakIndex < iNumPeaks; iPeakIndex++)
	{
	  Peak[iPeakIndex].TimePosition = (float) Peak[iPeakIndex].Timebin;
	  Peak[iPeakIndex].Timebin_saved = Peak[iPeakIndex].Timebin;
	  Peak[iPeakIndex].PadPosition = (float) Peak[iPeakIndex].pad;
	  Peak[iPeakIndex].pad_saved = Peak[iPeakIndex].pad;
	  Peak[iPeakIndex].OldTimePosition = 0;
	  Peak[iPeakIndex].OldPadPosition = 0;
	  Peak[iPeakIndex].OldPeakHeight = 0;
	  Peak[iPeakIndex].TimeSigma = sigmat((float) Peak[iPeakIndex].Timebin);
	  Peak[iPeakIndex].PadSigma = sigmax((float) Peak[iPeakIndex].Timebin);
	  Peak[iPeakIndex].PeakHeight = 
	    (float)(unsigned char) ftpcadc[Peak[iPeakIndex].Sequence.StartADCEntry
			   + Peak[iPeakIndex].Timebin 
			   - Peak[iPeakIndex].Sequence.StartTimebin].data;  
	}

      /* approximation loop */
      iNumUnfoldLoops=0;
      do 
      { /* beginning of: while((SumDeltaPos > 0.01... */
	SumDeltaPos=0;
	SumDeltaAmp=0;
	PeakHeightSum=0;

	/* loop over all peaks in cluster */ 
	for(iPeakIndex=0; iPeakIndex < iNumPeaks; iPeakIndex++)
	  {

	    do
	      {

		PeakShifted=0;
		/* get values for pad direction gaussfit */
		iADCValue=0;
		iADCPlus=0;
		iADCMinus=0;
		iADCTimePlus=0;
		iADCTimeMinus=0;
		/* find sequences on the right pads */ 
		for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
		  {
		    if((Cluster->SequencePad[iSequence] 
			== Peak[iPeakIndex].pad) && 
		       (Cluster->Sequence[iSequence].StartTimebin 
			<= Peak[iPeakIndex].Timebin) && 
		       (Cluster->Sequence[iSequence].StartTimebin + 
			Cluster->Sequence[iSequence].Length 
			> Peak[iPeakIndex].Timebin))
		      {
			iADCValue = (unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry 
					   - Cluster->Sequence[iSequence].StartTimebin 
					   + Peak[iPeakIndex].Timebin].data;
			/* get values for time direction gaussfit */ 
			if(Peak[iPeakIndex].Timebin > 
			   Cluster->Sequence[iSequence].StartTimebin)
			  {
			    iADCTimeMinus = (unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry 
						   + Peak[iPeakIndex].Timebin - 
						   Cluster->Sequence[iSequence].StartTimebin - 1].data;
			  }
			
			if(Peak[iPeakIndex].Timebin + 1 < 
			   Cluster->Sequence[iSequence].StartTimebin 
			   + Cluster->Sequence[iSequence].Length)
			  {
			    iADCTimePlus = (unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry 
						  + Peak[iPeakIndex].Timebin - 
						  Cluster->Sequence[iSequence].StartTimebin 
						  + 1].data;
			  }
			
		      }
		    if((Cluster->SequencePad[iSequence] 
			== Peak[iPeakIndex].pad -1) && 
		       (Cluster->Sequence[iSequence].StartTimebin 
			<= Peak[iPeakIndex].Timebin) && 
		       (Cluster->Sequence[iSequence].StartTimebin + 
			Cluster->Sequence[iSequence].Length 
			> Peak[iPeakIndex].Timebin))
		      {
			iADCMinus = (unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry 
					   - Cluster->Sequence[iSequence].StartTimebin 
					   + Peak[iPeakIndex].Timebin].data;
		      }
		    if((Cluster->SequencePad[iSequence] 
			== Peak[iPeakIndex].pad + 1) && 
		       (Cluster->Sequence[iSequence].StartTimebin 
			<= Peak[iPeakIndex].Timebin) && 
		       (Cluster->Sequence[iSequence].StartTimebin + 
			Cluster->Sequence[iSequence].Length 
			> Peak[iPeakIndex].Timebin))
		      {
			iADCPlus = (unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry 
					  - Cluster->Sequence[iSequence].StartTimebin 
					  + Peak[iPeakIndex].Timebin].data;
		      }
		  }
		
		/* calculate influence of other peaks */ 
		fDeltaADC=0;

		fDeltaADCPlus=0;
		fDeltaADCMinus=0;
		fDeltaADCTimePlus=0; 
		fDeltaADCTimeMinus=0;
		for(iInnerIndex=0; iInnerIndex < iNumPeaks; iInnerIndex++)
		  {
		    if(iInnerIndex != iPeakIndex)
		      {
			/* check if peaks are overlaid */
			if((Peak[iPeakIndex].pad == Peak[iInnerIndex].pad)
			   && (Peak[iPeakIndex].Timebin == Peak[iInnerIndex].Timebin))
			  {
			    /* one peak has been shifted on top of the other */
			    /* that kills the unfold procedure */ 
			    FailedFit=1;
			    break;
			  }
			
			fDeltaADC+=gauss_2d(Peak[iPeakIndex].pad, 
					    Peak[iPeakIndex].Timebin, 
					    Peak[iInnerIndex].PadPosition, 
					    Peak[iInnerIndex].PadSigma, 
					    Peak[iInnerIndex].TimePosition, 
					    Peak[iInnerIndex].TimeSigma, 
					    Peak[iInnerIndex].PeakHeight);
			fDeltaADCPlus+=gauss_2d(Peak[iPeakIndex].pad+1, 
						Peak[iPeakIndex].Timebin, 
						Peak[iInnerIndex].PadPosition, 
						Peak[iInnerIndex].PadSigma, 
						Peak[iInnerIndex].TimePosition, 
						Peak[iInnerIndex].TimeSigma, 
						Peak[iInnerIndex].PeakHeight);
			fDeltaADCMinus+=gauss_2d(Peak[iPeakIndex].pad-1, 
						 Peak[iPeakIndex].Timebin, 
						 Peak[iInnerIndex].PadPosition, 
						 Peak[iInnerIndex].PadSigma, 
						 Peak[iInnerIndex].TimePosition, 
						 Peak[iInnerIndex].TimeSigma, 
						 Peak[iInnerIndex].PeakHeight);
			fDeltaADCTimePlus+=gauss_2d(Peak[iPeakIndex].pad, 
						    Peak[iPeakIndex].Timebin+1, 
						    Peak[iInnerIndex].PadPosition, 
						    Peak[iInnerIndex].PadSigma, 
						    Peak[iInnerIndex].TimePosition, 
						    Peak[iInnerIndex].TimeSigma, 
						    Peak[iInnerIndex].PeakHeight);
			fDeltaADCTimeMinus+=gauss_2d(Peak[iPeakIndex].pad, 
						     Peak[iPeakIndex].Timebin-1, 
						     Peak[iInnerIndex].PadPosition, 
						     Peak[iInnerIndex].PadSigma, 
						     Peak[iInnerIndex].TimePosition, 
						     Peak[iInnerIndex].TimeSigma, 
						     Peak[iInnerIndex].PeakHeight);
		      }
		  }
		
		/* subtract influence of other peaks */
		iADCValue-=(int) (fDeltaADC+0.5);
		iADCPlus-=(int) (fDeltaADCPlus+0.5);
		iADCMinus-=(int) (fDeltaADCMinus+0.5);
		iADCTimePlus-=(int) (fDeltaADCTimePlus+0.5);
		iADCTimeMinus-=(int) (fDeltaADCTimeMinus+0.5);
		
		/* check if Gaussfit is possible */
		
		/* introduce minimal gradient if none is there */ 
		if(iADCValue == iADCPlus)
		  {
		    iADCPlus--;
		  }
		if(iADCValue == iADCMinus)
		  {
		    iADCMinus--;
		  }
		if(iADCValue == iADCTimePlus)
		  {
		    iADCTimePlus--;
		  }
		if(iADCValue == iADCTimeMinus)
		  {
		    iADCTimeMinus--;
		  }
		
		
		/* set all 0s to 1 to keep gauss from crashing */
                /* and iADCValue=2 if iADCValue <= 1 (09/28/98) */
		if(iADCValue <= 1)
		  {
		    iADCValue=2;
		    BadFit=1;
		  }
		if(iADCPlus <= 0)
		  {
		    iADCPlus=1;
		    BadFit=1;
		  }
		if(iADCMinus <= 0)
		  {
		    iADCMinus=1;
		    BadFit=1;
		  }
		if(iADCTimePlus <= 0)
		  {
		    iADCTimePlus=1;
		    BadFit=1;
		  }
		if(iADCTimeMinus <= 0)
		  {
		    iADCTimeMinus=1;
		    BadFit=1;
		  }
		
/*    move here to protect against iADCValue=iADCPlus=iADCMinus which
      causes Peak[..].PadSigma=Inf            09.30.98    HH,JCS   */
		if(FailedFit == 1)
		  {
		    break;
		  }
		
		/* shift if peak has moved after correction */ 
		if(iADCValue < iADCPlus)
		  {
		    Peak[iPeakIndex].pad++;
		    PeakShifted=1;
		  }
		if(iADCValue < iADCMinus && PeakShifted==0)
		  {
		    Peak[iPeakIndex].pad--;
		    PeakShifted=1;
		  }
		if(iADCValue < iADCTimePlus && PeakShifted==0)
		  {
		    Peak[iPeakIndex].Timebin++;
		    PeakShifted=1;
		  }
		if(iADCValue < iADCTimeMinus && PeakShifted==0)
		  {
		    Peak[iPeakIndex].Timebin--;
		    PeakShifted=1;
		  }
	      }
	    while(PeakShifted>0 && PeakShifted<5);
	    
	    /* recalculate peak position */
	    NewPeakHeight=iADCValue;
	    Peak[iPeakIndex].PadSigma = sqrt (1 / 
					      ((2 * fastlog[iADCValue]) -
					       (fastlog[iADCPlus] + 
						fastlog[iADCMinus])));
	    NewPadPosition = (float) Peak[iPeakIndex].pad + 
	      sqr(Peak[iPeakIndex].PadSigma) * 
	      (fastlog[iADCPlus] - fastlog[iADCMinus]);
	    Peak[iPeakIndex].TimeSigma = sqrt (1 / 
					       ((2 * fastlog[iADCValue]) -
						(fastlog[iADCTimePlus] + 
						 fastlog[iADCTimeMinus])));
	    NewTimePosition = (float) Peak[iPeakIndex].Timebin + 
	      timeoff[iRow*det[0].n_sectors*det[0].n_pads+
		     iSec*det[0].n_pads+Peak[iPeakIndex].pad].offset +
	      sqr(Peak[iPeakIndex].TimeSigma) * 
	      (fastlog[iADCTimePlus] - fastlog[iADCTimeMinus]);
	    
	    /* introduce inertia to break infinite loops */ 
 	    if(iNumUnfoldLoops > MAXFASTLOOPS)
 	      { 
		NewPeakHeight=(NewPeakHeight+Peak[iPeakIndex].PeakHeight)/2;
		NewPadPosition=(NewPadPosition+Peak[iPeakIndex].PadPosition)/2;
		NewTimePosition=(NewTimePosition+Peak[iPeakIndex].TimePosition)/2;
 	      } 

	    SumDeltaAmp+=fabs(NewPeakHeight - Peak[iPeakIndex].PeakHeight);
	    Peak[iPeakIndex].OldPeakHeight=Peak[iPeakIndex].PeakHeight;
	    Peak[iPeakIndex].PeakHeight=NewPeakHeight;
	    SumDeltaPos+=fabs(NewPadPosition - Peak[iPeakIndex].PadPosition);
	    Peak[iPeakIndex].OldPadPosition=Peak[iPeakIndex].PadPosition;
	    Peak[iPeakIndex].PadPosition=NewPadPosition;
	    SumDeltaPos+=fabs(NewTimePosition - Peak[iPeakIndex].TimePosition);
	    Peak[iPeakIndex].OldTimePosition=Peak[iPeakIndex].TimePosition;
	    Peak[iPeakIndex].TimePosition=NewTimePosition;
	    
	    PeakHeightSum+=NewPeakHeight;

	  } /* end of: for(iPeakIndex=0;... */ 
	iNumUnfoldLoops++;
      }
      while((SumDeltaPos > UNFOLDLIMIT || SumDeltaAmp > 1) 
	    && (iNumUnfoldLoops < MAXLOOPS || SumDeltaPos > UNFOLDFAILEDLIMIT)
	    && iNumUnfoldLoops < MAXLOOPS+1 && FailedFit == 0);
      /* try unfold loop once more if error > failedlimit */
      
      if(SumDeltaPos > UNFOLDFAILEDLIMIT || FailedFit == 1)
	{
	  /* unfolding has failed, use original peak positions */ 

	  for(iPeakIndex=0; iPeakIndex < iNumPeaks; iPeakIndex++)
	    {
	      Peak[iPeakIndex].TimePosition = (float) Peak[iPeakIndex].Timebin_saved;
	      Peak[iPeakIndex].Timebin = Peak[iPeakIndex].Timebin_saved;
	      Peak[iPeakIndex].PadPosition = (float) Peak[iPeakIndex].pad_saved;
	      Peak[iPeakIndex].pad = Peak[iPeakIndex].pad_saved;
		for(iSequence = 0; iSequence < Cluster->NumSequences; iSequence++)
		  {
		    if((Cluster->SequencePad[iSequence] 
			== Peak[iPeakIndex].pad) && 
		       (Cluster->Sequence[iSequence].StartTimebin 
			<= Peak[iPeakIndex].Timebin) && 
		       (Cluster->Sequence[iSequence].StartTimebin + 
			Cluster->Sequence[iSequence].Length 
			> Peak[iPeakIndex].Timebin))
		      {
			Peak[iPeakIndex].PeakHeight = 
			  (unsigned char) ftpcadc[Cluster->Sequence[iSequence].StartADCEntry 
				 - Cluster->Sequence[iSequence].StartTimebin 
				 + Peak[iPeakIndex].Timebin].data;
		      }
		  }
	    }
#ifdef DEBUG
	  printf("unfold failed!\n");
#endif
	  
	} /* end of: if(SumDeltaPos > UNFOLDFAILEDLIMIT ... */

      /* write unfolded peaks to point table */
      for(iPeakIndex=0; iPeakIndex < iNumPeaks; iPeakIndex++)
	{
	  /* transform from pad/time to x/y/z */
	  if(!Padtrans(&(Peak[iPeakIndex]), iRow, iSec, det, zrow, 
		       pRadius, pDeflection))
	    {
#ifdef DEBUG
	      printf("Peak position can't be transformed!\n");
#endif
	    }
	  
	  /* in very complicated clusters some hits may have been unfolded
	     with errors while the rest of the cluster is okay, don't fill 
	     these hits into tables: */
	  if(*point_nok < *point_maxlen - 1 && !isnan(Peak[iPeakIndex].x) && !isnan(Peak[iPeakIndex].y) && !isnan(Peak[iPeakIndex].PadSigma) && !isnan(Peak[iPeakIndex].TimeSigma))
	    {
	      /* fill point table */
	      fppoint[*point_nok].row=iRow+1;           
	      fppoint[*point_nok].sector=iSec+1;
	      fppoint[*point_nok].n_pads=Cluster->EndPad +1 - Cluster->StartPad;
	      fppoint[*point_nok].n_bins=Peak[iPeakIndex].Sequence.Length;
	      fppoint[*point_nok].max_adc=(long int) Peak[iPeakIndex].PeakHeight;
	      fppoint[*point_nok].charge
		=(long int)(ChargeSum*Peak[iPeakIndex].PeakHeight/PeakHeightSum);
	      fppoint[*point_nok].x=Peak[iPeakIndex].x;
	      fppoint[*point_nok].y=Peak[iPeakIndex].y;
	      fppoint[*point_nok].z=Peak[iPeakIndex].z;
	      fppoint[*point_nok].s_phi=Peak[iPeakIndex].PadSigma*det[0].rad_per_pad;
	      fppoint[*point_nok].s_r=Peak[iPeakIndex].TimeSigma
		* Peak[iPeakIndex].Rad/Peak[iPeakIndex].TimePosition;

	      fDriftLength = det[0].r_out - Peak[iPeakIndex].Rad;
	      fPhiError = det[0].pad_err_diff[0] 
		  + fDriftLength*det[0].pad_err_diff[1] 
		  + fDriftLength*fDriftLength*det[0].pad_err_diff[2];
	      fRadError = det[0].time_err_diff[0] 
		  + fDriftLength*det[0].time_err_diff[1] 
		  + fDriftLength*fDriftLength*det[0].time_err_diff[2];
	      
	      /* clusters are unfolded */ 
	      fppoint[*point_nok].flags=1;
	      fPhiError = sqrt(fPhiError * fPhiError
			       + det[0].pad_err_unfold * det[0].pad_err_unfold);
	      fRadError = sqrt(fRadError * fRadError
			       + det[0].time_err_unfold * det[0].time_err_unfold);
	      
	      if(iADCValue>254)
		{
		  fppoint[*point_nok].flags=5;
		  fPhiError = sqrt(fPhiError * fPhiError
				   + det[0].pad_err_sat * det[0].pad_err_sat);
		  fRadError = sqrt(fRadError * fRadError
				   + det[0].time_err_sat * det[0].time_err_sat);
		}	  
	      if(BadFit==1)
		{
		  fppoint[*point_nok].flags=fppoint[*point_nok].flags | 8;
		  fPhiError = sqrt(fPhiError * fPhiError
				   + det[0].pad_err_bad * det[0].pad_err_bad);
		  fRadError = sqrt(fRadError * fRadError
				   + det[0].time_err_bad * det[0].time_err_bad);
		}	  
	      if(iNumUnfoldLoops == MAXLOOPS)
		{
		  fppoint[*point_nok].flags=fppoint[*point_nok].flags | 10;
		  fPhiError = sqrt(fPhiError * fPhiError
				   + det[0].pad_err_failed * det[0].pad_err_failed);
		  fRadError = sqrt(fRadError * fRadError
				   + det[0].time_err_failed * det[0].time_err_failed);
		}	  
	      if(Cluster->CutOff==1)
		{
		  fppoint[*point_nok].flags=fppoint[*point_nok].flags | 16;
		  fPhiError = sqrt(fPhiError * fPhiError
				   + det[0].pad_err_cutoff * det[0].pad_err_cutoff);
		  fRadError = sqrt(fRadError * fRadError
				   + det[0].time_err_cutoff * det[0].time_err_cutoff);
		}
	      
	      /* transform errors to actual hit position */ 
	      PadtransBin=(int) ((Peak->TimePosition+0.5)*PadtransPerTimebin);
	      fPhiError *= Peak->Rad / det[0].r_out;
	      fRadError *= (pRadius[10*PadtransBin]-pRadius[10*PadtransBin+10])
		/ (pRadius[10]-pRadius[20]);

	      fppoint[*point_nok].x_err = 
		sqrt(fRadError*cos(Peak[iPeakIndex].Phi)
		     *fRadError*cos(Peak[iPeakIndex].Phi) 
		     + fPhiError*sin(Peak[iPeakIndex].Phi)
		     *fPhiError*sin(Peak[iPeakIndex].Phi));
	      fppoint[*point_nok].y_err = 
		sqrt(fRadError*sin(Peak[iPeakIndex].Phi)
		     *fRadError*sin(Peak[iPeakIndex].Phi) 
		     + fPhiError*cos(Peak[iPeakIndex].Phi)
		     *fPhiError*cos(Peak[iPeakIndex].Phi));
	      fppoint[*point_nok].z_err = det[0].z_err;
	      
	      (*point_nok)++;
	    }
	} /* end of: for(iPeakIndex=0;... */
    } /*end of: if(iNumPeaks==1) ... else { */
  return TRUE;
}

int StFtpcClusterFinder::Padtrans(TPeak *Peak, 
	     int iRow, 
	     int iSec, 
	     FCL_DET_ST *det, 
	     FCL_ZROW_ST *zrow, 
	     double *pRadius, 
	     double *pDeflection)
{  
  int PadtransPerTimebin;
  int PadtransLower;
  float PhiDeflect, TimeCoordinate;

  /* preparatory calculations */
  TimeCoordinate = Peak->TimePosition + 0.5; /*time start at beginning of bin 0*/
  PadtransPerTimebin = (int) det[0].n_int_steps / det[0].n_bins;
  PadtransLower= (int) (TimeCoordinate*PadtransPerTimebin);

  /* linear interpolation in radius table */
  Peak->Rad = ((pRadius[iRow + 10 * PadtransLower] * 
	  ((float) (PadtransLower+1) / (float) PadtransPerTimebin - 
	   TimeCoordinate) + pRadius[iRow + 10 * (PadtransLower+1)] * 
	  (TimeCoordinate - (float) PadtransLower / (float) PadtransPerTimebin)) / 
	 (((float) (PadtransLower + 1) / (float) PadtransPerTimebin - 
	   (float) PadtransLower / (float) PadtransPerTimebin)));

  /* linear interpolation in deflection table */
  PhiDeflect = det[0].magfld * ((pDeflection[iRow + 10 * PadtransLower] * 
				 ((float) (PadtransLower + 1.0) / 
				  (float) PadtransPerTimebin - TimeCoordinate) + 
				 pDeflection[iRow + 10 * (PadtransLower + 1)] *
				 (TimeCoordinate - (float) PadtransLower / 
				  (float) PadtransPerTimebin)) /
				(((float) (PadtransLower + 1.0) / 
				  (float) PadtransPerTimebin - 
				  (float) PadtransLower / 
				  (float) PadtransPerTimebin)));
			      
  /* calculate phi angle from pad position */
  Peak->Phi = det[0].rad_per_gap / 2 + 
    (Peak->PadPosition + 0.5) * det[0].rad_per_pad -
    PhiDeflect + iSec * (det[0].n_pads * det[0].rad_per_pad + 
			 det[0].rad_per_gap)+C_PI_2;


  /* transform to cartesian */
  Peak->x = Peak->Rad*cos(Peak->Phi);
  Peak->y = Peak->Rad*sin(Peak->Phi);
  Peak->z = zrow[iRow].z;
  return TRUE;
}

float StFtpcClusterFinder::gauss_2d(int x, int y, float x1, float sigma_x1, float y1, float sigma_y1, float amp1)
{
  float result;
  float g1, g2;
  
  if(sigma_x1==0 || sigma_y1==0)
    {
      return 0.0;
    }

  g1=exp(-sqr((float)x-x1)/(2*sqr(sigma_x1)));
  g2=exp(-sqr((float)y-y1)/(2*sqr(sigma_y1)));
  
  result=amp1*g1*g2;
  
  return result;
}

float StFtpcClusterFinder::sigmax(float timebin)
{
  return 0.9;
}

float StFtpcClusterFinder::sigmat(float timebin)
{
  return 0.6;
}

int StFtpcClusterFinder::calcpadtrans(FCL_DET_ST *det, 
		 FCL_PADTRANS_ST *padtrans, 
		 double *pradius, 
		 double *pdeflection)
{
  int i, j, v_buf, padrow;
  double t_last, t_next, r_last, r_next, e_now, v_now, psi_now;
  double step_size, deltap;
  
  step_size=((float) det[0].n_bins/ (float) det[0].n_int_steps);
  deltap=det[0].p_normalized-det[0].p_standard;
  
#ifdef DEBUG
  printf("integrating padtrans table...\n");
#endif

  for (padrow=0; padrow<10; padrow++)
    {
      /* determine starting values */
      t_last=0;
      v_buf=0;
      r_last=det[0].r_out;
      pradius[padrow]=det[0].r_out;
      pdeflection[padrow]=0;
      e_now = det[0].rad_times_field / (0.5*r_last);
      for(j=v_buf; padtrans[j].e < e_now && j<det[0].n_magboltz_bins; j++);
      if(j<1 || j>=det[0].n_magboltz_bins)
	{
	  printf("Error 1: j=%d, v_buf=%d e_drift=%f, e_now=%f\n", j, v_buf, padtrans[j].e, e_now);
	  return FALSE;
	}
      v_buf=j-1;
      v_now=((padtrans[v_buf].v[padrow]+deltap*padtrans[v_buf].dv_dp[padrow])*(padtrans[j].e-e_now)+(padtrans[j].v[padrow]+deltap*padtrans[j].dv_dp[padrow])*(e_now-padtrans[v_buf].e))/(padtrans[j].e-padtrans[v_buf].e);
      psi_now=((padtrans[v_buf].psi[padrow]+deltap*padtrans[v_buf].dpsi_dp[padrow])*(padtrans[j].e-e_now)+(padtrans[j].psi[padrow]+deltap*padtrans[j].dpsi_dp[padrow])*(e_now-padtrans[v_buf].e))/(padtrans[j].e-padtrans[v_buf].e);
      for (i=0; i<det[0].n_int_steps && e_now < padtrans[det[0].n_magboltz_bins-2].e; i++) 
	{
	  t_next = t_last + step_size;
	  /* first guess for r_next: */
	  r_next = r_last - v_now * step_size * det[0].timebin_size;
	  e_now = det[0].rad_times_field / (0.5*(r_last+r_next));
	  
	  for(j=v_buf; padtrans[j].e < e_now && j<det[0].n_magboltz_bins; j++);
	  
	  if(j<1 || j>=det[0].n_magboltz_bins)
	    {
	      printf("Error 2: j=%d, v_buf=%d e_drift=%f, e_now=%f\n", j, v_buf, padtrans[j].e, e_now);
	      return FALSE;
	    }
	  
	  v_buf=j-1;
	  v_now=((padtrans[v_buf].v[padrow]+deltap*padtrans[v_buf].dv_dp[padrow])*(padtrans[j].e-e_now)+(padtrans[j].v[padrow]+deltap*padtrans[j].dv_dp[padrow])*(e_now-padtrans[v_buf].e))/(padtrans[j].e-padtrans[v_buf].e);
	  psi_now=((padtrans[v_buf].psi[padrow]+deltap*padtrans[v_buf].dpsi_dp[padrow])*(padtrans[j].e-e_now)+(padtrans[j].psi[padrow]+deltap*padtrans[j].dpsi_dp[padrow])*(e_now-padtrans[v_buf].e))/(padtrans[j].e-padtrans[v_buf].e);
	  
	  /* correct r_next: */
	  r_next = r_last - v_now * step_size *det[0].timebin_size;
	  pradius[padrow+10*(i+1)]=r_next;
	  pdeflection[padrow+10*(i+1)]=pdeflection[padrow+10*i]+((r_last-r_next)*tan(det[0].deg_to_rad * psi_now)/r_last);
	  t_last=t_next;
	  r_last=r_next;
	}
#ifdef DEBUG
      printf("%d steps calculated, padrow %d\n", i, padrow);
#endif
      
    }
  return TRUE;
}

int StFtpcClusterFinder::CUCInit(TClusterUC memory[MAXNUMCUC], int RealMemory[MAXNUMCUC], int *pointer)
{
  int i;
  
  for(i=0; i<MAXNUMCUC; i++)
    {
      RealMemory[i]=i;
    }
  *pointer = -1;
  return TRUE;
}

TClusterUC *StFtpcClusterFinder::CUCAlloc(TClusterUC memory[MAXNUMCUC], int RealMemory[MAXNUMCUC], int *pointer)
{

  if((*pointer)+1 < MAXNUMCUC)
    {
      (*pointer)++;
      memory[RealMemory[*pointer]].MemoryPtr = RealMemory[*pointer];
      return &memory[RealMemory[*pointer]];
    }
  else
    {
#ifdef DEBUG
      printf("CUC memory exhausted! requested %d CUCs\n", *pointer);
#endif
      return NULL;
    }
}

int StFtpcClusterFinder::CUCFree(TClusterUC memory[MAXNUMCUC], int RealMemory[MAXNUMCUC], int *pointer, TClusterUC *OldCUC)
{
  if(*pointer >= 0)
    {
      RealMemory[*pointer] = OldCUC->MemoryPtr;
      (*pointer)--;
      return TRUE;
    }
  else
    {
#ifdef DEBUG
      printf("CUC memory management confused!\n");
#endif
      return FALSE;
    }
}
