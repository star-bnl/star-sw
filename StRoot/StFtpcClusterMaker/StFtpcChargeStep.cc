// $Id: StFtpcChargeStep.cc,v 1.1 2000/11/14 13:07:49 hummler Exp $
//
// $Log: StFtpcChargeStep.cc,v $
// Revision 1.1  2000/11/14 13:07:49  hummler
// add charge step calculation, minor cleanup
//

#include <iostream.h>
#include <stdlib.h>
#include "StFtpcChargeStep.hh"

StFtpcChargeStep::StFtpcChargeStep(TH2F *histo,
				   StFTPCReader *reader,  
				   StFtpcParamReader *paramReader)
{
  //   cout << "StFtpcChargeStep constructed" << endl;  
  mHisto=histo;
  mClear=0;
  mReader = reader;
  mParam = paramReader; 
}

StFtpcChargeStep::StFtpcChargeStep(StFTPCReader *reader,  
				   StFtpcParamReader *paramReader)
{
  //   cout << "StFtpcChargeStep constructed" << endl;
  mHisto= new TH2F("chargestepinternal"	,"FTPC charge steps by sector"	,60,-0.5,59.5, 260, -0.5, 259.5);
  mClear=1;
  mReader = reader;
  mParam = paramReader; 
}

StFtpcChargeStep::~StFtpcChargeStep()
{
//   cout << "StFtpcChargeStep destructed" << endl;
  if(mClear==1)
    delete mHisto;
}

int StFtpcChargeStep::histogram(int setPressure)
  // if !setPressure only the evaluation histogram will be filled,
  // but the normalized pressure will not be changed
{
  printf("in histogram!\n");
  /* is magboltz database loaded, if not exit */
  if(mParam->numberOfPadtransBins()<1)
    {
      printf("Couldn't find magboltz data table, exiting!\n");
      return 0;
    }

   /* allocate memory for padtrans table */
   pRadius = (double *)malloc(mParam->numberOfDriftSteps()*sizeof(double));

   if(pRadius == NULL)
     {
       printf("Padtrans memory allocation failed, exiting!\n");
       return 0;
     }

   /* integrate padtrans table from magboltz database */
   if(!calcpadtrans(pRadius))
     {
       printf("Couldn't calculate padtrans table, exiting!\n");
       return 0;
     }

  int iRow;
  for(iRow=mParam->firstPadrowToSearch()-1; 
      iRow<mParam->lastPadrowToSearch(); iRow++)
    {
      int iSec;
      for(iSec=mParam->firstSectorToSearch()-1; 
	  iSec<mParam->lastSectorToSearch(); iSec++)
	{
#ifdef DEBUG
	  printf("Now on Sector %d, Row %d\n", iSec, iRow);
#endif
	  
	  // calculate hardware (daq) sectors from software position
	  int iHardSec = mParam->numberOfSectors()*(int)(iRow/2) + iSec + 1;
	  int iHardRow = iRow%2 + 1;

	  // get list of occupied pads in sector
	  unsigned char *(padlist[2]);
	  int iOccPads=mReader->getPadList(iHardSec, iHardRow, 
					  padlist[iHardRow-1]);
	  
	  // loop over occupied pads
	  int iThPad;
	  for(iThPad=0; iThPad<iOccPads; iThPad++)
	    {
	      int iPad=padlist[iHardRow-1][iThPad];
	      int iSeqNumber;
	      TPCSequence *(SequencePointer[3]);
	      // get sequences on this pad
	      mReader->getSequences(iHardSec, iHardRow, iPad, &iSeqNumber,
				   SequencePointer[iHardRow]);
	      TPCSequence *sequences=SequencePointer[iHardRow];
	      
	      // loop over sequences
	      int iSeqIndex;
	      for(iSeqIndex=0; iSeqIndex < iSeqNumber; iSeqIndex++)
		{
		  int entry;
		  for(entry=0; entry<sequences[iSeqIndex].Length; entry++)
		    {
		      mHisto->Fill(iHardSec-1, // sector
				   entry+sequences[iSeqIndex].startTimeBin, //bin
				   sequences[iSeqIndex].FirstAdc[entry]); // weight
		    }
		}
	    }
	}
    }

  // get projection
  TH1D *proHisto= mHisto->ProjectionY();
  int i,j;
  int imax=proHisto->GetNbinsX();
  for(i=0; i<imax; i++)
    {  
      float temp=proHisto->GetBinContent(i);
//       cout << "bin " << i << " content " << temp << endl;
    }

  // find charge step roughly
  float average=0;
  for(i=0; i<50; i++)
    {  
      average+=proHisto->GetBinContent(i);
    }
  average/=50;
  int trigger=1, peak=0;
  float peakheight=0.0;
  for(i=imax-21; trigger==1 && i>0; i--)
    {
      trigger=0;
      for(j=i; j<i+20; j++)
	{
	  float temp=proHisto->GetBinContent(j);
	  if(temp<average)
	    trigger=1;
	  if(temp>peakheight)
	    {
	      peak=j;
	      peakheight=temp;
	    }
	}
    }

  // gauss fit beginning of charge step dropoff
  float peakplus1=proHisto->GetBinContent(peak+1);
  float peakplus2=proHisto->GetBinContent(peak+2);
  float lnAmps=log(peakplus1*peakplus1/(peakheight*peakplus2));
  if(lnAmps<=0)
    lnAmps=1000000;
  float sigmaSqrDropoff=1/lnAmps;
  float chargestep = peak+0.5-sigmaSqrDropoff*log(peakheight/peakplus1);

  float *dCharge = new float[imax];
  int maxdrop=0;
  dCharge[maxdrop]=0.0;
  for(i=peak+1; i<imax; i++)
    {
      dCharge[i]=proHisto->GetBinContent(i-1)-proHisto->GetBinContent(i);
      if(dCharge[i]>dCharge[maxdrop])
	maxdrop=i;
    }
  float chargestep2=(float) maxdrop;
  float lnChange=log(dCharge[maxdrop]*dCharge[maxdrop]/(dCharge[maxdrop-1]*dCharge[maxdrop+1]));
  if(lnChange<=0)
    lnChange=1000000;
  float sigmaSqrChange = 1/lnChange;
  if(dCharge[maxdrop+1]>0)
    chargestep2 = maxdrop-sigmaSqrChange*log(dCharge[maxdrop-1]/dCharge[maxdrop+1])-0.5;  

  float TimeCoordinate=chargestep2+0.5;// 0 is at beginning of 1st timebin
  int PadtransPerTimebin = (int) mParam->numberOfDriftSteps() 
    / mParam->numberOfTimebins();
  
  /* linear interpolation in radius table */
  for(i=0; pRadius[i]>mParam->sensitiveVolumeInnerRadius(); i++);
  float aimTime=(i*(pRadius[i-1]-mParam->sensitiveVolumeInnerRadius())
		 +(i-1)*(mParam->sensitiveVolumeInnerRadius()-pRadius[i]))
    /(pRadius[i-1]-pRadius[i]);
  aimTime/=PadtransPerTimebin;
  float newPressure=
    mParam->normalizedNowPressure()
    +((aimTime/TimeCoordinate-1)/
     (mParam->padtransdVDriftdP(mParam->numberOfPadtransBins()/2, 0)
      /mParam->padtransVDrift(mParam->numberOfPadtransBins()/2, 0)));
  
  if(setPressure)
    {
      mParam->setNormalizedNowPressure(newPressure);
      
      // reiterate time calculation to get better precision
      calcpadtrans(pRadius);
      /* linear interpolation in radius table */
      for(i=0; pRadius[i]>mParam->sensitiveVolumeInnerRadius(); i++);
      aimTime=(i*(pRadius[i-1]-mParam->sensitiveVolumeInnerRadius())
	       +(i-1)*(mParam->sensitiveVolumeInnerRadius()-pRadius[i]))
	/(pRadius[i-1]-pRadius[i]);
      aimTime/=PadtransPerTimebin;
      newPressure=
	mParam->normalizedNowPressure()
	+((aimTime/TimeCoordinate-1)/
	  (mParam->padtransdVDriftdP(mParam->numberOfPadtransBins()/2, 0)
	   /mParam->padtransVDrift(mParam->numberOfPadtransBins()/2, 0)));
      mParam->setNormalizedNowPressure(newPressure);

      // reiterate again to get even better precision (error<10E^-4)
      calcpadtrans(pRadius);
      /* linear interpolation in radius table */
      for(i=0; pRadius[i]>mParam->sensitiveVolumeInnerRadius(); i++);
      aimTime=(i*(pRadius[i-1]-mParam->sensitiveVolumeInnerRadius())
	       +(i-1)*(mParam->sensitiveVolumeInnerRadius()-pRadius[i]))
	/(pRadius[i-1]-pRadius[i]);
      aimTime/=PadtransPerTimebin;
      newPressure=
	mParam->normalizedNowPressure()
	+((aimTime/TimeCoordinate-1)/
	  (mParam->padtransdVDriftdP(mParam->numberOfPadtransBins()/2, 0)
	   /mParam->padtransVDrift(mParam->numberOfPadtransBins()/2, 0)));
      mParam->setNormalizedNowPressure(newPressure);
      cout << "StFtpcChargeStep set normalized pressure to " << newPressure << endl;
    }      
      
  delete dCharge;
  free(pRadius);
  return 1;
}


int StFtpcChargeStep::calcpadtrans(double *pRadius)
{
  int i, j, v_buf, padrow;
  double t_last, t_next, r_last, r_next, e_now, v_now, psi_now;
  double step_size, deltap;
  
  step_size=((float) mParam->numberOfTimebins()
	     / (float) mParam->numberOfDriftSteps());
  deltap=mParam->normalizedNowPressure()-mParam->standardPressure();
  
#ifdef DEBUG
  printf("integrating padtrans table...\n");
#endif

  for (padrow=0; padrow<1; padrow++)
    {
      /* determine starting values */
      t_last=0;
      v_buf=0;
      r_last=mParam->sensitiveVolumeOuterRadius();
      pRadius[padrow]=mParam->sensitiveVolumeOuterRadius();
      e_now = mParam->radiusTimesField() / (0.5*r_last);
      for(j=v_buf; j<mParam->numberOfPadtransBins() 
	    && mParam->padtransEField(j) < e_now; j++);
      if(j<1 || j>=mParam->numberOfPadtransBins())
	{
	  printf("Error 1: j=%d, v_buf=%d e_drift=%f, e_now=%f\n", 
		 j, v_buf, mParam->padtransEField(j), e_now);
	  return FALSE;
	}
      v_buf=j-1;
      v_now=((mParam->padtransVDrift(v_buf, padrow)
	      +deltap*mParam->padtransdVDriftdP(v_buf, padrow))
	     *(mParam->padtransEField(j)-e_now)
	     +(mParam->padtransVDrift(j, padrow)
	       +deltap*mParam->padtransdVDriftdP(j, padrow))
	     *(e_now-mParam->padtransEField(v_buf)))
	/(mParam->padtransEField(j)-mParam->padtransEField(v_buf));
      psi_now=((mParam->padtransDeflection(v_buf,padrow)
		+deltap*mParam->padtransdDeflectiondP(v_buf,padrow))
	       *(mParam->padtransEField(j)-e_now)
	       +(mParam->padtransDeflection(j,padrow)
		 +deltap*mParam->padtransdDeflectiondP(j,padrow))
	       *(e_now-mParam->padtransEField(v_buf)))
	/(mParam->padtransEField(j)-mParam->padtransEField(v_buf));
      for (i=0; i<mParam->numberOfDriftSteps() 
	     && e_now < mParam->padtransEField(mParam->numberOfPadtransBins()-2)
	     ; i++) 
	{
	  t_next = t_last + step_size;
	  /* first guess for r_next: */
	  r_next = r_last - v_now * step_size * mParam->microsecondsPerTimebin();
	  e_now = mParam->radiusTimesField() / (0.5*(r_last+r_next));
	  
	  for(j=v_buf; mParam->padtransEField(j) < e_now 
		       && j<mParam->numberOfPadtransBins(); j++);
	  
	  if(j<1 || j>=mParam->numberOfPadtransBins())
	    {
	      printf("Error 2: j=%d, v_buf=%d e_drift=%f, e_now=%f\n", 
		     j, v_buf, mParam->padtransEField(j), e_now);
	      return FALSE;
	    }
	  
	  v_buf=j-1;
	  v_now=((mParam->padtransVDrift(v_buf, padrow)
		  +deltap*mParam->padtransdVDriftdP(v_buf, padrow))
		 *(mParam->padtransEField(j)-e_now)
		 +(mParam->padtransVDrift(j, padrow)
		   +deltap*mParam->padtransdVDriftdP(j, padrow))
		 *(e_now-mParam->padtransEField(v_buf)))
	  /(mParam->padtransEField(j)-mParam->padtransEField(v_buf));
	  psi_now=((mParam->padtransDeflection(v_buf,padrow)
		    +deltap*mParam->padtransdDeflectiondP(v_buf,padrow))
		   *(mParam->padtransEField(j)-e_now)
		   +(mParam->padtransDeflection(j,padrow)
		     +deltap*mParam->padtransdDeflectiondP(j,padrow))
		   *(e_now-mParam->padtransEField(v_buf)))
	  /(mParam->padtransEField(j)-mParam->padtransEField(v_buf));
	  
	  /* correct r_next: */
	  r_next = r_last - v_now * step_size *mParam->microsecondsPerTimebin();
	  pRadius[padrow+(i+1)]=r_next;
	  t_last=t_next;
	  r_last=r_next;
	}
#ifdef DEBUG
      printf("%d steps calculated, padrow %d\n", i, padrow);
#endif
      
    }

  return TRUE;
}
