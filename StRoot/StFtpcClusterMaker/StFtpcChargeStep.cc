// $Id: StFtpcChargeStep.cc,v 1.15 2002/04/05 16:45:49 oldi Exp $
//
// $Log: StFtpcChargeStep.cc,v $
// Revision 1.15  2002/04/05 16:45:49  oldi
// Small code clean ups, to be sure that this part is recompiled. It relies
// on StFtpcTracker/StFtpcPoint.* which were changed.
//
// Revision 1.14  2001/11/21 12:50:02  jcs
// replace malloc/free with new/delete; prevent overstepping table boundaries
//
// Revision 1.13  2001/10/12 14:33:09  jcs
// create and fill charge step histograms for FTPC East and West
//
// Revision 1.12  2001/08/20 00:35:59  jcs
// check index (j) before using it
//
// Revision 1.11  2001/07/12 10:30:47  jcs
// change computing of average to use bins 50-99 instead of bins 0-49
//
// Revision 1.10  2001/04/24 13:12:54  oldi
// Typo fixed.
//
// Revision 1.9  2001/04/23 19:19:04  oldi
// This class had a problem if the decline of the edge of histogram was too steep!
// Therefore dCharge[peak] was set to 0.000001. This is mathematically incorrect,
// but solves the most cases with problems.
// Also this class gave wrong results for low density events. As a result the
// normalizedNowPressure is set only, if there are enough charge entries accumulated.
//
// Revision 1.8  2001/04/19 12:18:57  jcs
// change pRadius to pradius to remove warning on Sun
//
// Revision 1.7  2001/04/02 12:10:08  jcs
// get FTPC calibrations,geometry from MySQL database and code parameters
// from StarDb/ftpc
//
// Revision 1.6  2001/03/19 15:52:47  jcs
// use ftpcDimensions from database
//
// Revision 1.5  2001/03/06 23:33:27  jcs
// use database instead of params
//
// Revision 1.4  2001/01/25 15:25:09  oldi
// Fix of several bugs which caused memory leaks:
//  - Some arrays were not allocated and/or deleted properly.
//  - TClonesArray seems to have a problem (it could be that I used it in a
//    wrong way in StFtpcTrackMaker form where Holm cut and pasted it).
//    I changed all occurences to TObjArray which makes the program slightly
//    slower but much more save (in terms of memory usage).
//
// Revision 1.3  2000/11/27 14:08:54  hummler
// implement tzero and lorentz angle correction factor
//
// Revision 1.2  2000/11/14 14:01:39  hummler
// cleanup: comment out alternative charge step calculations, can be uncommented
// again if they turn out to be better
//
// Revision 1.1  2000/11/14 13:07:49  hummler
// add charge step calculation, minor cleanup
//

#include <iostream.h>
#include <stdlib.h>
#include "StMessMgr.h"
#include "StFtpcChargeStep.hh"

StFtpcChargeStep::StFtpcChargeStep(TH2F *histo,
                                   TH1F *histoW,
                                   TH1F *histoE,
				   StFTPCReader *reader,  
				   StFtpcParamReader *paramReader,
                                   StFtpcDbReader *dbReader)
{
  //  cout << "StFtpcChargeStep constructed" << endl;  
  mHisto=histo;
  mHistoW=histoW;
  mHistoE=histoE;
  mClear=0;
  mReader = reader;
  mParam = paramReader; 
  mDb    = dbReader;
}

StFtpcChargeStep::StFtpcChargeStep(StFTPCReader *reader,  
				   StFtpcParamReader *paramReader,
                                   StFtpcDbReader *dbReader)
{
  //   cout << "StFtpcChargeStep constructed" << endl;
  mHisto= new TH2F("chargestepinternal"	,"FTPC charge steps by sector"	,60,-0.5,59.5, 260, -0.5, 259.5);
  mClear=1;
  mReader = reader;
  mParam = paramReader; 
  mDb    = dbReader;
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
  /* is magboltz database loaded, if not exit */
  if(mDb->numberOfMagboltzBins()<1)
    {
      printf("Couldn't find magboltz data table, exiting!\n");
      return 0;
    }

   /* allocate memory for padtrans table */
   double *pRadius = new double[mParam->numberOfDriftSteps()];

   if(pRadius == 0)
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
  for(iRow=mDb->firstPadrowToSearch()-1; 
      iRow<mDb->lastPadrowToSearch(); iRow++)
    {
      int iSec;
      for(iSec=mDb->firstSectorToSearch()-1; 
	  iSec<mDb->lastSectorToSearch(); iSec++)
	{
#ifdef DEBUG
	  printf("Now on Sector %d, Row %d\n", iSec, iRow);
#endif
	  
	  // calculate hardware (daq) sectors from software position
	  int iHardSec = mDb->numberOfSectors()*(int)(iRow/2) + iSec + 1;
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
                      if (iHardSec >= 1 && iHardSec <= 30 ) { 
		         mHistoW->Fill( entry+sequences[iSeqIndex].startTimeBin, //bin
				   sequences[iSeqIndex].FirstAdc[entry]); // weight
                      }
                      if (iHardSec >= 31 && iHardSec <= 60 ) {
		         mHistoE->Fill( entry+sequences[iSeqIndex].startTimeBin, //bin
				   sequences[iSeqIndex].FirstAdc[entry]); // weight
                      }
		    }
		}
	    }
	}
    }

  // get projection
  TH1D *proHisto= mHisto->ProjectionY();
  int i,j;
  int imax=proHisto->GetNbinsX();
//   for(i=0; i<imax; i++)
//     {  
//       float temp=proHisto->GetBinContent(i);
//       cout << "bin " << i << " content " << temp << endl;
//     }

  // find charge step roughly
  float average=0;
  for(i=50; i<100; i++)
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
  // uncomment this if this is deemed more appropriate than maximum change
//   float peakplus1=proHisto->GetBinContent(peak+1);
//   float peakplus2=proHisto->GetBinContent(peak+2);
//   float lnAmps=log(peakplus1*peakplus1/(peakheight*peakplus2));
//   if(lnAmps<=0)
//     lnAmps=1000000;
//   float sigmaSqrDropoff=1/lnAmps;
//   float chargestep = peak+0.5-sigmaSqrDropoff*log(peakheight/peakplus1);

  // gauss fit derivative of charge step dropoff
  float *dCharge = new float[imax];
  int maxdrop=0;

  dCharge[maxdrop]=0.0;
  dCharge[peak] = 0.000001;  // This line had to be introduced to avoid problems if the decline of the 
                             // charge step histogram was to steep and had less than 4 bins with entries at 
                             // the right hand side of the maximum.
    
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

  // include tZero = time from collision to beginning of bin 0
  TimeCoordinate += mDb->tZero()/mDb->microsecondsPerTimebin();
  int PadtransPerTimebin = (int) mParam->numberOfDriftSteps() 
    / mDb->numberOfTimebins();
  
  /* linear interpolation in radius table */
  for(i=0; pRadius[i]>mDb->sensitiveVolumeInnerRadius(); i++);
  float aimTime=(i*(pRadius[i-1]-mDb->sensitiveVolumeInnerRadius())
		 +(i-1)*(mDb->sensitiveVolumeInnerRadius()-pRadius[i]))
    /(pRadius[i-1]-pRadius[i]);
  aimTime/=PadtransPerTimebin;
  float newPressure=
    mParam->normalizedNowPressure()
    +((aimTime/TimeCoordinate-1)/
     (mDb->magboltzdVDriftdP(mDb->numberOfMagboltzBins()/2, 0)
      /mDb->magboltzVDrift(mDb->numberOfMagboltzBins()/2, 0)));

  if(setPressure && mHisto->GetEntries() > 10000)  // set pressure only if enough entries collected
    {
      mParam->setNormalizedNowPressure(newPressure);
      
      // reiterate time calculation to get better precision
      calcpadtrans(pRadius);
      /* linear interpolation in radius table */
      for(i=0; pRadius[i]>mDb->sensitiveVolumeInnerRadius(); i++);
      aimTime=(i*(pRadius[i-1]-mDb->sensitiveVolumeInnerRadius())
	       +(i-1)*(mDb->sensitiveVolumeInnerRadius()-pRadius[i]))
	/(pRadius[i-1]-pRadius[i]);
      aimTime/=PadtransPerTimebin;
      newPressure=
	mParam->normalizedNowPressure()
	+((aimTime/TimeCoordinate-1)/
	  (mDb->magboltzdVDriftdP(mDb->numberOfMagboltzBins()/2, 0)
	   /mDb->magboltzVDrift(mDb->numberOfMagboltzBins()/2, 0)));
      mParam->setNormalizedNowPressure(newPressure);

      // reiterate again to get even better precision (error<10E^-4)
      calcpadtrans(pRadius);
      /* linear interpolation in radius table */
      for(i=0; pRadius[i]>mDb->sensitiveVolumeInnerRadius(); i++);
      aimTime=(i*(pRadius[i-1]-mDb->sensitiveVolumeInnerRadius())
	       +(i-1)*(mDb->sensitiveVolumeInnerRadius()-pRadius[i]))
	/(pRadius[i-1]-pRadius[i]);
      aimTime/=PadtransPerTimebin;
      newPressure=
	mParam->normalizedNowPressure()
	+((aimTime/TimeCoordinate-1)/
	  (mDb->magboltzdVDriftdP(mDb->numberOfMagboltzBins()/2, 0)
	   /mDb->magboltzVDrift(mDb->numberOfMagboltzBins()/2, 0)));

      mParam->setNormalizedNowPressure(newPressure);
      
      gMessMgr->Message("", "I", "OST") << "StFtpcChargeStep set normalized pressure to " << newPressure << "." << endm;
    }      
   
  else {
    gMessMgr->Message("", "I", "OST") << "StFtpcChargeStep did not set normalized pressure to " << newPressure << " (still at " << mParam->normalizedNowPressure() << ")" << endm;

    if (mHisto->GetEntries() > 10000) {
      gMessMgr->Message("", "I", "OST") << "because only histogram requested" << endm;
    }
    
    else {
      gMessMgr->Message("", "I", "OST") << "because chargestep histo has " << mHisto->GetEntries() << " entries up to now (at least 10000 entries are necessary)" << endm;
    }
  }

  delete proHisto;
  delete[] dCharge;
  delete[] pRadius;
  return 1;
}


int StFtpcChargeStep::calcpadtrans(double *pradius)
{
  int i, j, v_buf, padrow;
  double t_last, t_next, r_last, r_next, e_now, v_now, psi_now;
  double step_size, deltap;
  
  step_size=((float) mDb->numberOfTimebins()
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
      r_last=mDb->sensitiveVolumeOuterRadius();
      pradius[padrow]=mDb->sensitiveVolumeOuterRadius();
      e_now = mDb->radiusTimesField() / (0.5*r_last);
      for(j=v_buf; j<mDb->numberOfMagboltzBins()-1
	    && mDb->magboltzEField(j) < e_now; j++);
      if(j<1 || j>mDb->numberOfMagboltzBins())
	{
          gMessMgr->Message("", "E", "OST") << "Error 1: j=" << j << ", v_buf=" << v_buf << " e_drift=" << mDb->magboltzEField(j) << ", e_now=" << e_now << endm;
	  return FALSE;
	}
      v_buf=j-1;
      v_now=((mDb->magboltzVDrift(v_buf, padrow)
	      +deltap*mDb->magboltzdVDriftdP(v_buf, padrow))
	     *(mDb->magboltzEField(j)-e_now)
	     +(mDb->magboltzVDrift(j, padrow)
	       +deltap*mDb->magboltzdVDriftdP(j, padrow))
	     *(e_now-mDb->magboltzEField(v_buf)))
	/(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
      psi_now=((mDb->magboltzDeflection(v_buf,padrow)
		+deltap*mDb->magboltzdDeflectiondP(v_buf,padrow))
	       *(mDb->magboltzEField(j)-e_now)
	       +(mDb->magboltzDeflection(j,padrow)
		 +deltap*mDb->magboltzdDeflectiondP(j,padrow))
	       *(e_now-mDb->magboltzEField(v_buf)))
	/(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
      for (i=0; i<mParam->numberOfDriftSteps()-1 
	     && e_now < mDb->magboltzEField(mDb->numberOfMagboltzBins()-2)
	     ; i++) 
	{
	  t_next = t_last + step_size;
	  /* first guess for r_next: */
	  r_next = r_last - v_now * step_size * mDb->microsecondsPerTimebin();
	  e_now = mDb->radiusTimesField() / (0.5*(r_last+r_next));
	  
          for(j=v_buf; j<mDb->numberOfMagboltzBins()-1
                       && mDb->magboltzEField(j) < e_now; j++);
	  
	  if(j<1 || j>mDb->numberOfMagboltzBins())
	    {
              gMessMgr->Message("", "E", "OST") << "Error 2: j=" << j << ", v_buf=" << v_buf << " e_drift=" << mDb->magboltzEField(j) << ", e_now=" << e_now << endm;
	      return FALSE;
	    }
	  
	  v_buf=j-1;
	  v_now=((mDb->magboltzVDrift(v_buf, padrow)
		  +deltap*mDb->magboltzdVDriftdP(v_buf, padrow))
		 *(mDb->magboltzEField(j)-e_now)
		 +(mDb->magboltzVDrift(j, padrow)
		   +deltap*mDb->magboltzdVDriftdP(j, padrow))
		 *(e_now-mDb->magboltzEField(v_buf)))
	  /(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
	  psi_now=((mDb->magboltzDeflection(v_buf,padrow)
		    +deltap*mDb->magboltzdDeflectiondP(v_buf,padrow))
		   *(mDb->magboltzEField(j)-e_now)
		   +(mDb->magboltzDeflection(j,padrow)
		     +deltap*mDb->magboltzdDeflectiondP(j,padrow))
		   *(e_now-mDb->magboltzEField(v_buf)))
	  /(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
	  
	  /* correct r_next: */
	  r_next = r_last - v_now * step_size *mDb->microsecondsPerTimebin();
	  pradius[padrow+(i+1)]=r_next;
	  t_last=t_next;
	  r_last=r_next;
	}
#ifdef DEBUG
      printf("%d steps calculated, padrow %d\n", i, padrow);
#endif
      
    }

  return TRUE;
}
