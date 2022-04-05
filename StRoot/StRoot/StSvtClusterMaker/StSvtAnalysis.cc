/***************************************************************************
 *
 * $Id: StSvtAnalysis.cc,v 1.21 2005/07/23 03:37:33 perev Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: Fits the clusters to find the various moments (0th, 1st, 2nd)
 *              as well as peakADC, number of anodes, number of pixels,...
 *              
 *              Then catagorize the clusters with a flag. All flags<4 are
 *              good. A Flag==2 is a deconvoluted hit. All flags are mulitples
 *              of 2 so the user can check for all the reasons a hit was flag
 *              a certain way. When hits are catagorized we never judge solely
 *              on peakADc or integrated charge as this is a bias. See
 *              CatagorizeCluster() for details on how we callsify clusters. 
 *              An additional cut is made for noisy anodes or timebuckets 
 *              if there are more than 4 hits/event on them. 
 *
 *              Finally this code deconvolutes some clusters. At present we are 
 *              quite conservative in which clusters we deconvolute. There has
 *              to be a clear valley between peaks in a cluster for hits to be
 *              deconvoluted. Look at PEAK_TO_VALLEY in isValidPeak() for more 
 *              details.
 *
 *              Flags:    0 = standard good
 *                        2 = deconvoluted
 *                        4 = more than 4 hits on eithe this anodes or time bucket 
 *                            for this event
 *                        8 = 1 pixel cluster with ADC<1 (could reuse this for something else)
 *                       16 = undershoot on wrong side
 *                       32 = topological cut. Based on num pixels, and charge
 *                       64 = topological cut based on relative widths in anode and time dir. Gets
 *                            rid of tracks comming in at very shallow angles. You might want to
 *                            rethink this one for special physics studies.
 *                      128 = A hit which was deconvoluted.
 *
 * 
 *
 * Bugs:       The method used to flad noisy anodes and time buckets is inefficient
 *             and should be looked at. We zero a huge array  before each event. Sanjeev
 *
 *             We still cant seem to classify hits on hydrid 10 as bad. Probebly
 *             will never be able to as the blob keeps messing up the peds here. Sanjeev
 *
 *             At present I assume the arbitary offset to all anodes is 100. This will have 
 *             to be made a variable and replaced in *3* places. Sanjeev

 *             PedOffset is now subtracted properly  Helen 17/08/00
 *
 ***************************************************************************
 *
 * Modifications by Jun Takahashi Dec 2002.
 * 1. In oneOrTwoAnodeMoments, I included the line , mNumPixels++;
 * 2. Created the function: LoadAnodeGains(), but it is set to 1 at the moment.
 * 3. In CatagorizeCluster: Added new cuts and flags:
 *     flag: 44 laser spot.
 *           17 cluster sequence bigger than 80 in time direction
 *           10 single anode, with more than 10 time buckets.         
 *           13 Found a seconday peak after original peak.
 *           15 Found a secondary peak before original peak.
 *           
 * 
 * $Log: StSvtAnalysis.cc,v $
 * Revision 1.21  2005/07/23 03:37:33  perev
 * IdTruth + Cleanup
 *
 * Revision 1.20  2004/04/23 15:57:32  caines
 * Flag as bad hits with 1 anode and peak less than 11
 *
 * Revision 1.19  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.18  2003/04/30 20:38:45  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.17  2003/01/28 20:27:30  munhoz
 * new filters for clusters
 *
 * Revision 1.16  2002/05/09 16:55:39  munhoz
 * add reading bad anodes from DB
 *
 * Revision 1.15  2002/04/25 20:34:50  caines
 * Pass bad anode information into cluster fitter
 *
 * Revision 1.14  2002/01/04 16:05:48  caines
 * fix overstepping of array bounds
 *
 * Revision 1.13  2001/08/07 20:52:15  caines
 * Implement better packing of svt hardware and charge values
 *
 * Revision 1.12  2001/04/25 18:34:44  perev
 * HPcorrs 
 *
 * Revision 1.11  2001/04/21 21:47:39  caines
 * Fix int to double problem
 *
 * Revision 1.10  2000/11/30 20:42:58  caines
 * Fix flag error and anode position error, use database
 *
 * Revision 1.9  2000/10/31 16:20:57  caines
 * Added more functions to make the code more readable
 *
 * Revision 1.9  2000/10/23 13:47:03  Selemon
 * Added more functions to make the code more readable
 *
 * Revision 1.8  2000/10/02 13:47:03  caines
 * Fixed some array bound problems. Better flagging of hits
 *
 * Revision 1.7  2000/09/14 22:17:15  caines
 * Fix memory problems
 *
 * Revision 1.6  2000/08/29 22:46:26  caines
 * Fixed some memory leaks
 *
 * Revision 1.5  2000/08/24 04:27:56  caines
 * Fixed casting warnings so compiles without errors on linux
 *
 * Revision 1.4  2000/08/21 13:06:57  caines
 * Much improved hit finding and fitting
 *
 * Revision 1.3  2000/08 13:06:57  sanjeev
 * changed flags so that we only have positive flags. All
 *              flags<4 are good.
 *
 * Revision 1.2  2000/08 13:06:57  sanjeev
 * Added methods for peak finding, cluster fitting, and deconvoluting
 * 
 *
 **************************************************************************/
#include <Stiostream.h>
#include "Stiostream.h"
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "StMessMgr.h"
#include "StSequence.hh"
#include "StMCTruth.h"
#include "StDAQMaker/StSVTReader.h"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtHybridBadAnodes.hh"
#include "StSvtAnalysis.hh"

int    Compare_Point ( const void *, const void *);   //nedd to declare here for some odd reason.

StSvtAnalysis::StSvtAnalysis(int TotalNumberOfHybrids)
{
  mHybridData              = NULL;
  mHybridRawData           = NULL;
  mHybridCluster           = NULL;
  mSvtSequence             = NULL;
  mSvtBadAnode             = NULL;

  m_nWrkBkt = 0;
  m_nUndBkt = 0;
  m_nGt8    = 0; 
  m_nSig    = 0;
  m_SvtEvt  = 0;
  mHitId = 0;

  mNumPixels = 0, mPeakADC = 0, mSumAdc = 0;
  mMom0 = 0, mNeff = 0;
  mDriftMom1 = 0, mAnodeMom1 = 0;
  mDriftMom2 = 0, mAnodeMom2 = 0; mMom0 = 0, mNeff = 0;
  mX_err=72., mY_err=75.;                          //default bin size/::sqrt(12)
  
  mMaxClu=0;
  setMemory();
  setArrays(TotalNumberOfHybrids);

}

void StSvtAnalysis::setMemory()
 {
  //call all new's here in init and hope we never go above 500. If we do we recall new and make it
  //the larger size. Sanjeev

  mMaxClu = (mMaxClu)? mMaxClu*2 : 500;
  mAuxArr.Set(mMaxClu*sizeof(StSvtAnalysisAux));
  mAux = (StSvtAnalysisAux*)mAuxArr.GetArray();
}

void StSvtAnalysis::setArrays(int TotalNumberOfHybrids)
{

  m_countBadAn = malloc_matrix_d (TotalNumberOfHybrids+1, 240+2);             // number of hybrids bad anodes (>4 hits)
  m_countBadTb = malloc_matrix_d (TotalNumberOfHybrids+1, 128+1);             // bad time

  if (m_countBadAn == NULL || m_countBadTb == NULL) {
    cout<<"You have a bad error assigning memory for counting bad SVT pixels"<<endl;
    if (m_countBadAn==NULL) free_matrix_d(m_countBadAn, 240+2);
    if (m_countBadTb==NULL) free_matrix_d(m_countBadTb, 128+1);
  }

  m_Pixels = malloc_matrix_d (240+2, 128+2);                //use these for deconvolution
  m_Shadow = malloc_matrix_d (240+2, 128+2);
  m_Raw    = malloc_matrix_d (240+2, 128+2);

  if (m_Pixels == NULL || m_Shadow == NULL || m_Raw == NULL) {
    cout<<"You have a fatal error in assigning memory in the cluster finder"<<endl;
    if (m_Pixels) free_matrix_d (m_Pixels, 240+2);
    if (m_Shadow) free_matrix_d (m_Shadow, 240+2);
    if (m_Raw)    free_matrix_d (m_Raw   , 240+2);
  }
  
  for (int i1=0;i1<242;i1++) {
    for (int j1=0;j1<130;j1++) {
      m_Pixels[i1][j1] = 0;
      m_Shadow[i1][j1] = 0;
      m_Raw[i1][j1]    = 0;
    }  
   }

  }


StSvtAnalysis::~StSvtAnalysis()
{

} 
void StSvtAnalysis::SetPointers(StSvtHybridData* hybAdjData,
				StSvtHybridData* hybRawData,  
                                StSvtHybridCluster* hybClu,
				StSvtHybridBadAnodes* BadAnodes,
				int NumberOfHybrids, int PedOffset )
// This is how the Maker communicates with this object to tell it where the data is. We also 
// want access to the raw data for the deconvolution and for the one anode hits to get a better
// estimate of the 2nd moments. Sanjeev
{
  mHybridData    = hybAdjData;
  mHybridRawData = hybRawData;
  mHybridCluster = hybClu;
  mPedOffset     = PedOffset;
  mSvtBadAnode  =  BadAnodes;

  mNumOfClusters = mHybridCluster->getNumberOfClusters();
  if (mNumOfClusters>mMaxClu) setMemory();

  //this is called for each new event

   {for (int i=0; i<NumberOfHybrids; i++) {
     {for (int j=0; j<242; j++) m_countBadAn[i][j] = 0;}
     {for (int j=0; j<129; j++) m_countBadTb[i][j] = 0;}
   }}

}


void StSvtAnalysis::FirstAndLastAnodes()
  // Calculate the First and last Anodes of all the clusters. Selemon
{
  int actualAn = 0, actualan = 0, mem = 0;
  
  if (mNumOfClusters>mMaxClu) setMemory(); 
  
  for(int clu = 0; clu < mNumOfClusters; clu++)   
    {
      StSvtAnalysisAux *aux = mAux+clu;
      mem = 0;  //add apr00 SUP
      mNumOfMembers  = mHybridCluster->getNumberOfMembers(clu);
      aux->mInfo = mHybridCluster->getCluMemInfo(clu);
      

      if(mNumOfMembers==1)
        {
	  aux->mCluFirstAnode = aux->mInfo[mem].actualAnode;
	  aux->mCluLastAnode  = aux->mCluFirstAnode;
	}
      else
	{
	  {for(int j = 1; j<mNumOfMembers ; j++)
	    {
	      actualAn =  aux->mInfo[mem].actualAnode;
	      actualan =  aux->mInfo[j].actualAnode;
	      
	      if(actualAn < actualan)
		aux->mCluFirstAnode = actualAn;
	      else  
		{
		  aux->mCluFirstAnode= actualan;
		  mem = j;
		}
	    }}
	  
	  mem = 0;
	  {for(int j = 1; j<mNumOfMembers ; j++)
	    {
	      actualAn = aux->mInfo[mem].actualAnode;
	      actualan = aux->mInfo[j].actualAnode;
	      
	      if(actualAn > actualan)
		aux->mCluLastAnode = actualAn;
	      
	      else  
		{
		  aux->mCluLastAnode =  actualan;
		  mem = j;
		}
	    }}
	  
	}
    }
}


void StSvtAnalysis::CluFirstTimeBin()
  // Calculate the first time bin in the cluster. Selemon
{
  int status , Seq, SeqStart = 0, seqStart = 0;
  int listAn = 0, mseq = 0, mem;
  
  // StSequence* svtSequence;
  if (mNumOfClusters>mMaxClu) setMemory();
  
  for(int clu = 0; clu < mNumOfClusters; clu++)
    {
      StSvtAnalysisAux *aux = mAux+clu;
      aux->mInfo = mHybridCluster->getCluMemInfo(clu);
      mNumOfMembers = mHybridCluster->getNumberOfMembers(clu);
      
      mem = 0;
      
      if(mNumOfMembers==1)
	{
	  listAn = aux->mInfo[mem].listAnode;
	  mseq   = aux->mInfo[mem].seq; 
	  
	  status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
	  aux->mCluFirstTimeBin = mSvtSequence[mseq].startTimeBin;
	}
      else
	{
	  for(int j = 1; j< mNumOfMembers; j++)
	    {
	      listAn = aux->mInfo[mem].listAnode;
	      mseq   = aux->mInfo[mem].seq;
	      status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
	      SeqStart = mSvtSequence[mseq].startTimeBin; 
	      
	      listAn = aux->mInfo[j].listAnode;
	      mseq   = aux->mInfo[j].seq;
	      status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
	      seqStart = mSvtSequence[mseq].startTimeBin;
	      
	      if(SeqStart <= seqStart)
		aux->mCluFirstTimeBin = SeqStart;
	      else  
		{
		  aux->mCluFirstTimeBin = seqStart;
		  mem = j;
		}
	    }
	} 
    }
  }


void StSvtAnalysis::CluLastTimeBin()
//Calculate last time bin in cluster. Selemon.
{ 
  int status , Seq, SeqStart = 0, SeqLength = 0,  SeqEnd = 0, seqEnd = 0;
  int listAn = 0, mseq = 0, mem;
  
  //StSequence* svtSequence;
  if (mNumOfClusters>mMaxClu) setMemory();
  
  for(int clu = 0; clu < mNumOfClusters; clu++)
    {
      mAux[clu].mInfo = mHybridCluster->getCluMemInfo(clu);
      mNumOfMembers = mHybridCluster->getNumberOfMembers(clu);
      
      mem = 0;
      
      if(mNumOfMembers==1)
	{
	  listAn = mAux[clu].mInfo[mem].listAnode;
	  mseq = mAux[clu].mInfo[mem].seq;
	  status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
	  SeqStart =  mSvtSequence[mseq].startTimeBin;
	  SeqLength = mSvtSequence[mseq].length;
	  SeqEnd =  SeqStart + SeqLength - 1; 
	  mAux[clu].mCluLastTimeBin = SeqEnd; 
        }
      else
	{
	  for(int j = 1; j< mNumOfMembers ; j++){
	    listAn = mAux[clu].mInfo[mem].listAnode;
	    mseq = mAux[clu].mInfo[mem].seq;
	    status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
	    
	    SeqStart =  mSvtSequence[mseq].startTimeBin;
	    SeqLength = mSvtSequence[mseq].length;
	    SeqEnd =  SeqStart + SeqLength - 1;
	    
	    listAn = mAux[clu].mInfo[j].listAnode;
	    mseq = mAux[clu].mInfo[j].seq;
	    status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
	    
	    SeqStart =  mSvtSequence[mseq].startTimeBin;
	    SeqLength = mSvtSequence[mseq].length;
	    seqEnd =  SeqStart + SeqLength - 1; 
	    
	    if(SeqEnd > seqEnd)
	      mAux[clu].mCluLastTimeBin = SeqEnd;
	    else  {
	      mAux[clu].mCluLastTimeBin = seqEnd;
	      mem = j;
	    }
	  }
	}
    }
}   

void StSvtAnalysis::MomentAnalysis(){
//Calculate the moments of the cluster. We do not fit to determine the charge but rather 
//just count pixels ADC. This routine presently call the catagoriser and deconvoluter. 
//We might want to transfer those calls to the Maker. At present though the catagorizer 
//and deconvluter work on a cluster by cluser basis. The Maker works with collections
//of clusters so the logic would hav to change if this happens. Sanjeev
//
 
 if (m_hybIndex==6) m_SvtEvt++;

 FillRawAdc();                              //Put the raw adcs for the whole hybrid into an 2D
                                            //Array. If there is no info value is 0.	 

 if(mNumOfClusters > mMaxClu) setMemory();
 
 m_clu = mNumOfClusters-1;                              //keep track of # deconcoluted clusters. 

 for(int clu = 0; clu < mNumOfClusters; clu++){          //loop over all clusters for one hybrid 
   calcMoments(clu);
 }

 m_clu++;   //make equal to mNumOfClusters     

 ClearRawAdc();                          //have to reset the raw adc's since the number of anodes 
                                         //returned for each hybrid will not be the same and are not
                                         //guareented to be 240.
}


void StSvtAnalysis::LoadAnodeGains()
{
  /*
   FILE *fp = fopen("/home/jtakahas/star/gaincal/meanadc_out.dat","r");
 
   int hy,an;
   float meanadc;
   float gain=0;
   int ncols;
   int nlines = 0;

   if (fp){
     while (1) {
       ncols = fscanf(fp,"%d %d %f",&hy, &an, &meanadc);
       if (ncols < 0) break;
       if (meanadc > 50 || meanadc<15) gain=1;
       else gain = 20/meanadc;
       mAux[an].mAnodeGain[hy]=1;
       //mAux[an].mAnodeGain[hy]=gain;
       //cout << nlines << "\t" << hy << "\t" << an << "\t" << mAux[an].mAnodeGain[hy] << endl;
       nlines++;
     }
   }
   else{
  */
       for (int i=0;i<433;i++){
	 for (int j=0;j<241;j++){
	   mAnodeGain[i][j]=1;
	 }
       }
       //   }
       //fclose(fp);
}


void StSvtAnalysis::calcMoments(int clu){
  int listAn , actualAn, numAnodes;
  int mseq, Seq, stTimeBin, len;
  float ADC=0;
  unsigned char* adc;
  StSvtAnalysisAux *aux = mAux + clu;
  mNumPixels = 0, mPeakADC = 0,mSumAdc = 0, mHitId=0;
  mDriftMom1 = 0, mAnodeMom1 = 0, mDriftMom2 = 0, mAnodeMom2 = 0, mMom0 = 0, mNeff = 0;

  int igt3=0, peakPosAn=0, peakPosTim=0, peakMem=0, peakPixel=0;             //recall 1ADC = 4mV
  
  aux->mInfo = mHybridCluster->getCluMemInfo(clu);
  mNumOfMembers       = mHybridCluster->getNumberOfMembers(clu);   //I guess this is the number of anodes
	     
  numAnodes = GetLastAnode(clu)-GetFirstAnode(clu)+1;     //this is not the same as mNumOfMembers for clusters while curl around!!
 
  mMyflag=0;
  for(int mem = 0; mem < mNumOfMembers; mem++) {
    listAn   =  aux->mInfo[mem].listAnode;   //what is this??
    mseq     =  aux->mInfo[mem].seq;
    actualAn =  aux->mInfo[mem].actualAnode; //actual anode
     
    mHybridData->getListSequences(listAn,Seq,mSvtSequence);
    stTimeBin = mSvtSequence[mseq].startTimeBin; 
    len = mSvtSequence[mseq].length;
    adc = mSvtSequence[mseq].firstAdc;
    for(int j = 0; j < len; j++) {       //loop over time in one sequence
      ADC = (float)adc[j];
      if (ADC==0 || ADC==255) ADC=ADC; 
      else ADC=ADC-mPedOffset;   // mPedOffset subtraction.

      ADC=ADC*mAnodeGain[m_hybIndex][actualAn];
      //do this else we distort the means of small clusters.
      if (mPeakADC<ADC) { 
	mPeakADC=(int)ADC; peakPosAn=actualAn; peakPosTim=stTimeBin+j; peakMem=mem; peakPixel=j;
      }
      
      if (ADC>1 && ADC<4000 && (stTimeBin+j)>=0 && (stTimeBin+j)<128 && actualAn>0 && actualAn<=240){
	mNumPixels++;                                 
	if (ADC>3) igt3++;
	mDriftMom1 += ADC * (stTimeBin + j + 0.5);
	mAnodeMom1 += ADC * (actualAn - 0.5);
	mDriftMom2 += ADC * (stTimeBin + j + 0.5) * (stTimeBin + j + 0.5);
	mAnodeMom2 += ADC * (actualAn - 0.5) * (actualAn - 0.5);
	mNeff       += (int)(ADC * ADC);
	mSumAdc     += (int)ADC;
      } 
      else {
	//mHitId += -1;
	//cout<<"You have funny SVT ADC or timebin or anodes numbers in this cluster"<<endl;
      }
    }  
  }  //end loop over sequeces (members) from 1 cluster
  

  // This will add the information of neighboring anodes to the 1/2 anode clusters.
  if(numAnodes == 1 || numAnodes == 2){
    mMyflag=1;
    oneOrTwoAnodeMoments(clu, peakPosTim);
  }

  
  finalMoments(clu, numAnodes);
  newCluster(clu, numAnodes,igt3);
}


void StSvtAnalysis::oneOrTwoAnodeMoments(int clu, int peakPosTim) 
{     
  // Make the 2nd moments better for 1 anode hits. Look a little to the right of them for better calc.
  // This routine adds ONLY the extra neighboring anodes information -1 and +1 anodes. JT
  // Gets it from m_Raw
  
  int iAst, iAend;
  float ADC=0;
  iAst = GetFirstAnode(clu); iAend = GetLastAnode(clu);
  

  if (iAst>1 && iAend<240 && peakPosTim>0 && peakPosTim<127){    /*do a better job for the 1 anode hits*/
    for (int i=iAst-1; i<=iAend+1; i++)                          /*This is dangerous as we do it blindly*/
      {                                                          /*It will mess up for a small fraction of hits*/ 

        if (i==iAst || i==iAend)  continue;                      /*when another cluster is close by*/ 
        for (int j=peakPosTim-1; j<=peakPosTim+1; j++)
	  {    
	    ADC = (float)m_Raw[i][j];                        // obs. m_Raw is already pedoffset subtracted.
	    ADC=ADC*mAnodeGain[m_hybIndex][i];
	    
	    if (ADC>0 && ADC<mPeakADC/2){                    /*since we do this blindly lets at least put*/ 
	      mSumAdc     += (int)ADC;                       /*some safety checks in*/ 
	      mDriftMom1 += ADC * (j+0.5);
	      mAnodeMom1 += ADC * (i-0.5);
	      mDriftMom2 += ADC * (j+0.5) * (j+0.5);
	      mAnodeMom2 += ADC * (i-0.5) * (i-0.5);
	      mNeff       += int(ADC * ADC);
	      mNumPixels++;                                 // Added this new line. JT.
	    }
	  }
      }
  }
}


void StSvtAnalysis::finalMoments(int clu,int numAnodes)
{
  
   if (mSumAdc>3)  //fianlize the moment calculation and also estiame the cov's. 
   {
     mMom0 = (double)mSumAdc;
     mDriftMom1 = (double)mDriftMom1/mSumAdc;
     mAnodeMom1 = (double)mAnodeMom1/mSumAdc;
     mDriftMom2 = ::sqrt((double)mDriftMom2/mSumAdc - mDriftMom1*mDriftMom1); /*note no N/N-1*/
     mAnodeMom2 = ::sqrt((double)mAnodeMom2/mSumAdc - mAnodeMom1*mAnodeMom1);
     if (mDriftMom2>1000 || mDriftMom2<0 || mAnodeMom2>1000 || mAnodeMom2<0)   /*calc fails occasionally*/
     {
       mDriftMom2 = -999; mAnodeMom2 = -999; mNeff = 0;
     }

     if (mNeff>1)                                                     //the ones which are not are really bad 
     {
       double Neff = (double) mSumAdc*mSumAdc/mNeff;  
       if (Neff>1.5)                                                 //make sure calc. does not mess up
       {
         mDriftMom2 = mDriftMom2 * ::sqrt( (double(mNeff)/(mNeff-1)) );
         mAnodeMom2 = mAnodeMom2 * ::sqrt( (double(mNeff)/(mNeff-1)) );
         if (mDriftMom2!=0) mY_err = 260.e-4*mDriftMom2/::sqrt(double(mNeff)); /*is 0 for 1 drift wonders. Units: cm*/
         if (mAnodeMom2!=0) mX_err = 250.e-4*mAnodeMom2/::sqrt(double(mNeff)); /*is 0 for 1 anode wonders. Units: cm*/
       }
   
       if (numAnodes==1 && mAnodeMom2==0) mAnodeMom2=0.288;    //bin width/::sqrt(12)
       if ( (GetLastTimeBin(clu)-GetFirstTimeBin(clu))==0 && mDriftMom2==0) mDriftMom2=0.288;  //bin width/::sqrt(12)

     }
     else
     {
       mHitId += 8;
       mMom0      = -999.;
       mDriftMom1 = -999.;
       mAnodeMom1 = -999.;
       mDriftMom2 = -999.;
       mAnodeMom2 = -999.;
     }
   }

}


void StSvtAnalysis::newCluster(int clu, int numAnodes, int igt3)
{

    int iQual, iRetu;
    static int iRows=1, iCols=1;

   //These are all the objects used by the Maker to make the cluster object la   mAux[clu].mCluID                   = clu;            //unique ID
   StSvtAnalysisAux *aux = mAux+clu;
   aux->mCluDeconvID             = clu;            //at this point the origional cluster and deconv. one are the same
   aux->mCluCharge               = mMom0;          //charge
 //aux->mCluFlag                 = mHitId;         // It will be filled later, so no need here. JT.
   aux->m_oneortwo_flag          = mMyflag;        // added by JT.
   aux->mMeanClusterTimeBin      = mDriftMom1;     //mean time
   aux->mMeanClusterAnode        = mAnodeMom1;     //mean anode
   aux->mSecondMomClusterTimeBin = mDriftMom2;     //mean 2nd moment time
   aux->mSecondMomClusterAnode   = mAnodeMom2;     //mean 2nd moment anode
   aux->mCluXCov                 = 1.*mX_err*mX_err; //error in Anode
   aux->mCluYCov                 = 4.*mY_err*mX_err; //error in Time is made arbitarily 2 time larger (4 time for cov). Check dist.
   aux->mCluPeakAdc              = mPeakADC;        //peak ADC
   aux->mCluNumPixels            = mNumPixels;      //number of pixels
   aux->mCluNumAnodes            = numAnodes;      //number of anodes including loop backs.
   aux->mHybridNum               = m_hybIndex;     //hybrid index.


   for (int i1=0;i1<iRows;i1++)        //be more clever in the futre but for now....
     {                                 //zero out previous box
       for (int j1=0;j1<iCols;j1++)
	 {
	   m_Pixels[i1][j1] = 0;            //used by deconvolution
	   m_Shadow[i1][j1] = 0;
	 }
     }


   //size of search area for deconvolution
   iRows = mAux[clu].mCluLastAnode   - mAux[clu].mCluFirstAnode   +1+2;      //+2 to account for 0 paddind around side
   iCols = mAux[clu].mCluLastTimeBin - mAux[clu].mCluFirstTimeBin +1+2; 

   Fill_Pixel_Array(clu);                                      //Fill 2D array with ADC's. Easier for the search in this format.
   iQual = CatagorizeCluster(iRows,iCols,igt3,clu);            //Classify the cluster as good or bad.
   mHitId += iQual;                                            //Update previous classification
   aux->mCluFlag = mHitId;                                     //Set flag
   if (mHitId==0 && m_deconv==1) iRetu = Deconvolve_Cluster(iRows, iCols, clu);   //do the deconvolution only for some candidates
      
   if( (int)(.5+mAnodeMom1) > 0 && (int)(.5+mAnodeMom1) <= 241 )
     m_countBadAn[m_hybIndex][(int)(.5+mAnodeMom1)]++;      //count the noisy anodes and time for all hits whether good or bad
   if(  (int)(.5+mDriftMom1) >= 0 && (int)(.5+mDriftMom1) <= 128)
     m_countBadTb[m_hybIndex][(int)(.5+mDriftMom1)]++;         //invstigate this.
 

  //cout<<"******* Moment Analysis finished *******"<<endl;  
}



int StSvtAnalysis:: CatagorizeCluster(int iRows, int iCols, int igt3, int clu)
/* Classify Cluster by topology                       */
//Calssify the clusters. Note everything is in base to to allow one to figure out many
//conditions simultaneously. Also not these might need fine tuning. I copied them over from 
//what I saw in E896 but they might not be true in STAR. Also note that there are no crude
//cuts solely on peakAMP or Int. Charge. Sanjeev 
{
  int i, iUnderBkt, iWrgBkt;
  float fThres; 
  int iQual=0;
  int d_bkt=0, d_sig=0;
  float fCut;
  static int iNumCat = 0;
  StSvtAnalysisAux *aux = mAux + clu;

  iNumCat++;
  m_deconv = 0;                     //default is no deconvolution;

  // Check and change LASER cluster flag to 44. JT.
  if ((m_hybIndex==293 && (mAnodeMom1>195 && mAnodeMom1<205) && (mDriftMom1>80  && mDriftMom1<100)) ||
      (m_hybIndex==416 && (mAnodeMom1>195 && mAnodeMom1<203) && (mDriftMom1>85  && mDriftMom1<100)) ||
      (m_hybIndex==416 && (mAnodeMom1>195 && mAnodeMom1<203) && (mDriftMom1>110  && mDriftMom1<125)) )  {
    iQual=44; 
    return iQual;
  }

  if (iCols >80) {                         // added by JT
    iQual=17;
    return iQual;
  }
  
  if (aux->mCluNumAnodes ==1){             // added by JT
    if(aux->mCluPeakAdc  < 11) iQual = 11; // added by HC to remove noise hits
    if(aux->mCluNumPixels >10) iQual = 10;
  //if (aux->mMeanClusterTimeBin>30) iQual = 10;
  }

  //
  // Searches for secondary peaks, above my_noise variation.
  // If it finds it, ir will return clus flag=13 or 15.
  // 
  float my_noise=5;
  int secondary_peak_after=0;
  int secondary_peak_before=0;
  int member_high_limit=0;
  int member_low_limit=0;    
  for (int i1=0;i1<iRows;i1++){                                
    member_high_limit=0;
    member_low_limit=0;
    for (int j1=m_col_p;j1<iCols-1;j1++){
      if (m_Pixels[i1][j1+1] > (m_Pixels[i1][j1]+my_noise)) secondary_peak_after++;
    }
    for (int j2=m_col_p;j2>0;j2--){
      if (m_Pixels[i1][j2-1] > (m_Pixels[i1][j2]+my_noise)) secondary_peak_before++;
    }
  }
  if (secondary_peak_after>0) iQual=13;
  if (secondary_peak_before>0) iQual=15;

  /*loop over central anode in time for undershoot*/
  // iUnderBkt is the time index of when the undershoot starts in the central anode.
  fThres = -0.03*(float)m_adc_p;          // m_adc_p is the central anode pedoffset subtracted
  iUnderBkt = 127;                                  // if eq 0 or 61 then problem 
  i = m_col_p;                                             /*NOTE SCG does not save much of the undershoot so*/
  while (iUnderBkt==127 && i!=iCols-1)                    /*this is not so sensitive. Good for deconvolution*/
  {
    if ( (m_Pixels[m_row_p][i]+m_Pixels[m_row_p][i+1])/2. < fThres) iUnderBkt = i;
    i++;
  }


  fThres = -0.3*(float)m_adc_p;                         /*now loop in wrong time direction for undershoot*/
  iWrgBkt = 0;
  i = m_col_p;
  while (iWrgBkt==0 && i!=1)
  {
    if ( (m_Pixels[m_row_p][i]+m_Pixels[m_row_p][i-1])/2. < fThres) iWrgBkt = i;
    i--;
  }

  //Now figure out if we like these clusters.

  if (iWrgBkt>=1)                                                { iQual += 16;  m_nWrkBkt++;} // Undershoot in the wrong side
  if (igt3<3 && (aux->mCluCharge<15 || aux->mCluNumPixels<4))    { iQual += 32;  m_nGt8++;   } // Cluster too small
  if ((aux->mSecondMomClusterTimeBin>4*aux->mSecondMomClusterAnode && 
       aux->mSecondMomClusterAnode  <1 && aux->mSecondMomClusterAnode>0) ||
      (aux->mSecondMomClusterAnode  >4*aux->mSecondMomClusterTimeBin &&
       aux->mSecondMomClusterTimeBin<1 && aux->mSecondMomClusterTimeBin>0) )  
                                                                 { iQual += 64; m_nSig++;    }
  //added by JT
  if ( (aux->mSecondMomClusterTimeBin>6*aux->mSecondMomClusterAnode) ||
       (aux->mSecondMomClusterAnode>6*aux->mSecondMomClusterTimeBin) )  
                                                                 { iQual += 65; m_nSig++;    }


  //look for deconvolution candidates

  if (iUnderBkt>m_col_p-2){           // From what I know, this condition is always true (JT) .
    d_bkt = 1;                        //send to deconV
    m_nUndBkt++; }                     

  fCut = fabs(1.15*aux->mSecondMomClusterTimeBin - aux->mSecondMomClusterAnode);
  if ( (fCut<=0.5 && aux->mSecondMomClusterAnode<1.25 && aux->mSecondMomClusterTimeBin<1.25) ||
       (aux->mSecondMomClusterAnode<0.5 &&  aux->mSecondMomClusterTimeBin<0.5))
    d_sig = 0;   //too lazy to do the inverse of the above
  else
    d_sig = 1;

  if (aux->mCluNumAnodes>1 &&  (d_sig==1 || (aux->mCluCharge>80 && d_bkt==1))) m_deconv = 1;
  //if (aux->mCluNumAnodes>1 &&  (aux->mCluCharge>50 && d_bkt==1)) m_deconv = 1;
  //if (aux->mCluCharge>50) m_deconv = 1;
  //if (aux->mCluNumAnodes>1 &&  (d_sig==1)) m_deconv = 1;

  // if (iNumCat%50 == 1) cout<<"Wrong Clusters: iGt8: "<<m_nGt8<<" WrgBkt: "<<m_nWrkBkt<<" Sig: "<<m_nSig<<endl;  

  return iQual;

}

/* Do the deconvolution */

int StSvtAnalysis::Deconvolve_Cluster(int iRows, int iCols, int clu)
//Do the actual deconvolution here. We dont allow more than 10 peaks to be deconvluted. Sanjeev
{
  POINT *Peaks;
  int iNumPeaks = 0;
  int iRetu = 0;
 
  //cout<<"Enter Deconvolve Cluster"<<endl;

  iNumPeaks = 0;
  Peaks = Find_Peaks(iRows, iCols, &iNumPeaks);   //search for valid peaks
  if (Peaks==NULL) return 0;
  if (iNumPeaks==0) iNumPeaks=1;  /*Find Peak did not think the cluster was good but lets keep it anyway*/
  
  if (Peaks && iNumPeaks>1)
  {
    iRetu = Fit_Peaks(iRows, iCols, iNumPeaks, Peaks, clu);  //fit the deconvoluted peaks and add to the end of the hit table
    if (iRetu==0) return 0;
  }

  return 1;
  // cout<<"Exit Deconvolve Cluster"<<endl;

}

/*------------------------------------------------------------------
ROUTINE:      POINT * Find_peaks
DESCRIPTION:  find peaks within the local array
RETURN VALUE: 
------------------------------------------------------------------*/
POINT* StSvtAnalysis::Find_Peaks (int iRows, int iCols, int *iNumPeaks)
{
  POINT *Array;
  static POINT Peaks[10];       //only allow 10 peaks
  int i, j, k;
  float valley;

  Array = new POINT[(iRows+2) * (iCols+2)];  
  if (Array == NULL)
    {
      *iNumPeaks = 0;
      return NULL;
    }
  
  k = 0;
  for (i=1; i<iRows-1; i++)
    for (j=1; j<iCols-1; j++)
      {
	Array[k].x = i;
	Array[k].y = j;
	Array[k].val = m_Pixels[i][j];
        Array[k].error = 1.;
	k++;
      }

  qsort (Array, k, sizeof (POINT), Compare_Point);  //sort according to peak adc

  /* Now lets go find the peaks */
  
  *iNumPeaks = 0;
  for (i = 0; i < k && *iNumPeaks <10; i++)   //10 is the maximum number of peaks 
    {
      int x = Array[i].x;
      int y = Array[i].y;
   
      /*if (m_Shadow[x][y] == 0 && m_Pixels[x][y] > 10)*/
      if (m_Shadow[x][y] == 0 && m_Pixels[x][y] > 6)          //24 mV. This is the definition of a peak candidate 
	{                                                     //We use mShadow to block out areas already selected for a hit.
	  Peaks[*iNumPeaks].x = x;
	  Peaks[*iNumPeaks].y = y;
	  Peaks[*iNumPeaks].val = Array[i].val;
          Peaks[*iNumPeaks].error = Array[i].error;
	  if ((valley=IsValidPeak (iRows, iCols, Peaks, *iNumPeaks))) //Is it a nicely seperated hit (based on peak/valley)
          {
            Peaks[*iNumPeaks].error = valley;                //the error on the deconv. hits is proportional to how clean they
            *iNumPeaks += 1;                                 //are or how well seperated they are.
            if (*iNumPeaks==2) Peaks[0].error = Peaks[1].error;  /*make sure we change error on first peak*/
          }
          BlockOut (x, y);
	}
      
      /* Make sure that these pixels never get used again */
      
      /*BlockOut ( x, y); moved up SUP*/
    }
  
  delete[] Array;

  return Peaks; 

}

/*------------------------------------------------------------------
ROUTINE:      int Compare_Point
DESCRIPTION:  evaluate the diff between two points
RETURN VALUE: the amplitude difference
------------------------------------------------------------------*/
int Compare_Point ( const void *a, const void *b)
{
  return (int)(static_cast<const POINT*>(b)->val - static_cast<const POINT*>(a)->val);
}

/*------------------------------------------------------------------
  ROUTINE:      long IsValidPeak 
  DESCRIPTION:  tries to determine if a candidate peak is a "real"
  one.  The cut is a peak-to-valley ratio cut.  The p/v ratio is 
  determined between each candidate peak and
  all previously-found peaks by walking the 2-D line between each
  combination of peaks, looking for the 'valley' along the way.
  If a candidate peak passes the p/v cut against all previous peaks
  then it is accepted.
  Changed the search path. Sanjeev 9/99 
  ARGUMENTS:
  RETURN VALUE: 
  ------------------------------------------------------------------*/
float StSvtAnalysis::IsValidPeak(int iRows, int iCols, POINT *Peaks, int iNumPeaks)
{
  int Px = Peaks[iNumPeaks].x, Py = Peaks[iNumPeaks].y, i;
  float pval;
  float  val=0, peak=1, valley=0;
  //float PEAK_TO_VALLEY = 2;

  for (i = 0; i < iNumPeaks; i++)
  {
    int    dx, dy;
    float  fSlope;

    dx = Peaks[i].x - Px;
    dy = Peaks[i].y - Py;
      
    if (dy != 0) 
      fSlope = (float) dx / (float) dy;
    else 
      fSlope = 9999999.;      

    peak = valley = val = (float) m_Pixels[Px][Py];

    //cout<<"Is valid peak: dx: "<<dx<<" dy: "<<dy<<" fslope: "<<fSlope<<" peak: "<<peak<<" Px: "<<Px<<" Py: "<<Py<<endl;

    /*for (; dx > 0; dx--)*/
    while (dx!=0 || dy!=0)
    {
      if (dx!=0) 
        dy = (int)(dx /fSlope);
      else
      {
        if (dy>0) dy--; if (dy<0) dy++;
      }
      val = m_Pixels[Px + dx][Py + dy];
      //cout<<"is valid peak: loop dx: "<<dx<<" dy: "<<dy<<" val: "<<val<<endl;
      if (val<valley && val!=0 && val!=1) valley = val;  /*0 and 1 for dead anodes*/
      if (dx>0) dx--;
      if (dx<0)  dx++;
    }
      
    //cout<<"Is Valid Peak: valley: "<<valley<<endl;

    /*fRatio = (float) peak / (float) valley;
    if (fRatio < PEAK_TO_VALLEY) return 0; */

    //Note we are quite conservative here. Or you could  set this to 50% is you disagree. Sanjeev

    if (valley>0.75*peak) return 0;  /*this must be true for all hits wrt each other*/
    //if (valley>par->cuts[8]*peak) return 0;

  }
 
  //cout<<"IsValidPeak. Valley: "<<valley<<" Peak: "<<peak<<endl; 

  //error set by looking at p/v

  pval = valley/peak;
  if (pval<0.2) 
    pval=1;
  else if (pval>=0.2 && pval<0.5)
    pval=1.25;
  else if (pval>=0.5)
    pval=3*pval;  
  
  return pval;

}

/*------------------------------------------------------------------
  ROUTINE:      long Fit_Peaks
  DESCRIPTION:  Fit the peaks in a merge configuration
  ARGUMENTS:
  RETURN VALUE: 
  ------------------------------------------------------------------*/
int StSvtAnalysis::Fit_Peaks(int iRows, int iCols, int iNumPeaks, POINT *Peaks, int clu)
//Fitthe deconv. clusters. Add them to the end of the cluster table
{
  int     i, j, k, iSum;
  int     iX, iY;
  double  fX, fY;
  int     iSum1;
  float   CovX, CovY;
  float   MomX, MomY;
  int iFirstAnode, iFirstTime;

  mAux[clu].mCluFlag += 128;               //Lets mark this cluster as being deconvoluted  

  CovX = mAux[clu].mCluXCov;                //use these for the deconv. Clusters
  CovY = mAux[clu].mCluYCov;
  MomX = mAux[clu].mSecondMomClusterAnode;
  MomY = mAux[clu].mSecondMomClusterTimeBin;
  
  iFirstAnode = mAux[clu].mCluFirstAnode-1;  //need to knoe relative to where numbers
  iFirstTime  = mAux[clu].mCluFirstTimeBin-1;

  for (i = 0; i < iNumPeaks; i++)
  {
    if (Peaks[i].x < 0 || Peaks[i].x > iRows)   //sanity checks
      continue;
    if (Peaks[i].y < 0 || Peaks[i].y > iCols)
      continue;
      
    iSum1 = iSum = iX = iY = 0;
    for (j = Peaks[i].x - 1; j <= Peaks[i].x + 1; j++)
    {
      for (k = Peaks[i].y - 1; k <= Peaks[i].y + 1; k++)
      {
        iSum1 += m_Pixels[j][k];
        if (m_Pixels[j][k]>0)
        {
          iSum += m_Pixels[j][k];
          iX   += (j) * m_Pixels[j][k];
          iY   += (k) * m_Pixels[j][k];
        }
      }
    }

    if (iSum1 <=0) continue;   /*Should hardly happen*/

    m_clu++;
    if (m_clu>mMaxClu) setMemory();

    fX = (double) iX / iSum;
    fY = (double) iY / iSum;

    mAux[m_clu].mCluID                   = m_clu;                 //new id gets put at end of hit table
    mAux[m_clu].mCluDeconvID             = clu;                   //points to hit which was deconvoluted
    mAux[m_clu].mMeanClusterAnode        = fX + iFirstAnode+0.5;  /*so goes from 0-239  CHECK THIS!!!!!!!!*/
    mAux[m_clu].mMeanClusterTimeBin      = fY + iFirstTime+0.5;   /*so goes from 0-127*/
    mAux[m_clu].mCluPeakAdc              = (int)Peaks[i].val;          //peakADC
    mAux[m_clu].mCluCharge               = iSum;                  //should not be used in de/dx
    mAux[m_clu].mCluFlag                 = 2;                     //says deconvoluted hit
    mAux[m_clu].mSecondMomClusterTimeBin = MomX;                  //Not really accuratly determined
    mAux[m_clu].mSecondMomClusterAnode   = MomY;                  //Not really accuratly determined

    mAux[m_clu].mCluXCov                 = ::sqrt(2.)*CovX*Peaks[i].error; //errors based on origional error scaled by peak/valley
    mAux[m_clu].mCluYCov                 = ::sqrt(2.)*CovY*Peaks[i].error;
    mAux[m_clu].mCluNumPixels            = 9999;   //so dont mess up signal/noise calculation.
    mAux[m_clu].mCluNumAnodes            = 9999;   //ill defined for deconvolution
   
    //cout<<"In Deconvolute: x:" << mAux[m_clu].mMeanClusterAnode << " y: " << mAux[m_clu].mMeanClusterTimeBin 
    //    << " Peak: " << mCluPeakAdc[m_clu] << endl;
    //cout<<"_____________________________"<<endl<<endl;
      
  }  //end loop over peaks
  
  return 1;
}

/*------------------------------------------------------------------
ROUTINE:      int BlockOut
DESCRIPTION:  mark pixel position as blocked
RETURN VALUE: 0
------------------------------------------------------------------*/
int StSvtAnalysis::BlockOut (int x, int y)
//used in deconvolution to mask out areas already considered. Sanjeev
{
  m_Shadow[x-1][y-1] = 1;
  m_Shadow[x-1][ y ] = 1;
  m_Shadow[x-1][y+1] = 1;
  
  m_Shadow[ x ][y-1] = 1;
  m_Shadow[ x ][ y ] = 1;
  m_Shadow[ x ][y+1] = 1;
  
  m_Shadow[x+1][y-1] = 1;
  m_Shadow[x+1][ y ] = 1;
  m_Shadow[x+1][y+1] = 1;
  
  return 0;
}

/*------------------------------------------------------------------
ROUTINE:      int **malloc_matrix
DESCRIPTION:  allocates memory for matrix to do deconvolution
RETURN VALUE: pointer to the array
ARGUMENTS: iRows, iCols  : size of the array needed 
------------------------------------------------------------------*/
int** StSvtAnalysis::malloc_matrix_d (int iRows, int iCols)
{
  int **Array = new int*[iRows];
  int  *buf   = new int[iRows*iCols];
  memset(buf,0,sizeof(int)*iRows*iCols);
  for (int i = 0; i < iRows; i++){Array[i] = buf; buf+=iCols;}
  
  return Array;
}


/*------------------------------------------------------------------
ROUTINE:      void free_matrix
DESCRIPTION:  deallocates memory used by matrix
RETURN VALUE: none
ARGUMENTS:    *Array[] pointer to the array
ARGUMENTS:    iRows    number of rows in the array
------------------------------------------------------------------*/

void StSvtAnalysis::free_matrix_d (int *Array[], int )
{
   delete [] Array[0];
   delete [] Array;
}



/*------------------------------------------------------------------
ROUTINE:      int Fill_Pixel_Array
DESCRIPTION:  fill the pixel array with ADC values
RETURN VALUE: 0
------------------------------------------------------------------*/
int StSvtAnalysis::Fill_Pixel_Array(int clu)
//Fill a 2D array (m_Pixels[i][j]) with the cluster in question to be deconvoluted.
// With pedestaloffset subtracted already
{
  int listAn, mseq, actualAn, stTimeBin, len, Seq;
  unsigned char* adc;
  int iRow, iCol;
  int val;

  m_col_p = 0; 
  m_row_p = 0; 
  m_adc_p = 0;   // ADC of the cluster PEAK, pedoffset subtracted

  mAux[clu].mInfo = mHybridCluster->getCluMemInfo(clu);
  mNumOfMembers        = mHybridCluster->getNumberOfMembers(clu);

  //cout<<"in fill: number of anodes: "<<numAnodes<<endl;

  for(int mem = 0; mem < mNumOfMembers; mem++)            //loop over anodes (can be same anode if curls!!)
  {
    listAn   =  mAux[clu].mInfo[mem].listAnode; 
    mseq     =  mAux[clu].mInfo[mem].seq;
    actualAn =  mAux[clu].mInfo[mem].actualAnode; //actual anode
		  
    mHybridData->getListSequences(listAn,Seq,mSvtSequence);
    stTimeBin  = mSvtSequence[mseq].startTimeBin; 
    adc        = mSvtSequence[mseq].firstAdc;

    len  = mSvtSequence[mseq].length + 1-1;   
    iRow = actualAn  - mAux[clu].mCluFirstAnode   + 1;     //we pad the sides with 0 hence the -1
    iCol = stTimeBin - mAux[clu].mCluFirstTimeBin + 1;

    //cout << "Clu=" << clu<< " Irow=" << iRow << " Actual An=" << actualAn << " iCol=" << iCol <<  " stTimeBucket=" << stTimeBin << " and finally len=" << len << endl;
     
    for (int j=0; j<len; j++) {
      if (iRow<1 || iCol+j<1 || iRow>240 || iCol+j>128) {
	cout<<"Problem in Fill Pixel Array. iRow: " << iRow << " iCol: " << iCol+j<<endl;
        continue;
      }
      val = (int)adc[j];
      (val==0 || val==255) ? val=val : val=val-mPedOffset;    // Note take off the Pedoffset
      m_Pixels[iRow][iCol+j] =val;
      if (val>m_adc_p) {
	m_adc_p=val; 
	m_row_p=iRow;
	m_col_p=iCol+j;
      }
    }
  }
  return 1;
}

int StSvtAnalysis::FillRawAdc()
//Get the raw adc data and fill a 2D array. Usefull if you want to look outside the found sequences
{
  int* anodeList;
  int numAnodes,actualAn,numOfSeq,seqStart,seqLength;
  unsigned char* adc;
  StSequence* svtSequence;
  int ADC;

  //ofstream outFile("SvtHybAll_a.dat",ios::app);
  //if (m_hybIndex!=10 && m_hybIndex!=11) 
  //  outFile<<"Event # "<<m_SvtEvt<<" Hybrid #: "<<m_hybIndex<<endl;

  numAnodes = mHybridRawData->getAnodeList(anodeList);
  //cout<<"Raw number of anodes: "<<numAnodes<<endl;


  for(int an = 0; an < numAnodes; an++) {
    actualAn = anodeList[an];
    if( mSvtBadAnode)
      if(mSvtBadAnode->isBadAnode(actualAn)) continue; //Dont fill pixel array if anode is bad
    mHybridRawData->getListSequences(an,numOfSeq,svtSequence);
    for(int seq = 0; seq < numOfSeq; seq++) {
      seqStart =  svtSequence[seq].startTimeBin;
      seqLength = svtSequence[seq].length;
      adc = svtSequence[seq].firstAdc;
      for (int j=0; j<seqLength; j++) {
        ADC = (int)adc[j];
        (ADC==0 || ADC==255) ? ADC=ADC : ADC=ADC-mPedOffset;   //note subtract the PedOffset
        m_Raw[actualAn][seqStart+j] = ADC;
        //cout<<"Raw ADC: "<<actualAn<<" "<<seqStart+j<<" "<<ADC<<endl;
	/*
        if (m_hybIndex==417){
	  cout << "RAW ADC = " << m_Raw[actualAn][seqStart+j] << "  of anode " << actualAn << " and time bin " << seqStart+j << endl;
	}
	*/
        //if (m_hybIndex!=10 && m_hybIndex!=11) outFile<<setw(4)<<ADC;
      }
    }
    //if (m_hybIndex!=10 && m_hybIndex!=11) outFile<<endl;
  }

  //outFile.close();
  return 1;
}

void StSvtAnalysis::ClearRawAdc()
{
  for (int i1=0;i1<242;i1++) {
    for (int j1=0;j1<130;j1++) {
      m_Raw[i1][j1] = 0;
    }
  }
}


void StSvtAnalysis::SetBadAnTb(int nClus)
//sets hot anodes and timebuckets if they have >4 hits/event.
// coomneted out in StSVtClusteranalysismaker. JT.
{
  int iHyb;
  int iAn, iTb;

  for (int i=0; i<nClus; i++) {
    iHyb = mAux[i].mHybridNum;
    iAn  = (int)(.5+mAux[i].mMeanClusterAnode);
    iTb  = (int)(.5+mAux[i].mMeanClusterTimeBin);
    if( iAn >=1 && iAn < 242){
      if (m_countBadAn[iHyb][iAn]>4) {mAux[i].mCluFlag += 4;}// cout<<"Hot Anodes: "<<iAn<<" Hyb: "<<iHyb<<endl;}
    }
    if( iTb >=0 && iTb < 129){
      if (m_countBadTb[iHyb][iTb]>4) {mAux[i].mCluFlag += 4;}//cout<<"Hot Time: "<<iTb<<" Hyb: "<<iHyb<<endl;}
      //if (iHyb==11) cout<<"Hot Stuff: "<<iAn<<" "<<m_countBadAn[iHyb][iAn]<<" "<<mCluFlag[i]<<endl;
    }
  }

}

void StSvtAnalysis::SetHybIndex(int index)
{
  m_hybIndex = index;
}

int StSvtAnalysis::GetnSvtClu()
{
  return m_clu;
}

int StSvtAnalysis::GetCluID(int clu)
{
  return mAux[clu].mCluID;
}

int StSvtAnalysis::GetCluDeconvID(int clu)
{
  return mAux[clu].mCluDeconvID;
}

int StSvtAnalysis::GetFirstAnode(int clu)
{
return mAux[clu].mCluFirstAnode;
}

int StSvtAnalysis::GetLastAnode(int clu)
{
return mAux[clu].mCluLastAnode;
}

int StSvtAnalysis::GetFirstTimeBin(int clu)
{
return mAux[clu].mCluFirstTimeBin;
}

int StSvtAnalysis::GetLastTimeBin(int clu)
{
return mAux[clu].mCluLastTimeBin;
}


int StSvtAnalysis::GetCluFlag(int clu)
{
    return mAux[clu].mCluFlag;
}

double StSvtAnalysis::GetCluCharge(int clu)
{
return mAux[clu].mCluCharge;
}

int StSvtAnalysis::GetDeconvFlag(int clu)
{
return m_deconv;
}

double StSvtAnalysis::GetMeanClusterAnode(int clu)
{
 return mAux[clu].mMeanClusterAnode;
}

double StSvtAnalysis::GetMeanClusterTimeBin(int clu)
{
 return mAux[clu].mMeanClusterTimeBin;
} 

double StSvtAnalysis::GetSecondMomClusterAnode(int clu)
{
 return mAux[clu].mSecondMomClusterAnode;
}

double StSvtAnalysis::GetSecondMomClusterTimeBin(int clu)
{
 return mAux[clu].mSecondMomClusterTimeBin;
}

double StSvtAnalysis::GetCluXCov(int clu)
{
 return mAux[clu].mCluXCov;
}  

double StSvtAnalysis::GetCluYCov(int clu)
{
 return mAux[clu].mCluYCov;
}

int StSvtAnalysis::GetCluPeakAdc(int clu)
{
 return mAux[clu].mCluPeakAdc;
} 

int StSvtAnalysis::GetCluNumPixels(int clu)
{
 return mAux[clu].mCluNumPixels;
} 

int StSvtAnalysis::GetCluNumAnodes(int clu)
{
 return mAux[clu].mCluNumAnodes;
} 

int StSvtAnalysis::GetTruth(int clu)
{
 return mAux[clu].mTruth;
} 
int StSvtAnalysis::return_oneortwoanode_flag(int clu)
{
 return mAux[clu].m_oneortwo_flag;
} 
void StSvtAnalysis::updateTruth()
  // Calculate the first time bin in the cluster. Selemon
{
  int status , nTru;
  int listAn = 0, mseq = 0, mem;
  StMCTruth *truList;
  
  for(int clu = 0; clu < mNumOfClusters; clu++)
    {
      StSvtAnalysisAux *aux = mAux+clu;
      if (!aux->mInfo) continue;
      mNumOfMembers = mHybridCluster->getNumberOfMembers(clu);
      
      mem = 0;
      StMCPivotTruth pivo(1);
      for(int j = 0; j< mNumOfMembers; j++){
	listAn = aux->mInfo[mem].listAnode;
	mseq   = aux->mInfo[mem].seq;
	status = mHybridData->getListTruth(listAn,nTru,truList);
        if (!truList) return;
	pivo.Add(truList[mseq]);
      } 
      aux->mTruth = int(pivo.Get());
    }
  }


