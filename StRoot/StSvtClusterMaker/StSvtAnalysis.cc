/***************************************************************************
 *
 * $Id: StSvtAnalysis.cc,v 1.9 2000/10/31 16:20:57 caines Exp $
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
 * $Log: StSvtAnalysis.cc,v $
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
#include <iostream.h>
#include <fstream.h>
#include <iomanip.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "StSequence.hh"
#include "StDAQMaker/StSVTReader.h"
#include "StSvtClassLibrary/StSvtHybridData.hh"

#include "StSvtAnalysis.hh"

int    Compare_Point ( const void *, const void *);   //nedd to declare here for some odd reason.

StSvtAnalysis::StSvtAnalysis(int TotalNumberOfHybrids)
{
  mHybridData              = NULL;
  mHybridRawData           = NULL;
  mHybridCluster           = NULL;
  mSvtSequence             = NULL;

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
  mX_err=72., mY_err=75.;                          //default bin size/sqrt(12)

  setMemory();
  setArrays(TotalNumberOfHybrids);

}

void StSvtAnalysis::setMemory()
 {
  //call all new's here in init and hope we never go above 500. If we do we recall new and make it
  //the larger size. Sanjeev

  tempMemberInfo           = new StSvtClusterMemberInfo*[500];	 

  mCluFlag                 = new int[500];     //heap not the stack to increase speed.
  mCluPeakAdc              = new int[500];     //note new is very slow to I do this here and only
  mCluNumPixels            = new int[500];     //recall if more than 500 clusters/hybrid. Very unlikely
  mCluNumAnodes            = new int[500];     //number of anodes>1 ADC
  mCluCharge               = new double[500];     //ideally should make these static so they are on the 
  mMeanClusterTimeBin      = new double[500];  //Mean Time
  mMeanClusterAnode        = new double[500];  //Mean Anode
  mSecondMomClusterTimeBin = new double[500];  //2nd momemnt Time
  mSecondMomClusterAnode   = new double[500];  //2nd momemnt Anode
  mCluXCov                 = new double[500];  //Cov or error in Anode
  mCluYCov                 = new double[500];  //Cov or error in Time
  mCluFirstAnode           = new int[500];     //First Anode
  mCluLastAnode            = new int[500];     //Last  Anode
  mCluFirstTimeBin         = new int[500];     //First Time
  mCluLastTimeBin          = new int[500];     //Last  Time
  mHybridNum               = new int[500];     //unique hybrid number
  mCluID                   = new int[500];     //unique ID of cluster
  mCluDeconvID             = new int[500];     //points to the parent cluster if deconvoluted

  assert(tempMemberInfo);
  assert(mCluLastTimeBin);          assert(mCluFirstTimeBin);
  assert(mCluFirstAnode);           assert(mCluLastAnode);
  assert(mCluCharge);               assert(mCluFlag);
  assert(mMeanClusterTimeBin);      assert(mMeanClusterAnode); 
  assert(mSecondMomClusterTimeBin); assert(mSecondMomClusterAnode); 
  assert(mCluXCov);                 assert(mCluYCov);
  assert(mCluPeakAdc);              assert(mCluNumPixels);           assert(mCluNumAnodes);
  assert(mHybridNum);               assert(mCluID);                  assert(mCluDeconvID);

 
}

void StSvtAnalysis::setMoreMemory(int numOfClusters)
{
 if (numOfClusters>500)                                //Then we have to allocate more memory.
 {
   mCluFlag                 = new int[numOfClusters];  //then just refill. Will speed up a lot.
   mCluPeakAdc              = new int[numOfClusters];
   mCluNumPixels            = new int[numOfClusters];
   mCluNumAnodes            = new int[numOfClusters];
   mHybridNum               = new int[numOfClusters];
   mCluID                   = new int[numOfClusters];
   mCluDeconvID             = new int[numOfClusters];
   mCluCharge               = new double[numOfClusters];  //shouldn't call these each time. Make static and
   mMeanClusterTimeBin      = new double[numOfClusters];
   mMeanClusterAnode        = new double[numOfClusters];
   mSecondMomClusterTimeBin = new double[numOfClusters];
   mSecondMomClusterAnode   = new double[numOfClusters];
   mCluXCov                 = new double[numOfClusters];
   mCluYCov                 = new double[numOfClusters];

   assert(mCluCharge);               assert(mCluFlag);
   assert(mMeanClusterTimeBin);      assert(mMeanClusterAnode); 
   assert(mSecondMomClusterTimeBin); assert(mSecondMomClusterAnode); 
   assert(mCluXCov);                 assert(mCluYCov);
   assert(mCluPeakAdc);              assert(mCluNumPixels);           assert(mCluNumAnodes);
   assert(mHybridNum);               assert(mCluID);                  assert(mCluDeconvID);
 }	

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
				int NumberOfHybrids, int PedOffset )
// This is how the Maker communicates with this object to tell it where the data is. We also 
// want access to the raw data for the deconvolution and for the one anode hits to get a better
// estimate of the 2nd moments. Sanjeev
{
  mHybridData    = hybAdjData;
  mHybridRawData = hybRawData;
  mHybridCluster = hybClu;
  mPedOffset     = PedOffset;

  mNumOfClusters = mHybridCluster->getNumberOfClusters();
  if (mNumOfClusters>500) {
    tempMemberInfo =  new StSvtClusterMemberInfo*[mNumOfClusters];	 
    assert(tempMemberInfo);
  }

  //this is called for each new event
  for (int i=0; i<NumberOfHybrids; i++) {
    for (int j=0; j<242; j++) m_countBadAn[i][j] = 0;
    for (int j=0; j<129; j++) m_countBadTb[i][j] = 0;
  }

}

void StSvtAnalysis::FirstAndLastAnodes()
  // Calculate the First and last Anodes of all the clusters. Selemon
{
  int actualAn = 0, actualan = 0, mem = 0;
  
  if (mNumOfClusters>500) { 
    mCluFirstAnode = new int[mNumOfClusters];
    mCluLastAnode = new int[mNumOfClusters];
    assert(mCluFirstAnode); assert(mCluLastAnode);
  }
  
  for(int clu = 0; clu < mNumOfClusters; clu++)   
    {
      mem = 0;  //add apr00 SUP
      mNumOfMembers = mHybridCluster->getNumberOfMembers(clu);
      tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);
      
      if(mNumOfMembers==1)
        {
	  mCluFirstAnode[clu] = tempMemberInfo[clu][mem].actualAnode;
	  mCluLastAnode[clu] = mCluFirstAnode[clu];
	}
      else
	{
	  for(int j = 1; j<mNumOfMembers ; j++)
	    {
	      actualAn =  tempMemberInfo[clu][mem].actualAnode;
	      actualan = tempMemberInfo[clu][j].actualAnode;
	      
	      if(actualAn < actualan)
		mCluFirstAnode[clu] = actualAn;
	      else  
		{
		  mCluFirstAnode[clu]= actualan;
		  mem = j;
		}
	    }
	  
	  mem = 0;
	  for(int j = 1; j<mNumOfMembers ; j++)
	    {
	      actualAn = tempMemberInfo[clu][mem].actualAnode;
	      actualan = tempMemberInfo[clu][j].actualAnode;
	      
	      if(actualAn > actualan)
		mCluLastAnode[clu] = actualAn;
	      
	      else  
		{
		  mCluLastAnode[clu] =  actualan;
		  mem = j;
		}
	    }
	  
	}
    }
}


void StSvtAnalysis::CluFirstTimeBin()
// Calculate the first time bin in the cluster. Selemon
  {
   int status , Seq, SeqStart = 0, seqStart = 0;
   int listAn = 0, mseq = 0, mem;

   // StSequence* svtSequence;
   if (mNumOfClusters>500) {
     mCluFirstTimeBin = new int[mNumOfClusters];
     assert(mCluFirstTimeBin);
   }

   for(int clu = 0; clu < mNumOfClusters; clu++)
     {
      tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);
      mNumOfMembers = mHybridCluster->getNumberOfMembers(clu);

       mem = 0;

      if(mNumOfMembers==1)
       {
        listAn = tempMemberInfo[clu][mem].listAnode;
        mseq =  tempMemberInfo[clu][mem].seq; 

        status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
        mCluFirstTimeBin[clu] = mSvtSequence[mseq].startTimeBin;
       }
      else
       {
        for(int j = 1; j< mNumOfMembers; j++)
	 {
          listAn = tempMemberInfo[clu][mem].listAnode;
          mseq =  tempMemberInfo[clu][mem].seq;
          status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
          SeqStart = mSvtSequence[mseq].startTimeBin; 

          listAn = tempMemberInfo[clu][j].listAnode;
          mseq =  tempMemberInfo[clu][j].seq;
          status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
          seqStart = mSvtSequence[mseq].startTimeBin;

          if(SeqStart <= seqStart)
              mCluFirstTimeBin[clu] = SeqStart;
          else  
            {
              mCluFirstTimeBin[clu] = seqStart;
              mem = j;
            }
	 }
      }

     //cout << mCluFirstTimeBin[clu] <<endl;

     }
 
  }


void StSvtAnalysis::CluLastTimeBin()
//Calculate last time bin in cluster. Selemon.
  { 
   int status , Seq, SeqStart = 0, SeqLength = 0,  SeqEnd = 0, seqEnd = 0;
   int listAn = 0, mseq = 0, mem;

   //StSequence* svtSequence;
   if (mNumOfClusters>500) {
     mCluLastTimeBin = new int[mNumOfClusters];
     assert(mCluLastTimeBin);
   }

   for(int clu = 0; clu < mNumOfClusters; clu++)
     {
      tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);
      mNumOfMembers = mHybridCluster->getNumberOfMembers(clu);

      mem = 0;

      if(mNumOfMembers==1)
       {
        listAn = tempMemberInfo[clu][mem].listAnode;
        mseq = tempMemberInfo[clu][mem].seq;
        status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
        SeqStart =  mSvtSequence[mseq].startTimeBin;
        SeqLength = mSvtSequence[mseq].length;
        SeqEnd =  SeqStart + SeqLength - 1; 
        mCluLastTimeBin[clu] = SeqEnd; 
        }
     else
      {
         for(int j = 1; j< mNumOfMembers ; j++)
	   {
            listAn = tempMemberInfo[clu][mem].listAnode;
            mseq = tempMemberInfo[clu][mem].seq;
            status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);

            SeqStart =  mSvtSequence[mseq].startTimeBin;
            SeqLength = mSvtSequence[mseq].length;
            SeqEnd =  SeqStart + SeqLength - 1;

            listAn = tempMemberInfo[clu][j].listAnode;
            mseq = tempMemberInfo[clu][j].seq;
            status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);

            SeqStart =  mSvtSequence[mseq].startTimeBin;
            SeqLength = mSvtSequence[mseq].length;
            seqEnd =  SeqStart + SeqLength - 1; 

            if(SeqEnd > seqEnd)
               mCluLastTimeBin[clu] = SeqEnd;
            else  
             {
               mCluLastTimeBin[clu] = seqEnd;
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

 FillRawAdc();                                         //Put the raw adcs for the whole hybrid into an 2D
                                                       //Array. If there is no info value is 0.	 

if(mNumOfClusters > 500)
  setMoreMemory(mNumOfClusters);
  
 m_clu = mNumOfClusters-1;                              //keep track of # deconcoluted clusters. 

 for(int clu = 0; clu < mNumOfClusters; clu++)          //loop over all clusters for one hybrid 
   calcMoments(clu);


 m_clu++;   //make equal to mNumOfClusters     

 ClearRawAdc();                                              //have to reset the raw adc's since the number of anodes 
                                                             //returned for each hybrid will not be the same and are not
                                                             //guareented to be 240.
}


void StSvtAnalysis::calcMoments(int clu)
{
 int listAn , actualAn, numAnodes;
 int mseq, Seq, stTimeBin, len, ADC = 0;
 unsigned char* adc;

 mNumPixels = 0, mPeakADC = 0,mSumAdc = 0;
 mDriftMom1 = 0, mAnodeMom1 = 0, mDriftMom2 = 0, mAnodeMom2 = 0, mMom0 = 0, mNeff = 0;

 int igt3=0, peakPosAn=0, peakPosTim=0, peakMem=0, peakPixel=0;                         //recall 1ADC = 4mV

 tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);
 mNumOfMembers        = mHybridCluster->getNumberOfMembers(clu);   //I guess this is the number of anodes
	     
 numAnodes = GetLastAnode(clu)-GetFirstAnode(clu)+1;     //this is not the same as mNumOfMembers for clusters while curl around!!

 for(int mem = 0; mem < mNumOfMembers; mem++)         //loop over anodes
   {
     listAn   =  tempMemberInfo[clu][mem].listAnode;   //what is this??
     mseq     =  tempMemberInfo[clu][mem].seq;
     actualAn =  tempMemberInfo[clu][mem].actualAnode; //actual anode
		  
     mHybridData->getListSequences(listAn,Seq,mSvtSequence);
     stTimeBin = mSvtSequence[mseq].startTimeBin; 
     len = mSvtSequence[mseq].length;
     adc = mSvtSequence[mseq].firstAdc;
     //numPixels += len;  move out since i now have undershoot and stuff here
     for(int j = 0; j < len; j++)       //loop over time in one sequence
     {
       //cout<<"mAdc"<<"\["<<j<<"] = "<<(int)adc[j]<<endl;
       ADC = (int)adc[j];
       (ADC==0 || ADC==255) ? ADC=ADC : ADC=ADC-mPedOffset;   //check this. I subtract mPedOffset to get rid of arbittary offset. Have to
                                                       //do this else we distort the means of small clusters.
       if (mPeakADC<ADC) { mPeakADC=ADC; peakPosAn=actualAn; peakPosTim=stTimeBin+j; peakMem=mem; peakPixel=j;}

       if (ADC>1 && ADC<4000 && (stTimeBin+j)>=0 && (stTimeBin+j)<128 && actualAn>0 && actualAn<=240)
       {
         mNumPixels++;                                  //calculate the various moments
         if (ADC>3) igt3++;
         mDriftMom1 += ADC * (stTimeBin + j + 0.5);
         mAnodeMom1 += ADC * (actualAn + 0.5);
         mDriftMom2 += ADC * (stTimeBin + j + 0.5) * (stTimeBin + j + 0.5);
         mAnodeMom2 += ADC * (actualAn + 0.5) * (actualAn + 0.5);
         mNeff       += ADC * ADC;
         mSumAdc     += ADC;
	 //cout<<"values: "<<ADC<<" "<<actualAn<<" "<<mAnodeMom2<<endl;
       } 
       else 
       {
         //mHitId += -1;
         //cout<<"You have funny SVT ADC or timebin or anodes numbers in this cluster"<<endl;
       }
     }	  
   }  //end loop over sequeces from 1 cluster


 if(numAnodes == 1 || numAnodes == 2)
   oneOrTwoAnodeMoments(clu, peakPosTim);

 finalMoments(clu, numAnodes);
 newCluster(clu, numAnodes,igt3);
	

}

void StSvtAnalysis::oneOrTwoAnodeMoments(int clu, int peakPosTim)
{     
   //make the 2nd moments better for 1 anode hits. Look a little to the right of them for better calc. Sanjeev

   int iAst, iAend, ADC;
   iAst = GetFirstAnode(clu); iAend = GetLastAnode(clu);

   if (iAst>1 && iAend<240 && peakPosTim>0 && peakPosTim<127){    /*do a better job for the 1 anode hits*/
     for (int i=iAst-1; i<=iAend+1; i++)                 /*This is dangerous as we do it blindly*/
       {                                                 /*It will mess up for a small fraction of hits*/ 
        if (i==iAst || i==iAend)  continue;               /*when another cluster is close by*/ 
        for (int j=peakPosTim-1; j<=peakPosTim+1; j++)
         {    
          ADC = m_Raw[i][j];
          if (ADC>0 && ADC<mPeakADC/2)                                      /*since we do this blindly lets at least put*/ 
          {                                                                /*some safety checks in*/ 
           mSumAdc     += ADC;
           mDriftMom1 += ADC * (j+0.5);
           mAnodeMom1 += ADC * (i+0.5);
           mDriftMom2 += ADC * (j+0.5) * (j+0.5);
           mAnodeMom2 += ADC * (i+0.5) * (i+0.5);
           mNeff       += ADC * ADC;
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
     mDriftMom2 = sqrt((double)mDriftMom2/mSumAdc - mDriftMom1*mDriftMom1); /*note no N/N-1*/
     mAnodeMom2 = sqrt((double)mAnodeMom2/mSumAdc - mAnodeMom1*mAnodeMom1);
     if (mDriftMom2>1000 || mDriftMom2<0 || mAnodeMom2>1000 || mAnodeMom2<0)   /*calc fails occasionally*/
     {
       mDriftMom2 = -999; mAnodeMom2 = -999; mNeff = 0;
     }

     if (mNeff>1)                                                     //the ones which are not are really bad 
     {
       mNeff = (double) mSumAdc*mSumAdc/mNeff;  
       if (mNeff>1.5)                                                 //make sure calc. does not mess up
       {
         mDriftMom2 = mDriftMom2 * sqrt( (mNeff/(mNeff-1)) );
         mAnodeMom2 = mAnodeMom2 * sqrt( (mNeff/(mNeff-1)) );
         if (mDriftMom2!=0) mY_err = 260.e-4*mDriftMom2/sqrt(mNeff); /*is 0 for 1 drift wonders. Units: cm*/
         if (mAnodeMom2!=0) mX_err = 250.e-4*mAnodeMom2/sqrt(mNeff); /*is 0 for 1 anode wonders. Units: cm*/
       }
   
       if (numAnodes==1 && mAnodeMom2==0) mAnodeMom2=0.288;    //bin width/sqrt(12)
       if ( (GetLastTimeBin(clu)-GetFirstTimeBin(clu))==0 && mDriftMom2==0) mDriftMom2=0.288;  //bin width/sqrt(12)

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

   //These are all the objects used by the Maker to make the cluster object later.
   mCluID[clu]                   = clu;            //unique ID
   mCluDeconvID[clu]             = clu;            //at this point the origional cluster and deconv. one are the same
   mCluCharge[clu]               = mMom0;          //charge
   mCluFlag[clu]                 = mHitId;         //this might be modified below. cafefull
   mMeanClusterTimeBin[clu]      = mDriftMom1;     //mean time
   mMeanClusterAnode[clu]        = mAnodeMom1;     //mean anode
   mSecondMomClusterTimeBin[clu] = mDriftMom2;     //mean 2nd moment time
   mSecondMomClusterAnode[clu]   = mAnodeMom2;     //mean 2nd moment anode
   mCluXCov[clu]                 = 1.*mX_err*mX_err; //error in Anode
   mCluYCov[clu]                 = 4.*mY_err*mX_err; //error in Time is made arbitarily 2 time larger (4 time for cov). Check dist.
   mCluPeakAdc[clu]              = mPeakADC;        //peak ADC
   mCluNumPixels[clu]            = mNumPixels;      //number of pixels
   mCluNumAnodes[clu]            = numAnodes;      //number of anodes including loop backs.
   mHybridNum[clu]               = m_hybIndex;     //hybrid index.



   for (int i1=0;i1<iRows;i1++)        //be more clever in the futre but for now....
    {                                 //zero out previous box
     for (int j1=0;j1<iCols;j1++)
     {
      m_Pixels[i1][j1] = 0;            //used by deconvolution
      m_Shadow[i1][j1] = 0;
     }  
   }


   //size of search area for deconvolution
   iRows = mCluLastAnode[clu] - mCluFirstAnode[clu] +1+2;      //+2 to account for 0 paddind around side
   iCols = mCluLastTimeBin[clu] -  mCluFirstTimeBin[clu] +1+2;

   Fill_Pixel_Array(clu);                                      //Fill 2D array with ADC's. Easier for the search in this format.
   iQual = CatagorizeCluster(iRows,iCols,igt3,clu);            //Classify the cluster as good or bad.
   mHitId += iQual;                                            //Update previous classification
   mCluFlag[clu] = mHitId;                                     //Set flag
   if (mHitId<4 && m_deconv==1) iRetu = Deconvolve_Cluster(iRows, iCols, clu);   //do the deconvolution only for some candidates
   // Print_Pixels(iRows, iCols, clu);                         //creates nice picture of cluster with 0's padding the edges
      
   //if (hit_id<4) {                                        //only good hits. NO NO. Look at all hits.
   if( (int)(.5+mAnodeMom1) > 0 && (int)(.5+mAnodeMom1) <= 241 )
     m_countBadAn[m_hybIndex][(int)(.5+mAnodeMom1)]++;         //count the noisy anodes and time for all hits whether good or bad
     if(  (int)(.5+mDriftMom1) >= 0 && (int)(.5+mDriftMom1) <= 128)
     m_countBadTb[m_hybIndex][(int)(.5+mDriftMom1)]++;         //invstigate this.
 
     //}  


  //cout<<"******* Moment Analysis finished *******"<<endl;
  
}

//now the deconvolution stuff

//typedef struct POINT_TYPE
//{
//  long  x,y;  
//  float val;
//  float error;
//}  POINT;

/* Classify Cluster by topology                       */

int StSvtAnalysis:: CatagorizeCluster(int iRows, int iCols, int igt3, int clu)
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

  iNumCat++;
  m_deconv = 0;                     //default is no deconvolution;

  fThres = -0.03*(float)m_adc_p;                        /*loop over central anode in time for undershoot*/
  iUnderBkt = 127;                                      /*if eq 0 or 61 then problem*/ 
  i = m_col_p;                                             /*NOTE SCG does not save much of the undershoot so*/
  while (iUnderBkt==127 && i!=iCols)                    /*this is not so sensitive. Good for deconvolution*/
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

  if (iWrgBkt>=1)                                                { iQual += 16;  m_nWrkBkt++; }
  if (igt3<3 && (mCluCharge[clu]<20 || mCluNumPixels[clu]<4))    { iQual += 32;  m_nGt8++;    }
  if ( (mSecondMomClusterTimeBin[clu]>4*mSecondMomClusterAnode[clu] && 
        mSecondMomClusterAnode[clu]<1 &&mSecondMomClusterAnode[clu]>0) ||
       (mSecondMomClusterAnode[clu]>4*mSecondMomClusterTimeBin[clu] &&
        mSecondMomClusterTimeBin[clu]<1 && mSecondMomClusterTimeBin[clu]>0) )  
                                                                 { iQual += 64; m_nSig++;    }

  //look for deconvolution candidates

  if (iUnderBkt>m_col_p-2)                                       { d_bkt = 1;     m_nUndBkt++; } //send to deconV 
  fCut = fabs(1.15*mSecondMomClusterTimeBin[clu] - mSecondMomClusterAnode[clu]);
  if ( (fCut<=0.5 && mSecondMomClusterAnode[clu]<1.25 && mSecondMomClusterTimeBin[clu]<1.25) ||
       (mSecondMomClusterAnode[clu]<0.5 &&  mSecondMomClusterTimeBin[clu]<0.5))
    d_sig = 0;   //too lazy to do the inverse of the above
  else
    d_sig = 1;

  if (mCluNumAnodes[clu]>1 &&  (d_sig==1 || (mCluCharge[clu]>50 && d_bkt==1))) m_deconv = 1;

  if (iNumCat%50 == 1) cout<<"Wrong Clusters: iGt8: "<<m_nGt8<<" WrgBkt: "<<m_nWrkBkt<<" Sig: "<<m_nSig<<endl;  

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

  Array = (POINT *) new POINT[(iRows+2) * (iCols+2)];  
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
      if (m_Shadow[x][y] == 0 && m_Pixels[x][y] > 6)          //24 mV. 
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
  int  iX, iY;
  double  fX, fY;
  int     iSum1;
  float   CovX, CovY;
  float   MomX, MomY;
  int iFirstAnode, iFirstTime;
  
  mCluFlag[clu] += 128;               //Lets mark this cluster as being deconvoluted  

  CovX = mCluXCov[clu];                //use these for the deconv. Clusters
  CovY = mCluYCov[clu];
  MomX = mSecondMomClusterAnode[clu];
  MomY = mSecondMomClusterTimeBin[clu];
  
  iFirstAnode = mCluFirstAnode[clu]-1;  //need to knoe relative to where numbers
  iFirstTime  = mCluFirstTimeBin[clu]-1;

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
    if (m_clu>499) {
      m_clu = 499;
      cout<<"Only enough space for 500 clusters so no deconvolution"<<endl;
      mCluFlag[clu] -= 128;   //make good again
      return 0;
    }

    fX = (double) iX / iSum;
    fY = (double) iY / iSum;

    mCluID[m_clu]                   = m_clu;                 //new id gets put at end of hit table
    mCluDeconvID[m_clu]             = clu;                   //points to hit which was deconvoluted
    mMeanClusterAnode[m_clu]        = fX + iFirstAnode+0.5;  /*so goes from 0-239  CHECK THIS!!!!!!!!*/
    mMeanClusterTimeBin[m_clu]      = fY + iFirstTime+0.5;   /*so goes from 0-127*/
    mCluPeakAdc[m_clu]              = (int)Peaks[i].val;          //peakADC
    mCluCharge[m_clu]               = iSum;                  //should not be used in de/dx
    mCluFlag[m_clu]                 = 2;                     //says deconvoluted hit
    mSecondMomClusterTimeBin[m_clu] = MomX;                  //Not really accuratly determined
    mSecondMomClusterAnode[m_clu]   = MomY;                  //Not really accuratly determined
    mCluXCov[m_clu]                 = sqrt(2.)*CovX*Peaks[i].error; //errors based on origional error scaled by peak/valley
    mCluYCov[m_clu]                 = sqrt(2.)*CovY*Peaks[i].error;
    mCluNumPixels[m_clu]            = 9999;   //so dont mess up signal/noise calculation.
    mCluNumAnodes[m_clu]            = 9999;   //ill defined for deconvolution
   
    //cout<<"In Deconvolute: x:" << mMeanClusterAnode[m_clu] << " y: " << mMeanClusterTimeBin[m_clu] 
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
  int     **Array;
  int       i, j;
  
  //cout<<"Enter malloc"<<endl;

  Array = (int**) new int[iRows];
  if (Array == NULL) return NULL;
  
  for (i = 0; i < iRows; i++)
  {
    Array[i] = (int*) new int[iCols];
    if (Array[i] == NULL)
    {
      cout<<"Error allocating memory for pixel array!"<<endl;
      while (--i >= 0) delete [] Array[i];
      delete [] Array;
      break;
    }
      
    for (j=0; j<iCols; j++) Array[i][j] = 0;
  }
  
  return Array;
}


/*------------------------------------------------------------------
ROUTINE:      void free_matrix
DESCRIPTION:  deallocates memory used by matrix
RETURN VALUE: none
ARGUMENTS:    *Array[] pointer to the array
ARGUMENTS:    iRows    number of rows in the array
------------------------------------------------------------------*/

void StSvtAnalysis::free_matrix_d (int *Array[], int iRows)
{
  int i;
  for (i = iRows - 1; i >= 0; i--)
    if (Array[i]) delete [] Array[i];
  delete [] Array;
}


/*------------------------------------------------------------------
ROUTINE:      long Print_Pixels
DESCRIPTION:  debugging tool to print pixel information
RETURN VALUE: 
------------------------------------------------------------------*/
int StSvtAnalysis::Print_Pixels (int iRows, int iCols, int clu)
//Prints a cluster and pads the sides with 0's. You can uncomment some of the code to
//write the output to a file.
{
  int i, j, val;
  int iWriteFile=1;

  cout<<" first An: "<<mCluFirstAnode[clu]<<" last An: "<<mCluLastAnode[clu]     << "\t\t" 
      <<" first tb: "<<mCluFirstTimeBin[clu]<<" last tb: "<<mCluLastTimeBin[clu] << "\t"
      <<" numPix: "<<mCluNumPixels[clu]
      <<" flag: "<<mCluFlag[clu]<<" Index: "<<m_hybIndex<<endl;
  cout<<" mean X: "<<mMeanClusterAnode[clu]<<" mean Y: "<<mMeanClusterTimeBin[clu] << "\t" 
      <<" sigX: "<<mSecondMomClusterAnode[clu]<<" sigY: "<<mSecondMomClusterTimeBin[clu]<<endl;
 
  for (j = 0; j < iCols; j++) {
    for (i = 0; i < iRows; i++) {
      val = (int)m_Pixels[i][j];
      cout<<setw(4)<<val;     //already subtracted the pedOffset at this point in fill_Pixels
    }
    cout<<"\n";
  }
    cout<<"\n";


  if (iWriteFile==1) {
    ofstream outGood("SvtHybGood_a.dat",ios::app);
    ofstream outBad("SvtHybBad_a.dat",ios::app);

    //if(m_hybIndex!=10 && m_hybIndex!=11) {
    if (mCluFlag[clu]<4 || mCluFlag[clu]==128) {
      outGood<<" first An: "<<mCluFirstAnode[clu]<<" last An: "<<mCluLastAnode[clu]     << "\t\t" 
             <<" first tb: "<<mCluFirstTimeBin[clu]<<" last tb: "<<mCluLastTimeBin[clu] << "\t" 
             <<" numPix: "<<mCluNumPixels[clu]
             <<" flag: "<<mCluFlag[clu]<<" Index: "<<m_hybIndex<<" Event #: "<<m_SvtEvt<<endl;
      outGood<<" mean X: "<<mMeanClusterAnode[clu]<<" mean Y: "<<mMeanClusterTimeBin[clu] << "\t" 
             <<" sigX: "<<mSecondMomClusterAnode[clu]<<" sigY: "<<mSecondMomClusterTimeBin[clu]<<endl;
    }
    else {
      outBad<<" first An: "<<mCluFirstAnode[clu]<<" last An: "<<mCluLastAnode[clu]     << "\t\t" 
            <<" first tb: "<<mCluFirstTimeBin[clu]<<" last tb: "<<mCluLastTimeBin[clu] << "\t" 
            <<" numPix: "<<mCluNumPixels[clu]
            <<" flag: "<<mCluFlag[clu]<<" Index: "<<m_hybIndex<<" Event #: "<<m_SvtEvt<<endl;
      outBad<<" mean X: "<<mMeanClusterAnode[clu]<<" mean Y: "<<mMeanClusterTimeBin[clu] << "\t" 
            <<" sigX: "<<mSecondMomClusterAnode[clu]<<" sigY: "<<mSecondMomClusterTimeBin[clu]<<endl;
    }
    //}

    for (j = 0; j < iCols; j++) {
      for (i = 0; i < iRows; i++) {
        val = (int)m_Pixels[i][j];
        //if(m_hybIndex!=10 && m_hybIndex!=11) {
        if (mCluFlag[clu]<4 || mCluFlag[clu]==128) 
          outGood<<setw(4)<<val;
        else
          outBad<<setw(4)<<val;
        //}
      }
      //if(m_hybIndex!=10 && m_hybIndex!=11) {
      if (mCluFlag[clu]<4 || mCluFlag[clu]==128) 
        outGood<<endl;
      else
        outBad<<endl;
      //}
    }
    //if(m_hybIndex!=10 && m_hybIndex!=11) {
    if (mCluFlag[clu]<4 || mCluFlag[clu]==128) 
      outGood<<endl;
    else
      outBad<<endl;
    //}
  
    outGood.close();
    outBad.close();

  }  //end of iWriteFile
  
 
  return 1;
}

/*------------------------------------------------------------------
ROUTINE:      int Fill_Pixel_Array
DESCRIPTION:  fill the pixel array with ADC values
RETURN VALUE: 0
------------------------------------------------------------------*/
int StSvtAnalysis::Fill_Pixel_Array(int clu)
//Fill a 2D array with the cluster in question to be deconvoluted.
{
  int listAn, mseq, actualAn, stTimeBin, len, Seq;
  unsigned char* adc;
  int iRow, iCol;
  int val;

  m_col_p = 0; m_row_p = 0; m_adc_p = 0;

  tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);
  mNumOfMembers        = mHybridCluster->getNumberOfMembers(clu);

  //cout<<"in fill: number of anodes: "<<numAnodes<<endl;

  for(int mem = 0; mem < mNumOfMembers; mem++)            //loop over anodes (can be same anode if curls!!)
  {
    listAn   =  tempMemberInfo[clu][mem].listAnode; 
    mseq     =  tempMemberInfo[clu][mem].seq;
    actualAn =  tempMemberInfo[clu][mem].actualAnode; //actual anode
		  
    mHybridData->getListSequences(listAn,Seq,mSvtSequence);
    stTimeBin  = mSvtSequence[mseq].startTimeBin; 
    adc        = mSvtSequence[mseq].firstAdc;

    len  = mSvtSequence[mseq].length + 1-1;   
    iRow = actualAn - mCluFirstAnode[clu] + 1;     //we pad the sides with 0 hence the -1
    iCol = stTimeBin - mCluFirstTimeBin[clu] + 1;

    //cout << "Clu=" << clu<< " Irow=" << iRow << " Actual An=" << actualAn << " iCol=" << iCol <<  " stTimeBucket=" << stTimeBin << " and finally len=" << len << endl;
     
    for (int j=0; j<len; j++) {
      if (iRow<1 || iCol+j<1 || iRow>240 || iCol+j>128) {
	cout<<"Problem in Fill Pixel Array. iRow: " << iRow << " iCol: " << iCol+j<<endl;
        continue;
      }
      //cout<<"values: " <<iRow<<" "<<iCol+j<<" "<<(int)adc[j]<<endl;
      val = (int)adc[j];
      (val==0 || val==255) ? val=val : val=val-mPedOffset;    // Note take off the Pedoffset
      m_Pixels[iRow][iCol+j] =val;
      if (val>m_adc_p) {m_adc_p=val;m_row_p=iRow;m_col_p=iCol+j;}
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
{
  int iHyb;
  int iAn, iTb;

  for (int i=0; i<nClus; i++) {
    iHyb = mHybridNum[i];
    iAn  = (int)(.5+mMeanClusterAnode[i]);
    iTb  = (int)(.5+mMeanClusterTimeBin[i]);
    if( iAn >=1 && iAn < 242){
      if (m_countBadAn[iHyb][iAn]>4) {mCluFlag[i] += 4;}// cout<<"Hot Anodes: "<<iAn<<" Hyb: "<<iHyb<<endl;}
    }
    if( iTb >=0 && iTb < 129){
      if (m_countBadTb[iHyb][iTb]>4) {mCluFlag[i] += 4;}//cout<<"Hot Time: "<<iTb<<" Hyb: "<<iHyb<<endl;}
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
  return mCluID[clu];
}

int StSvtAnalysis::GetCluDeconvID(int clu)
{
  return mCluDeconvID[clu];
}

int StSvtAnalysis::GetFirstAnode(int clu)
{
return mCluFirstAnode[clu];
}

int StSvtAnalysis::GetLastAnode(int clu)
{
return mCluLastAnode[clu];
}

int StSvtAnalysis::GetFirstTimeBin(int clu)
{
return mCluFirstTimeBin[clu];
}

int StSvtAnalysis::GetLastTimeBin(int clu)
{
return mCluLastTimeBin[clu];
}


int StSvtAnalysis::GetCluFlag(int clu)
{
  if( mCluFlag[clu] < 0 || mCluFlag[clu] > 255){
    return 255;
  }
  else{
    return mCluFlag[clu];
  }
}


double StSvtAnalysis::GetCluCharge(int clu)
{
return mCluCharge[clu];
}

double StSvtAnalysis::GetMeanClusterAnode(int clu)
{
 return mMeanClusterAnode[clu];
}

double StSvtAnalysis::GetMeanClusterTimeBin(int clu)
{
 return mMeanClusterTimeBin[clu];
} 

double StSvtAnalysis::GetSecondMomClusterAnode(int clu)
{
 return mSecondMomClusterAnode[clu];
}

double StSvtAnalysis::GetSecondMomClusterTimeBin(int clu)
{
 return mSecondMomClusterTimeBin[clu];
}

double StSvtAnalysis::GetCluXCov(int clu)
{
 return mCluXCov[clu];
}  

double StSvtAnalysis::GetCluYCov(int clu)
{
 return mCluYCov[clu];
}

int StSvtAnalysis::GetCluPeakAdc(int clu)
{
 return mCluPeakAdc[clu];
} 

int StSvtAnalysis::GetCluNumPixels(int clu)
{
 return mCluNumPixels[clu];
} 

int StSvtAnalysis::GetCluNumAnodes(int clu)
{
 return mCluNumAnodes[clu];
} 

void StSvtAnalysis::Report(int index)
{
 
 cout<<"##############################################################"<<endl;
 cout<<"##                                                          ##"<<endl;
 cout<<"## Cluster Analysis Report For hybrid index = "<<index<<"   ##"<<endl;
 cout<<"##                                                          ##"<<endl;
 cout<<"##############################################################"<<endl;
 cout<<"\n";
 
 mNumOfClusters = mHybridCluster->getNumberOfClusters();

 if(mNumOfClusters == 0)                     
   {
    cout <<"+++++++++ Clusters found:  None "<<"\n";
    cout<<"\n";
   }

 else {

 for(int clu = 0; clu <  mNumOfClusters; clu++)
   { 
    mNumOfMembers = mHybridCluster->getNumberOfMembers(clu);

    cout << "cluster index = "<<' '<< clu <<"\n";
    cout << "Number of Members = " <<' '<<  mNumOfMembers <<"\n";
    cout << "First anode number =" <<' '<<mCluFirstAnode[clu]<<"\n";
    cout << "Last anode number ="<<' '<<mCluLastAnode[clu]<<"\n";
    cout << "minimum time bucket =" <<' '<<mCluFirstTimeBin[clu]<<"\n";
    cout << "maximum timebucket ="<<' '<<mCluLastTimeBin[clu]<<"\n";
    cout<< endl;
    
    cout<<"****************************************************"<<"\n";
    cout<<"\n";
   }
 cout<<"****************************************************"<<endl;
 cout<<"------- Moment Analysis Results for hybrid "<<index<<" ------"<<endl;
 cout<<"****************************************************"<<endl;
 
 for( int clu = 0; clu <  mNumOfClusters; clu++)
   {
    cout<<"mAverageHybAn"<<"["<<clu<<"] ="<<mMeanClusterAnode[clu]<<endl;
    cout<<"mAverageTimeBin"<<"["<<clu<<"] ="<<mMeanClusterTimeBin[clu]<<endl;
   }

  cout<<"--------------------------------------------------------"<<endl;
 }

}

void StSvtAnalysis::ResetMeanValues()
{
 delete [] mMeanClusterTimeBin;
 delete [] mMeanClusterAnode;
 delete [] mSecondMomClusterTimeBin;
 delete [] mSecondMomClusterAnode;
 delete [] mCluFirstAnode;
 delete [] mCluLastAnode;
 delete [] mCluLastTimeBin;
 delete [] mCluFirstTimeBin;
 delete [] mCluCharge;
 delete [] mCluFlag;
 delete [] mCluXCov;
 delete [] mCluYCov;
 delete [] mCluPeakAdc;
 delete [] mCluNumPixels;
 delete [] mCluNumAnodes;
}

