/***************************************************************************
 *
 * $Id: StSvtAnalysis.cc,v 1.1 2000/07/06 03:50:32 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtAnalysis.cc,v $
 * Revision 1.1  2000/07/06 03:50:32  caines
 * First version of cluster finder and fitter
 *
 **************************************************************************/
#include <iostream.h>
#include "StSequence.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"

#include "StSvtAnalysis.hh"

StSvtAnalysis::StSvtAnalysis()
{
  mHybridData = NULL;
  mHybridCluster = NULL;
  mSvtSequence = NULL;
  mCluFirstAnode = NULL;
  mCluLastAnode = NULL;
  mCluFirstTimeBin = NULL;
  mCluLastTimeBin = NULL;
  mMeanClusterTimeBin = NULL;
  mMeanClusterAnode = NULL;
  mTotalADCCounts = NULL;

}


StSvtAnalysis::~StSvtAnalysis()
{
  delete [] mMeanClusterTimeBin;
  delete [] mMeanClusterAnode;
  delete [] mCluFirstAnode;
  delete [] mCluLastAnode;
  delete [] mCluLastTimeBin;
  delete [] mCluFirstTimeBin;
  delete [] mTotalADCCounts;
} 

void StSvtAnalysis::SetPointers(StSvtHybridData* hybData, StSvtHybridCluster* hybClu)
{
  mHybridData = hybData;
  mHybridCluster = hybClu;

  numOfClusters = mHybridCluster->getNumberOfClusters();
  tempMemberInfo =  new StSvtClusterMemberInfo*[numOfClusters];	 
  mTotalADCCounts = new int[numOfClusters];
  mCluFirstAnode = new int[numOfClusters];
  mCluLastAnode = new int[numOfClusters];
  mCluFirstTimeBin = new int[numOfClusters];
  mCluLastTimeBin = new int[numOfClusters];
  mMeanClusterTimeBin = new double[numOfClusters];
  mMeanClusterAnode = new double[numOfClusters];
  
}

void StSvtAnalysis::FirstAndLastAnodes()
  {
    int actualAn = 0, actualan = 0, mem = 0; 

    for(int clu = 0; clu < numOfClusters; clu++)
     {
      numOfMembers = mHybridCluster->getNumberOfMembers(clu);
      tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);

      if(numOfMembers==1)
        {
         mCluFirstAnode[clu] = tempMemberInfo[clu][mem].actualAnode;
         mCluLastAnode[clu] = mCluFirstAnode[clu];
	}
      else
       {
        for(int j = 1; j<numOfMembers ; j++)
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
        for(int j = 1; j<numOfMembers ; j++)
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
  {
   int status , Seq, SeqStart = 0, seqStart = 0;
   int listAn = 0, mseq = 0, mem;

 

   for(int clu = 0; clu < numOfClusters; clu++)
     {
      tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);
      numOfMembers = mHybridCluster->getNumberOfMembers(clu);

       mem = 0;

      if(numOfMembers==1)
       {
        listAn = tempMemberInfo[clu][mem].listAnode;
        mseq =  tempMemberInfo[clu][mem].seq; 

        status = mHybridData->getListSequences(listAn,Seq,mSvtSequence);
        mCluFirstTimeBin[clu] = mSvtSequence[mseq].startTimeBin;
       }
      else
       {
        for(int j = 1; j< numOfMembers; j++)
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
  { 
    int status , Seq, SeqStart = 0, SeqLength = 0,  SeqEnd = 0, seqEnd = 0;
   int listAn = 0, mseq = 0, mem;

   for(int clu = 0; clu < numOfClusters; clu++)
     {
      tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);
      numOfMembers = mHybridCluster->getNumberOfMembers(clu);

      mem = 0;

      if(numOfMembers==1)
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
         for(int j = 1; j< numOfMembers ; j++)
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

void StSvtAnalysis::MomentAnalysis(int PedOffset)
{
  int listAn , actualAn, numAnodes;
  int mseq, Seq, stTimeBin, len;
  int sumTimeBinTimesAdc ,sumHybAnTimesAdc, sumAdc;
  int* anolist;
  unsigned char* adc;

  //StSequence* svtSequence;

  numAnodes = mHybridData->getAnodeList(anolist);
	
	  
 for(int clu = 0; clu < numOfClusters; clu++)
   {
    tempMemberInfo[clu] = mHybridCluster->getCluMemInfo(clu);
    numOfMembers = mHybridCluster->getNumberOfMembers(clu);
	      
    sumTimeBinTimesAdc = 0;
    sumHybAnTimesAdc = 0;
    sumAdc = 0;
	      
   for(int mem = 0; mem < numOfMembers; mem++)
     {
      listAn = tempMemberInfo[clu][mem].listAnode;
      mseq =  tempMemberInfo[clu][mem].seq;
      actualAn =  tempMemberInfo[clu][mem].actualAnode; //actual anode
		  
      mHybridData->getListSequences(listAn,Seq,mSvtSequence);
      stTimeBin = mSvtSequence[mseq].startTimeBin; 
      len = mSvtSequence[mseq].length;
      adc = mSvtSequence[mseq].firstAdc;
      for(int j = 0; j < len; j++)
	{
	  //cout<<"mAdc"<<"\["<<j<<"] = "<<(int)adc[j]<<endl;
	 sumTimeBinTimesAdc +=(stTimeBin + j)*(int)(adc[j]);
	 sumHybAnTimesAdc += actualAn*(int)adc[j];
	 sumAdc += (int)adc[j]; 
	 mTotalADCCounts[clu] += (int)adc[j]-PedOffset;
	}
		  
      }
	      
      mMeanClusterTimeBin[clu] = (double)sumTimeBinTimesAdc/(double)sumAdc + 0.5;
      mMeanClusterAnode[clu] = (double)sumHybAnTimesAdc/(double)sumAdc + 0.5;
	      
     }



  //cout<<"******* Moment Analysis finished *******"<<endl;
  cout<<"\n";
  
}

void StSvtAnalysis::Report(int index)
{
 
 cout<<"##############################################################"<<endl;
 cout<<"##                                                          ##"<<endl;
 cout<<"## Cluster Analysis Report For hybrid index = "<<index<<"   ##"<<endl;
 cout<<"##                                                          ##"<<endl;
 cout<<"##############################################################"<<endl;
 cout<<"\n";
 
 numOfClusters = mHybridCluster->getNumberOfClusters();

 if(numOfClusters == 0)                     
   {
    cout <<"+++++++++ Clusters found:  None "<<"\n";
    cout<<"\n";
   }

 else {

 for(int clu = 0; clu <  numOfClusters; clu++)
   { 
    numOfMembers = mHybridCluster->getNumberOfMembers(clu);

    cout << "cluster index = "<<' '<< clu <<"\n";
    cout << "Number of Members = " <<' '<<  numOfMembers <<"\n";
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
 
 for( int clu = 0; clu <  numOfClusters; clu++)
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
 delete [] mCluFirstAnode;
 delete [] mCluLastAnode;
 delete [] mCluLastTimeBin;
 delete [] mCluFirstTimeBin;
 delete [] mTotalADCCounts;
}

