 /***************************************************************************
 *
 * $Id: StSvtClusterFinder.cc,v 1.10 2005/07/23 03:37:33 perev Exp $
 *
 * Author: Selemon Bekele
 ***************************************************************************
 *
 * Description: SVT Cluster Finder Class
 *
 ***************************************************************************
 *
 * $Log: StSvtClusterFinder.cc,v $
 * Revision 1.10  2005/07/23 03:37:33  perev
 * IdTruth + Cleanup
 *
 * Revision 1.9  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.8  2003/01/28 20:28:18  munhoz
 * new filters for clusters
 *
 * Revision 1.7  2001/04/25 18:38:28  perev
 * HPcorrs
 *
 * Revision 1.6  2000/11/30 20:43:17  caines
 * Use database
 *
 * Revision 1.5  2000/10/31 16:19:37  caines
 * Improved speed of zeroing arrays
 *
 * Revision 1.4  2000/08/21 13:06:58 bekele
 * Improved restting arrays, cluster finder
 *  now runs faster.
 *
 * $Log: StSvtClusterFinder.cc,v $
 * Revision 1.10  2005/07/23 03:37:33  perev
 * IdTruth + Cleanup
 *
 * Revision 1.9  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.8  2003/01/28 20:28:18  munhoz
 * new filters for clusters
 *
 * Revision 1.7  2001/04/25 18:38:28  perev
 * HPcorrs
 *
 * Revision 1.6  2000/11/30 20:43:17  caines
 * Use database
 *
 * Revision 1.5  2000/10/31 16:19:37  caines
 * Improved speed of zeroing arrays
 *
 * Revision 1.4  2000/08/21 13:06:58  caines
 * Much improved hit finding and fitting
 *
 * Revision 1.3  2000/07/16 22:32:54  caines
 * Fills spacepoint table correctly
 *
 * Revision 1.2  2000/07/13 14:50:49  caines
 * Improvements on not saving single pixels
 *
 * Revision 1.1  2000/07/06 03:50:33  caines
 * First version of cluster finder and fitter
 *
 **************************************************************************/

#include <Stiostream.h>
#include "StSvtClusterFinder.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSequence.hh"

//ClassImp(StSvtClusterFinder)

StSvtClusterFinder::StSvtClusterFinder()
{

hybdata = NULL;
sequence = NULL;
mAnolist = NULL;
cluIndex = 0;
mNumOfAnodes = 0;
mSequence = 0;


int i,j;

for ( i=0; i<8000; i++)
 for ( j=0; j<500; j++)
  mCluster[i][j] = 0;

for ( i=0; i<240; i++)
 {
  mNumSeq[i] = 0;
  for ( j=0; j<128; j++)
   mSeqFlag[i][j] = 0;
 }

for ( i=0; i<500; i++)
 {mContainer1[i] = 0;
  mContainer2[i] = 0;}

 }


StSvtClusterFinder::~StSvtClusterFinder()
{ 
}

void StSvtClusterFinder::setHybridPointer(StSvtHybridData* hdata)
{
  hybdata = hdata;
  mNumOfAnodes = hdata->getAnodeList(mAnolist);
}


void StSvtClusterFinder::ClusterFinder()
 { 
  cluIndex = 0;
  //if (m_hybIndex==69){
  //cout << " In clumaker, the number of anodes is " << mNumOfAnodes << endl;
  //}

  for(int mAnode = 0; mAnode<mNumOfAnodes; mAnode++)
   {
     hybdata->getListSequences(mAnode,mSequence,sequence); //need to decide where to get sequences
     //cout<<"mSequence = "<<mSequence<<endl;

    mNumSeq[mAnode] = mSequence;
    for(int mSeq = 0; mSeq < mSequence; mSeq++)
     { 
 
      if(mSeqFlag[mAnode][mSeq] == 0)
         getClusterMembers(mAnode,mSeq);
     }
   }

  mNumOfClusters = cluIndex;

 }


void StSvtClusterFinder::getClusterMembers(int& mAnode, int &mSeq)
  {                                                                        
   int members, memCount , breakAn = 0, status;
   int mSeqStart,  mSeqLength, mSeqEnd;
   int newMem = 0;
   int mRightFlagCounter = 0;
   int mLeftFlagCounter = 0;

   ++cluIndex;
   memCount = 0;
   mCluster[cluIndex-1][memCount] = 1000*mAnode + mSeq;
   //cout<<mAnolist[mAnode]<<endl;
   //cout<<mSeq<<endl;
   mContainer1[0] =  mCluster[cluIndex-1][memCount];
   mSeqFlag[mAnode][mSeq] = 1;
   members = 1;
   memCount = 1; 

// mContainer1[0] =  mCluster[cluIndex-1][memCount];
            
   for(int mem = 0; mem < members ; mem++)
     {                                                                             
      mAnode = mContainer1[mem]/1000;
      mSeq = mContainer1[mem]%1000;

      mContainer1[mem] = 0;   //zeroed after being used

      status = hybdata->getListSequences(mAnode,mSequence,sequence);

      mSeqStart = sequence[mSeq].startTimeBin;
      mSeqLength = sequence[mSeq].length;
      mSeqEnd =  mSeqStart + mSeqLength - 1;

      mRightFlagCounter = getSeqOnRight(mAnode,mSeqStart, mSeqEnd, memCount, newMem);
      mLeftFlagCounter  = getSeqOnLeft(mAnode,breakAn,mSeqStart, mSeqEnd, memCount, newMem);

      //      if((mem == members - 1) && (mRightFlagCounter != 0 || mLeftFlagCounter != 0))
     if((mem == members - 1) && newMem!=0)
       {     
        members = newMem;
        mRightFlagCounter = 0;
        mLeftFlagCounter = 0;

       // we don't need to do this now since we  made each zero already
	                                   
        //for(mem = 0; mem < 500; mem++)
        //  mContainer1[mem] = 0;          
                      
        int j = 0;

        for(mem = 0; mem < members && j<newMem; mem++)
          {
           mContainer1[mem] = mContainer2[j];
           ++j;
          }

        for( j = 0; j < newMem; j++)
          mContainer2[j] = 0;
   
          newMem = 0;
	  mem = -1;
       }

     // else if((mem == members - 1) && (mRightFlagCounter == 0 && mLeftFlagCounter == 0))
     else if((mem == members - 1) && newMem==0)
       { 
	 //cout<< memCount <<' '<<"members found in cluster"<<' '<<cluIndex - 1<<endl;

	 mNumOfCluMem[cluIndex-1] = memCount;
	 
	 if( memCount == 1 && mSeqLength==1){
	   cluIndex--;
	 }
	 
	 /* we don't need to do this now since we made each one zero already 	
	                                   	 
	 for(int j = 0; j < 500; j++)       
	   {
	     mContainer1[j] = 0;
	     mContainer2[j] = 0;
	   }
	 */
	 
	 mSeq = -1;
	 mAnode = breakAn;
	 status = hybdata->getListSequences(mAnode,mSequence,sequence);
	 
       }
     }
   
  }

//______________________________________________________________________________

int StSvtClusterFinder::getSeqOnRight(int mAnode, int mSeqStart, int mSeqEnd, int& memCount,int& newmem)
{
 int mRightAnode = 0, mRightSequence = 0, rightFlagCounter = 0, status;
 int  mRightSeqStart, mRightSeqLength, mRightSeqEnd;
  
 mRightAnode = mAnode + 1;
 if(mRightAnode != 240)
   {
    status = hybdata->getListSequences(mRightAnode,mRightSequence,sequence);
              
    for(int mRightSeq = 0; mRightSeq < mRightSequence ; mRightSeq++)
      {
       if(mAnolist[mAnode] == 240)
        break;
                  
       if(mAnolist[mRightAnode] != mAnolist[mAnode] + 1) //are they consecutive?
        break;
                   
       mRightSeqStart = sequence[mRightSeq].startTimeBin;
       mRightSeqLength = sequence[mRightSeq].length;
       mRightSeqEnd =  mRightSeqStart + mRightSeqLength - 1; 

    // cout << "initial clusterflag is " <<' '<<mSeqFlag[mRightAnode][mRightSeq]<<endl;

       if(mSeqFlag[mRightAnode][mRightSeq] == 0)
         {
          if(mRightSeqStart == mSeqStart)
            {
             mCluster[cluIndex-1][memCount] = 1000*mRightAnode + mRightSeq;
             mSeqFlag[mRightAnode][mRightSeq] = 1;
             mContainer2[newmem] = mCluster[cluIndex-1][memCount];
             ++memCount;
             ++newmem; 
             ++rightFlagCounter; 
            }
  
	  if((mRightSeqStart < mSeqStart)&&(mRightSeqEnd >= mSeqStart - 1))
            {
	     mCluster[cluIndex-1][memCount] = 1000*mRightAnode + mRightSeq;
             mSeqFlag[mRightAnode][mRightSeq] = 1;
             mContainer2[newmem] = mCluster[cluIndex-1][memCount];
             ++memCount;
             ++newmem;
             ++rightFlagCounter;
             }
 
           if((mRightSeqStart > mSeqStart)&&(mRightSeqStart <= mSeqEnd + 1))
             {
              mCluster[cluIndex-1][memCount] = 1000*mRightAnode + mRightSeq;
              mSeqFlag[mRightAnode][mRightSeq] = 1;
              mContainer2[newmem] = mCluster[cluIndex-1][memCount];
              ++memCount;
              ++newmem; 
              ++rightFlagCounter;
              }
	    
        //cout<<"mContainer2[newmem-1] ="<<' '<<mContainer2[newmem-1]<<endl;       
	//cout<<"mCluster[cluIndex-1][memCount-1] = "<<' '<<mCluster[cluIndex-1][memCount]<<endl;
  	//cout << "final sequence flag is " <<' '<<mSeqFlag[mRightAnode][mRightSeq]<<endl;
        //cout << "right flag counter is " <<' '<<rightFlagCounter<<endl;
	// cout << "new members ="<<' '<< k <<endl;
	// cout <<"done with mRightSeq"<<endl;

	        
	     
	 }
      }
   }
  return rightFlagCounter;
}
	
 //_____________________________________________________________________________
              

int StSvtClusterFinder::getSeqOnLeft(int mAnode, int& breakAn,int mSeqStart, int mSeqEnd, int& memCount,int& newmem)
{
  int mLeftAnode = 0, mLeftSequence = 0, leftFlagCounter = 0, status;
  int mLeftSeqStart, mLeftSeqLength, mLeftSeqEnd;
 
  mLeftAnode = mAnode - 1;

  if(mLeftAnode != -1)
    {
     status = hybdata->getListSequences(mLeftAnode,mLeftSequence,sequence);

     for(int mLeftSeq = 0; mLeftSeq < mLeftSequence ; mLeftSeq++)
	{
         if(mAnolist[mAnode] == 1)
           {
            breakAn = 0;
	    break;
           }
                  
          if(mAnolist[mLeftAnode] != mAnolist[mAnode] - 1) //are they consecutive?
            {
             breakAn = mAnode;
             break;
            }

           mLeftSeqStart = sequence[mLeftSeq].startTimeBin;
           mLeftSeqLength = sequence[mLeftSeq].length;
           mLeftSeqEnd =  mLeftSeqStart + mLeftSeqLength - 1; 
              
	// cout << "initial clusterflag is " <<' '<<mSeqFlag[mLeftAnode][mLeftSeq]<<endl;                
           if(mSeqFlag[mLeftAnode][mLeftSeq] == 0)
             {
           // cout<<"looking for mSeqLeft"<<endl;

              if(mLeftSeqStart == mSeqStart)
                {
                 mCluster[cluIndex-1][memCount] = 1000*mLeftAnode + mLeftSeq;
                 mSeqFlag[mLeftAnode][mLeftSeq] = 1;
                 mContainer2[newmem] = mCluster[cluIndex-1][memCount];
                 ++memCount;
                 ++newmem; 
                 ++leftFlagCounter; 
                }                   

              if((mLeftSeqStart < mSeqStart) && (mLeftSeqEnd >= mSeqStart - 1))
                {
		 mCluster[cluIndex-1][memCount] = 1000*mLeftAnode + mLeftSeq;
                 mSeqFlag[mLeftAnode][mLeftSeq] = 1;
                 mContainer2[newmem] = mCluster[cluIndex-1][memCount];
                 ++memCount;
                 ++newmem;
                 ++leftFlagCounter; 
                }
 
              if((mLeftSeqStart > mSeqStart) && (mLeftSeqStart <= mSeqEnd + 1))
                {
                 mCluster[cluIndex-1][memCount] = 1000*mLeftAnode + mLeftSeq;
                 mSeqFlag[mLeftAnode][mLeftSeq] = 1;
                 mContainer2[newmem] = mCluster[cluIndex-1][memCount];
                 ++memCount;
                 ++newmem;
                 ++leftFlagCounter;
		}
         
	   //cout<<"mContainer2[newmem-1] ="<<' '<<mContainer2[newmem-1]<<endl;
           //cout<<"mCluster[cluIndex-1][memCount]= "<<' '<<mCluster[cluIndex][memCount]<<endl;  
	   //cout << "final sequence flag is " <<' '<<mSeqFlag[mLeftAnode][mLeftSeq]<<endl;
	   //cout << "left flag counter is " <<' '<<leftFlagCounter<<endl;
	  // cout << "done with mLeftSeq"<<endl;
		
	     }
	}
    }

  return leftFlagCounter;
}


//____________________________________________________________________________

void StSvtClusterFinder::SetHybIndex(int index)
{
  m_hybIndex = index;
}

int StSvtClusterFinder::ClusterIndex()
  {
   return mNumOfClusters;
  }       

int StSvtClusterFinder::ClusterMembers(int clu)
  {
   return mNumOfCluMem[clu];
  }

int StSvtClusterFinder::ClusterListAnode(int clu, int mem)
  {
    return mCluster[clu][mem]/1000;
  }  

int StSvtClusterFinder::ClusterSequence(int clu, int mem)
  {
    return mCluster[clu][mem]%1000;

  } 

int StSvtClusterFinder::ClusterActualAnode(int listAn)
{
 return mAnolist[listAn];
}


//---------------------------------------------------------------------------

void StSvtClusterFinder::ResetContainers()
{
 int i,j;
 /*
 for( j = 0; j < 500; j++)
    {
      mContainer1[j] = 0;
      mContainer2[j] = 0;
    }
  for ( i=0; i<8000; i++)
    for ( j=0; j<500; j++)
      mCluster[i][j] = 0;
  
  for ( i=0; i<240; i++)
    for ( j=0; j<128; j++)
      mSeqFlag[i][j] = 0;
 */

//array memebers > mNumOfClusters already zero
                                             
 for ( i=0; i< mNumOfClusters; i++)        
    for ( j=0; j< mNumOfCluMem[i]; j++)
      mCluster[i][j] = 0;

// do only the ones that have been flagged

 {for(int i = 0; i<mNumOfAnodes; i++)
    for( j=0; j<mNumSeq[i];j++)
      mSeqFlag[i][j] = 0;}

  cluIndex=0;
}


  
     
