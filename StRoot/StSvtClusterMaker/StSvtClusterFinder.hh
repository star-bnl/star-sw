/***************************************************************************
 *
 * $Id: StSvtClusterFinder.hh,v 1.1 2000/07/06 03:50:33 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: SVT Cluster Finder Class
 *
 ***************************************************************************
 *
 * $Log: StSvtClusterFinder.hh,v $
 * Revision 1.1  2000/07/06 03:50:33  caines
 * First version of cluster finder and fitter
 *
 **************************************************************************/


#ifndef STSVTCLUSTERFINDER_HH
#define STSVTCLUSTERFINDER_HH

class StSvtHybridData;
class StSequence;

class StSvtClusterFinder
{
public:
      StSvtClusterFinder();
      StSvtClusterFinder(StSvtHybridData* hdata);
     ~StSvtClusterFinder();

     void ClusterFinder();
     void getClusterMembers(int& mAnode, int &mSeq);
     void ResetContainers();
     int getSeqOnRight(int mAnode, int mSeqStart, int mSeqEnd, int& memCount,int& newmem);
     int getSeqOnLeft(int mAnode, int& breakAn,int mSeqStart, int mSeqEnd, int& memCount,int& newmem);
     void setHybridPointer(StSvtHybridData* hdata);
     int ClusterMembers(int clu);               //returns the number of members in a cluster
     int ClusterIndex();                        //returns the number of clusters
     int ClusterListAnode(int clu,int mem);     //returns anode index on anodelist 
     int ClusterActualAnode(int mAn);            //returns actual anode on hybrid corresponding to a member
     int ClusterSequence(int clu,int mem);       //returns sequence corres[onding to a memeber

     StSvtHybridData*  hybdata;
     StSequence* sequence;
    
 
private:
 
      int numAnodes;
      int mSequence;
      int mNumOfCluMem[30720];                   // array of clusters
      int cluIndex;

      int mCluster[8000][500];                    //array for storing anode and sequence of members
      int mSeqFlag[240][128]; 
      int mContainer1[500];                      //temporary container for new members
      int mContainer2[500];                      //temporary container for new members
      
      int* tempList;
      int* anolist;

  //ClassDef(StSvtClusterFinder,1)

  };

#endif
