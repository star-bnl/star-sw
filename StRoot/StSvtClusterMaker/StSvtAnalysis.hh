/***************************************************************************
 *
 * $Id: StSvtAnalysis.hh,v 1.1 2000/07/06 03:50:33 caines Exp $
 *
 * Author: Helen Caines
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StSvtAnalysis.hh,v $
 * Revision 1.1  2000/07/06 03:50:33  caines
 * First version of cluster finder and fitter
 *
 **************************************************************************/


#ifndef STSVTANALYSIS_HH
#define STSVTANALYSIS_HH

#include  "StSvtHybridCluster.hh"

class StSvtHybridData;
class StSequence;

class StSvtAnalysis 
{

public:
  StSvtAnalysis();
  ~StSvtAnalysis();

  void SetPointers(StSvtHybridData* hybData,StSvtHybridCluster* hybClu);
  void FirstAndLastAnodes();
  void CluFirstTimeBin();
  void CluLastTimeBin();
  void MomentAnalysis(int PedOffSet);
  int GetFirstAnode(int clu){return mCluFirstAnode[clu];}
  int GetLastAnode(int clu) {return mCluLastAnode[clu];}
  int GetFirstTimeBin(int clu){return mCluFirstTimeBin[clu];}
  int GetLastTimeBin(int clu) {return mCluLastTimeBin[clu];}
  int GetSumADC(int clu) {return mTotalADCCounts[clu];}
  double MeanClusterAnode(int clu){ return mMeanClusterAnode[clu];}
  double MeanClusterTimeBin(int clu){ return mMeanClusterTimeBin[clu];}
  void Report(int index);
  void ResetMeanValues();

private:

  StSvtHybridData* mHybridData;        
  StSvtHybridCluster* mHybridCluster;      
  StSequence* mSvtSequence;                
  int numOfClusters, numOfMembers;
  int* mCluFirstAnode;                 
  int* mCluLastAnode;                  
  int* mCluFirstTimeBin;               
  int* mCluLastTimeBin;                
  double* mMeanClusterTimeBin;          
  double* mMeanClusterAnode;            
  int* mTotalADCCounts;              
 
  StSvtClusterMemberInfo** tempMemberInfo;  
  };

#endif
