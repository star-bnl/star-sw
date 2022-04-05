// $Id: StSvtClusterMaker.cxx,v 1.13 2007/04/28 17:57:05 perev Exp $
// $Log: StSvtClusterMaker.cxx,v $
// Revision 1.13  2007/04/28 17:57:05  perev
// Redundant StChain.h removed
//
// Revision 1.12  2007/03/21 17:22:58  fisyak
// Ivan Kotov's drift velocities, use TGeoHMatrix for coordinate transformation
//
// Revision 1.11  2005/08/04 04:06:38  perev
// clear of collection added
//
// Revision 1.10  2004/01/27 02:32:58  perev
// LeakOff
//
// Revision 1.9  2003/01/28 20:28:34  munhoz
// new filters for clusters
//
// Revision 1.8  2002/03/20 00:33:32  munhoz
// temporary fix for memory leaks and new vertex finder params for pp
//
// Revision 1.7  2001/10/06 00:09:00  caines
// Fix deleting that was already done by clear so no longer crashes on exit
//
// Revision 1.6  2001/09/22 01:07:09  caines
// Fixes now that AddData() is cleared everyevent
//
// Revision 1.5  2001/08/07 20:52:15  caines
// Implement better packing of svt hardware and charge values
//
// Revision 1.4  2001/04/29 20:11:58  caines
// Added reset command for Online monitor
//
// Revision 1.3  2000/08/29 22:46:26  caines
// Fixed some memory leaks
//
// Revision 1.2  2000/08/21 13:06:58  caines
// Much improved hit finding and fitting
//
// Revision 1.1  2000/07/06 03:50:34  caines
// First version of cluster finder and fitter
//
//
//////////////////////////////////////////////////////////////////////////
//                                                                      //
// StSvtClusterMaker class                                             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include "TH1.h"
#include "TH2.h"
#include "St_DataSetIter.h"
#include "TObjectSet.h"
#include "StSequence.hh"

#include "StMessMgr.h"

#include "StSvtClassLibrary/StSvtHybridCollection.hh"
#include "StSvtClassLibrary/StSvtHybridData.hh"
#include "StSvtClassLibrary/StSvtData.hh"
#include "StSvtHybridCluster.hh"
#include "StSvtClusterMaker.h"
#include "StDbUtilities/St_svtRDOstrippedC.h"



//_____________________________________________________________________________
StSvtClusterMaker::StSvtClusterMaker(const char *name) : StMaker(name)
{
 mSvtEvent = NULL;
 mHybridData = NULL;
 mHybridCluster = NULL;
 mClusterColl = NULL;
 mClusterSet = NULL;
 mClusterFinder = NULL;
}
//_____________________________________________________________________________
StSvtClusterMaker::~StSvtClusterMaker()
{

  delete mClusterFinder;
}

//_____________________________________________________________________________
Int_t StSvtClusterMaker::Init(){


 if (Debug()) gMessMgr->Debug() << "In StSvtClusterMaker::Init() ..."  << 
		 GetName() << endm;
 
 if( GetSvtRawData()) gMessMgr->Warning() << "No SVT Raw data..."  << endm;
		 
 SetSvtCluster();
 mClusterFinder = new StSvtClusterFinder();

  return StMaker::Init();

}

//_____________________________________________________________________________
Int_t StSvtClusterMaker::GetSvtRawData(){

 St_DataSet *dataSet = GetDataSet("StSvtData");
  //assert(dataSet); 
 if( !dataSet) return kStWarn;
 
 mSvtEvent = (StSvtData*)(dataSet->GetObject());
 if( !mSvtEvent) return kStWarn;

 return kStOk;
 

}
//_____________________________________________________________________________
Int_t StSvtClusterMaker::SetSvtCluster()
{
  mClusterSet = new St_ObjectSet("StSvtCluster");
  //AddData(mClusterSet);  
  AddConst(mClusterSet);  
  SetOutput(mClusterSet); //Declare for output

  
    mClusterColl = new StSvtHybridCollection(mSvtEvent->getConfiguration());
    mClusterSet->SetObject(mClusterColl);

  return kStOK;
}


//_____________________________________________________________________________
Int_t StSvtClusterMaker::Make(){
  

 if (Debug()) gMessMgr->Debug() << "In StSvtClusterMaker::Make() ..."  << 
		 GetName() << endm;

 if( GetSvtRawData()) {
   gMessMgr->Warning() << " Problem with SVt raw in ClusterMaker" << endm;
   return kStWarn;
 }
 // SetSvtCluster();
 SetHybridClusters();
   
 return kStOK;
}

//______________________________________________________________________________________
Int_t StSvtClusterMaker::SetHybridClusters()
{
 int index =0;
 
 for(int barrel = 1;barrel <= mSvtEvent->getNumberOfBarrels();barrel++) {
   //cout<<mSvtEvent->getNumberOfBarrels()<<endl;
   for (int ladder = 1;ladder <= mSvtEvent->getNumberOfLadders(barrel);ladder++) {
     //cout<<mSvtEvent->getNumberOfLadders(barrel)<<endl;
      for (int wafer = 1;wafer <= mSvtEvent->getNumberOfWafers(barrel);wafer++) {
	// cout<<mSvtEvent->getNumberOfWafers(barrel)<<endl;

	if (St_svtRDOstrippedC::instance()->svtRDOstrippedStatus(barrel,ladder,wafer)) continue; // bad or missing wafer

	for (int hybrid = 1;hybrid <=mSvtEvent->getNumberOfHybrids();hybrid++){
          //cout<<mSvtEvent->getNumberOfHybrids()<<endl;  
           
          index = mSvtEvent->getHybridIndex(barrel,ladder,wafer,hybrid);
          if(index < 0) continue;
	  //	  if (index != 69) continue;
	  
          mHybridData = (StSvtHybridData *)mSvtEvent->at(index);
	  if( !mHybridData) continue;
	  
          mClusterFinder->setHybridPointer(mHybridData);
	  mClusterFinder->SetHybIndex(index);
          mClusterFinder->ClusterFinder();
          
	  
	  mHybridCluster = (StSvtHybridCluster* )mClusterColl->at(index);
	  if( mHybridCluster){
	    delete mHybridCluster;
            mClusterColl->at(index)=0;
	  }
	  
	  mHybridCluster = new StSvtHybridCluster();
	  
	  mHybridCluster->setCluster(mClusterFinder);
	  mClusterColl->put_at(mHybridCluster,index);
          mClusterFinder->ResetContainers();
	  
	}
      }
    }
  }

  return kStOK;
}


//_____________________________________________________________________________

Int_t StSvtClusterMaker::Finish(){

  if (Debug()) gMessMgr->Debug() << "In StSvtClusterMaker::Finish() ..."  << 
		 GetName() << endm;

  return kStOK;
}

//_____________________________________________________________________________
Int_t StSvtClusterMaker::Reset(){

  if (Debug()) gMessMgr->Debug() << "In StSvtClusterMaker::reset() ..."  << 
		 GetName() << endm;

//VP delete mHybridCluster;
 mClusterSet->SetObject(0);  //it will be deleted inside
 delete mClusterFinder;

 mHybridCluster = NULL;
 mClusterColl = NULL;
 mClusterFinder = NULL;

 return kStOK;
}
//_____________________________________________________________________________
void StSvtClusterMaker::Clear(const char *)
{
 if (mClusterColl) {
   int n =  mClusterColl->size();
   mClusterColl->clear();
   mClusterColl->resize(n);
 }
 StMaker::Clear();
}
//_____________________________________________________________________________
ClassImp(StSvtClusterMaker)










