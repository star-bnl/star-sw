//\class StFgtClusterMaker
//\author Anselm Vossen (avossen@indiana.edu)
//
// 
//   $Id
//   $Log
//
//

#include "StFgtClusterMaker.h"


Int_t StFgtClusterMaker::Make()
{
  TStopwatch clock;
  clock.Start();
  LOG_DEBUG <<"StEmcRawMaker::Make()******************************************************************"<<endm;

  if( !mIsInitialized )
     {
        LOG_ERROR << "Not initialized" << endm;
        return kStFatal;
     }
  else
     {
       
     };


Int_t StFgtClusterMaker::Init()
{
   Int_t ierr = kStOk;

   return ierr;
};


 StFgtClusterMaker::StFgtCluster(const Char_t* name): mIsInitialized(0),StMaker("fgt");
   {

     SetName(name);
   };


};
