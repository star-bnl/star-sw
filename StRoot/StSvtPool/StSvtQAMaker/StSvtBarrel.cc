/***************************************************************************
 *
 * $Id: StSvtBarrel.cc,v 1.1 2004/02/06 02:30:33 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT GUI Monitor
 *
 ***************************************************************************
 *
 * $Log: StSvtBarrel.cc,v $
 * Revision 1.1  2004/02/06 02:30:33  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
 
#include "TObject.h"
#include "TList.h"

#include "StSvtBarrel.hh"
#include "StSvtLadder.hh"
#include "temp.hh"


ClassImp(StSvtBarrel)

//--------------------------------------------------------------------------//
StSvtBarrel::StSvtBarrel(Int_t id){

 mBarrelNumber = id;
 mLadderList = new TList;
 
 Int_t ii = 0 ;
 
 switch(mBarrelNumber){
 
 case 1 : 
   mBarrelName = "Barrel 1";    
   
   for ( ii = 1 ; ii <= N_LADDERS_BARREL_1 ; ii++){
     StSvtLadder* ladder = new StSvtLadder(mBarrelNumber,ii);
     mLadderList->Add(ladder);
   }
   break;
 
 case 2 :
   mBarrelName = "Barrel 2"; 

   for ( ii = 1 ; ii <= N_LADDERS_BARREL_2 ; ii++){
     StSvtLadder* ladder = new StSvtLadder(mBarrelNumber,ii);
     mLadderList->Add(ladder);
   }   
   break;

 case 3 :
   mBarrelName = "Barrel 3"; 

   for ( ii = 1 ; ii <= N_LADDERS_BARREL_3 ; ii++){
     StSvtLadder* ladder = new StSvtLadder(mBarrelNumber,ii);
     mLadderList->Add(ladder);
   }
   break;
 }
}
//--------------------------------------------------------------------------//
StSvtBarrel::~StSvtBarrel()
{
 delete mLadderList;
}
//--------------------------------------------------------------------------//
void StSvtBarrel::DrawLadder(Int_t ladderID)
{
  ((StSvtLadder*)mLadderList->At(ladderID-1))->DrawVertical();
}
//--------------------------------------------------------------------------//

void StSvtBarrel::UpdateLadderButtons(Int_t ladderID, Int_t waferID, Int_t hybridID)
{
  ((StSvtLadder*)mLadderList->At(ladderID-1))->UpdateButtons(waferID, hybridID);
}


