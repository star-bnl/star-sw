#include "StFwdAnaSpin.h"

ClassImp(StFwdAnaSpin)

StFwdAnaSpin::StFwdAnaSpin()
{
}

StFwdAnaSpin::~StFwdAnaSpin()
{
}

UInt_t StFwdAnaSpin::LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata)
{
  return 0;
}

Int_t StFwdAnaSpin::DoMake(StFwdAnaData* anadata)
{
  //Local copy of needed variables to make things easier
  StFwdDataEvent* EvtData = anadata->getEvtData();
  StSpinDbMaker* SpinDbMkr = anadata->spinDbMkr();
  const StTriggerData* TrigData = anadata->trigData();
  //Spin information
  if( SpinDbMkr==0 || TrigData==0 ){
    Double_t rndm = anadata->randomNum();
    //The numbers below are chosen for their bit representation as described and correspond to the source polarization. Need to flip to convert it to STAR polarization direction because of the Siberian Snakes; i.e. '+' -> '-' and '-' -> '+'
    if( rndm<=0.25 ){ EvtInfo->mSpin = 5; }                    //Bits 0101 is B+ and Y+
    else if( 0.25<rndm && rndm<=0.5 ){ EvtInfo->mSpin = 6; }   //Bits 0110 is B+ and Y-
    else if( 0.5<rndm && rndm<=0.75 ){ EvtInfo->mSpin = 9; }   //Bits 1001 is B- and Y+
    else{ EvtInfo->mSpin = 10; }                               //Bits 1010 is B- and Y-
  }
  else{
    EvtInfo->mSpin = SpinDbMkr->spin4usingBX7( EvtData->mBx7Id ); //This is also source polarization
  }
  return kStOk;
}


			  
