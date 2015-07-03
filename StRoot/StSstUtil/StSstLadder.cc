//$Id: StSstLadder.cc,v 1.3 2015/07/02 18:18:46 bouchet Exp $
//
//$Log: StSstLadder.cc,v $
//Revision 1.3  2015/07/02 18:18:46  bouchet
//fixed the decoding of sstWafersPosition table
//
//Revision 1.2  2015/06/24 17:37:21  smirnovd
//StSstUtil: Prepend included headers with path to submodule
//
//Revision 1.1  2015/06/23 16:26:20  jeromel
//First version created from the SSD code and reshaped
//
//Revision 1.1  2015/04/19 17:30:32  bouchet
//initial commit ; SST codes
//
//fork from the SSD code, move along - see history therein
#include "StSstUtil/StSstLadder.hh"
#include "St_base/Stiostream.h"
#include "tables/St_sstWaferConfiguration_Table.h"

StSstLadder::StSstLadder(Int_t rLadderNumb,Int_t rSstLayer,Int_t rNWaferPerLadder,Int_t rNStripPerSide) : mDebug(0)
{
  // Note          iWaf = 0->15 whereas iW = 1->16 !
  // mLadderNumb = iLad = 0->19 whereas iL = 1->20 !
  memset (first, 0, last-first);
  mLadderNumb      = rLadderNumb;
  mSstLayer        = rSstLayer;
  mNWaferPerLadder = rNWaferPerLadder;
  mNStripPerSide   = rNStripPerSide;
  
  Int_t nWafer  = mNWaferPerLadder;
  Int_t idWaf   = 0;
  
  mWafers = new StSstWafer*[nWafer];
  for (Int_t iWaf=0; iWaf < nWafer; iWaf++)
    {
      idWaf   = waferNumbToIdWafer(iWaf);
      mWafers[iWaf] = new StSstWafer(idWaf);
      if (Debug()) mWafers[iWaf]->SetDebug(Debug());
    }
}

StSstLadder::~StSstLadder()
{
  for (Int_t iWaf = 0 ; iWaf < mNWaferPerLadder ; iWaf++)
    delete mWafers[iWaf];
}

void StSstLadder::initWafers(St_sstWafersPosition *Position)
{
  //init is done for 0<iWaf<16
  //idWafer is the old convention : idWafer = 7000 + (wafer[1-16]*100) + ladder[1-20]
  // --> 7101 <= idWafer <= 8620
  Int_t idWafer = 0;
  Int_t iWaf    = 0;
  Int_t iLad    = 0;
  sstWafersPosition_st *position = Position->GetTable();
  Int_t N = 320;
  for (Int_t i = 0; i < N; i++){
    iWaf = i/20;
    iLad = i%20;
    idWafer = 7000 + (iWaf+1)*100 + (iLad)+1;
    int counter = i*3;
    if (mLadderNumb == idWafer%100-1){
      //std::cout <<" i/iWaf/idWafer/counter : " << i <<" " << iWaf <<" " << idWafer << " " << counter << std::endl; 
      Double_t rr[3] = {position[0].driftDirection[i*3],      position[0].driftDirection[i*3+1],      position[0].driftDirection[i*3+2]};
      Double_t nn[3] = {position[0].normalDirection[i*3],     position[0].normalDirection[i*3+1],     position[0].normalDirection[i*3+2]};
      Double_t tr[3] = {position[0].transverseDirection[i*3], position[0].transverseDirection[i*3+1], position[0].transverseDirection[i*3+2]};
      Double_t cr[3] = {position[0].centerPosition[i*3],      position[0].centerPosition[i*3+1],      position[0].centerPosition[i*3+2]};      
      mWafers[iWaf]->init(idWafer,rr,nn,tr,cr);
      //cout <<"driftDirection      : " << position[0].driftDirection[counter] <<" " <<position[0].driftDirection[counter+1] <<" " <<position[0].driftDirection[counter+2]<< endl;
      //cout <<"normalDirection     : " << position[0].normalDirection[counter] <<" " <<position[0].normalDirection[counter+1] <<" " <<position[0].normalDirection[counter+2]<< endl;
      //cout <<"transverseDirection : " << position[0].transverseDirection[counter] <<" " <<position[0].transverseDirection[counter+1] <<" " <<position[0].transverseDirection[counter+2]<< endl;
      //cout <<"centerPosition      : " << position[0].centerPosition[counter] <<" " <<position[0].centerPosition[counter+1] <<" " <<position[0].centerPosition[counter+2]<< endl;
    }
  }
}

void StSstLadder::Reset(){
  for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)mWafers[iWaf]->Reset();
}


Int_t StSstLadder::idWaferToWaferNumb(Int_t idWafer)
{
   Int_t iW = (int)((idWafer - mSstLayer*1000)/100);
   return (iW-1);
}

Int_t StSstLadder::waferNumbToIdWafer(Int_t waferNumb)
{
  Int_t iL = mLadderNumb+1;                          // iL:1->20
  Int_t iW = waferNumb+1;                            // iW:1->16
  return mSstLayer*1000 + iW*100 + iL;
}

void StSstLadder::debugUnPeu(Int_t monwafer)
{
  for (Int_t j=0;j<this->getWaferPerLadder();j++) 
    {
      if (this->mWafers[j]->getIdWafer()==this->waferNumbToIdWafer(monwafer)) 
	{
	  cout<<" Wafer "<<monwafer<<" found with id :"<<this->mWafers[j]->getIdWafer()<<endl;
	  this->mWafers[j]->debugStrips();
	  this->mWafers[j]->debugClusters();
	}
    }

}
