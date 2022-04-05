// $Id: StSsdLadder.cc,v 1.5 2014/10/18 19:31:56 smirnovd Exp $
//
// $Log: StSsdLadder.cc,v $
// Revision 1.5  2014/10/18 19:31:56  smirnovd
// Revert "1st commit" asked by Jonathan
//
// Revision 1.3  2008/01/11 10:40:38  bouchet
// Use of the wafer configuration table
//
// Revision 1.2  2007/03/21 17:20:41  fisyak
// use TGeoHMatrix for coordinate transformation
//
// Revision 1.1  2006/10/16 16:43:29  bouchet
// StSsdUtil regroups now methods for the classes StSsdStrip, StSsdCluster and StSsdPoint
//
// Revision 1.7  2006/01/18 22:49:22  jeromel
// Removed latest change (no time to check new method)
//
// Revision 1.5  2005/03/18 14:06:30  lmartin
// missing CVS header added
//

#include "StSsdUtil/StSsdLadder.hh"
#include <Stiostream.h>

#include "tables/St_ssdWaferConfiguration_Table.h"

StSsdLadder::StSsdLadder(Int_t rLadderNumb,Int_t rSsdLayer,Int_t rNWaferPerLadder,Int_t rNStripPerSide) : mDebug(0)
{
  // Note          iWaf = 0->15 whereas iW = 1->16 !
  // mLadderNumb = iLad = 0->19 whereas iL = 1->20 !
  memset (first, 0, last-first);
  mLadderNumb      = rLadderNumb;
  mSsdLayer        = rSsdLayer;
  mNWaferPerLadder = rNWaferPerLadder;
  mNStripPerSide   = rNStripPerSide;

  Int_t nWafer  = mNWaferPerLadder;
  Int_t idWaf   = 0;

  mWafers = new StSsdWafer*[nWafer];
  for (Int_t iWaf=0; iWaf < nWafer; iWaf++)
    {
      idWaf   = waferNumbToIdWafer(iWaf);
      mWafers[iWaf] = new StSsdWafer(idWaf);
      if (Debug()) mWafers[iWaf]->SetDebug(Debug());
    }
}

StSsdLadder::~StSsdLadder()
{
  for (Int_t iWaf = 0 ; iWaf < mNWaferPerLadder ; iWaf++)
    delete mWafers[iWaf];
}

void StSsdLadder::initWafers(St_ssdWafersPosition *Position)
{
  Int_t idWafer = 0;
  Int_t iWaf    = 0;
  ssdWafersPosition_st *position = Position->GetTable();
  Int_t N = Position->GetNRows();
  for (Int_t i = 0; i < N; i++)    // loop over the full table now.
    {
      idWafer = position[i].id;
      iWaf = idWaferToWafer(idWafer);
      if ((idWafer > mSsdLayer*1000)&&
	  (mLadderNumb == idWafer%100-1)){
	mWafers[iWaf]->init(idWafer, position[i].driftDirection, position[i].transverseDirection, position[i].normalDirection, position[i].centerPosition);}
    }
}

void StSsdLadder::initWafers(St_ssdWafersPosition *Position,Int_t WafStatus[20][16]) 
{ 
  Int_t idWafer = 0; 
  Int_t iWaf    = 0; 
  ssdWafersPosition_st *position = Position->GetTable(); 
  Int_t N = Position->GetNRows(); 
  for (Int_t i = 0; i < N; i++)    // loop over the full table now. 
    { 
      idWafer = position[i].id; 
      iWaf = idWaferToWafer(idWafer); 
      if ((idWafer > mSsdLayer*1000)&& 
          (mLadderNumb == idWafer%100-1)){
	if(WafStatus[mLadderNumb][iWaf]!=0){
	  printf("Ladder %d Wafer %d  status %d\n",mLadderNumb,iWaf,WafStatus[mLadderNumb][iWaf]); 
	  mWafers[iWaf]->init(idWafer, position[i].driftDirection, position[i].transverseDirection, position[i].normalDirection, position[i].centerPosition);
	}
      } 
    } 
}

void StSsdLadder::Reset(){
  for (Int_t iWaf = 0; iWaf < mNWaferPerLadder; iWaf++)mWafers[iWaf]->Reset();
}


Int_t StSsdLadder::idWaferToWaferNumb(Int_t idWafer)
{
   Int_t iW = (int)((idWafer - mSsdLayer*1000)/100);
   return (iW-1);
}

Int_t StSsdLadder::waferNumbToIdWafer(Int_t waferNumb)
{
  Int_t iL = mLadderNumb+1;                          // iL:1->20
  Int_t iW = waferNumb+1;                            // iW:1->16
  return mSsdLayer*1000 + iW*100 + iL;
}

void StSsdLadder::debugUnPeu(Int_t monwafer)
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
