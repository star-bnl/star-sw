#include "StSsdLadder.hh"
#include "StSsdWafer.hh"
#include <Stiostream.h>
#include "tables/St_ssdWafersPosition_Table.h"

StSsdLadder::StSsdLadder(int rLadderNumb,int rSsdLayer,int rNWaferPerLadder,int rNStripPerSide)
{
  // Note          iWaf = 0->15 whereas iW = 1->16 !
  // mLadderNumb = iLad = 0->19 whereas iL = 1->20 !

  mLadderNumb      = rLadderNumb;
  mSsdLayer        = rSsdLayer;
  mNWaferPerLadder = rNWaferPerLadder;
  mNStripPerSide   = rNStripPerSide;

  int nWafer  = mNWaferPerLadder;
  int idWaf   = 0;

  mWafers = new StSsdWafer*[nWafer];

  for (int iWaf=0; iWaf < nWafer; iWaf++)
    {
      idWaf   = waferNumbToIdWafer(iWaf);
      mWafers[iWaf] = new StSsdWafer(idWaf);
    }
}

StSsdLadder::~StSsdLadder()
{
  for (int iWaf = 0 ; iWaf < mNWaferPerLadder ; iWaf++)
    delete mWafers[iWaf];
}

void StSsdLadder::initWafers(St_ssdWafersPosition *wafpos)
{
  ssdWafersPosition_st *wpos =  wafpos->GetTable();
  int idWafer = 0;
  int iWaf    = 0;
  for (int i = 0; i < wafpos->GetNRows(); i++)
    {
      idWafer = wpos[i].id;
      iWaf = idWaferToWaferNumb(idWafer);
      if (
	  (idWafer > mSsdLayer*1000)&&
	  (mLadderNumb == idWafer - mSsdLayer*1000 - (iWaf+1)*100 - 1)
	  )
	mWafers[iWaf]->init(idWafer, wpos[i].driftDirection, wpos[i].transverseDirection, wpos[i].normalDirection, wpos[i].centerPosition);
    }
}



void StSsdLadder::initWafers(ssdWafersPosition_st *position)
{
  int idWafer = 0;
  int iWaf    = 0;
  for (int i = 0; i < 10*mNWaferPerLadder ; i++)    // need to be change
    {
      idWafer = position[i].id;
      iWaf = idWaferToWaferNumb(idWafer);
      if (
	  (idWafer > mSsdLayer*1000)&&
	  (mLadderNumb == idWafer - mSsdLayer*1000 - (iWaf+1)*100 - 1)
	  )
	mWafers[iWaf]->init(idWafer, position[i].driftDirection, position[i].transverseDirection, position[i].normalDirection, position[i].centerPosition);
    }
}



int StSsdLadder::idWaferToWaferNumb(int idWafer)
{
   int iW = (int)((idWafer - mSsdLayer*1000)/100);
   return (iW-1);
}

int StSsdLadder::waferNumbToIdWafer(int waferNumb)
{
  int iL = mLadderNumb+1;                          // iL:1->20
  int iW = waferNumb+1;                            // iW:1->16
  return mSsdLayer*1000 + iW*100 + iL;
}

void StSsdLadder::debugUnPeu(int monwafer)
{
  for (int j=0;j<this->getWaferPerLadder();j++) 
    {
      if (this->mWafers[j]->getIdWafer()==this->waferNumbToIdWafer(monwafer)) 
	{
	  cout<<" Wafer "<<monwafer<<" found with id :"<<this->mWafers[j]->getIdWafer()<<endl;
	  this->mWafers[j]->debugStrips();
	  this->mWafers[j]->debugClusters();
	}
    }

}
