// $Id: StSsdLadder.cc,v 1.6 2005/12/31 01:43:22 perev Exp $
//
// $Log: StSsdLadder.cc,v $
// Revision 1.6  2005/12/31 01:43:22  perev
// Mack/Upack simplified
//
// Revision 1.5  2005/03/18 14:06:30  lmartin
// missing CVS header added
//

#include "StSsdLadder.hh"
#include "StSsdWafer.hh"
#include "StSsdUtil/StSsdEnumerations.hh"
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
  for (int i = 0; i < wafpos->GetNRows(); i++)
    {
      StSsdPack idWafer(wpos[i].id);
      int iWaf = idWafer.getWaf();
      if (iWaf>=0 && idWafer.getLad() == mLadderNumb)
	mWafers[iWaf]->init(idWafer, wpos[i].driftDirection, wpos[i].transverseDirection, wpos[i].normalDirection, wpos[i].centerPosition);
    }
}



void StSsdLadder::initWafers(ssdWafersPosition_st *position, int positionSize)
{
  for (int i = 0; i <positionSize ; i++)    // loop over the full table now.
    {
      StSsdPack idWafer(position[i].id);
      int iWaf = idWafer.getWaf();
      if (iWaf>=0 && mLadderNumb == idWafer.getLad())
	mWafers[iWaf]->init(idWafer, position[i].driftDirection, position[i].transverseDirection, position[i].normalDirection, position[i].centerPosition);
    }
}



int StSsdLadder::idWaferToWaferNumb(int idWafer)
{
   return StSsdPack::getWaf(idWafer);
}

int StSsdLadder::waferNumbToIdWafer(int waferNumb)
{
  return StSsdPack::pack(waferNumb,mLadderNumb);
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
