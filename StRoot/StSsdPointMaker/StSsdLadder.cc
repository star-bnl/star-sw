#include "StSsdLadder.hh"
#include "StSsdWafer.hh"
#include "tables/St_svg_geom_Table.h"

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

void StSsdLadder::initWafers(St_svg_geom *geom_class)
{
  svg_geom_st *geom =  geom_class->GetTable();
  int idWafer = 0;
  int iWaf    = 0;
  for (int i = 0; i < geom_class->GetNRows(); i++)
    {
      idWafer = geom[i].id;
      iWaf = idWaferToWaferNumb(idWafer);
      if (
	  (idWafer > mSsdLayer*1000)&&
	  (mLadderNumb == idWafer - mSsdLayer*1000 - (iWaf+1)*100 - 1)
	  )
	mWafers[iWaf]->init(idWafer, geom[i].d, geom[i].t, geom[i].n, geom[i].x);
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
