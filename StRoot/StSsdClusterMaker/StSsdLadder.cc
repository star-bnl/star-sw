#include "StSsdLadder.hh"
#include "tables/St_svg_geom_Table.h"
#include "tables/St_spa_strip_Table.h"
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"
#include "tables/St_sdm_calib_db_Table.h"

StSsdLadder::StSsdLadder(int rLadderNumb,int rSsdLayer,int rNWaferPerLadder,int rNStripPerSide)// checked !
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

StSsdLadder::~StSsdLadder() // checked !
{
  for (int iWaf = 0 ; iWaf < mNWaferPerLadder ; iWaf++)
    delete mWafers[iWaf];
}

void StSsdLadder::initWafers(St_svg_geom *geom_class) // checked !
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
	  (mLadderNumb == idWafer - mSsdLayer*1000 - (iWaf+1)*100 - 1) // just added -1
	  )
 	{
  	  mWafers[iWaf]->init(idWafer, geom[i].d, geom[i].t, geom[i].n, geom[i].x);
 	}
    }
//   printf("####  END OF SSD WAFERS FOR LADDER %d INITIALIZATION   ####\n",mLadderNumb);
}

int StSsdLadder::idWaferToWaferNumb(int idWafer) // checked
{
  // idwafer = layer*1000+waf*100+ladder

//   int iW = (int)((idWafer - mSsdLayer*1000)/100);
//   int iL = idWafer - mSsdLayer*1000 - iW*100;
//   int iL = mLadderNumb;
//   return ((iL-1)*mNWaferPerLadder + iW -1);

   int iW = (int)((idWafer - mSsdLayer*1000)/100);
   return (iW-1);
}

int StSsdLadder::waferNumbToIdWafer(int waferNumb) // checked
{
//   int iL = 1+(int)((waferNumb)/mNWaferPerLadder);
  int iL = mLadderNumb+1;                          // iL:1->20
//   int iW = waferNumb-((iL-1)*mNWaferPerLadder)+1;
  int iW = waferNumb+1;                            // iW:1->16
  return mSsdLayer*1000 + iW*100 + iL;
}
