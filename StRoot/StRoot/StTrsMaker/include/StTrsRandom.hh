/***************************************************************************
 *
 * $Id: StTrsRandom.hh,v 1.1 2005/12/12 21:00:12 perev Exp $
 *
 * Author: Victor Perev Dec 2005
 ***************************************************************************
 *
 * Description: Random generator based on ROOT TRandom
 *              allows to use stl random_shuffle
 *
 ***************************************************************************
 *
 * $Log: StTrsRandom.hh,v $
 * Revision 1.1  2005/12/12 21:00:12  perev
 * 3 random generators ==> 1
 *
 * Revision 1.0  2005/10/11 21:42:23  perev
 * Initial Revision
 *
 **************************************************************************/
#ifndef ST_TRS_RANDOM_HH
#define ST_TRS_RANDOM_HH
#include "TRandom.h"
class StTrsRandom : public TRandom
{
public:
   StTrsRandom(){};
  ~StTrsRandom(){if (this==fgInst) fgInst=0;}
static StTrsRandom &inst()	{if (!fgInst) fgInst= new StTrsRandom(); return *fgInst;}  
int operator()(int limit)  	{return (int)Rndm()*limit;}

static StTrsRandom *fgInst;
};
#ifdef ST_TRS_RANDOM_SRC
StTrsRandom *StTrsRandom::fgInst = 0;
#undef ST_TRS_RANDOM_SRC
#endif //ST_TRS_RANDOM_SRC

#endif
