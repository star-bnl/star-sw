/*!
 * \class St_db_Maker
 * \author Valery Fine(fine@bnl.gov)   
 * \date 10/08/98 
 *
 * This class is C++ implementation of the 
 * <a href="/STAR/comp/pkg/dev/pams/db/sdb/doc/">Simple Database Manager</a>.
 *
 */

// Most of the history moved at the bottom
//
// $Id: StValiSet.cxx,v 1.2 2007/03/09 20:01:03 perev Exp $
// $Log: StValiSet.cxx,v $
// Revision 1.2  2007/03/09 20:01:03  perev
// Request by user defined time now allowed
//
// Revision 1.1  2005/07/20 17:43:21  perev
// Cleanup
//

#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "TError.h"
#include "TDataSetIter.h"
#include "TUnixTime.h"
#include "StValiSet.h"

ClassImp(StValiSet)

//_____________________________________________________________________________
StValiSet::StValiSet(const char *name,TDataSet *parent): TDataSet(name,parent)
{
  SetTitle(".Val");
  fFla = "ofl";
  fTimeMin.Set(kMaxTime,0);
  fTimeMax.Set(kMinTime,0);
  fDat =0;
  fVers=0;
  fGood=0;
  fTabId=0;
  fParId=0;
  Modified(0);
}

//_____________________________________________________________________________
void StValiSet::ls(Int_t lev) const
{
  printf("  %s(%d).Validity = %s ",GetName(),fVers,fTimeMin.AsString());
  printf(" <-> %s\n",     fTimeMax.AsString());
  if (fDat) printf("  Contains DataSet %s\n",fDat->GetName());
  TDataSet::ls(lev);
}

