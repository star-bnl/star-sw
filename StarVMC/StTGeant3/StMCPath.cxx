// $Id: StMCPath.cxx,v 1.1 2005/03/09 18:35:35 perev Exp $
//
//
// Class StPath
// ------------------
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "StMCPath.h"
//_____________________________________________________________________________
StMCPath::StMCPath(const StMCPath &from)
{
  fSize = from.fSize; 
  memcpy(fPath[0],from.fPath[0],fSize*4*sizeof(int));
}
//_____________________________________________________________________________
StMCPath &StMCPath::operator=(const StMCPath &from)
{
  fSize = from.fSize; 
  memcpy(fPath[0],from.fPath[0],fSize*4*sizeof(int));
  return *this;
}
//_____________________________________________________________________________
void StMCPath::GetLevel(int lev,int &volid,int &copy,int &idx, int &ndx) const
{
  volid=copy=idx=ndx=0;
  if (idx>=fSize) return;		
  volid	= fPath[lev][0];	
  copy	= fPath[lev][1];	
  idx	= fPath[lev][2];	
  ndx	= fPath[lev][3];	
}		
//_____________________________________________________________________________
void StMCPath::SetLevel(int lev,int  volid,int  copy,int  idx, int  ndx)  
{		
  fPath[lev][0] = volid;		
  fPath[lev][1] = copy;		
  fPath[lev][2] = idx;		
  fPath[lev][3] = ndx;		
  if (fSize<=idx) fSize = idx+1;
}		
		
		
		
