/***************************************************************************
 *
 * $Id: StSvtBadAnode.hh,v 1.2 2001/09/28 15:36:51 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: Flags Bad anodes on hybrids
 *
 ***************************************************************************/
 
#ifndef STSVTBADANODE_HH
#define STSVTBADANODE_HH

#include "StSvtClassLibrary/StSvtHybridObject.hh" 

class StSvtBadAnode:public StSvtHybridObject   //StSvtHybridObject inherits from TObject
{
public:
   StSvtBadAnode();
   ~StSvtBadAnode();
  void SetBadAnode(int AnodeNumber, int value);
  int IsBadAnode(int AnodeNumber);

protected:
 
  int mBadAnode[240];
  
};

inline void StSvtBadAnode::SetBadAnode(int AnodeNumber, int value)
{mBadAnode[AnodeNumber-1] = value;}
inline int StSvtBadAnode::IsBadAnode(int AnodeNumber)
{ return ((mBadAnode[AnodeNumber-1]>0) ? 1 : 0);}

#endif
