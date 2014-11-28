////////////////////////////////////////////////////////////////////////////
//                                                                         // 
// It represents the BadAnodes on a hybrid data.                           //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StSvtBadAnode.hh"

StSvtBadAnode::StSvtBadAnode()
{
  for( int i=0; i<240; i++){
    mBadAnode[i] = 0;
  }
}

StSvtBadAnode::~StSvtBadAnode()
{}
