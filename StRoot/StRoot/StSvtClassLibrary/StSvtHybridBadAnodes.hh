/***************************************************************************
 *
 * $Id: StSvtHybridBadAnodes.hh,v 1.1 2002/02/15 22:44:07 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 *
 ***************************************************************************
 *
 * Description: Flags Bad anodes on hybrids
 *
 ***************************************************************************/
 
#ifndef STSVTHYBRIDBADANODE_HH
#define STSVTHYBRIDBADANODE_HH

#include "StSvtClassLibrary/StSvtHybridObject.hh" 

#define MAX_NUMBER_OF_ANODES 240

class StSvtHybridBadAnodes:public StSvtHybridObject 
{
public:
  StSvtHybridBadAnodes();
  StSvtHybridBadAnodes(int barrel, int ladder, int wafer, int hybrid);
   ~StSvtHybridBadAnodes();

  void setBadAnode(int anode);
  void setNotBadAnode(int anode);
  Bool_t isBadAnode(int anode);
  void reset();

  void addOverloadedAdc(int anode, Bool_t isBad, int nEvents);
  void addNullAdc(int anode, Bool_t isBad, int nEvents);
  void addHighOccup(int anode, Bool_t isBad, int nEvents);
  void addBadRMS(int anode, Bool_t isBad, int nEvents);

  float getOverloadedAdc(int anode){return mOverloadedAdc[anode-1];}
  float getNullAdc(int anode){return mNullAdc[anode-1];}
  float getHighOccup(int anode){return mHighOccup[anode-1];}
  float getBadRMS(int anode){return mBadRMS[anode-1];}

protected:
 
  Bool_t mBadAnode[MAX_NUMBER_OF_ANODES];
  int mNBadAnodes;

  // fraction of events that anode is bad
  float mOverloadedAdc[MAX_NUMBER_OF_ANODES];
  float mNullAdc[MAX_NUMBER_OF_ANODES];
  float mHighOccup[MAX_NUMBER_OF_ANODES];
  float mBadRMS[MAX_NUMBER_OF_ANODES]; 
};

#endif
