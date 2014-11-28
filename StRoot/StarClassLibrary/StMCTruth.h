/***************************************************************************
 *
 * $Id: StMCTruth.h,v 1.3 2009/12/17 08:37:26 fisyak Exp $
 *
 * Author: Victor Perev, Jun 2005
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StMCTruth.h,v $
 * Revision 1.3  2009/12/17 08:37:26  fisyak
 * account signature change snce root 5.24
 *
 * Revision 1.2  2005/09/09 21:42:03  perev
 * Method Size() added
 *
 * Revision 1.1  2005/07/19 22:40:32  perev
 * IdTruth classes
 *
 *
 **************************************************************************/
#ifndef ST_MCTRUTH_H
#define ST_MCTRUTH_H
#include "Rtypes.h"
#if ROOT_VERSION_CODE <= 333312 /*  ROOT_VERSION(5,22,0) */
typedef Long_t   LongKey_t;
#else
typedef Long64_t LongKey_t;
#endif
struct StMCTruth {
    StMCTruth(int id,int wt) 		{trackId=(short)id; trackWt=(short)wt;}
    StMCTruth(int word=0) 		{*this=word;}
StMCTruth &operator=(int word);   
           operator int() const;	//conversion to int

    short trackId;     // Geant track id 
    short trackWt;     // The contrubution or weight to signal (in promille)

};


class StMCPivotTruth {
public:
       StMCPivotTruth(int normInput=0);
  void Reset() {fN=0;}
  void Add(int trackId, double wt);
  void Add(StMCTruth Id,double wt);
  void Add(StMCTruth Id);
  int  Size() const {return fN;}
  StMCTruth Get(int byCount=0) const;
private:
  enum {HOWMANY=20};
int    fN;
int    fNorm;
int    mTrackIds[HOWMANY];
float  mTrackWts[HOWMANY];
float  mTrackNum[HOWMANY];
int qwe;
};

class TExMap;
class TExMapIter;
class StMCPivotTruthMap {

public:
       StMCPivotTruthMap(int normInput=0);
      ~StMCPivotTruthMap();
//void Reset();
  void Add(LongKey_t token, int trackId, double wt);
  void Add(LongKey_t token, StMCTruth truth);
  StMCTruth Get(LongKey_t token,int byCount=0) const;
  StMCTruth Iter(LongKey_t &token) const; //token=-1 to start iteration
                                     //token=-1 at the end of iteration
private:
int    fNorm;
TExMap *fMap;
mutable TExMapIter *fIter;
};




#endif
