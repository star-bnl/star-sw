/***************************************************************************
 *
 * $Id: StMCTruth.h,v 1.1 2005/07/19 22:40:32 perev Exp $
 *
 * Author: Victor Perev, Jun 2005
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StMCTruth.h,v $
 * Revision 1.1  2005/07/19 22:40:32  perev
 * IdTruth classes
 *
 *
 **************************************************************************/
#ifndef ST_MCTRUTH_H
#define ST_MCTRUTH_H

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
  void Add(long token, int trackId, double wt);
  void Add(long token, StMCTruth truth);
  StMCTruth Get(long token,int byCount=0) const;
  StMCTruth Iter(long &token) const; //token=-1 to start iteration
                                     //token=-1 at the end of iteration
private:
int    fNorm;
TExMap *fMap;
mutable TExMapIter *fIter;
};




#endif
