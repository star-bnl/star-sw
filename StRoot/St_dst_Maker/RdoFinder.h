
#ifndef  RdoFinder_HH
#define  RdoFinder_HH

class StTrack;
class dst_track_st;

class  RdoFinder{
 private:
  static RdoFinder *mRdoFinder;
  RdoFinder();

  float magneticField;
  float avg_gap;
  float OuterRdoRs[5];
  long  OuterOffRdos[4];

  int   removeOffRdos(dst_track_st *t);
  int   removeOffRdos(StTrack* t);
  int   nRowsCrossed(dst_track_st *t,float r_first_row,float r_last_row, int sector);
  int   nRowsCrossed(StTrack* t,float r_first_row,float r_last_row, int sector);

  int   InGap(float *p);
  int   GetOutOfGap(float *p,int *last_cross);
  int   GotoNextCross(float *p,int *last_cross,int pos_side_sector);

  int   FindEndPoints(dst_track_st *t,float *p);
  int   FindEndPoints(StTrack *t,float *p);
  int   FindPointsBetween(dst_track_st *t,float *p,float r_first_row, float r_last_row, float* limits);
  int   FindPointsBetween(StTrack *t,float *p,float r_first_row, float r_last_row, float* limits);
  int   FindRowCrossing(float* p,float y_row, float* x, int first_or_second);

  int   IsItBetween(float x,float y,float* p);
  int   Sector(float x,float y,float z);
  int   Row(float x,float y);
  float Local_y(float x,float y);

 public:

  static RdoFinder* Instance();

  void setMagneticField(float bField);

  int  PossiblePoints(dst_track_st *t);
  int  PossiblePoints(StTrack *t);

};

#endif





















