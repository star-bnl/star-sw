#ifndef  RdoFinder_HH
#define  RdoFinder_HH

class dst_track_st;

class  RdoFinder{
 private:
  static RdoFinder *mRdoFinder;
  RdoFinder();

  float  magneticField;
  float  avg_gap;

  int    nRowsInSectorGaps(float x_first,float y_first,float x_cent,float y_cent,float x_last,float y_last);
  int    CageCrossings(float x_cent, float y_cent, float radius, float cage_radius, float* x_out,float* y_out, int first_or_second);
  int    RowCrossings(float x_cent, float y_cent, float radius, float row_y, float* x_out,float* y_out, int first_or_second);
  int    FindSectorCrossings(float x_first,float y_first,float x_cent,float y_cent,float x_last,float y_last,float r_first_row,float r_last_row);
  int   Sector(float x, float y, float z);
  int   Row(float x, float y);
  float Local_y(float x,float y);

 public:
  //  RdoFinder(){avg_gap = 3.8*cos(M_PI/12);}
  static RdoFinder* Instance(); 
  void setMagneticField(float bField) { magneticField=bField; }

  int PossiblePoints(dst_track_st *dstTrack);
  int CheckRdoInt(int rdoFinderInt, int sector){
    if(sector<0||sector>24) return 0;
    return (rdoFinderInt&(1<<sector-1)) ? 1:0;
  }
  int  BinarySubset(int a,int b,int n=24);
  void PrintResults(int rdoFinderInt);
};

#endif





















