// StSsdObjects.hh
// Ludovic Gaudichet

#ifndef  STAR_StSsdObjects_hh
#define  STAR_StSsdObjects_hh

const double PI = 3.141592653589793238;

struct localPoint{  double X; double Y; double Z; };
struct globalPoint{ double x; double y; double z; };

struct track {
  globalPoint p[16];
  globalPoint a,b;
  int waferID[16];
  int pointID[16];
  double chi2_1, chi2_2;
  int numberOfHits;
  int flag;
};

const int maxNumberOfEvents = 41;

#endif
