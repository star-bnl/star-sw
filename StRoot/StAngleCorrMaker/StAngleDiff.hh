#ifndef StAngleDiff_hh
#define StAngleDiff_hh

class StThreeVectorD;

class StAngleDiff {

public:

  double phiDiff(StThreeVectorD m1, StThreeVectorD m2) ;
  double alphaDiff(StThreeVectorD m1, StThreeVectorD m2) ;
  double weightPhiDiff(StThreeVectorD m1, StThreeVectorD m2) ;
  double weightAlpha(double alphadiff) ;
 
protected:
private:
 

};


#endif


