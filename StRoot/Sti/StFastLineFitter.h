//StFastLineFitter.h
//M.L. Milelr (Yale Software)
//03/01

//Adapted from uitLineFit.c:
//AUTHOR: Jawluen Tang, Physics department, UT-Austin 
//        J. T. Mitchell - adapted for PHENIX use. Converted to C.
//        M. L. Miller   - adapted for STAR use, Converted to C++

#ifndef StFastLineFitter_HH
#define StFastLineFitter_HH

#include <vector>

class StFastLineFitter
{
 public:
  StFastLineFitter();
  virtual ~StFastLineFitter();

  //Gets
  double slope() const {return mslope;}
  double intercept() const {return mintercept;}
  double chiSquared() const {return mchisq;}
  double sigmaA() const {return msiga;}
  double sigmaB() const {return msigb;}
  int numberOfPoints() const;
  int rc() const {return mcode;}

  //Sets
  void addPoint(double x, double y, double weight);

  //Action
  void clear(); // full reset
  bool fit();
  void print() const;

 private:
  int dofit();

  vector<double> mx;
  vector<double> my;
  vector<double> mw;
 
  double mchisq;
  double msiga;
  double msigb;
  double mslope;
  double mintercept;
  int mcode; //Fit return code
};

#endif




