#ifndef STSVTANGLES_HH
#define STSVTANGLES_HH

#include "StThreeVector.hh"
//class StThreeVector;           //rcas has a problem with a template class?

class StSvtAngles
{
 public:
    StSvtAngles();
    ~StSvtAngles();

    void svtTheta(const StThreeVector <double>& V, const StThreeVector <double>& u);
    void svtTheta(double Vx, double Vy, double Vz, double ux,double uy,double uz);
    void svtPhi(const StThreeVector <double>& V, const StThreeVector <double>& ux, const StThreeVector <double>& uy);

    double getTheta(){ return mTheta;}
    double getPhi()  { return mPhi;  }

 private:
   
   double mTheta;
   double mPhi;
  
  //ClassDef(StSvtAngles, 1)
};

#endif
