//
//StiHitError
//
//Class containing hit error parameterization and calculation
//methods. Correct use involves setting parameters relevant to
//a particular detector, either through construction or explicit
//assignment methods.
//
//A. Rose 01.21.2002
//
//Revision history
//----------------
//01.25.2002   ver 1.0  Initial check in of code
//
//---------------------------------------------------------------

#ifndef StiHitError_HH
#define StiHitError_HH

class StiHitError
{
 public:
  //default constructor
  StiHitError();
  //Constructor with parameters
  StiHitError(double, double, double, double, double, double);

  //Destructor
  ~StiHitError();

  //Get errors
  double getDipError(double, double);   //input z and dip angle
  double getCrossError(double, double); //input z and cross angle

 private:
  //private methods
  double dlen(double); 
  double calcEr(double, double, double, double, double);
  void   setIntrinsic(double,double);
  void   setDrift(double,double);
  void   setTan(double,double);

  //data members
  double driftCross;
  double tanCross;
  double intrinsicCross;
  double driftDip;
  double tanDip;
  double intrinsicDip;
};

#endif
