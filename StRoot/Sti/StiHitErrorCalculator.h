#ifndef StiHitErrorCalculator_HH
#define StiHitErrorCalculator_HH

/*!
  Class containing hit error parameterization and calculation
  methods. Correct use involves setting parameters relevant to
  a particular detector, either through construction or explicit
  assignment methods.
  
  \author Andrew Rose 01.21.2002
  
  Revision history
  ----------------
  01.25.2002   ver 1.0  Initial check in of code
  02.10.2002   ver 2.0  Encapsulated version
  02.17.2002   ver 2.1  IOBroker hooks added
*/
class StiHitErrorCalculator
{
 public:
  //default constructor
  StiHitErrorCalculator();

  //Destructor
  virtual ~StiHitErrorCalculator();

  //Get errors
  virtual pair<double,double> getHitError(StiHit*, double, double) = 0;   //input hit, cross and dip angle

 private:
   StiIOBroker* mIOBroker;
};

class StiHitErrorDefault: public StiHitErrorCalculator
{
 public:

   //sets default parameters
   StiHitErrorDefault();
   ~StiHitErrorDefault();

   //init routine reads parameters from IOBroker
   void Init();
   
   pair<double, double> getHitError(StiHit*, double, double);   //input z, cross and dip angle
   int querySource(StiHit*);

   void SetTpcInnerParam(double, double, double, double, double, double); //input integral cross, drift cross,
                                                                              //dip cross, integral dip, drift 
   void SetTpcOuterParam(double, double, double, double, double, double); //same as Inner Param
   void SetSvtParam(double, double, double, double, double, double);
   void SetFtpcParam(double, double, double, double, double, double);

 private:
   void SetTpcSource(int);           
   void SetSvtSource(int);
   void SetFtpcSource(int);

   int   fTpcSource;                 //-1=error, 0=Default, 1=IOBroker, 2=User Defined 
   int   fSvtSource;                 //-1=error, 0=Default, 1=IOBroker, 2=User Defined 
   int   fFtpcSource;                //-1=error, 0=Default, 1=IOBroker, 2=User Defined

   pair<double, double> TpcInnerHitError(StiHit*, double, double);
   pair<double, double> TpcOuterHitError(StiHit*, double, double);
   pair<double, double> SvtHitError(StiHit*, double, double);
   pair<double, double> FtpcHitError(StiHit*, double, double);

   double TpcInnerErrorParam[6];   //set of function parameters for Tpc
   double TpcOuterErrorParam[6];   //set of function parameters for Tpc
   double SvtErrorParam[6];        //set of function parameters for Tpc
   double FtpcErrorParam[6];       //set of function parameters for Tpc
};

#endif








