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
//02.10.2002   ver 2.0  Encapsulated version
//02.17.2002   ver 2.1  IOBroker hooks added
//---------------------------------------------------------------

#ifndef StiHitErrorCalculator_HH
#define StiHitErrorCalculator_HH


class StiHitErrorCalculator: public Observer
{
 public:
  //default constructor
  StiHitErrorCalculator();

  //Destructor
  virtual ~StiHitErrorCalculator();

  //Implementation of Observer pattern
  //void getNewState();
  void update(Subject* changedSubject);
  void forgetSubject(Subject* obsolete);

  //Get errors
  virtual pair<double,double> getHitError(StiHit*, double, double) = 0;   //input hit, cross and dip angle

 private:
   StiIOBroker* mIOBroker;
   Subject*     mSubject;

};

class StiHitErrorDefault: public StiHitErrorCalculator
{
 public:

   //sets default parameters
   StiHitErrorDefault();
   ~StiHitErrorDefault();

   //StiHitErrorDefault* instance();

   //static StiHitErrorDefault* sinstance;

   //init routine reads parameters from IOBroker
   void Init();
   
   pair<double, double> getHitError(StiHit*, double, double);   //input z, cross and dip angle
   int querySource(StiHit*);

   void SetTpcInnerParam(double, double, double, double, double, double); //input integral cross, drift cross,
                                                                              //dip cross, integral dip, drift 
                                                                              //dip, cross dip 
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

//IOBroker inlines

class StiHitErrorCalculator;

inline void StiHitErrorCalculator::update(Subject* changedSubject)
{
    //cout <<"StiHitErrorDefault::update(Subject*)"<<endl;
    if (changedSubject!=mSubject) {
	cout <<"StiHitErrorDefault::update(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
    else {
	//cout <<"getting new values"<<endl;
	//getNewState();
	//cout <<"\tdone getting new values"<<endl;
    }   
}

inline void StiHitErrorCalculator::forgetSubject(Subject* obsolete)
{
    //cout <<"StiEvaluableTrackSeedFinder::forgetSubject(Subject*)"<<endl;
    if (obsolete==mSubject) {
	mSubject=0;
    }
    else {
	cout <<"StiHitErrorDefault::forgetSubject(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
}
#endif








