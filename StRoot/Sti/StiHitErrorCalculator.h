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

class StiIOBroker;

class StiHitErrorCalculator
{
 public:
  //default constructor
  StiHitErrorCalculator();

  //Destructor
  virtual ~StiHitErrorCalculator();

  //Get errors
  virtual pair<double,double> getHitError(StiHit*, double, double);   //input z and dip angle
};

class StiHitErrorDefault: public Observer, public StiHitErrorCalculator
{
 public:

   //sets default parameters
   StiHitErrorDefault();
   ~StiHitErrorDefault();

   //init routine reads parameters from IOBroker
   void Init();
   
   virtual pair<double, double> getHitError(StiHit*, double, double);
   int querySource(StiHit*);

   void userSetParameterization(StiDetector*, TF2*);

    //Implementation of Observer pattern
    void getNewState();
    void update(Subject* changedSubject);
    void forgetSubject(Subject* obsolete);



 private:

   void SetTpcSource(int);
   void SetSvtSource(int);
   void SetFtpcSource(int);

   void SetTpcDipParameterization(TF2*);
   void SetTpcCrossParameterization(TF2*);
   void SetSvtParameterization(TF2*);
   void SetFtpcParameterization(TF2*);

   StiIOBroker* mIOBroker;
   Subject*     mSubject;

   int   fTpcSource;                 //-1=error, 0=Default, 1=IOBroker, 2=User Defined 
   int   fSvtSource;                 //-1=error, 0=Default, 1=IOBroker, 2=User Defined 
   int   fFtpcSource;                //-1=error, 0=Default, 1=IOBroker, 2=User Defined

   TF2*  fTpcCrossParameterization;
   TF2*  fTpcDipParameterization;
   TF2*  fSvtParameterization;
   TF2*  fFtpcParameterization;
};

//IOBroker inlines

inline void StiHitErrorDefault::update(Subject* changedSubject)
{
    //cout <<"StiHitErrorDefault::update(Subject*)"<<endl;
    if (changedSubject!=mSubject) {
	cout <<"StiHitErrorDefault::update(Subject*). ERROR:\t"
	     <<"changedSubject!=mSubject"<<endl;
    }
    else {
	//cout <<"getting new values"<<endl;
	getNewState();
	//cout <<"\tdone getting new values"<<endl;
    }   
}

inline void StiHitErrorDefault::forgetSubject(Subject* obsolete)
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



