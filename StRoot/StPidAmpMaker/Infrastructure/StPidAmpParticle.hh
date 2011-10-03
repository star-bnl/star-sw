/***************************************************************************
 *
 * $Id: StPidAmpParticle.hh,v 1.1.1.1 2000/03/09 17:48:34 aihong Exp $
 *
 * Author: Aihong Tang & Richard Witt (FORTRAN Version),Kent State U.
 *         Send questions to aihong@cnr.physics.kent.edu
 ***************************************************************************
 *
 * Description:part of StPidAmpMaker package
 *             particle type definition
 *             
 ***************************************************************************
 *
 * $Log: StPidAmpParticle.hh,v $
 * Revision 1.1.1.1  2000/03/09 17:48:34  aihong
 * Installation of package
 *
 **************************************************************************/


#ifndef StPidAmpParticle_hh
#define StPidAmpParticle_hh

#include <string>

#if !defined(ST_NO_NAMESPACES)
using std::string;
#endif


#include "StPidAmpMaker/Infrastructure/StPidAmpWindow.hh"

//avoid use "*StPidAmpPartticle=StPidAmpParticle::mElectron" 
//use object instead pointer for copying.
//see test9.C for the reason.(TMyPidGraph::mPts not got copied by operator "=")

class StPidAmpParticle {

public:

    StPidAmpParticle(const string  name,
                     int            id, //geant ID
		     double         charge, //in units of e
		     double         mass,
                     double         peakPos,//peak of pop. on rig axis
                     double         maxllWidth,//maxllBltz fitting width.
                     double         maxllRatio,//peakHeight / # total tracks.
		     double         rigStart,
                     double         rigEnd,
                     StPidAmpWindow  wd);
      
    StPidAmpParticle();

  ~StPidAmpParticle();
      
  string name() const;         
  int     id() const;          
  double  charge() const;        
  double  mass() const;          
  double  maxllPeakPos() const;  
  double  maxllWidth() const;    
  double  maxllRatio() const;    
  double  start() const;       
  double  end() const;         
  StPidAmpWindow window() const; 


  void setName(string s);
  void setId(int i);
  void setCharge(double chg);
  void setMass(double m);
  void setMaxllPeakPos(double pkPos);
  void setMaxllWidth(double w);
  void setMaxllRatio(double r);
  void setStart(double st);
  void setEnd(double ed);
  void setWindow(StPidAmpWindow& w);

  static const StPidAmpParticle mPositron;
  static const StPidAmpParticle mElectron;
  static const StPidAmpParticle mPiPlus;
  static const StPidAmpParticle mPiMinus;
  static const StPidAmpParticle mKaonPlus;
  static const StPidAmpParticle mKaonMinus;
  static const StPidAmpParticle mProton;
  static const StPidAmpParticle mAntiProton;
  static const StPidAmpParticle mDeuteron;

  static const StPidAmpParticle mBGParticle;
  static const StPidAmpParticle mBGElectronParticle;
  static const StPidAmpParticle mBGPositronParticle;
  static const StPidAmpParticle mBGPionPlusParticle;
  static const StPidAmpParticle mBGPionMinusParticle;
  static const StPidAmpParticle mBGKaonPlusParticle;
  static const StPidAmpParticle mBGKaonMinusParticle;
  static const StPidAmpParticle mBGProtonParticle;
  static const StPidAmpParticle mBGAntiProtonParticle;


private:
  
  string         mName;
  int            mID;
  double         mCharge;
  double         mMass;
  double         mMaxllPeakPos;
  double         mMaxllWidth;
  double         mMaxllRatio;//peakHeight of amp grph./ # of total tracks.  >0
  double         mStart;
  double         mEnd;
  StPidAmpWindow mWindow;


};


inline  string  StPidAmpParticle::name() const           {return mName;}
inline  int     StPidAmpParticle::id() const             {return mID;}
inline  double  StPidAmpParticle::charge() const         {return mCharge;}
inline  double  StPidAmpParticle::mass() const           {return mMass;}
inline  double  StPidAmpParticle::maxllPeakPos() const   {return mMaxllPeakPos;}
inline  double  StPidAmpParticle::maxllWidth() const     {return mMaxllWidth;}
inline  double  StPidAmpParticle::maxllRatio() const     {return mMaxllRatio;}
inline  double  StPidAmpParticle::start() const          {return mStart;} //might be positive  or -
inline  double  StPidAmpParticle::end() const            {return mEnd;}  
inline  StPidAmpWindow StPidAmpParticle::window() const  {return mWindow;}

inline void StPidAmpParticle::setName(string s){ mName=s;}
inline void StPidAmpParticle::setId(int i){mID=i;}
inline void StPidAmpParticle::setCharge(double chg){mCharge=chg;}
inline void StPidAmpParticle::setMass(double m){mMass=m;}
inline void StPidAmpParticle::setMaxllPeakPos(double pkPos){mMaxllPeakPos=pkPos;}
inline void StPidAmpParticle::setMaxllWidth(double w){mMaxllWidth=w;}
inline void StPidAmpParticle::setMaxllRatio(double r){mMaxllRatio=r;}
inline void StPidAmpParticle::setStart(double st){mStart=st;}
inline void StPidAmpParticle::setEnd(double ed){mEnd=ed;}


#endif

