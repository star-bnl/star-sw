/***************************************************************************
 *
 * $Id: StuProbabilityPidAlgorithm.h,v 1.9 2000/10/24 22:36:47 aihong Exp $
 *
 * Author:Aihong Tang, Richard Witt(FORTRAN version). Kent State University
 *        Send questions to aihong@cnr.physics.kent.edu 
 ***************************************************************************
 *
 * Description: A functor that do PID base on Probability (Amplitude) info.
 *              
 ***************************************************************************
 *
 * $Log: StuProbabilityPidAlgorithm.h,v $
 * Revision 1.9  2000/10/24 22:36:47  aihong
 * pass if no parameter file read
 *
 * Revision 1.8  2000/09/11 17:08:29  aihong
 * add geantID access
 *
 * Revision 1.7  2000/08/23 01:18:14  aihong
 * remove a bug
 *
 * Revision 1.6  2000/08/16 12:46:07  aihong
 * bug killed
 *
 * Revision 1.5  2000/08/15 23:04:18  aihong
 * speed it up by looking up table
 *
 * Revision 1.4  2000/07/22 22:45:27  aihong
 * change include path
 *
 * Revision 1.3  2000/05/05 19:25:39  aihong
 * modified ctor
 *
 * Revision 1.2  2000/03/09 20:44:56  aihong
 * add head for Log
 *
 **************************************************************************/
#ifndef StuProbabilityPidAlgorithm_hh
#define StuProbabilityPidAlgorithm_hh


#include "TString.h"
#include "TObjArray.h"
#include "TMap.h"
//#include "TGraph.h"
#include "TVectorD.h"


//#include "tables/St_tpcDedxPidAmpDb_Table.h"
#include "StEventTypes.h"
#include "StParticleTable.hh"
#include "StParticleTypes.hh"
#include "StEventUtilities/StPidAmpNetOut.h"
#include "StEventUtilities/StuObjPidReport.h"

class StEvent;
class StPidAmpNetOut;
class StuObjPidReport;
class TString;
class TObject;
class TObjArray;



class StuProbabilityPidAlgorithm : public StPidAlgorithm {

 public:

      StuProbabilityPidAlgorithm(StEvent& ev);
      virtual  ~StuProbabilityPidAlgorithm();



      StParticleDefinition* mostLikelihoodParticle();
      StParticleDefinition* secondLikelihoodParticle();     
      StParticleDefinition* thirdLikelihoodParticle();

      StParticleDefinition* getParticle(int i);

      int mostLikelihoodParticleGeantID()    const;
      int secondLikelihoodParticleGeantID()  const;     
      int thirdLikelihoodParticleGeantID()   const;

      int getParticleGeantID(int i)          const;
      
      double getProbability(int i);
      double mostLikelihoodProbability();
      double secondLikelihoodProbability();
      double thirdLikelihoodProbability();
  
      bool isExtrap();

      StParticleDefinition*
      operator() (const StTrack&, const StSPtrVecTrackPidTraits&);

      
      static void setDedxMethod(StDedxMethod method);
             //use what dedx to perform the job.
      static void setDynamicallyCalculatePID();//set a tag.
    
      static void readParametersFromFile(TString fileName);
      //      static void readParametersFromTable(St_Table* tb);
                  //from data base.
      static void printParameters();


     static  void   fillReportObjArray(TString fileName);

     /*     
     static  void   debug(double theDedx, double theRig, double theNHits, double thePt, int theCharge, double theDca);
     static  void   debug2(double theDedx, double theRig, int theNHits, double thePt, int theCharge, double theDca);
     */



 private:
     
     void    fillPIDByLookUpTable(int myCharge, double myDca, int myNhits, double myPt, double myDedx, double myRig);
     void    fillPIDByCalculation(int myCharge, double myDca, int myNhits, double myPt, double myDedx, double myRig);


     static  double bandCenter(double rig, TArrayD* bandPars);
     static  double bandCenter(double rig, int NChannel, int NType);

     static  double resolution(double rig, TArrayD* linrPars,TArrayD* bandPars);
     static  double resolution(double rig, int NChannel, int NType);

     static  double peak(double rig, TArrayD* ampPars);
     static  double peak(double rig, int NChannel, int NType);

     static  double tossTail(double rig);


     static  double amplitude(double dedx, double rig, TArrayD* bandPars, TArrayD* linrPars, TArrayD* ampPars);
     static  double amplitude(double dedx, double rig, int NChannel, int NType);

      static void readAType(StParticleDefinition* def, Float_t* mean, Float_t* amp,Float_t* sig,Float_t cal,TObjArray*  theChannel);
      //      static void refreshParameters(St_Table* theTable);

     static  bool   tagExtrap(double rig, double dedx,TObjArray* channelLevel);

     static  void   fill(double* probAry, int* pidAry, double prob, StPidAmpNetOut* netOut); 
     static  double lowRigReso(double xa, double xb, double ya, double yb,double theX);
     static  void   setBins4Table();
     static  void   setRanges4Table(TVectorD* theSetting);


      void   fill(double prob, StPidAmpNetOut* netOut);
      void   fillAsUnknown();
      void   lowRigPID(double rig,double dedx,int theCharge);


      StParticleTable* table;

      int      PID[3];
      double   mProb[3];

      bool     mExtrap;

      StEvent* mEvent; //!

     static    double   mNoise;
     static    double   mDedxStart;
     static    double   mDedxEnd;
     static    double   mRigStart;
     static    double   mRigEnd;
     static    double   mNHitsStart;
     static    double   mNHitsEnd;
     static    double   mPtStart;
     static    double   mPtEnd;
     static    double   mDcaCutPoint; //just 2 bins for dca

    static  bool     mTurnOnNoise;
    static  bool     mDynamicallyCalculatePID;
    static  bool     mHasParameterFile;
    static  double   mDedxBinWidth;
    static  double   mRigBinWidth; //do not let NBins exceed 999.
    static  double   mPtBinWidth; //not implemented yet.
    static  double   mNHitsBinWidth;
      
    static  int      mNDedxBins;
    static  int      mNRigBins;
    static  int      mNNhitsBins;
    static  int      mNPtBins;
    static  int      mNDcaBins;
    static  int      mNChargeBins;

  static  StDedxMethod mDedxMethod;
  static  TObjArray    mDataSet;
  //array of Channel datas. be initialize by readParametersFromFile
  static  TObjArray    mMaxllFuncSet; 
  static  TObjArray    mLinearFuncSet;
  static  TObjArray    mBetheBlochFuncSet; 


 //parallel sturcture with mDataSet, 
  //but there functions taken parameters from mFuncSet.

  static TObjArray*    mTheReportTable;
  static TVectorD*     mTheRangeSettingVector;

};


inline   int StuProbabilityPidAlgorithm::mostLikelihoodParticleGeantID()    const { return PID[0];}
inline   int StuProbabilityPidAlgorithm::secondLikelihoodParticleGeantID()  const { return PID[1];}     
inline   int StuProbabilityPidAlgorithm::thirdLikelihoodParticleGeantID()   const { return PID[2];}  
inline   int StuProbabilityPidAlgorithm::getParticleGeantID(int i)          const {
          if (i<3 && i>=0) return PID[i]; else return -1;}

#endif

