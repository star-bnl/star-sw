/***************************************************************************
 *
 * $Id: StuProbabilityPidAlgorithm.h,v 1.3 2000/05/05 19:25:39 aihong Exp $
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
//#include "tables/St_tpcDedxPidAmpDb_Table.h"
#include "StEventTypes.h"
#include "StParticleTable.hh"
#include "StParticleTypes.hh"
#include "StPidAmpMaker/StPidAmpNetOut.h"

class StEvent;


class StuProbabilityPidAlgorithm : public StPidAlgorithm {

 public:

      StuProbabilityPidAlgorithm(StEvent& ev);
      virtual  ~StuProbabilityPidAlgorithm();


      
      void setDedxMethod(StDedxMethod method);//use what dedx to perform the job.

      StParticleDefinition* mostLikelihoodParticle();
      StParticleDefinition* secondLikelihoodParticle();     
      StParticleDefinition* thirdLikelihoodParticle();

      StParticleDefinition* getParticle(int i);
      
      double getProbability(int i);
      double mostLikelihoodProbability();
      double secondLikelihoodProbability();
      double thirdLikelihoodProbability();
      double lowRigReso(double xa, double xb, double ya, double yb,double theX);
      bool isExtrap();

      StParticleDefinition*
      operator() (const StTrack&, const StSPtrVecTrackPidTraits&);


      static void readParametersFromFile(TString fileName);
      //      static void readParametersFromTable(St_Table* tb);
      static void printParameters();
      //      static void refreshParameters(St_Table* theTable);
      static void readAType(StParticleDefinition* def, Float_t* mean, Float_t* amp,Float_t* sig,Float_t cal,TObjArray*  theChannel);

 private:

      double ( *funcBandPt) (double *, double *);
      double ( *funcAmpPt ) (double *, double *);
      double ( *funcResoPt) (double *, double *);

     
      double bandCenter(double rig, TArrayD* bandPars);
      double resolution(double rig, TArrayD* linrPars,TArrayD* bandPars);
      double peak(double rig, TArrayD* ampPars);
      double tossTail(double rig);
      void   lowRigPID(double rig,double dedx);
      void   fill(double prob, StPidAmpNetOut* netOut);
      void   fillAsUnknown();
      void   tagExtrap(double rig, double dedx,TObjArray* channelLevel);

      double amplitude(double dedx, double rig, TArrayD* bandPars, TArrayD* linrPars, TArrayD* ampPars);

      void lowRigPID();

      StDedxMethod mDedxMethod;
      StParticleTable* table;

      int      PID[3];
      double   mProb[3];
      double   mNoise;
      bool     mExtrap;
      bool     mTurnOnNoise;
      StEvent* mEvent; //!
      


  static TObjArray mDataSet;//array of Channel datas. be initialize by readParametersFromFile
  //  static St_Table* mDataTable;



};

#endif

