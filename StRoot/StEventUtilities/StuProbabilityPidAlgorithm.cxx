/***************************************************************************
 *
 * $Id: StuProbabilityPidAlgorithm.cxx,v 1.34 2004/09/19 00:07:28 perev Exp $
 *
 * Author:Aihong Tang, Richard Witt(FORTRAN version). Kent State University
 *        Send questions to aihong@cnr.physics.kent.edu 
 ***************************************************************************
 *
 * Description: A functor that do PID base on Probability (Amplitude) info.
 *
 ***************************************************************************
 *
 * $Log: StuProbabilityPidAlgorithm.cxx,v $
 * Revision 1.34  2004/09/19 00:07:28  perev
 * Small Walgrind leak fixed
 *
 * Revision 1.33  2004/04/09 15:46:16  aihong
 * add isPIDTableRead()
 *
 * Revision 1.32  2003/09/02 17:58:09  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.31  2003/06/24 02:53:14  aihong
 * update for dAu PIDtable
 *
 * Revision 1.30  2003/05/02 21:32:08  aihong
 * destroy myBandBGFcn in destructor
 *
 * Revision 1.29  2003/04/30 20:37:12  perev
 * Warnings cleanup. Modified lines marked VP
 *
 * Revision 1.28  2002/12/20 20:26:44  aihong
 * let it handle P02gd table
 *
 * Revision 1.27  2002/12/11 15:35:53  aihong
 * put in fabs in processPIDAsFuntion()
 *
 * Revision 1.26  2002/04/19 16:36:41  perev
 * bug fix,init to zero
 *
 * Revision 1.25  2002/01/17 03:25:27  aihong
 * add production Tag to take care of different centrality def. between different productions
 *
 * Revision 1.24  2001/03/21 18:16:13  aihong
 * constructor without StEvent added
 *
 * Revision 1.23  2001/03/21 17:54:30  aihong
 * add processPIDAsFunction()
 *
 * Revision 1.22  2000/12/28 21:00:57  aihong
 * remove temp. fix
 *
 * Revision 1.21  2000/12/26 23:27:05  aihong
 * temperory fix for e amp tail
 *
 * Revision 1.20  2000/12/20 16:55:16  aihong
 * let it survive when no support PIDTable is present
 *
 * Revision 1.19  2000/12/18 23:51:36  aihong
 * mExtrap related bug fixed
 *
 * Revision 1.10  2000/08/16 12:46:07  aihong
 * bug killed
 *
 * Revision 1.9  2000/08/15 23:04:18  aihong
 * speed it up by looking up table
 *
 * Revision 1.8  2000/07/22 22:45:27  aihong
 * change include path
 *
 * Revision 1.7  2000/07/12 16:29:38  aihong
 * change to avoid possible name confliction from StarClassLibary
 *
 * Revision 1.6  2000/05/24 14:35:41  ullrich
 * Added 'const' to compile on Sun CC5.
 *
 * Revision 1.5  2000/05/05 19:25:39  aihong
 * modified ctor
 *
 * Revision 1.2  2000/03/09 20:45:04  aihong
 * add head for Log
 *
 **************************************************************************/
#include <float.h>
#include <Stsstream.h>
#include "Stiostream.h" //this line should be deleted later
#include "TFile.h"
#include "TF1.h"

#include "StMessMgr.h"
#include "StPhysicalHelixD.hh"
#include "PhysicalConstants.h"

#include "StuProbabilityPidAlgorithm.h"

#include "StEventUtilities/BetheBlochFunction.hh"
#include "StEventUtilities/MaxllBoltz.hh"
#include "StEventUtilities/Linear.hh"

#include "StEventUtilities/StuFtpcRefMult.hh"
#include "StEventUtilities/StuRefMult.hh"

//TMap::FindObject goes wild!! TMap::GetValue works.

//-------------------------------
StuProbabilityPidAlgorithm::StuProbabilityPidAlgorithm(StEvent& ev): 
               mPionMinusProb(0.),
               mElectronProb(0.),
               mKaonMinusProb(0.),
               mAntiProtonProb(0.),
               mPionPlusProb(0.),
               mPositronProb(0.),
               mKaonPlusProb(0.),
	       mProtonProb(0.){


      PID[0]=-1;//should be sth.standard say unIdentified.
      PID[1]=-1;     
      PID[2]=-1;
      PID[3]=-1;

      mProb[0]=0;
      mProb[1]=0;
      mProb[2]=0;
      mProb[3]=0;

     table = StParticleTable::instance();

     mExtrap=false;
     mEvent=&ev;

	//init funtions
  myBandBGFcn
     =new TF1("myBandBGFcn",BetheBlochFunction, 0.,5., 7); 

  Double_t myPars[7]
        ={ 1.072, 0.3199, 1.66032e-07, 1, 1, 2.71172e-07, 0.0005 };

   myBandBGFcn->SetParameters(&myPars[0]);

}

//-------------------------------
StuProbabilityPidAlgorithm::StuProbabilityPidAlgorithm(): 
               mPionMinusProb(0.),
               mElectronProb(0.),
               mKaonMinusProb(0.),
               mAntiProtonProb(0.),
               mPionPlusProb(0.),
               mPositronProb(0.),
               mKaonPlusProb(0.),
	       mProtonProb(0.){


      PID[0]=-1;//should be sth.standard say unIdentified.
      PID[1]=-1;     
      PID[2]=-1;
      PID[3]=-1;

      mProb[0]=0;
      mProb[1]=0;
      mProb[2]=0;
      mProb[3]=0;

     table = StParticleTable::instance();

     mExtrap=false;

	//init funtions
  myBandBGFcn
     =new TF1("myBandBGFcn",BetheBlochFunction, 0.,5., 7); 

  Double_t myPars[7]
        ={ 1.072, 0.3199, 1.66032e-07, 1, 1, 2.71172e-07, 0.0005 };

   myBandBGFcn->SetParameters(&myPars[0]);

}

//-------------------------------
StuProbabilityPidAlgorithm::~StuProbabilityPidAlgorithm(){
  /* no op */
  delete myBandBGFcn;
}
//-------------------------------
void StuProbabilityPidAlgorithm::setDedxMethod(StDedxMethod method){
      StuProbabilityPidAlgorithm::mDedxMethod=method;
}

//-------------------------------
bool StuProbabilityPidAlgorithm::isPIDTableRead(){
      return StuProbabilityPidAlgorithm::mPIDTableRead;
}


//-------------------------------
StParticleDefinition* StuProbabilityPidAlgorithm::mostLikelihoodParticle(){
  
       
      return table->findParticleByGeantId(PID[0]);
}

//-------------------------------
StParticleDefinition* StuProbabilityPidAlgorithm::secondLikelihoodParticle(){
  
       
      return table->findParticleByGeantId(PID[1]);
}

//-------------------------------
StParticleDefinition* StuProbabilityPidAlgorithm::thirdLikelihoodParticle(){
  
       
      return table->findParticleByGeantId(PID[2]);
}
//-------------------------------
StParticleDefinition* StuProbabilityPidAlgorithm::getParticle(int i){

   if (i>=0 && i<4){
           return table->findParticleByGeantId(PID[i]);
   }   else { 

	gMessMgr->Error()<<"StuProbabilityPidAlgorithm::getParticle(int i), i must be 0,1,2,3 only. "<<endm;

     return 0;
   }
 }
//-------------------------------
double StuProbabilityPidAlgorithm::getProbability(int i){
   if (i>=0 && i<4){
           return mProb[i];
   }   else { 

	gMessMgr->Error()<<"StuProbabilityPidAlgorithm::getProbability(int i), i must be 0,1,2,3 only. "<<endm;

     return 0.0;
 }
   
} 

//-------------------------------
double StuProbabilityPidAlgorithm::mostLikelihoodProbability(){
           return mProb[0];
 }
//-------------------------------
double StuProbabilityPidAlgorithm::secondLikelihoodProbability(){
           return mProb[1];
 }
//-------------------------------
double StuProbabilityPidAlgorithm::thirdLikelihoodProbability(){
           return mProb[2];
 }

//-------------------------------
bool StuProbabilityPidAlgorithm::isExtrap(){
           return mExtrap;
}


//-------------------------------
StParticleDefinition* StuProbabilityPidAlgorithm::operator() (const StTrack& theTrack, const StSPtrVecTrackPidTraits& traits){



      PID[0]=-1;//should be sth.standard say unIdentified.
      PID[1]=-1;     
      PID[2]=-1;
      PID[3]=-1;

     mProb[0]=0;
     mProb[1]=0;
     mProb[2]=0;
     mProb[3]=0;

     mExtrap=false;


     mPionMinusProb=0.;
     mElectronProb=0.;
     mKaonMinusProb=0.;
     mAntiProtonProb=0.;
     mPionPlusProb=0.;
     mPositronProb=0.;
     mKaonPlusProb=0.;
     mProtonProb=0.;

     if (mPIDTableRead) {

          double rig    =0.0;
          double dedx   =0.0;
	  double dca    =0.0; //in units of cm.
          int    nhits  =0;
          int    charge =0;
          double eta    =0.; 
          double cent   =0.; // % central


           StPrimaryVertex* primaryVtx=mEvent->primaryVertex();
    const StPhysicalHelixD& helix=theTrack.geometry()->helix();
           dca=helix.distance(primaryVtx->position());

  //cent in cross section

  if (mProductionTag){

  if ( (mProductionTag->GetString()).Contains("P01gl")
       || (mProductionTag->GetString()).Contains("P02gd") ){
        cent = getCentrality(uncorrectedNumberOfNegativePrimaries(*mEvent));  
  }  else if ( (mProductionTag->GetString()).Contains("P03ia_dAu") ){
        cent = getCentrality(uncorrectedNumberOfFtpcEastPrimaries(*mEvent)); 
  }  else {
        gMessMgr->Error()<<"Production tag "<<mProductionTag->GetString().Data()<<" in PIDTable is filled but its name is not recognized ! "<<endm;
  }


  } else { //the first PID table has no production tag
        cent = getCentrality(uncorrectedNumberOfNegativePrimaries(*mEvent));
  }


   
          const StDedxPidTraits* dedxPidTr=0;


       charge=(theTrack.geometry())->charge();

       for (int itrait = 0; itrait < int(traits.size()); itrait++){

           dedxPidTr = 0;
	   if (traits[itrait]->detector() == kTpcId) {
	     //
	     // tpc pid trait
	     //
	      const StTrackPidTraits* thisTrait = traits[itrait];
	     //
	     // perform cast to make the pid trait a dedx trait
	     //
	      dedxPidTr = dynamic_cast<const StDedxPidTraits*>(thisTrait);
             
	   }
	   if (dedxPidTr &&  dedxPidTr->method() == mDedxMethod) break;

       }

           if (dedxPidTr) {
             dedx=dedxPidTr->mean();
             nhits=dedxPidTr->numberOfPoints();
	   }


       if (dedx!=0.0 && nhits>=0 //dedx ==0.0 no sense 
	   && thisPEnd > 0. && thisEtaEnd > 0. // *End ==0, no PIDTable read.
           && thisNHitsEnd > 0. ){

    const StThreeVectorF& p=theTrack.geometry()->momentum();
    rig=double(p.mag()/charge);



    if (mProductionTag){ //for AuAu, +/- eta were folded together when building PID
      //table, for dAu, + and - eta were treated differently.

  if ( (mProductionTag->GetString()).Contains("P01gl")
       || (mProductionTag->GetString()).Contains("P02gd") ){
    eta=fabs(p.pseudoRapidity());
  }  else if ( (mProductionTag->GetString()).Contains("P03ia_dAu") ){
    eta=p.pseudoRapidity();
  }  else {
        gMessMgr->Error()<<"Production tag "<<mProductionTag->GetString().Data()<<" in PIDTable is filled but its name is not recognized ! "<<endm;
  }

  } else { //the first PID table has no production tag
    eta = fabs(p.pseudoRapidity());
  }

    rig   = fabs(rig);
    dedx  = (dedx>thisDedxStart) ? dedx : thisDedxStart;
    rig   = (rig >thisPStart)  ? rig  : thisPStart;
    rig   = (rig <thisPEnd  )  ? rig  : thisPEnd*0.9999;   
    eta   = (eta  >thisEtaStart)   ? eta   : thisEtaStart;
    eta   = (eta  <thisEtaEnd  )   ? eta   : thisEtaEnd*0.9999;
    nhits = (nhits > int(thisNHitsStart)) ? nhits : int(thisNHitsStart);
    nhits = (nhits < int(thisNHitsEnd) ) ? nhits : int(thisNHitsEnd-1);

    //----------------get all info. I want for a track. now do PID

    setCalibrations(eta, nhits);

   if (dedx<thisDedxEnd){

   fillPIDByLookUpTable(cent, dca, charge,rig, eta, nhits,dedx);

   } else { lowRigPID(rig,dedx,charge);}
 
       } else if (dedx==0.0){ fillAsUnknown();}

     // do not do deuteron or higher
      myBandBGFcn->SetParameter(3,1);
      myBandBGFcn->SetParameter(4,1.45);
      if (dedx>myBandBGFcn->Eval(rig,0,0)) fillAsUnknown();

     } else fillAsUnknown();

       fillPIDHypothis();

       return table->findParticleByGeantId(PID[0]);

 }

//-------------------------------
void StuProbabilityPidAlgorithm::lowRigPID(double rig,double dedx,int theCharge){

       double upper;
       double lower;
       double rigidity=fabs(rig);      
       double mdedx=dedx; 
      
       double fakeMass=0.;

      //pion
      fakeMass=0.32075026;
      myBandBGFcn->SetParameter(3,1);
      myBandBGFcn->SetParameter(4,0.32075026 );
        

      lower =0.;
       upper =myBandBGFcn->Eval(rigidity,0,0);

       if (mdedx>lower && mdedx<upper){
	 PID[0]=(theCharge>0.0)? 8 : 9;  //pi+/-
         mProb[0]=1.0;
         mProb[1]=0.0;
         mProb[2]=0.0;
         mProb[3]=0.0;
       }

           lower = upper;

       //kaon
	   fakeMass=0.709707;
      myBandBGFcn->SetParameter(3,1);
      myBandBGFcn->SetParameter(4,fakeMass);
        
      upper =myBandBGFcn->Eval(rigidity,0,0);

       if (mdedx>lower && mdedx<upper){
         PID[0]=(theCharge>0.0)? 11:12;  //k+/-
         mProb[0]=1.0;
         mProb[1]=0.0;
         mProb[2]=0.0;
         mProb[3]=0.0;
       }


       lower = upper;

       //proton/pBar
	   fakeMass=1.45;
      myBandBGFcn->SetParameter(3,1);
      myBandBGFcn->SetParameter(4,fakeMass);
        
      upper =myBandBGFcn->Eval(rigidity,0,0);

       if (mdedx>lower && mdedx<upper){
         PID[0]=(theCharge>0.0)? 14:15;  //proton/antiproton
         mProb[0]=1.0;
         mProb[1]=0.0;
         mProb[2]=0.0;
         mProb[3]=0.0;
       } 

       /*                
       lower = upper;

       //deuteron
	   fakeMass=2.4;
      myBandBGFcn->SetParameter(3,1);
      myBandBGFcn->SetParameter(4,fakeMass);

      upper =myBandBGFcn->Eval(rigidity,0,0);

       if (mdedx>lower && mdedx<upper){
         PID[0]=45;  //deuteron
         mProb[0]=1.0;
         mProb[1]=0.0;
         mProb[2]=0.0;
         mProb[3]=0.0;
       }

       lower = upper;

       //triton
       m = -1.5374;  //New slope needed for deuterons and tritons.
       a = 1.8121e-5;
       upper = a*::pow(rigidity,m);
       if (mdedx>lower && mdedx<upper){
         PID[0]=46;  //triton
         mProb[0]=1.0;
         mProb[1]=0.0;
         mProb[2]=0.0;
         mProb[3]=0.0;
       }

       */



}  



//-------------------------------
void StuProbabilityPidAlgorithm::fillAsUnknown(){

  for (int i=0; i<4; i++) {
      PID[i]=-1; mProb[i]=-1;
  }
}


//-------------------------------
void StuProbabilityPidAlgorithm::readParametersFromFile(TString fileName){



      TFile f(fileName,"READ");

      if (f.IsOpen()){

        delete StuProbabilityPidAlgorithm::mEAmp     ;
        delete StuProbabilityPidAlgorithm::mECenter  ;
        delete StuProbabilityPidAlgorithm::mESigma   ;
        delete StuProbabilityPidAlgorithm::mPiAmp    ;
        delete StuProbabilityPidAlgorithm::mPiCenter ;
        delete StuProbabilityPidAlgorithm::mPiSigma  ;
        delete StuProbabilityPidAlgorithm::mKAmp     ;
        delete StuProbabilityPidAlgorithm::mKCenter  ;
        delete StuProbabilityPidAlgorithm::mKSigma   ;
        delete StuProbabilityPidAlgorithm::mPAmp     ;
        delete StuProbabilityPidAlgorithm::mPCenter  ;
        delete StuProbabilityPidAlgorithm::mPSigma   ;
        delete StuProbabilityPidAlgorithm::mEqualyDividableRangeStartSet;
        delete StuProbabilityPidAlgorithm::mEqualyDividableRangeEndSet;
        delete StuProbabilityPidAlgorithm::mEqualyDividableRangeNBinsSet;
        delete StuProbabilityPidAlgorithm::mNoEqualyDividableRangeNBinsSet;
        delete StuProbabilityPidAlgorithm::mMultiBinEdgeSet ;
        delete StuProbabilityPidAlgorithm::mDcaBinEdgeSet   ;
        delete StuProbabilityPidAlgorithm::mBBPrePar;
        delete StuProbabilityPidAlgorithm::mBBTurnOver;
        delete StuProbabilityPidAlgorithm::mBBOffSet;
        delete StuProbabilityPidAlgorithm::mBBScale;
        delete StuProbabilityPidAlgorithm::mBBSaturate;
        delete StuProbabilityPidAlgorithm::mProductionTag;

	StuProbabilityPidAlgorithm::mEAmp     =(TVectorD* )f.Get("eAmp");
	StuProbabilityPidAlgorithm::mECenter  =(TVectorD* )f.Get("eCenter");
	StuProbabilityPidAlgorithm::mESigma   =(TVectorD* )f.Get("eSigma");

	StuProbabilityPidAlgorithm::mPiAmp    =(TVectorD* )f.Get("piAmp");
	StuProbabilityPidAlgorithm::mPiCenter =(TVectorD* )f.Get("piCenter");
	StuProbabilityPidAlgorithm::mPiSigma  =(TVectorD* )f.Get("piSigma");

	StuProbabilityPidAlgorithm::mKAmp     =(TVectorD* )f.Get("kAmp");
	StuProbabilityPidAlgorithm::mKCenter  =(TVectorD* )f.Get("kCenter");
	StuProbabilityPidAlgorithm::mKSigma   =(TVectorD* )f.Get("kSigma");

	StuProbabilityPidAlgorithm::mPAmp     =(TVectorD* )f.Get("pAmp");
	StuProbabilityPidAlgorithm::mPCenter  =(TVectorD* )f.Get("pCenter");
	StuProbabilityPidAlgorithm::mPSigma   =(TVectorD* )f.Get("pSigma");

	StuProbabilityPidAlgorithm::mEqualyDividableRangeStartSet
                        =(TVectorD* )f.Get("EqualyDividableRangeStartSet");
	StuProbabilityPidAlgorithm::mEqualyDividableRangeEndSet
                        =(TVectorD* )f.Get("EqualyDividableRangeEndSet");
	StuProbabilityPidAlgorithm::mEqualyDividableRangeNBinsSet
                        =(TVectorD* )f.Get("EqualyDividableRangeNBinsSet");
	StuProbabilityPidAlgorithm::mNoEqualyDividableRangeNBinsSet
                        =(TVectorD* )f.Get("NoEqualyDividableRangeNBinsSet");

	StuProbabilityPidAlgorithm::mMultiBinEdgeSet 
                        =(TVectorD* )f.Get("MultiBinEdgeSet");
	StuProbabilityPidAlgorithm::mDcaBinEdgeSet   
                        =(TVectorD* )f.Get("DcaBinEdgeSet");


	StuProbabilityPidAlgorithm::mBBPrePar
                   	  =(TVectorD* )f.Get("BBPrePar");
	StuProbabilityPidAlgorithm::mBBTurnOver
                   	  =(TVectorD* )f.Get("BBTurnOver");
	StuProbabilityPidAlgorithm::mBBOffSet
                	  =(TVectorD* )f.Get("BBOffSet");
	StuProbabilityPidAlgorithm::mBBScale
                   	  =(TVectorD* )f.Get("BBScale");
	StuProbabilityPidAlgorithm::mBBSaturate
                   	  =(TVectorD* )f.Get("BBSaturate");

	StuProbabilityPidAlgorithm::mProductionTag
                	  =(TObjString* )f.Get("productionTag");



	StuProbabilityPidAlgorithm::thisMultBins
                        =int((*mNoEqualyDividableRangeNBinsSet)(0));
	StuProbabilityPidAlgorithm::thisDcaBins
                        =int((*mNoEqualyDividableRangeNBinsSet)(1));
	StuProbabilityPidAlgorithm::thisChargeBins
                        =int((*mNoEqualyDividableRangeNBinsSet)(2));


	StuProbabilityPidAlgorithm::thisPBins
	                 =int((*mEqualyDividableRangeNBinsSet)(1));
	StuProbabilityPidAlgorithm::thisEtaBins
                         =int((*mEqualyDividableRangeNBinsSet)(2));
	StuProbabilityPidAlgorithm::thisNHitsBins
                         =int((*mEqualyDividableRangeNBinsSet)(3));


	StuProbabilityPidAlgorithm::thisDedxStart
	                 =(*mEqualyDividableRangeStartSet)(0);
	StuProbabilityPidAlgorithm::thisPStart
                         =(*mEqualyDividableRangeStartSet)(1);
	StuProbabilityPidAlgorithm::thisEtaStart
                         =(*mEqualyDividableRangeStartSet)(2);
	StuProbabilityPidAlgorithm::thisNHitsStart
                         =(*mEqualyDividableRangeStartSet)(3);

	StuProbabilityPidAlgorithm::thisDedxEnd
                    	  =(*mEqualyDividableRangeEndSet)(0);
	StuProbabilityPidAlgorithm::thisPEnd
                	  =(*mEqualyDividableRangeEndSet)(1);
	StuProbabilityPidAlgorithm::thisEtaEnd
                	  =(*mEqualyDividableRangeEndSet)(2);
	StuProbabilityPidAlgorithm::thisNHitsEnd
                	  =(*mEqualyDividableRangeEndSet)(3);




        StuProbabilityPidAlgorithm::mPIDTableRead=true;

      } else if (!f.IsOpen()) {
   
	gMessMgr->Error()<<"Data file "<<fileName<<" open failed "<<endm;
        return;
      }

}

/* uncomment this method when data base is available and stable.
//-------------------------------
void StuProbabilityPidAlgorithm::readParametersFromTable(St_Table* tb){


  if (mDataTable!=tb) {//database changed.
     StuProbabilityPidAlgorithm::refreshParameters(tb);
     mDataTable=tb;
  }else if (mDataTable==tb) {return;}//database no change. no need refreshing.

}
*/

/*uncomment this method when data base is available and stable.
//-------------------------------
void StuProbabilityPidAlgorithm::refreshParameters(St_Table* theTable){
   int i;

  if (mDataSet.GetEntries()>0) mDataSet.Delete();

  St_tpcDedxPidAmpDb* temp=(St_tpcDedxPidAmpDb *)theTable;
  tpcDedxPidAmpDb_st* pars=(tpcDedxPidAmpDb_st *)temp->GetTable();
  
  StPidAmpChannelInfoOut* theInfoOut=new StPidAmpChannelInfoOut(0,45,0,FLT_MAX);
  
  TObjArray* theOnlyChannel=new TObjArray();
  theOnlyChannel->Add(theInfoOut);

  StuProbabilityPidAlgorithm::readAType(StElectron::instance(),pars->eMeanPar, pars->eAmpPar, pars->eSigPar,pars->gasCalib, theOnlyChannel);



     mDataSet.Add(theOnlyChannel);


}

*/



//-------------------------------
int StuProbabilityPidAlgorithm::thisMultBins=0;
int StuProbabilityPidAlgorithm::thisDcaBins=0;
int StuProbabilityPidAlgorithm::thisChargeBins=0;

int StuProbabilityPidAlgorithm::thisPBins=0;
int StuProbabilityPidAlgorithm::thisEtaBins=0;
int StuProbabilityPidAlgorithm::thisNHitsBins=0;

double StuProbabilityPidAlgorithm::thisDedxStart=0.;
double StuProbabilityPidAlgorithm::thisDedxEnd=0;
double StuProbabilityPidAlgorithm::thisPStart=0;
double StuProbabilityPidAlgorithm::thisPEnd=0;
double StuProbabilityPidAlgorithm::thisEtaStart=0;
double StuProbabilityPidAlgorithm::thisEtaEnd=0;
double StuProbabilityPidAlgorithm::thisNHitsStart=0;
double StuProbabilityPidAlgorithm::thisNHitsEnd=0;


bool StuProbabilityPidAlgorithm::mPIDTableRead=false;

StDedxMethod  StuProbabilityPidAlgorithm::mDedxMethod=kTruncatedMeanId;

TVectorD* StuProbabilityPidAlgorithm::mEAmp    = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mECenter = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mESigma  = new TVectorD();

TVectorD* StuProbabilityPidAlgorithm::mPiAmp    = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mPiCenter = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mPiSigma  = new TVectorD();

TVectorD* StuProbabilityPidAlgorithm::mKAmp    = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mKCenter = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mKSigma  = new TVectorD();

TVectorD* StuProbabilityPidAlgorithm::mPAmp    = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mPCenter = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mPSigma  = new TVectorD();


TVectorD* StuProbabilityPidAlgorithm::mEqualyDividableRangeStartSet   = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mEqualyDividableRangeEndSet     = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mEqualyDividableRangeNBinsSet   = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mNoEqualyDividableRangeNBinsSet = new TVectorD();

TVectorD* StuProbabilityPidAlgorithm::mMultiBinEdgeSet = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mDcaBinEdgeSet   = new TVectorD();  

TVectorD* StuProbabilityPidAlgorithm::mBBPrePar   = new TVectorD(); 
TVectorD* StuProbabilityPidAlgorithm::mBBTurnOver = new TVectorD(); 
TVectorD* StuProbabilityPidAlgorithm::mBBOffSet   = new TVectorD();
TVectorD* StuProbabilityPidAlgorithm::mBBScale    = new TVectorD(); 
TVectorD* StuProbabilityPidAlgorithm::mBBSaturate = new TVectorD(); 

TObjString* StuProbabilityPidAlgorithm::mProductionTag = new TObjString();




//-------------------------------
//St_Table* StuProbabilityPidAlgorithm::mDataTable=0;




//-------------------------------
void StuProbabilityPidAlgorithm::fillPIDByLookUpTable(double myCentrality, double myDca, int myCharge, double myRig, double myEta, int myNhits, double myDedx)
{


  //assume bound has been checked before they enter this func.
  int theMultBin=getCentralityBin(myCentrality);
  int theDcaBin = (myDca>(*mDcaBinEdgeSet)(1)) ? 1 : 0;
  int theChargeBin=(myCharge>0) ? 1 : 0;
  int thePBin=int(thisPBins*myRig/(thisPEnd-thisPStart));
  int theEtaBin=int(thisEtaBins*(myEta-thisEtaStart)/(thisEtaEnd-thisEtaStart));
  int theNHitsBin=int(thisNHitsBins*float(myNhits)/(thisNHitsEnd-thisNHitsStart));


    int totalEntry
      = thisMultBins*thisDcaBins*thisChargeBins*thisPBins*thisEtaBins*thisNHitsBins;
    
    int positionPointer=0;
    
    totalEntry=totalEntry/thisMultBins;
    positionPointer=positionPointer+totalEntry*theMultBin;
    
    totalEntry=totalEntry/thisDcaBins;
    positionPointer=positionPointer+totalEntry*theDcaBin;
    
    totalEntry=totalEntry/thisChargeBins;
    positionPointer=positionPointer+totalEntry*theChargeBin;
    
    totalEntry=totalEntry/thisPBins;
    positionPointer=positionPointer+totalEntry*thePBin;
    
    totalEntry=totalEntry/thisEtaBins;
    positionPointer=positionPointer+totalEntry*theEtaBin;
    
    totalEntry=totalEntry/thisNHitsBins;
    positionPointer=positionPointer+totalEntry*theNHitsBin;



    //now calculate prob.
    TF1 eGaus("eGaus","gaus",thisDedxStart,thisDedxStart);
    TF1 piGaus("piGaus","gaus",thisDedxStart,thisDedxStart);
    TF1 kGaus("kGaus","gaus",thisDedxStart,thisDedxStart);
    TF1 pGaus("pGaus","gaus",thisDedxStart,thisDedxStart);

    
    eGaus.SetParameter(0, (*mEAmp)(positionPointer));
    eGaus.SetParameter(1, (*mECenter)(positionPointer));
    eGaus.SetParameter(2, (*mESigma)(positionPointer));

    piGaus.SetParameter(0, (*mPiAmp)(positionPointer));
    piGaus.SetParameter(1, (*mPiCenter)(positionPointer));
    piGaus.SetParameter(2, (*mPiSigma)(positionPointer));

    kGaus.SetParameter(0, (*mKAmp)(positionPointer));
    kGaus.SetParameter(1, (*mKCenter)(positionPointer));
    kGaus.SetParameter(2, (*mKSigma)(positionPointer));

    pGaus.SetParameter(0, (*mPAmp)(positionPointer));
    pGaus.SetParameter(1, (*mPCenter)(positionPointer));
    pGaus.SetParameter(2, (*mPSigma)(positionPointer));


    double eContribution=0;
   if (mEAmp && mECenter && mESigma) eContribution = eGaus.Eval(myDedx,0.,0.);
   //   if (fabs(myRig)>0.95) eContribution=0; //will deal with it later.


    double piContribution=0;
   if (mPiAmp && mPiCenter && mPiSigma) piContribution = piGaus.Eval(myDedx,0.,0.);

    double kContribution=0;
   if (mKAmp && mKCenter && mKSigma) kContribution = kGaus.Eval(myDedx,0.,0.);

    double pContribution=0;
   if (mPAmp && mPCenter && mPSigma) pContribution = pGaus.Eval(myDedx,0.,0.);


   double total = eContribution+piContribution+kContribution+pContribution;

   double eProb=0; double piProb=0; double kProb=0; double pProb=0;

   if (total>0) {
     eProb=eContribution/total;
     piProb=piContribution/total;
     kProb=kContribution/total;
     pProb=pContribution/total;
   }


	    fill(eProb,  int((myCharge>0) ? 2 : 3   ));
	    fill(piProb, int((myCharge>0) ? 8 : 9   ));
	    fill(kProb,  int((myCharge>0) ? 11 : 12 ));
	    fill(pProb,  int((myCharge>0) ? 14 : 15 ));

	    int nn=-1;
            float PathHeight=1.0e-7;
            float halfHeight=(11-2.0)*PathHeight/2.0;

     if (fabs(myRig) > 0.3) {
      if ((myDedx<(((*mPiCenter)(positionPointer))+halfHeight-nn*PathHeight)) 
 && (myDedx > ( ((*mKCenter)(positionPointer))-halfHeight+nn*PathHeight) )) 
        mExtrap=true;
     }
}
//-------------------------------
void StuProbabilityPidAlgorithm::fillPIDHypothis(){
  
  for (int i=0; i<4; i++){

    switch (PID[i]) {

    case 2 : mPositronProb=mProb[i];
               break;
    case 3 : mElectronProb=mProb[i];
               break;
    case 8 : mPionPlusProb=mProb[i];
               break;
    case 9 : mPionMinusProb=mProb[i];
               break;
    case 11 : mKaonPlusProb=mProb[i];
               break;
    case 12 : mKaonMinusProb=mProb[i];
               break;
    case 14 : mProtonProb=mProb[i];
               break;
    case 15 : mAntiProtonProb=mProb[i];
               break;
    }
  }

}
//-------------------------------
int StuProbabilityPidAlgorithm::getCentralityBin(double theCent){
  int theBin=0; //input % centrality. output the corres. bin.
  
  for (int mm = (thisMultBins-1); mm>0; mm--) {
    if (theCent< (*mMultiBinEdgeSet)(mm) ) theBin=mm;
  }

  return theBin;

}

//-------------------------------


double StuProbabilityPidAlgorithm::getCentrality(int theMult){


  if (mProductionTag){


  if ( (mProductionTag->GetString()).Contains("P01gl")
       || (mProductionTag->GetString()).Contains("P02gd") ){
  return  getCentrality_P01gl(theMult);
  } else if ((mProductionTag->GetString()).Contains("P03ia_dAu")){
  return  getCentrality_P03ia_dAu(theMult);
  }  else {
  gMessMgr->Error()<<"Production tag "<<mProductionTag->GetString().Data()<<" in PIDTable is filled but its name is not recognized ! "<<endm;
  }


  }  else {//the first PID table does not have a productionTag

     // limits 
    // For Cut Set 1       
    // 225 ~ 3%            
    // 215 ~ 5%            
    // 200 ~ 7%            
    // 180 ~ 10%           
    // 140 ~ 18%           
    // 130 ~ 20%           
    // 120 ~ 23%
    // 115 ~ 24%
    // 100 ~ 28%

    //  highMedLimit = 180;  // 10%
    //  medLowLimit = 108;   //~26%
     
  if (theMult > 225 ) return 0.03;
  else if (theMult > 215 ) return 0.05;
  else if (theMult > 200 ) return 0.07;
  else if (theMult > 180 ) return 0.10;
  else if (theMult > 140 ) return 0.18;
  else if (theMult > 130 ) return 0.20; 
  else if (theMult > 120 ) return 0.23;
  else if (theMult > 115 ) return 0.24;
  else if (theMult > 100 ) return 0.28;
  else return 0.99;

  }
  return 0.99;


}
//-------------------------------


double StuProbabilityPidAlgorithm::getCentrality_P01gl(int theMult){
     
  //from Zhangbu's study     
  //5% 474 
  //10% 409 
  //20% 302 
  //30% 216 
  //40% 149 
  //50% 98 
  //60% 59 
  //70% 32 
  //80% 14 

  if (theMult*2. > 474 )      return 0.03; //use Nch instead of hminus. so *2.
  else if (theMult*2. > 409 ) return 0.10;
  else if (theMult*2. > 302 ) return 0.20;
  else if (theMult*2. > 216 ) return 0.30;
  else if (theMult*2. > 149 ) return 0.40;
  else if (theMult*2. > 98  ) return 0.50; 
  else if (theMult*2. > 59  ) return 0.60;
  else if (theMult*2. > 32  ) return 0.70;
  else if (theMult*2. > 14  ) return 0.80;
  else return 0.99;
}

//-------------------------------


double StuProbabilityPidAlgorithm::getCentrality_P03ia_dAu(int theMult){
     
  //from Joern's study    
  // * centrality bin    multiplicity FTPC east    percentOfEvents
  // *        1                   <=11                 100-40
  // *        2                   <=17                 40-20
  // *        3                   >=18                 20-0

  if (theMult >= 18 )      return 0.19; //do not need to be exact. 
  else if (theMult >= 12 ) return 0.39; //just for gettting bin # correctly.
  else if (theMult > 0 )   return 0.8;
  else return 0.99;
}

//-------------------------------
void StuProbabilityPidAlgorithm::fill(double prob, int geantId){

              if (prob>mProb[0]) {
	 mProb[3]=mProb[2];
         mProb[2]=mProb[1];
         mProb[1]=mProb[0];
         mProb[0]=prob;

         PID[3]=PID[2];    
         PID[2]=PID[1];
         PID[1]=PID[0];
         PID[0]=geantId;
	    }
            else if (prob>mProb[1]){
          mProb[3]=mProb[2];
          mProb[2]=mProb[1];
          mProb[1]=prob;

          PID[3]=PID[2];
          PID[2]=PID[1];
          PID[1] =geantId;
	    }
            else if (prob>mProb[2]){
            mProb[3]=mProb[2];
            mProb[2]=prob;
            PID[3]=PID[2];
            PID[2]=geantId;
	    }
	      else if (prob>=mProb[3]){
		mProb[3]=prob;
                PID[3]=geantId;
	      }

}  
//------------------------------------------------
int StuProbabilityPidAlgorithm::getCalibPosition(double theEta, int theNHits){

  int theEtaBin=int(thisEtaBins*(theEta-thisEtaStart)/(thisEtaEnd-thisEtaStart));
  int theNHitsBin=int(thisNHitsBins*float(theNHits)/(thisNHitsEnd-thisNHitsStart));

    int totalEntry
      = thisEtaBins*thisNHitsBins;
    
    int positionPointer=0;
    
    totalEntry=totalEntry/thisEtaBins;
    positionPointer=positionPointer+totalEntry*theEtaBin;
    
    totalEntry=totalEntry/thisNHitsBins;
    positionPointer=positionPointer+totalEntry*theNHitsBin;



    return positionPointer;
}
//--------------------------------------------
void StuProbabilityPidAlgorithm::setCalibrations(double theEta, int theNhits){
   
  int thePosition=getCalibPosition(theEta,theNhits);

   myBandBGFcn->SetParameter(5,(*mBBScale)(thePosition));
   myBandBGFcn->SetParameter(2,(*mBBOffSet)(thePosition));


 if (mProductionTag){

   if ( (mProductionTag->GetString()).Contains("P01gl") 
     || (mProductionTag->GetString()).Contains("P02gd")){
     
   myBandBGFcn->SetParameter(0,(*mBBPrePar)(thePosition));
   myBandBGFcn->SetParameter(1,(*mBBTurnOver)(thePosition));
   myBandBGFcn->SetParameter(6,(*mBBSaturate)(thePosition));

   }
 }


}
//-------------------------------------cent,dca,charge,rig,eta,nhits,dedx
void StuProbabilityPidAlgorithm::processPIDAsFunction (double theCent, double theDca, int theCharge, double theRig, double theEta, int theNhits, double theDedx){



      PID[0]=-1;//should be sth.standard say unIdentified.
      PID[1]=-1;     
      PID[2]=-1;
      PID[3]=-1;

     mProb[0]=0;
     mProb[1]=0;
     mProb[2]=0;
     mProb[3]=0;

     mExtrap=false;


     mPionMinusProb=0.;
     mElectronProb=0.;
     mKaonMinusProb=0.;
     mAntiProtonProb=0.;
     mPionPlusProb=0.;
     mPositronProb=0.;
     mKaonPlusProb=0.;
     mProtonProb=0.;

     if (mPIDTableRead) {

          double rig    =fabs(theRig);
          double dedx   =theDedx;
	  double dca    =theDca; //in units of cm.
          int    nhits  =theNhits;
          int    charge =theCharge;
          double eta    =0.; 
          double cent   =theCent; // % central


    if (mProductionTag){ //for AuAu, +/- eta were folded together when building PID
      //table, for dAu, + and - eta were treated differently.

  if ( (mProductionTag->GetString()).Contains("P01gl")
       || (mProductionTag->GetString()).Contains("P02gd") ){
    eta=fabs(theEta);
  }  else if ( (mProductionTag->GetString()).Contains("P03ia_dAu") ){
    eta=theEta;
  }  else {
        gMessMgr->Error()<<"Production tag "<<mProductionTag->GetString().Data()<<" in PIDTable is filled but its name is not recognized ! "<<endm;
  }

  } else { //the first PID table has no production tag
    eta = fabs(theEta);
  }





       if (dedx!=0.0 && nhits>=0 //dedx ==0.0 no sense 
	   && thisPEnd > 0. && thisEtaEnd > 0. // *End ==0, no PIDTable read.
           && thisNHitsEnd > 0. ){

    rig   = fabs(rig);
    dedx  = (dedx>thisDedxStart) ? dedx : thisDedxStart;
    rig   = (rig >thisPStart)  ? rig  : thisPStart;
    rig   = (rig <thisPEnd  )  ? rig  : thisPEnd*0.9999;   
    eta   = (eta  >thisEtaStart)   ? eta   : thisEtaStart;
    eta   = (eta  <thisEtaEnd  )   ? eta   : thisEtaEnd*0.9999;
    nhits = (nhits > int(thisNHitsStart)) ? nhits : int(thisNHitsStart);
    nhits = (nhits < int(thisNHitsEnd) ) ? nhits : int(thisNHitsEnd-1);

    //----------------get all info. I want for a track. now do PID

    setCalibrations(eta, nhits);

   if (dedx<thisDedxEnd){

   fillPIDByLookUpTable(cent, dca, charge,rig, eta, nhits,dedx);

   } else { lowRigPID(rig,dedx,charge);}
 
       } else if (dedx==0.0){ fillAsUnknown();}

     // do not do deuteron or higher
      myBandBGFcn->SetParameter(3,1);
      myBandBGFcn->SetParameter(4,1.45);
      if (dedx>myBandBGFcn->Eval(rig,0,0)) fillAsUnknown();

     } else fillAsUnknown();

       fillPIDHypothis();



 }
