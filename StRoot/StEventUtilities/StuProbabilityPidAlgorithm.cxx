/***************************************************************************
 *
 * $Id: StuProbabilityPidAlgorithm.cxx,v 1.3 2000/04/06 15:57:47 aihong Exp $
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
 * Revision 1.3  2000/04/06 15:57:47  aihong
 * fix operator() for working fine when constructed above the track loop
 *
 * Revision 1.5  2000/05/05 19:25:39  aihong
 * modified ctor
 *
 * Revision 1.2  2000/03/09 20:45:04  aihong

 *
 **************************************************************************/
#include <float.h>

#include "TF1.h"
#include "TTree.h"
#include "StMessMgr.h"
#include "StPhysicalHelixD.hh"
#include "PhysicalConstants.h"
#include "StPidAmpMaker/Include/BetheBlock.hh"
#include "StuProbabilityPidAlgorithm.h"

#include "StPidAmpMaker/Include/BetheBloch.hh"
#include "StPidAmpMaker/Include/MaxllBoltz.hh"
#include "StPidAmpMaker/Include/Linear.hh"
#include "StPidAmpMaker/Include/StPidAmpConst.hh"
double lowRigReso(double xa, double xb, double ya, double yb,double theX);


StuProbabilityPidAlgorithm::StuProbabilityPidAlgorithm(){


StuProbabilityPidAlgorithm::StuProbabilityPidAlgorithm(StEvent& ev){
     PID[0]   = -1;//should be sth.standard say unIdentified.
     PID[1]   = -1;     
     PID[2]   = -1;
     mProb[0] = 0;
     mProb[1] = 0;
     mProb[2] = 0;
     mExtrap  = false;

     mDedxMethod=kTruncatedMeanId;
     mProb[0]=0;
     StuProbabilityPidAlgorithm::funcBandPt=&BetheBlock;
     mProb[2]=0;
      
     StuProbabilityPidAlgorithm::funcBandPt=&BetheBloch;
     StuProbabilityPidAlgorithm::funcAmpPt =&MaxllBoltz;   
     StuProbabilityPidAlgorithm::funcResoPt=&Linear;

     table = StParticleTable::instance();

//-------------------------------

StuProbabilityPidAlgorithm::~StuProbabilityPidAlgorithm(){
  /* no op */
}
//-------------------------------
void StuProbabilityPidAlgorithm::setDedxMethod(StDedxMethod method){
      mDedxMethod=method;
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

   if (i>=0 && i<3){
           return table->findParticleByGeantId(PID[i]);
   }   else { 

	gMessMgr->Error()<<"StuProbabilityPidAlgorithm::getParticle(int i), i must be 0,1,3 only. "<<endm;

     return 0;
   }
 }
//-------------------------------
double StuProbabilityPidAlgorithm::getProbability(int i){
   if (i>=0 && i<3){
           return mProb[i];
   }   else { 

	gMessMgr->Error()<<"StuProbabilityPidAlgorithm::getProbability(int i), i must be 0,1,3 only. "<<endm;

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

  //following parameters might (or not) be refreshed at the end of this method.
     PID[0]   = -1;//should be sth.standard say unIdentified.
     PID[1]   = -1;     
     PID[2]   = -1;
     mProb[0] = 0;
     mProb[1] = 0;
     mProb[2] = 0;
     mExtrap  = false;


     //local variables.
          double rig    = 0.0;
          double dedx   = 0.0;
          double total  = 0;
          double pt     = 0;
          int    nhits  = 0;
          int    charge = 0;
          int    i      = 0;
	  int    j      = 0;
	  int    jj     = 0;
    const StPhysicalHelixD& helix=theTrack.geometry()->helix();
           dca=helix.distance(primaryVtx->position());


       charge=int((theTrack.geometry())->charge());

       for (int itrait = 0; itrait < traits.size(); itrait++){
       charge=(theTrack.geometry())->charge();

       for (int itrait = 0; itrait < int(traits.size()); itrait++){

           dedxPidTr = 0;
	   if (traits[itrait]->detector() == kTpcId) {
	     //
	     // tpc pid trait
	     //
             StTrackPidTraits* thisTrait = traits[itrait];
	     //
	     // perform cast to make the pid trait a dedx trait
	     //
	     dedxPidTr = dynamic_cast<StDedxPidTraits*>(thisTrait);
	   }

           if (dedxPidTr &&  dedxPidTr->method() == mDedxMethod) {


             dedx=dedxPidTr->mean();
             nhits=dedxPidTr->numberOfPoints();
	   }
       }

    StPhysicalHelixD& helix=theTrack.geometry()->helix();


    const StPhysicalHelixD& helix=theTrack.geometry()->helix();
    const StThreeVectorF& p=theTrack.geometry()->momentum();
    rig=double(p.mag()/charge);
    //----------------get all info. I want for a track. now do PID


    if (dedx<4.0e-6){

      TObjArray*               channelLevel;// =new TObjArray();
      StPidAmpNetOut*          netOut;//       =new StPidAmpNetOut();
      StPidAmpChannelInfoOut*  channelInfoOut;//=new StPidAmpChannelInfoOut();

    for (i=0;i<mDataSet.GetEntries(); i++){

     
     channelLevel=(TObjArray *)mDataSet.At(i);
     channelInfoOut=(StPidAmpChannelInfoOut *)channelLevel->At(0);
     if (channelInfoOut->IsInChannel(nhits, pt)) {//pick up the right channel
 

     if (channelInfoOut->IsInChannel(nhits, pt,dca )) {//pick up the right channel
       mNoise=((protonNetOut->GetAmpParArray()).At(0))*0.002;
       if (mTurnOnNoise){
       StPidAmpNetOut*  protonNetOut=(StPidAmpNetOut *)channelLevel->At(8);
       mNoise=((protonNetOut->GetAmpParArray())->At(0))*0.002;
       }//turn on noise.
      
      double amplitudeArray[NParticleTypes]; 
      //the size should be channelLevel->GetEntries()
      //but we need a integer constant to declare an array.


      //true vaule begin with amplitudeArray[1]
       
 
      
         jj=j-1; //index for amplitudeArray
	 TArrayD* bArry=netOut->GetBandParArray();
    if (jj<NParticleTypes) amplitudeArray[jj]= amplitude(dedx,rig,netOut->GetBandParArray(),netOut->GetResoParArray(),netOut->GetAmpParArray());

         if (charge*((netOut->GetBandParArray()).At(3))>0.0)//the same sign of z
     amplitudeArray[jj]= amplitude(dedx,rig, bArry, rArry,aArry);
 
         if (charge*((netOut->GetBandParArray())->At(3))>0.0)//the same sign of z
	total +=amplitudeArray[jj];   
     
       }

       for (j=1;j<channelLevel->GetEntries();j++){//do not use j=0. j=0 is for channelInfoOut.
          jj=j-1;
	 if (charge*((netOut->GetBandParArray()).At(3))>0.0) {
         netOut=(StPidAmpNetOut *)channelLevel->At(j);  
 
	 if (charge*((netOut->GetBandParArray())->At(3))>0.0) {


            double probability=amplitudeArray[jj]/total;
            
           fill(probability,netOut);
	 }
       }

            tagExtrap(rig, dedx,channelLevel);

     }//is inchanel
    }


    } else {lowRigPID(rig,dedx);}

       }else if (dedx==0.0){ fillAsUnknown();}

      return table->findParticleByGeantId(PID[0]);


}            

   


double StuProbabilityPidAlgorithm::bandCenter(double rig,TArrayD& bandPars){

 TF1 mBandBetheBlockFcn("mBandBetheBlockFcn",funcBandPt, BandsBegin,BandsEnd, NBandParam);
double StuProbabilityPidAlgorithm::bandCenter(double rig,TArrayD* bandPars){
       
       mBandBetheBlockFcn.SetParameter(i,bandPars.At(i));
      
       return mBandBetheBlockFcn.Eval(fabs(rig),0,0);
       mBandBetheBlochFcn.SetParameter(i,bandPars->At(i));
        rig=tossTail(rig);
       return mBandBetheBlochFcn.Eval(fabs(rig),0,0);
       

}
double StuProbabilityPidAlgorithm::resolution(double rig, TArrayD& linrPars,TArrayD& bandPars){

//-------------------------------
   if (linrPars.GetSize()==NResoParam){//if deuteron not filled, its linrPars.GetSize()=0
       mResoFcn.SetParameter(0,linrPars.At(0));
       mResoFcn.SetParameter(1,linrPars.At(1));
   if (linrPars->GetSize()==NResoParam){//if deuteron not filled, its linrPars.GetSize()=0
       mResoFcn.SetParameter(0,linrPars->At(0));
       mResoFcn.SetParameter(1,linrPars->At(1));
    
        if (bandPars.At(4)>0.139 && bandPars.At(4)<0.140 && fabs(rig)<0.15 && fabs(rig)>0.05 )//for pion resolution fit at low rig adjudement.


        if (bandPars->At(4)>0.139 && bandPars->At(4)<0.140 && fabs(rig)<0.15 && fabs(rig)>0.05 )//for pion resolution fit at low rig adjudement.
         
   } else if (linrPars.GetSize()!=NResoParam){
        return lowRigReso(0.15,0.05,mResoFcn.Eval(fabs(0.15),0,0),0.7,rig);
        return 0.0;
   }

   } else  return 0.0;
 

       
double StuProbabilityPidAlgorithm::peak(double rig, TArrayD& ampPars){

//-------------------------------
double StuProbabilityPidAlgorithm::peak(double rig, TArrayD* ampPars){
     mMaxllBoltzFcn.SetParameter(i,ampPars.At(i));
 TF1 mMaxllBoltzFcn("mMaxllBoltzFcn",funcAmpPt, BandsBegin,BandsEnd,NAmpParam);    
     for (int i=0; i<NAmpParam; i++)
     mMaxllBoltzFcn.SetParameter(i,ampPars->At(i));
    
      rig=tossTail(rig);

      return mMaxllBoltzFcn.Eval(fabs(rig),0,0);
       

}
//-------------------------------
double StuProbabilityPidAlgorithm::tossTail(double rig){
   
      double therig=(fabs(rig)>BandsEnd) ? BandsEnd : fabs(rig);
      
       therig=(therig==0.0) ? 1.0e-10 : fabs(rig); //just to avoid possible crash.

       return therig;
}



//-------------------------------
void StuProbabilityPidAlgorithm::lowRigPID(double rig,double dedx){

              

       double m; 
       double a;
       double upper;
       double lower;
       double rigidity=fabs(rig);      
       double mdedx=dedx; 

       m = -1.74072;
       a = 1.7548e-8;

       lower=a*pow(rigidity,m);
       a = 2.3952e-7;
       upper = a*pow(rigidity,m);
       if (mdedx>lower && mdedx<upper){
	 PID[0]=(rigidity>0.0)? 8 : 9;  //pi+/-
         mProb[0]=1.0;
         mProb[1]=0.0;
         mProb[2]=0.0;
       }


       lower = upper;
       a = 6.6238e-7;
       upper = a*pow(rigidity,m);
       if (mdedx>lower && mdedx<upper){
         PID[0]=(rigidity>0.0)? 11:12;  //k+/-
         mProb[0]=1.0;
         mProb[1]=0.0;
         mProb[2]=0.0;
       }


       lower = upper;
       a = 3.1824e-6;
       upper = a*pow(rigidity,m);
       if (mdedx>lower && mdedx<upper){
         PID[0]=(rigidity>0.0)? 14:15;  //proton/antiproton
         mProb[0]=1.0;
         mProb[1]=0.0;
         mProb[2]=0.0;
       } 

                
       lower = upper;
       m = -1.5374;  //New slope needed for deuterons and tritons.
       a = 8.8982e-6;
       upper = a*pow(rigidity,m);
       if (mdedx>lower && mdedx<upper){
         PID[0]=45;  //deuteron
         mProb[0]=1.0;
         mProb[1]=0.0;
         mProb[2]=0.0;
       }

       lower = upper;
       a = 1.8121e-5;
       upper = a*pow(rigidity,m);
       if (mdedx>lower && mdedx<upper){
         PID[0]=46;  //triton
         mProb[0]=1.0;
         mProb[1]=0.0;
         mProb[2]=0.0;
       }




}  


//-------------------------------
void StuProbabilityPidAlgorithm::fill(double prob, StPidAmpNetOut* netOut){

              if (prob>mProb[0]) {
         mProb[2]=mProb[1];
         mProb[1]=mProb[0];
        mProb[0]=prob;
    
         PID[2]=PID[1];
         PID[1]=PID[0];
         PID[0]=netOut->GetGeantID();
	    }
            else if (prob>mProb[1]){
          mProb[2]=mProb[1];
          mProb[1]=prob;
          PID[2]=PID[1];
          PID[1] =netOut->GetGeantID();
	    }
            else if (prob>mProb[2]){
            mProb[2]=prob;
            PID[2]=netOut->GetGeantID();
	    }
}  
//-------------------------------
void StuProbabilityPidAlgorithm::fillAsUnknown(){

  for (int i=0; i<3; i++) {
      PID[i]=-1; mProb[i]=-1;
  }
double StuProbabilityPidAlgorithm::amplitude(double dedx, double rig, TArrayD& bandPars, TArrayD& linrPars, TArrayD& ampPars){

//-------------------------------
double StuProbabilityPidAlgorithm::amplitude(double dedx, double rig, TArrayD* bandPars, TArrayD* linrPars, TArrayD* ampPars){

    double dedxHighLimit=12.0e-6;

    double theMean=bandCenter(rig,bandPars);
    double thePeak=peak(rig,ampPars);
     
    if (thePeak<=0) return mNoise;

    double theSigma=theMean*resolution(rig,linrPars,bandPars);


    if (theSigma<=0.0) return mNoise;

    TF1 mGaussFcn("mGaussFcn", "gaus",0.0,dedxHighLimit);

        mGaussFcn.SetParameter(0,thePeak);
        mGaussFcn.SetParameter(1,theMean);
        mGaussFcn.SetParameter(2,theSigma);


        double theDedx=(fabs(dedx)>dedxHighLimit) ? dedxHighLimit : fabs(dedx);
      
       return mGaussFcn.Eval(fabs(theDedx),0,0)+mNoise;
 

 

   }

void StuProbabilityPidAlgorithm::readInputFile(TString fileName){
void StuProbabilityPidAlgorithm::readParametersFromFile(TString fileName){

      if (mDataSet.GetEntries()>0) mDataSet.Delete();

      TFile f(fileName,"READ",fileName);

      if (f.IsOpen()){
      
      TTree* netSetTree=(TTree*)f.Get("netSetTree");

      int numOfChannels=netSetTree->GetEntries();
      

      int numOfChannels=int(netSetTree->GetEntries());

      int i;
      int nb=0;

      for (i=0; i<numOfChannels; i++){
      TObjArray* theArray=new TObjArray();
      br->SetAddress(&theArray);
        nb=netSetTree->GetEvent(i);
        mDataSet.Add(theArray);
      }

      } else if (!f.IsOpen()) {
   
	gMessMgr->Error()<<"Data file "<<fileName<<" open failed "<<endm;
        return;
  


  //StPidAmpNetOut*   electronNetOut=(StPidAmpNetOut *)channelLevel->At(1);
void StuProbabilityPidAlgorithm::tagExtrap(double rig, double dedx,TObjArray* channelLevel){

   StPidAmpNetOut*      antiprotonNetOut=(StPidAmpNetOut *)channelLevel->At(4);
   //StPidAmpNetOut*   positronNetOut=(StPidAmpNetOut *)channelLevel->At(5);
   StPidAmpNetOut*       kaonMinusNetOut=(StPidAmpNetOut *)channelLevel->At(3);
 //StPidAmpNetOut*      antiprotonNetOut=(StPidAmpNetOut *)channelLevel->At(4);
 //StPidAmpNetOut*   positronNetOut=(StPidAmpNetOut *)channelLevel->At(5);
   StPidAmpNetOut*        pionPlusNetOut=(StPidAmpNetOut *)channelLevel->At(6);
   StPidAmpNetOut*        kaonPlusNetOut=(StPidAmpNetOut *)channelLevel->At(7);
   StPidAmpNetOut*          protonNetOut=(StPidAmpNetOut *)channelLevel->At(8);

   int nn=-1;    
   int mm=0;

     if ((rig>(kaonPlusNetOut->GetAmpParArray()).At(3)) && ( (dedx<(bandCenter(rig,(pionPlusNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-nn*PathHeight)) && (dedx>(bandCenter(rig,(kaonPlusNetOut->GetBandParArray()))-(NPaths-2.0)*PathHeight/2.0+nn*PathHeight))) ) mExtrap=true;


         if ((rig<(kaonMinusNetOut->GetAmpParArray()).At(3)) && ( (dedx<(bandCenter(rig,(pionMinusNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-nn*PathHeight))  && (dedx>(bandCenter(rig,(kaonMinusNetOut->GetBandParArray()))-(NPaths-2.0)*PathHeight/2.0+nn*PathHeight)) ) ) mExtrap=true;

         if ((rig>(protonNetOut->GetAmpParArray()).At(3)) && (dedx > (bandCenter(rig,(protonNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-(mm+3)*PathHeight))) mExtrap=true;
         if ((rig<(kaonMinusNetOut->GetAmpParArray())->At(3)) && ( (dedx<(bandCenter(rig,(pionMinusNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-nn*PathHeight))  && (dedx>(bandCenter(rig,(kaonMinusNetOut->GetBandParArray()))-(NPaths-2.0)*PathHeight/2.0+nn*PathHeight)) ) ) mExtrap=true;
         if ((rig>(protonNetOut->GetAmpParArray()).At(3)) && (dedx > (bandCenter(rig,(protonNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-(mm+3)*PathHeight))) mExtrap=true;
         if ((rig>(protonNetOut->GetAmpParArray())->At(3)) && (dedx > (bandCenter(rig,(protonNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-(mm+3)*PathHeight))) mExtrap=true;

         if ((rig>(protonNetOut->GetAmpParArray())->At(3)) && (dedx > (bandCenter(rig,(protonNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-(mm+3)*PathHeight))) mExtrap=true;



  BandParArray.AddAt(Saturation,NFitBandParam+3);


}




//-------------------------------
double lowRigReso(double xa, double xb, double ya, double yb,double theX){

//-------------------------------
double StuProbabilityPidAlgorithm::lowRigReso(double xa, double xb, double ya, double yb,double theX){

    return yb-(yb-ya)*sqrt(1.0-(fabs(theX)-xa)*(fabs(theX)-xa)/((xa-xb)*(xa-xb))) ;

}

     






