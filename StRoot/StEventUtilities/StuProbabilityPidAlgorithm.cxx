/***************************************************************************
 *
 * $Id: StuProbabilityPidAlgorithm.cxx,v 1.17 2000/10/24 22:36:47 aihong Exp $
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
 * Revision 1.17  2000/10/24 22:36:47  aihong
 * pass if no parameter file read
 *
 * Revision 1.16  2000/10/24 15:19:58  aihong
 * fix bug in fillPIDByLookUpTable()
 *
 * Revision 1.15  2000/08/30 12:55:19  aihong
 * upgrade lowRigPID()
 *
 * Revision 1.14  2000/08/23 15:15:33  aihong
 * *** empty log message ***
 *
 * Revision 1.13  2000/08/23 01:18:13  aihong
 * remove a bug
 *
 * Revision 1.12  2000/08/22 23:50:04  aihong
 * fix lowRigPID
 *
 * Revision 1.11  2000/08/17 13:32:13  aihong
 * remove edge effect
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
#include <strstream.h>
#include <fstream.h> //this line should be deleted later
#include "TFile.h"
#include "TF1.h"
#include "TTree.h"

#include "StMessMgr.h"
#include "StPhysicalHelixD.hh"
#include "PhysicalConstants.h"

#include "StuProbabilityPidAlgorithm.h"

#include "StEventUtilities/BetheBlochFunction.hh"
#include "StEventUtilities/MaxllBoltz.hh"
#include "StEventUtilities/Linear.hh"
#include "StEventUtilities/StPidAmpConst.hh"

#include "StEventUtilities/StPidAmpChannelInfoOut.h"
#include "StEventUtilities/StuObjPidReport.h"


//TMap::FindObject goes wild!! TMap::GetValue works.


//-------------------------------
StuProbabilityPidAlgorithm::StuProbabilityPidAlgorithm(StEvent& ev){



      
      PID[0]=-1;//should be sth.standard say unIdentified.
      PID[1]=-1;     
      PID[2]=-1;

     mProb[0]=0;
     mProb[1]=0;
     mProb[2]=0;
  

     table = StParticleTable::instance();

     mExtrap=false;
     mNoise=0.0;
     mEvent=&ev;
}
//-------------------------------
StuProbabilityPidAlgorithm::~StuProbabilityPidAlgorithm(){
  /* no op */
}
//-------------------------------
void StuProbabilityPidAlgorithm::setDedxMethod(StDedxMethod method){
      StuProbabilityPidAlgorithm::mDedxMethod=method;
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

	gMessMgr->Error()<<"StuProbabilityPidAlgorithm::getParticle(int i), i must be 0,1,2 only. "<<endm;

     return 0;
   }
 }
//-------------------------------
double StuProbabilityPidAlgorithm::getProbability(int i){
   if (i>=0 && i<3){
           return mProb[i];
   }   else { 

	gMessMgr->Error()<<"StuProbabilityPidAlgorithm::getProbability(int i), i must be 0,1,2 only. "<<endm;

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
     mProb[0]=0;
     mProb[1]=0;
     mProb[2]=0;
     mExtrap=false;

     if (StuProbabilityPidAlgorithm::mHasParameterFile){

          double rig    =0.0;
          double dedx   =0.0;
	  double dca    =0.0; //in units of cm.
          double pt     =0;
          int    nhits  =0;
          int    charge =0;

           StPrimaryVertex* primaryVtx=mEvent->primaryVertex();
    const StPhysicalHelixD& helix=theTrack.geometry()->helix();
           dca=helix.distance(primaryVtx->position());


          const StDedxPidTraits* dedxPidTr;


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





       if (dedx!=0.0){//dedx =0.0 means not this track is not in Tpc dector.


	 // const StPhysicalHelixD& helix=theTrack.geometry()->helix();
    const StThreeVectorF& p=theTrack.geometry()->momentum();
    rig=double(p.mag()/charge);
    pt=double(p.perp());

    rig=fabs(rig); 
    dedx = (dedx>mDedxStart) ? dedx : mDedxStart;
    rig  = (rig >mRigStart)  ? rig  : mRigStart;
    rig  = (rig <mRigEnd  )  ? rig  : mRigEnd*0.9999; //*0.9999 to remove edge effect.   
        //this has to be deal with later. 
        //if rig go to 5 in TMap, we end up with a big file

    pt   = (pt  >mPtStart)   ? pt   : mPtStart;
    pt   = (pt  <mPtEnd  )   ? pt   : mPtEnd*0.9999;


    //----------------get all info. I want for a track. now do PID

   if (dedx<mDedxEnd){



   if (mDynamicallyCalculatePID)
      fillPIDByCalculation(charge, dca, nhits, pt, dedx, rig);
   else fillPIDByLookUpTable(charge, dca, nhits, pt, dedx, rig);

   } else { lowRigPID(rig,dedx,charge);}

 
       } else if (dedx==0.0){ fillAsUnknown();}
     } else { fillAsUnknown();}


       return table->findParticleByGeantId(PID[0]);

       }

//-------------------------------
double StuProbabilityPidAlgorithm::bandCenter(double rig,TArrayD* bandPars){
       
 TF1 mBandBetheBlochFcn("mBandBetheBlochFcn",BetheBlochFunction, BandsBegin,BandsEnd, NBandParam);
       
    for (int i=0; i<NBandParam; i++)
       mBandBetheBlochFcn.SetParameter(i,bandPars->At(i));
    // rig=tossTail(rig);
       return mBandBetheBlochFcn.Eval(fabs(rig),0,0);

}
//-------------------------------
double StuProbabilityPidAlgorithm::bandCenter(double rig, int NChannel, int NType){

    TObjArray* myChannelLevelFuncAry
      =(TObjArray *)mBetheBlochFuncSet.At(NChannel);
    TF1*       myBandBGFcn
      =(TF1 *)myChannelLevelFuncAry->At(NType);
    
    // rig=tossTail(rig);

       return myBandBGFcn->Eval(fabs(rig),0,0);
}

//-------------------------------
double StuProbabilityPidAlgorithm::resolution(double rig, TArrayD* linrPars,TArrayD* bandPars){

  TF1 mResoFcn("mResoFcn",Linear, BandsBegin,BandsEnd,NResoParam);
   
   if (linrPars->GetSize()==NResoParam){//if deuteron not filled, its linrPars.GetSize()=0
       mResoFcn.SetParameter(0,linrPars->At(0));
       mResoFcn.SetParameter(1,linrPars->At(1));
    
       // rig=tossTail(rig);


        if (bandPars->At(4)>0.139 && bandPars->At(4)<0.140 && fabs(rig)<0.15 && fabs(rig)>0.05 )//for pion resolution fit at low rig adjudement.
         
        return lowRigReso(0.15,0.05,mResoFcn.Eval(fabs(0.15),0,0),0.7,rig);
       
         else   return mResoFcn.Eval(fabs(rig),0,0);

   } else  return 0.0;
 

       
}
//-------------------------------
double StuProbabilityPidAlgorithm::resolution(double rig, int NChannel, int NType){
    TObjArray* myChannelLevelFuncAry
      =(TObjArray *)mLinearFuncSet.At(NChannel);
    TF1*       myResoFcn
      =(TF1 *)myChannelLevelFuncAry->At(NType);

//for pion resolution fit at low rig adjudement.
    if ((NType==1 || NType==3) &&  fabs(rig)<0.15 && fabs(rig)>0.05)
     return lowRigReso(0.15,0.05,myResoFcn->Eval(fabs(0.15),0,0),0.7,rig);
    else return myResoFcn->Eval(fabs(rig),0,0);
}


//-------------------------------
double StuProbabilityPidAlgorithm::peak(double rig, TArrayD* ampPars){


   TF1 mMaxllBoltzFcn("mMaxllBoltzFcn",MaxllBoltz, BandsBegin,BandsEnd,NAmpParam);    
     for (int i=0; i<NAmpParam; i++)
     mMaxllBoltzFcn.SetParameter(i,ampPars->At(i));
    
     //  rig=tossTail(rig);

      return mMaxllBoltzFcn.Eval(fabs(rig),0,0);
       

}
//-------------------------------
double StuProbabilityPidAlgorithm::peak(double rig, int NChannel, int NType){
    TObjArray* myChannelLevelFuncAry
      =(TObjArray *)mMaxllFuncSet.At(NChannel);
    TF1*       myMaxllBoltzFcn
      =(TF1 *)myChannelLevelFuncAry->At(NType);

    //  rig=tossTail(rig);

  return myMaxllBoltzFcn->Eval(fabs(rig),0,0);
}




//-------------------------------
double StuProbabilityPidAlgorithm::tossTail(double rig){
   
      double therig=(fabs(rig)>BandsEnd) ? BandsEnd : fabs(rig);
      
       therig=(therig==0.0) ? 1.0e-10 : fabs(rig); //just to avoid possible crash.

       return therig;
}



//-------------------------------
void StuProbabilityPidAlgorithm::lowRigPID(double rig,double dedx, int theCharge){

    TObjArray* myChannelLevelFuncAry
      =(TObjArray *)mBetheBlochFuncSet.At(0);
    TF1*       myBandBGFcn
      =(TF1 *)myChannelLevelFuncAry->At(1);
    

       double m; 
       double a;
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
       } 

                
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
       }

       lower = upper;

       //triton
       m = -1.5374;  //New slope needed for deuterons and tritons.
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
void StuProbabilityPidAlgorithm::fill(double* probAry, int* pidAry, double prob,StPidAmpNetOut* netOut){

 

              if (prob>probAry[0]) {
         probAry[2]=probAry[1];
         probAry[1]=probAry[0];
         probAry[0]=prob;
    
         pidAry[2]=pidAry[1];
         pidAry[1]=pidAry[0];
         pidAry[0]=netOut->GetGeantID();
	    }
            else if (prob>probAry[1]){
          probAry[2]=probAry[1];
          probAry[1]=prob;
          pidAry[2]=pidAry[1];
          pidAry[1] =netOut->GetGeantID();
	    }
            else if (prob>probAry[2]){
            probAry[2]=prob;
            pidAry[2]=netOut->GetGeantID();
	    }



}  
//-------------------------------
void StuProbabilityPidAlgorithm::fillAsUnknown(){

  for (int i=0; i<3; i++) {
      PID[i]=-1; mProb[i]=-1;
  }
}

//-------------------------------
double StuProbabilityPidAlgorithm::amplitude(double dedx, double rig, TArrayD* bandPars, TArrayD* linrPars, TArrayD* ampPars){

    rig=tossTail(rig);

    double dedxHighLimit=12.0e-6;

    double theMean=StuProbabilityPidAlgorithm::bandCenter(rig,bandPars);
    double thePeak=StuProbabilityPidAlgorithm::peak(rig,ampPars);
     
    if (thePeak<=0) return mNoise;

    double theSigma=theMean*(StuProbabilityPidAlgorithm::resolution(rig,linrPars,bandPars));


    if (theSigma<=0.0) return mNoise;

    TF1 mGaussFcn("mGaussFcn", "gaus",0.0,dedxHighLimit);

        mGaussFcn.SetParameter(0,thePeak);
        mGaussFcn.SetParameter(1,theMean);
        mGaussFcn.SetParameter(2,theSigma);


        double theDedx=(fabs(dedx)>dedxHighLimit) ? dedxHighLimit : fabs(dedx);
      
       return mGaussFcn.Eval(fabs(theDedx),0,0)+mNoise;
 

 

   }

//-------------------------------
double StuProbabilityPidAlgorithm::amplitude(double dedx, double rig, int NChannel, int NType){


    rig=tossTail(rig);
    double dedxHighLimit=12.0e-6;

    double theMean=StuProbabilityPidAlgorithm::bandCenter(rig,NChannel,NType);
    double thePeak=StuProbabilityPidAlgorithm::peak(rig,NChannel,NType);


   if (thePeak<=0) return mNoise;

   double theSigma=theMean*(StuProbabilityPidAlgorithm::resolution(rig,NChannel,NType));
   if (theSigma<=0.0) return mNoise;

    TF1 mGaussFcn("mGaussFcn", "gaus",0.0,dedxHighLimit);

        mGaussFcn.SetParameter(0,thePeak);
        mGaussFcn.SetParameter(1,theMean);
        mGaussFcn.SetParameter(2,theSigma);

 
        double theDedx=(fabs(dedx)>dedxHighLimit) ? dedxHighLimit : fabs(dedx);

     
       return mGaussFcn.Eval(fabs(theDedx),0,0)+mNoise;

}



//-------------------------------
void StuProbabilityPidAlgorithm::readParametersFromFile(TString fileName){

      if (mDataSet.GetEntries()>0) mDataSet.Delete();
      if (mMaxllFuncSet.GetEntries()>0) mMaxllFuncSet.Delete();
      if (mLinearFuncSet.GetEntries()>0) mLinearFuncSet.Delete();
      if (mBetheBlochFuncSet.GetEntries()>0) mBetheBlochFuncSet.Delete();

      TFile f(fileName,"READ",fileName);

      if (f.IsOpen()){

	StuProbabilityPidAlgorithm::mHasParameterFile=true;

      StPidAmpNetOut*          netOut;
      StPidAmpChannelInfoOut*  channelInfoOut;      

      TTree* netSetTree=(TTree*)f.Get("netSetTree");

      TBranch* br=netSetTree->GetBranch("netSetBranch");
      

      int numOfChannels=int(netSetTree->GetEntries());

      int i,j;
      int nb=0;

      for (i=0; i<numOfChannels; i++){
      TObjArray* theArray=new TObjArray();
      br->SetAddress(&theArray);
        nb=netSetTree->GetEvent(i);
         mDataSet.Add(theArray);


	//construct functions
	TObjArray* channelMaxllFuncArray      = new TObjArray();
	TObjArray* channelLinearFuncArray     = new TObjArray();
	TObjArray* channelBetheBlochFuncArray = new TObjArray();

    channelInfoOut=(StPidAmpChannelInfoOut *)theArray->At(0);             
    channelMaxllFuncArray->Add(channelInfoOut);
    channelLinearFuncArray->Add(channelInfoOut);
    channelBetheBlochFuncArray->Add(channelInfoOut);

       for (j=1;j<theArray->GetEntries();j++){

	 strstream maxllSt;
         strstream linearSt;
         strstream BetheBlochSt;

         maxllSt<<"theMaxllBoltzFunc"<<i<<j;
         linearSt<<"theLinearFunc"<<i<<j;
         BetheBlochSt<<"theBetheBlochFunc"<<i<<j;

         netOut=(StPidAmpNetOut *)theArray->At(j);

	 TArrayD* bArry=netOut->GetBandParArray();
         TArrayD* rArry=netOut->GetResoParArray();
         TArrayD* aArry=netOut->GetAmpParArray();


 TF1* myBandBetheBlochFcn =
  new TF1(BetheBlochSt.str(),BetheBlochFunction, BandsBegin,BandsEnd, NBandParam);
 TF1* myResoFcn           =
  new TF1(linearSt.str(),Linear, BandsBegin,BandsEnd,NResoParam);
 TF1* myMaxllBoltzFcn     = 
  new TF1(maxllSt.str(),MaxllBoltz, BandsBegin,BandsEnd,NAmpParam);

 int hh=0;

   for (hh=0; hh<NBandParam; hh++)
       myBandBetheBlochFcn->SetParameter(hh,bArry->At(hh));
    
   for (hh=0; hh<NAmpParam; hh++)
       myMaxllBoltzFcn->SetParameter(hh,aArry->At(hh));

   if (rArry->GetSize()==NResoParam) //if deuteron not filled, its linrPars.GetSize()=0
   for (hh=0; hh<NResoParam; hh++)
       myResoFcn->SetParameter(hh,rArry->At(hh));

   channelBetheBlochFuncArray->Add(myBandBetheBlochFcn);
   channelMaxllFuncArray->Add(myMaxllBoltzFcn);
   channelLinearFuncArray->Add(myResoFcn);
       }

       mBetheBlochFuncSet.Add(channelBetheBlochFuncArray);
       mMaxllFuncSet.Add(channelMaxllFuncArray);
       mLinearFuncSet.Add(channelLinearFuncArray);

      }

      cout<<"reading PID tables, this takes a while..."<<endl;
       
     StuProbabilityPidAlgorithm::mTheReportTable = 
        (TObjArray *)f.Get("mProbabilityTable");

   
          StuProbabilityPidAlgorithm::mTheRangeSettingVector = 
        (TVectorD* )f.Get("mTheRangeSettingVector");

       if (mTheRangeSettingVector)
          setRanges4Table(mTheRangeSettingVector);

       //  setBins4Table(); 

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
void StuProbabilityPidAlgorithm::printParameters(){
  //print parameters stored in mDataSet.
 

     TObjArray*              channelLevel;
     StPidAmpNetOut*         theNetOut;
     StPidAmpChannelInfoOut* infoOut;
 
     for (int i=0; i<mDataSet.GetEntries(); i++){

        channelLevel=(TObjArray *)mDataSet.At(i);
        infoOut=(StPidAmpChannelInfoOut *)channelLevel->At(0);
 
        infoOut->PrintContent();

        for(int j=1; j<channelLevel->GetEntries();j++){
        theNetOut = (StPidAmpNetOut *)channelLevel->At(j);
 
        theNetOut->PrintContent();
	}
     }
}
  


//-------------------------------
bool StuProbabilityPidAlgorithm::tagExtrap(double rig, double dedx,TObjArray* channelLevel){
   bool myTag=false;

 //StPidAmpNetOut*   electronNetOut=(StPidAmpNetOut *)channelLevel->At(1);
   //   StPidAmpNetOut*       pionMinusNetOut=(StPidAmpNetOut *)channelLevel->At(2);
   //  StPidAmpNetOut*       kaonMinusNetOut=(StPidAmpNetOut *)channelLevel->At(3);
 //StPidAmpNetOut*      antiprotonNetOut=(StPidAmpNetOut *)channelLevel->At(4);
 //StPidAmpNetOut*   positronNetOut=(StPidAmpNetOut *)channelLevel->At(5);
   //   StPidAmpNetOut*        pionPlusNetOut=(StPidAmpNetOut *)channelLevel->At(6);
    StPidAmpNetOut*        kaonPlusNetOut=(StPidAmpNetOut *)channelLevel->At(7);
   // StPidAmpNetOut*          protonNetOut=(StPidAmpNetOut *)channelLevel->At(8);

   int nn=-1;    
   //   int mm=0;

   int channel4BG=0;//here I always the BetheBloch function from the 1st chanel
   //not safe from the code view. but here run speed is most concerned.
   //int piMinusIdx=2;
   int piPlusIdx=6;
   int kaonPlusIdx=7;
   //int kaonMinusIdx=3;
   //int protonIdx=8;
   float halfHeight=(NPaths-2.0)*PathHeight/2.0;

   //     if ((rig>(kaonPlusNetOut->GetAmpParArray())->At(3)) && ( (dedx<(bandCenter(rig,(pionPlusNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-nn*PathHeight)) && (dedx>(bandCenter(rig,(kaonPlusNetOut->GetBandParArray()))-(NPaths-2.0)*PathHeight/2.0+nn*PathHeight))) ) mExtrap=true;
     if ((fabs(rig)>(kaonPlusNetOut->GetAmpParArray())->At(3))) {
   if( ( (dedx<(bandCenter(rig,channel4BG,piPlusIdx)+halfHeight-nn*PathHeight)) && (dedx>(bandCenter(rig,channel4BG,kaonPlusIdx)-halfHeight+nn*PathHeight))) ) myTag=true;
     }


     //    if ((rig<(kaonMinusNetOut->GetAmpParArray())->At(3)) && ( (dedx<(bandCenter(rig,(pionMinusNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-nn*PathHeight))  && (dedx>(bandCenter(rig,(kaonMinusNetOut->GetBandParArray()))-(NPaths-2.0)*PathHeight/2.0+nn*PathHeight)) ) ) mExtrap=true;

     //  if ((rig<(kaonMinusNetOut->GetAmpParArray())->At(3)) && ( (dedx<(bandCenter(rig,channel4BG,piMinusIdx)+(NPaths-2.0)*PathHeight/2.0-nn*PathHeight))  && (dedx>(bandCenter(rig,channel4BG,kaonMinusIdx)-(NPaths-2.0)*PathHeight/2.0+nn*PathHeight)) ) ) mExtrap=true;


 
     // if ((rig>(protonNetOut->GetAmpParArray())->At(3)) && (dedx > (bandCenter(rig,(protonNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-(mm+3)*PathHeight))) myTag=true;
     //if ((rig>(protonNetOut->GetAmpParArray())->At(3)) && (dedx > (bandCenter(rig,channel4BG,protonIdx)+(NPaths-2.0)*PathHeight/2.0-(mm+3)*PathHeight))) myTag=true;

     //  if ((rig>(protonNetOut->GetAmpParArray())->At(3)) && (dedx > (bandCenter(rig,(protonNetOut->GetBandParArray()))+(NPaths-2.0)*PathHeight/2.0-(mm+3)*PathHeight))) mExtrap=true;
	 // if ((rig>(protonNetOut->GetAmpParArray())->At(3)) && (dedx > (bandCenter(rig,channel4BG,protonIdx)+(NPaths-2.0)*PathHeight/2.0-(mm+3)*PathHeight))) mExtrap=true;



   return myTag;
}

//-------------------------------
void StuProbabilityPidAlgorithm::readAType(StParticleDefinition* def, Float_t* mean, Float_t* amp,Float_t* sig,Float_t cal,TObjArray*  theChannel){
  int i=0;

  TArrayD BandParArray;
  TArrayD AmpParArray;
  TArrayD ResoParArray;

  BandParArray.Set(NBandParam);
   AmpParArray.Set(NAmpParam);
  ResoParArray.Set(NResoParam);

 
  for (i=0; i<NFitBandParam;i++)
  BandParArray.AddAt(mean[i],i);

  BandParArray.AddAt(def->charge()/eplus, NFitBandParam);
  BandParArray.AddAt(def->mass(),NFitBandParam+1);
  BandParArray.AddAt(CalibFactor,NFitBandParam+2);
  BandParArray.AddAt(Saturation,NFitBandParam+3);



  for (i=0; i<NAmpParam; i++)
  AmpParArray.AddAt(amp[i],i);

  for (i=0; i<NResoParam; i++)
  ResoParArray.AddAt(sig[i],i);



  StPidAmpNetOut*  theNetOut=new StPidAmpNetOut(def->name().c_str(), def->name().c_str(), 3, BandParArray,AmpParArray,ResoParArray);
  theNetOut->SetCalibConst(cal);

  theChannel->Add(theNetOut);


}



//-------------------------------
TObjArray StuProbabilityPidAlgorithm::mDataSet           = TObjArray();
TObjArray StuProbabilityPidAlgorithm::mMaxllFuncSet      = TObjArray();
TObjArray StuProbabilityPidAlgorithm::mLinearFuncSet     = TObjArray();
TObjArray StuProbabilityPidAlgorithm::mBetheBlochFuncSet = TObjArray();
StDedxMethod  StuProbabilityPidAlgorithm::mDedxMethod=kTruncatedMeanId;
TObjArray* StuProbabilityPidAlgorithm::mTheReportTable = new TObjArray();
TVectorD* StuProbabilityPidAlgorithm::mTheRangeSettingVector = new TVectorD();

 int  StuProbabilityPidAlgorithm::mNDedxBins=200;
 int  StuProbabilityPidAlgorithm::mNRigBins=200;
 int  StuProbabilityPidAlgorithm::mNNhitsBins=4;
 int  StuProbabilityPidAlgorithm::mNPtBins=1;
 int  StuProbabilityPidAlgorithm::mNDcaBins=2;
 int  StuProbabilityPidAlgorithm::mNChargeBins=2;

bool StuProbabilityPidAlgorithm::mDynamicallyCalculatePID=false;
bool StuProbabilityPidAlgorithm::mHasParameterFile=false;

double   StuProbabilityPidAlgorithm::mDedxBinWidth=0.4e-05/200.;
double   StuProbabilityPidAlgorithm::mRigBinWidth=2.0/200; //do not let NBins exceed 999.
double   StuProbabilityPidAlgorithm::mPtBinWidth=5.0/1.; //not implemented yet.
double   StuProbabilityPidAlgorithm::mNHitsBinWidth=10.;

double   StuProbabilityPidAlgorithm::mDedxStart=1e-12;
double   StuProbabilityPidAlgorithm::mDedxEnd=0.4e-05;
double   StuProbabilityPidAlgorithm::mRigStart=1e-12;
double   StuProbabilityPidAlgorithm::mRigEnd=2.0;
double   StuProbabilityPidAlgorithm::mNHitsStart=1;
double   StuProbabilityPidAlgorithm::mNHitsEnd=40;
double   StuProbabilityPidAlgorithm::mPtStart=1e-12;
double   StuProbabilityPidAlgorithm::mPtEnd=5.;
double   StuProbabilityPidAlgorithm::mDcaCutPoint=2; //just 2 bins for dca
double   StuProbabilityPidAlgorithm::mNoise=0.0; 
bool     StuProbabilityPidAlgorithm::mTurnOnNoise=true;



//-------------------------------
//St_Table* StuProbabilityPidAlgorithm::mDataTable=0;

//-------------------------------
double StuProbabilityPidAlgorithm::lowRigReso(double xa, double xb, double ya, double yb,double theX){

    return yb-(yb-ya)*sqrt(1.0-(fabs(theX)-xa)*(fabs(theX)-xa)/((xa-xb)*(xa-xb))) ;

}


//-------------------------------
void StuProbabilityPidAlgorithm::fillReportObjArray(TString fileName){

  //set idx schema for table

int    thisNDedxBins=200;
int    thisNRigBins=200;
int    thisNNhitsBins=4;
int    thisNPtBins=1;
int    thisNDcaBins=2;
int    thisNChargeBins=2;



   double        thisDedxStart=1e-12;
   double        thisDedxEnd=0.4e-05;
   double        thisRigStart=1e-12;
   double        thisRigEnd=2.0;
   double        thisNHitsStart=0;
   double        thisNHitsEnd=40;
   double        thisPtStart=1e-12;
   double        thisPtEnd=5.;
   double        thisDcaCutPoint=2; //just 2 bins for dca
   double        thisNoise=0.0; 

   double    thisDedxBinWidth=
      (thisDedxEnd-thisDedxStart)/(thisNDedxBins);



 double      thisRigBinWidth=
(thisRigEnd-thisRigStart)/(thisNRigBins); //do not let NBins exceed 999.
 double      thisPtBinWidth=
(thisPtEnd-thisPtStart)/(thisNPtBins); //not implemented yet.
 double      thisNHitsBinWidth=(thisNHitsEnd-thisNHitsStart)/(thisNNhitsBins);



     TFile f(fileName,"UPDATE",fileName);

      if (f.IsOpen()){
	cout<<" opened "<<endl;

	TObjArray* myReportTable = new TObjArray(645000);



   double  total  =0.0;

      int dedxIdx=0;
      int rigIdx=0;
      int nhitsIdx=0;
      int ptIdx=0;
      int zIdx=0;
      int dcaIdx=0;

   int     objAryIdx=0; //index of the array

   int     i=0;
   int     j=0;
   int     jj=0;


   double  theDedx=0;
   double  theRig=0;     
   int     theNHits=0;
   double  thePt=0;
   int     theCharge=0;
   double  theDca=0;

   //charge dca nhits pt dedx p
  
   for (zIdx=0; zIdx<thisNChargeBins; zIdx++){

     for (dcaIdx=0; dcaIdx<thisNDcaBins;dcaIdx++){

       for (nhitsIdx=0; nhitsIdx<thisNNhitsBins;nhitsIdx++){
 
	 for (ptIdx=0; ptIdx<thisNPtBins;ptIdx++){

	   for (dedxIdx=0; dedxIdx<thisNDedxBins;dedxIdx++){

	     for (rigIdx=0; rigIdx<thisNRigBins; rigIdx++){

	       if (zIdx==0) theCharge=-1;
               else if (zIdx==1) theCharge=1;
         
               if (dcaIdx==0) theDca= thisDcaCutPoint*0.7; //force a primary trk.
               else if (dcaIdx==1) theDca=thisDcaCutPoint*1.3;


     StuObjPidReport* tempReport = new StuObjPidReport();
                  
	       theNHits=int((nhitsIdx+0.5)*thisNHitsBinWidth);
               thePt=(ptIdx+0.5)*thisPtBinWidth;
               theDedx=(dedxIdx+0.5)*thisDedxBinWidth;
               theRig=(rigIdx+0.5)*thisRigBinWidth;


     total=0.0;

      TObjArray*               channelLevel;
      StPidAmpNetOut*          netOut;
      StPidAmpChannelInfoOut*  channelInfoOut;

    for (i=0;i<mDataSet.GetEntries(); i++){

     channelLevel=(TObjArray *)mDataSet.At(i);
     channelInfoOut=(StPidAmpChannelInfoOut *)channelLevel->At(0);
 
     if (channelInfoOut->IsInChannel(theNHits, thePt,theDca )) {//pick up the right channel

       if (mTurnOnNoise){
       StPidAmpNetOut*  protonNetOut=(StPidAmpNetOut *)channelLevel->At(8);
       mNoise=((protonNetOut->GetAmpParArray())->At(0))*0.002;
       }//turn on noise.
      
      double amplitudeArray[NParticleTypes]; 
      //the size should be channelLevel->GetEntries()
      //but we need a integer constant to declare an array.

       for (j=1;j<channelLevel->GetEntries();j++){
 
         netOut=(StPidAmpNetOut *)channelLevel->At(j);
      

         jj=j-1; //index for amplitudeArray

	 //TArrayD* bArry=netOut->GetBandParArray();
         //TArrayD* rArry=netOut->GetResoParArray();
         //TArrayD* aArry=netOut->GetAmpParArray();

     if (jj<NParticleTypes) 
       //  amplitudeArray[jj]= amplitude(dedx,rig, bArry, rArry,aArry);
       amplitudeArray[jj]= StuProbabilityPidAlgorithm::amplitude(theDedx,theRig, i,j);

     if (theCharge*((netOut->GetBandParArray())->At(3))>0.0)//the same sign of z
	total +=amplitudeArray[jj];   
     
       }

      for (j=1;j<channelLevel->GetEntries();j++){//do not use j=0. j=0 is for channelInfoOut.
          jj=j-1;
          
         netOut=(StPidAmpNetOut *)channelLevel->At(j);  
 
	 if (theCharge*((netOut->GetBandParArray())->At(3))>0.0) {


            double probability=amplitudeArray[jj]/total;
            
   fill(tempReport->GetProbArray(), tempReport->GetPIDArray(), probability, netOut);
  

	 }
       }

           tempReport->SetExtrap(tagExtrap(theRig, theDedx,channelLevel));
 

	   //following block for debugging
	   /*
      tempReport->SetCharge(theCharge);
      tempReport->SetDca(theDca);
      tempReport->SetNHits(theNHits);
      tempReport->SetPt(thePt);
      tempReport->SetDedx(theDedx);
      tempReport->SetRig(theRig);
	   */



     }//is inchanel
    }


    myReportTable->AddAt(tempReport,objAryIdx); objAryIdx++;
    cout<<objAryIdx<<endl;
    

	     }

	   }
	 }
       }//for (nhitsIdx=0; nhitsIdx<mNNhitsBins;nhitsIdx++)
     }
   }//for (zIdx=0; zIdx<mNChargeBins; zIdx++)



   myReportTable->Write("mProbabilityTable",TObject::kOverwrite | TObject::kSingleKey);




   TVectorD* rangeSet = new TVectorD(40);

   (*rangeSet)(0)=thisDedxStart;
   (*rangeSet)(1)=thisDedxEnd;
   (*rangeSet)(2)=thisRigStart;
   (*rangeSet)(3)=thisRigEnd;
   (*rangeSet)(4)=thisNHitsStart;
   (*rangeSet)(5)=thisNHitsEnd;
   (*rangeSet)(6)=thisPtStart;
   (*rangeSet)(7)=thisPtEnd;
   (*rangeSet)(8)=thisDcaCutPoint;
   (*rangeSet)(9)=thisNoise;
   (*rangeSet)(10)=thisDedxBinWidth;
   (*rangeSet)(11)=thisRigBinWidth;
   (*rangeSet)(12)=thisPtBinWidth;
   (*rangeSet)(13)=thisNHitsBinWidth;
   (*rangeSet)(14)=thisNDedxBins;
   (*rangeSet)(15)=thisNRigBins;
   (*rangeSet)(16)=thisNNhitsBins;
   (*rangeSet)(17)=thisNPtBins;
   (*rangeSet)(18)=thisNDcaBins;
   (*rangeSet)(19)=thisNChargeBins;

   rangeSet->Write("mTheRangeSettingVector",TObject::kOverwrite);


 f.Close();    

      }



}

//-------------------------------
void StuProbabilityPidAlgorithm::setBins4Table(){


    StuProbabilityPidAlgorithm::mNDedxBins=200;
    StuProbabilityPidAlgorithm::mNRigBins=200;
    StuProbabilityPidAlgorithm::mNNhitsBins=4;
    StuProbabilityPidAlgorithm::mNPtBins=1;
    StuProbabilityPidAlgorithm::mNDcaBins=2;
    StuProbabilityPidAlgorithm::mNChargeBins=2;



    StuProbabilityPidAlgorithm::mDedxStart=1e-12;
    StuProbabilityPidAlgorithm::mDedxEnd=0.4e-05;
    StuProbabilityPidAlgorithm::mRigStart=1e-12;
    StuProbabilityPidAlgorithm::mRigEnd=2.0;
    StuProbabilityPidAlgorithm::mNHitsStart=0;
    StuProbabilityPidAlgorithm::mNHitsEnd=40;
    StuProbabilityPidAlgorithm::mPtStart=1e-12;
    StuProbabilityPidAlgorithm::mPtEnd=5.;
    StuProbabilityPidAlgorithm::mDcaCutPoint=2; //just 2 bins for dca
    StuProbabilityPidAlgorithm::mNoise=0.0; 
    StuProbabilityPidAlgorithm::mTurnOnNoise=true;

    StuProbabilityPidAlgorithm::mDedxBinWidth=
      (StuProbabilityPidAlgorithm::mDedxEnd-StuProbabilityPidAlgorithm::mDedxStart)/(StuProbabilityPidAlgorithm::mNDedxBins);
    StuProbabilityPidAlgorithm::mRigBinWidth=
(StuProbabilityPidAlgorithm::mRigEnd-StuProbabilityPidAlgorithm::mRigStart)/(StuProbabilityPidAlgorithm::mNRigBins); //do not let NBins exceed 999.
    StuProbabilityPidAlgorithm::mPtBinWidth=
(StuProbabilityPidAlgorithm::mPtEnd-StuProbabilityPidAlgorithm::mPtStart)/(StuProbabilityPidAlgorithm::mNPtBins); //not implemented yet.
    StuProbabilityPidAlgorithm::mNHitsBinWidth=(StuProbabilityPidAlgorithm::mNHitsEnd-StuProbabilityPidAlgorithm::mNHitsStart)/(StuProbabilityPidAlgorithm::mNNhitsBins);



}
//-------------------------------
void StuProbabilityPidAlgorithm::fillPIDByLookUpTable(int myCharge, double myDca, int myNhits, double myPt, double myDedx, double myRig){
     int totalEntry
      =mNChargeBins*mNDcaBins*mNNhitsBins*mNPtBins*mNDedxBins*mNRigBins;

     int positionPointer=0;     
     totalEntry=totalEntry/2;

     if (myCharge>0) positionPointer=totalEntry;
     //     else positionPointer=0;

     totalEntry=totalEntry/2;
     if (myDca>mDcaCutPoint) positionPointer=positionPointer+totalEntry;


     int myNhitsBin = int((myNhits-mNHitsStart)/mNHitsBinWidth);
     totalEntry=totalEntry/mNNhitsBins;
     positionPointer=positionPointer+totalEntry*myNhitsBin;


     int myPtBin    =int((myPt-mPtStart)/mPtBinWidth);
     totalEntry=totalEntry/mNPtBins;
     positionPointer=positionPointer+totalEntry*myPtBin;


     int myDedxBin=int((myDedx-mDedxStart)/mDedxBinWidth);
     totalEntry=totalEntry/mNDedxBins;
     positionPointer=positionPointer+totalEntry*myDedxBin;

     int myRigBin=int((myRig-mRigStart)/mRigBinWidth);
     totalEntry=totalEntry/mNRigBins;//should =1 here
     positionPointer=positionPointer+totalEntry*myRigBin;



     if (positionPointer<mTheReportTable->GetEntries()) {
     StuObjPidReport* myReport=(StuObjPidReport *)(mTheReportTable->At(positionPointer));
    

	    for (int jk=0; jk<3; jk++){
            PID[jk]=myReport->GetPID(jk);
            mProb[jk]=myReport->GetProb(jk);
	    }

            mExtrap=myReport->GetExtrapTag();
     }          

}

//-------------------------------
void StuProbabilityPidAlgorithm::setRanges4Table(TVectorD* theSetting){

    StuProbabilityPidAlgorithm::mNDedxBins  =int((*theSetting)(14));
    StuProbabilityPidAlgorithm::mNRigBins   =int((*theSetting)(15));
    StuProbabilityPidAlgorithm::mNNhitsBins =int((*theSetting)(16));
    StuProbabilityPidAlgorithm::mNPtBins    =int((*theSetting)(17));
    StuProbabilityPidAlgorithm::mNDcaBins   =int((*theSetting)(18));
    StuProbabilityPidAlgorithm::mNChargeBins=int((*theSetting)(19));



    StuProbabilityPidAlgorithm::mDedxStart  =(*theSetting)(0);
    StuProbabilityPidAlgorithm::mDedxEnd    =(*theSetting)(1);
    StuProbabilityPidAlgorithm::mRigStart   =(*theSetting)(2);
    StuProbabilityPidAlgorithm::mRigEnd     =(*theSetting)(3);
    StuProbabilityPidAlgorithm::mNHitsStart =(*theSetting)(4);
    StuProbabilityPidAlgorithm::mNHitsEnd   =(*theSetting)(5);
    StuProbabilityPidAlgorithm::mPtStart    =(*theSetting)(6);
    StuProbabilityPidAlgorithm::mPtEnd      =(*theSetting)(7);
    StuProbabilityPidAlgorithm::mDcaCutPoint=(*theSetting)(8); //just 2 bins for dca
    StuProbabilityPidAlgorithm::mNoise      =(*theSetting)(9); 
    StuProbabilityPidAlgorithm::mTurnOnNoise=true;

    StuProbabilityPidAlgorithm::mDedxBinWidth=
      (StuProbabilityPidAlgorithm::mDedxEnd-StuProbabilityPidAlgorithm::mDedxStart)/(StuProbabilityPidAlgorithm::mNDedxBins);
    StuProbabilityPidAlgorithm::mRigBinWidth=
(StuProbabilityPidAlgorithm::mRigEnd-StuProbabilityPidAlgorithm::mRigStart)/(StuProbabilityPidAlgorithm::mNRigBins); //do not let NBins exceed 999.
    StuProbabilityPidAlgorithm::mPtBinWidth=
(StuProbabilityPidAlgorithm::mPtEnd-StuProbabilityPidAlgorithm::mPtStart)/(StuProbabilityPidAlgorithm::mNPtBins); //not implemented yet.
    StuProbabilityPidAlgorithm::mNHitsBinWidth=(StuProbabilityPidAlgorithm::mNHitsEnd-StuProbabilityPidAlgorithm::mNHitsStart)/(StuProbabilityPidAlgorithm::mNNhitsBins);

}
    
//-------------------------------
void StuProbabilityPidAlgorithm::fillPIDByCalculation(int myCharge, double myDca, int myNhits, double myPt, double myDedx, double myRig){
 
   double   total=0.0;
   int     i=0;
   int     j=0;
   int     jj=0;

      TObjArray*               channelLevel;
      StPidAmpNetOut*          netOut;
      StPidAmpChannelInfoOut*  channelInfoOut;

    for (i=0;i<mDataSet.GetEntries(); i++){

     channelLevel=(TObjArray *)mDataSet.At(i);
     channelInfoOut=(StPidAmpChannelInfoOut *)channelLevel->At(0);
 
 

     if (channelInfoOut->IsInChannel(myNhits, myPt,myDca )) {//pick up the right channel

       if (mTurnOnNoise){
       StPidAmpNetOut*  protonNetOut=(StPidAmpNetOut *)channelLevel->At(8);
       mNoise=((protonNetOut->GetAmpParArray())->At(0))*0.002;
       }//turn on noise.
      
      double amplitudeArray[NParticleTypes]; 
      //the size should be channelLevel->GetEntries()
      //but we need a integer constant to declare an array.


      //true vaule begin with amplitudeArray[1]
       
       for (j=1;j<channelLevel->GetEntries();j++){
 
         netOut=(StPidAmpNetOut *)channelLevel->At(j);
      

         jj=j-1; //index for amplitudeArray

	 //TArrayD* bArry=netOut->GetBandParArray();
         //TArrayD* rArry=netOut->GetResoParArray();
         //TArrayD* aArry=netOut->GetAmpParArray();

     if (jj<NParticleTypes) 
       //  amplitudeArray[jj]= amplitude(dedx,rig, bArry, rArry,aArry);
       amplitudeArray[jj]= amplitude(myDedx,myRig, i,j);
 
         if (myCharge*((netOut->GetBandParArray())->At(3))>0.0)//the same sign of z
	total +=amplitudeArray[jj];   
     
       }

       for (j=1;j<channelLevel->GetEntries();j++){//do not use j=0. j=0 is for channelInfoOut.
          jj=j-1;
          
         netOut=(StPidAmpNetOut *)channelLevel->At(j);  
 
	 if (myCharge*((netOut->GetBandParArray())->At(3))>0.0) {
            double probability=amplitudeArray[jj]/total;
           fill(probability,netOut);
	 }
       }
            mExtrap=tagExtrap(myRig, myDedx,channelLevel);
     }//is inchanel
    }
}
//-------------------------------
void StuProbabilityPidAlgorithm::setDynamicallyCalculatePID(){

  StuProbabilityPidAlgorithm::mDynamicallyCalculatePID=true;
}

/*
//-------------------------------
void StuProbabilityPidAlgorithm::debug(double theDedx, double theRig, double theNHits, double thePt, int theCharge, double theDca){

     StuObjPidReport* tempReport = new StuObjPidReport();
 
  double   total=0.0;
   int     i=0;
   int     j=0;
   int     jj=0;

      TObjArray*               channelLevel;
      StPidAmpNetOut*          netOut;
      StPidAmpChannelInfoOut*  channelInfoOut;

    for (i=0;i<mDataSet.GetEntries(); i++){

     channelLevel=(TObjArray *)mDataSet.At(i);
     channelInfoOut=(StPidAmpChannelInfoOut *)channelLevel->At(0);
 
     if (channelInfoOut->IsInChannel(theNHits, thePt,theDca )) {//pick up the right channel
       cout<<" in the above channel "<<endl;

       if (mTurnOnNoise){
       StPidAmpNetOut*  protonNetOut=(StPidAmpNetOut *)channelLevel->At(8);
       mNoise=((protonNetOut->GetAmpParArray())->At(0))*0.002;
       }//turn on noise.
      
      double amplitudeArray[NParticleTypes]; 
      //the size should be channelLevel->GetEntries()
      //but we need a integer constant to declare an array.

       for (j=1;j<channelLevel->GetEntries();j++){
 
         netOut=(StPidAmpNetOut *)channelLevel->At(j);
      

         jj=j-1; //index for amplitudeArray

	 //TArrayD* bArry=netOut->GetBandParArray();
         //TArrayD* rArry=netOut->GetResoParArray();
         //TArrayD* aArry=netOut->GetAmpParArray();

     if (jj<NParticleTypes) 
       //  amplitudeArray[jj]= amplitude(dedx,rig, bArry, rArry,aArry);
       amplitudeArray[jj]= StuProbabilityPidAlgorithm::amplitude(theDedx,theRig, i,j);

     if (theCharge*((netOut->GetBandParArray())->At(3))>0.0)//the same sign of z
	total +=amplitudeArray[jj];   
     
       }

      for (j=1;j<channelLevel->GetEntries();j++){//do not use j=0. j=0 is for channelInfoOut.
          jj=j-1;
          
         netOut=(StPidAmpNetOut *)channelLevel->At(j);  
 
	 if (theCharge*((netOut->GetBandParArray())->At(3))>0.0) {


            double probability=amplitudeArray[jj]/total;
            
   fill(tempReport->GetProbArray(), tempReport->GetPIDArray(), probability, netOut);
  

	 }
       }

           tempReport->SetExtrap(tagExtrap(theRig, theDedx,channelLevel));

     }//is inchanel
    }

      
  for (int y=0; y<10; y++)  cout<<"tempReport->GetProb(0) "<<tempReport->GetProb(0)<<endl;

     


}
*/


/*
//-------------------------------
void StuProbabilityPidAlgorithm::debug2(double theDedx, double theRig, int theNHits, double thePt, int theCharge, double theDca){
  int charge=theCharge;
  int nhits=theNHits;
  double dedx=theDedx;
  double rig=theRig;
  double pt=thePt;
  double dca=theDca;

  int totalEntry=mTheReportTable->GetEntries();
  //      =mNChargeBins*mNDcaBins*mNNhitsBins*mNPtBins*mNDedxBins*mNRigBins;

     int positionPointer=0;     
     totalEntry=totalEntry/2;

     if (charge>0) positionPointer=totalEntry;
     //     else positionPointer=0;

     totalEntry=totalEntry/2;
     if (dca>mDcaCutPoint) positionPointer=positionPointer+totalEntry;


     int myNhitsBin = (nhits)/mNHitsBinWidth;

     //int myNhitsBin = (nhits-mNHitsStart)/mNHitsBinWidth;
     totalEntry=totalEntry/mNNhitsBins;
     positionPointer=positionPointer+totalEntry*myNhitsBin;


     int myPtBin  =0;// =(Pt-mPtStart)/mPtBinWidth
     totalEntry=totalEntry/mNPtBins;
     positionPointer=positionPointer+totalEntry*myPtBin;


     int myDedxBin=(dedx)/mDedxBinWidth;
     //int myDedxBin=(dedx-mDedxStart)/mDedxBinWidth;
     totalEntry=totalEntry/mNDedxBins;
     positionPointer=positionPointer+totalEntry*myDedxBin;

     int myRigBin=(rig)/mRigBinWidth;
     //int myRigBin=(rig-mRigStart)/mRigBinWidth;
     cout<<"totalEntry "<<totalEntry<<" =? "<<" mNRigBins "<<mNRigBins<<endl;
     totalEntry=totalEntry/mNRigBins;//should =1 here
     cout<<" totalEntry shoud =1 :"<<totalEntry<<endl;
     positionPointer=positionPointer+totalEntry*myRigBin;

     
     StuObjPidReport* myReport=(StuObjPidReport *)(mTheReportTable->At(positionPointer));
    

          
	    for (int y=0; y<2; y++){
	     cout<<" positionPointer "<<positionPointer<<endl;
	     cout<<"myReport->GetPID(0) "<<myReport->GetPID(0)<<" myReport->GetProb(0)  "<<myReport->GetProb(0)<<endl;

            //  cout<<" charge   "<<myReport->GetCharge()<<endl;
            //  cout<<" dca      "<<myReport->GetDca()<<endl;
            //  cout<<" GetNHits "<<myReport->GetNHits()<<endl;
            //  cout<<" GetPt    "<<myReport->GetPt()<<endl;
            //  cout<<" GetDedx  "<<myReport->GetDedx()<<endl;
            //  cout<<" GetRig   "<<myReport->GetRig()<<endl;


	    }


       }
*/

