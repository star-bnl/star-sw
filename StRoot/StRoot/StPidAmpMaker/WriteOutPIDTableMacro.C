#include <float.h>

#include <fstream.h>

#include "/afs/rhic.bnl.gov/star/replicas/DEV/StRoot/StEventUtilities/BetheBlochFunction.hh"

#include "/star/data05/scratch/aihong/pidamp_dAu/StRoot/StPidAmpMaker/StPidProbabilityConst.hh"


void WriteOutPIDTableMacro( char* myOutputName){

  ///////////////initializations.....................

    char* mInputAmpFileName[mMultiplicityBins][mNDcaBins][mNChargeBins];

    char* mInputSigmaFileName[mMultiplicityBins][mNDcaBins][mNChargeBins];
    char* mInputCalibFileName[mMultiplicityBins][mNDcaBins][mNChargeBins];

    char* mOutputFileName;

    double* mSigmaOfSingleTrail;
    mSigmaOfSingleTrail = new double[mNEtaBins];

    TF1* EBandCenter;
    TF1* PiBandCenter;
    TF1* KBandCenter;
    TF1* PBandCenter; 

    TF1* BandCenterPtr;


    double    **BBPrePar;
    double    **BBTurnOver;
    double    **BBOffSetPar; 
    double    **BBScalePar; 
    double    **BBSaturate;


  BBPrePar    = new double*[mNEtaBins];
  BBTurnOver  = new double*[mNEtaBins];
  BBOffSetPar = new double*[mNEtaBins];
  BBScalePar  = new double*[mNEtaBins];
  BBSaturate  = new double*[mNEtaBins];



  for (int ii=0; ii<mNEtaBins; ii++) {
    BBPrePar[ii]    = new double[mNNHitsBins];
    BBTurnOver[ii]  = new double[mNNHitsBins];
    BBOffSetPar[ii] = new double[mNNHitsBins];
    BBScalePar[ii]  = new double[mNNHitsBins];
    BBSaturate[ii]  = new double[mNNHitsBins];
  }


  //allocate functions...
  EBandCenter 
     =new TF1("EBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 
  PiBandCenter 
     =new TF1("PiBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 
  KBandCenter 
     =new TF1("KBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 
  PBandCenter 
     =new TF1("PBandCenter",BetheBlochFunction, mPStart,mPEnd, NParameters); 



  Double_t electronPars[7]
        ={ 1.072, 0.3199, 1.66032e-07, 1, 0.511e-3, 2.71172e-07, 0.0005 };
  Double_t pionPars[7]
        ={ 1.072, 0.3199, 1.66032e-07, 1, 0.13957,  2.71172e-07, 0.0005 };
  Double_t kaonPars[7]
        ={ 1.072, 0.3199, 1.66032e-07, 1, 0.49368,  2.71172e-07, 0.0005 };
  Double_t antiprotonPars[7]
        ={ 1.072, 0.3199, 1.66032e-07, 1, 0.93827,  2.71172e-07, 0.0005 };


   EBandCenter->SetParameters(&electronPars[0]);
       PiBandCenter->SetParameters(&pionPars[0]);
       KBandCenter->SetParameters(&kaonPars[0]);
     PBandCenter->SetParameters(&antiprotonPars[0]);


     /////////////end of initializations.



     TString OutPutName(myOutputName);

     char* PidTag;

          if (OutPutName.Contains("PiPIDTable")) 
           {PidTag="Pi"; BandCenterPtr=PiBandCenter;}
     else if (OutPutName.Contains("EPIDTable"))  
           {PidTag="E";  BandCenterPtr=EBandCenter;}
     else if (OutPutName.Contains("KPIDTable")) 
           {PidTag="K";  BandCenterPtr=KBandCenter;}
     else if (OutPutName.Contains("PPIDTable"))  
           {PidTag="P";  BandCenterPtr=PBandCenter;}

     //set file names
   for (int i=0; i<mMultiplicityBins; i++)
     for (int j=0; j<mNDcaBins; j++) 
        for (int k=0; k<mNChargeBins; k++) {



      //-------------amp file
      mInputAmpFileName[i][j][k]= new char[80];
      sprintf(mInputAmpFileName[i][j][k],"./PidHistoAmp_%d%d%d.root",i,j,k);

      //------------sigma file
      mInputSigmaFileName[i][j][k]= new char[80];
      sprintf(mInputSigmaFileName[i][j][k],"PidSigmaOfSingleTrail_%d%d%d_basedOn_%d00.txt",i,j,k,i);

      //------------calib file
      mInputCalibFileName[i][j][k]= new char[80];
      sprintf(mInputCalibFileName[i][j][k], "./PhaseSpaceCalib%d%d%dButItisbasedOn_%d01.txt",i,j,k,i);

      //-----------out put file
      mOutputFileName=myOutputName;
      }


   //////////filling............
   TFile outFile(mOutputFileName,"RECREATE");

   outFile.cd();


   int thisMultBins=mMultiplicityBins;
   int thisDcaBins=mNDcaBins;
   int thisChargeBins=mNChargeBins;

   int thisDedxBins=200; //fake
   int thisPBins=200;
   int thisEtaBins=mNEtaBins;
   int thisNHitsBins=mNNHitsBins;

////////////////////////

   double thisDedxStart=0.;
   double thisDedxEnd=0.53e-5;
   double thisPStart=1e-12;
   double thisPEnd=2.0;
   double thisEtaStart=mEtaStart;
   double thisEtaEnd=mEtaEnd;
   double thisNHitsStart=mNNHitsStart;
   double thisNHitsEnd=mNNHitsEnd;

////////////////////////
//p, eta, nhits can be equavily dividable.

   TVectorD* EqualyDividableRangeStartSet = new TVectorD(20);

   (*EqualyDividableRangeStartSet)(0)=thisDedxStart;
   (*EqualyDividableRangeStartSet)(1)=thisPStart;
   (*EqualyDividableRangeStartSet)(2)=thisEtaStart;
   (*EqualyDividableRangeStartSet)(3)=thisNHitsStart;

   EqualyDividableRangeStartSet->Write("EqualyDividableRangeStartSet",TObject::kOverwrite | TObject::kSingleKey);
//-------------
   TVectorD* EqualyDividableRangeEndSet = new TVectorD(20);

   (*EqualyDividableRangeEndSet)(0)=thisDedxEnd;
   (*EqualyDividableRangeEndSet)(1)=thisPEnd;
   (*EqualyDividableRangeEndSet)(2)=thisEtaEnd;
   (*EqualyDividableRangeEndSet)(3)=thisNHitsEnd;

   EqualyDividableRangeEndSet->Write("EqualyDividableRangeEndSet",TObject::kOverwrite | TObject::kSingleKey);
   
//-------------

    TVectorD* EqualyDividableRangeNBinsSet = new TVectorD(20);

   (*EqualyDividableRangeNBinsSet)(0)=thisDedxBins;
   (*EqualyDividableRangeNBinsSet)(1)=thisPBins;
   (*EqualyDividableRangeNBinsSet)(2)=thisEtaBins;
   (*EqualyDividableRangeNBinsSet)(3)=thisNHitsBins;

   EqualyDividableRangeNBinsSet->Write("EqualyDividableRangeNBinsSet",TObject::kOverwrite | TObject::kSingleKey);
    
////////////////////////////////
// multi, dca, charge , not easy to be equavily dividable

     TVectorD* NoEqualyDividableRangeNBinsSet = new TVectorD(20);

   (*NoEqualyDividableRangeNBinsSet)(0)=thisMultBins;
   (*NoEqualyDividableRangeNBinsSet)(1)=thisDcaBins;
   (*NoEqualyDividableRangeNBinsSet)(2)=thisChargeBins;

   NoEqualyDividableRangeNBinsSet->Write("NoEqualyDividableRangeNBinsSet",TObject::kOverwrite | TObject::kSingleKey);
    
//----------
     TVectorD* MultiBinEdgeSet = new TVectorD(20);
     // for dAu
     (*MultiBinEdgeSet)(0) = 1.; //
     (*MultiBinEdgeSet)(1) =.4;   
     (*MultiBinEdgeSet)(2) =.2;   //20% central 

     MultiBinEdgeSet->Write("MultiBinEdgeSet",TObject::kOverwrite | TObject::kSingleKey);
//----------
     TVectorD* DcaBinEdgeSet = new TVectorD(20);
     
     (*DcaBinEdgeSet)(0) = 0.0000; //
     (*DcaBinEdgeSet)(1) = 3.0;  // 3cm   
      
     DcaBinEdgeSet->Write("DcaBinEdgeSet",TObject::kOverwrite | TObject::kSingleKey);

//----------charge bin edge is obviouse, charge<0, bin0. charge>0 bin1. so forget it.


    outFile.Write();

    
    int objAryIdx=0;
 
     TVectorD* theAmpTable;
     TVectorD* theCenterTable;
     TVectorD* theSigmaTable;


     TVectorD* theBBPreParTable;
     TVectorD* theBBTurnOverTable;
     TVectorD* theBBScaleTable;
     TVectorD* theBBOffSetTable;
     TVectorD* theBBSaturateTable;



  /////////have to fill them seperately. otherwise the mem. will be exausted.
     ////this is ugly, but I have no better choice.


     //-------fill and write eAmp
     objAryIdx=0;
     theAmpTable = new TVectorD(thisMultBins*thisDcaBins*thisChargeBins*thisPBins*thisEtaBins*thisNHitsBins);
     theCenterTable = new TVectorD(thisMultBins*thisDcaBins*thisChargeBins*thisPBins*thisEtaBins*thisNHitsBins);
     theSigmaTable = new TVectorD(thisMultBins*thisDcaBins*thisChargeBins*thisPBins*thisEtaBins*thisNHitsBins);



     theBBPreParTable   = new TVectorD(thisEtaBins*thisNHitsBins);
     theBBTurnOverTable = new TVectorD(thisEtaBins*thisNHitsBins);
     theBBScaleTable    = new TVectorD(thisEtaBins*thisNHitsBins);
     theBBOffSetTable   = new TVectorD(thisEtaBins*thisNHitsBins);
     theBBSaturateTable = new TVectorD(thisEtaBins*thisNHitsBins);


   for (int multIdx=0; multIdx<thisMultBins; multIdx++)
    for (int dcaIdx=0; dcaIdx<thisDcaBins; dcaIdx++)
      for (int chargeIdx=0; chargeIdx<thisChargeBins; chargeIdx++){

      TFile AmpFile(mInputAmpFileName[multIdx][dcaIdx][chargeIdx],"READ");

      ifstream sigmaFile(mInputSigmaFileName[multIdx][dcaIdx][chargeIdx]);
      ifstream calibFile(mInputCalibFileName[multIdx][dcaIdx][chargeIdx]);


      cout<<mInputSigmaFileName[multIdx][dcaIdx][chargeIdx]<<endl;

   int ie,in;
//read in calib parameters.
  for (int h=0; h<thisEtaBins*thisNHitsBins;h++){
      calibFile>>ie; calibFile>>in;


      calibFile>>BBPrePar[ie][in];
      calibFile>>BBTurnOver[ie][in];
      calibFile>>BBOffSetPar[ie][in];
      calibFile>>BBScalePar[ie][in];
      calibFile>>BBSaturate[ie][in];


     (*theBBPreParTable)(h)=BBPrePar[ie][in];
     (*theBBTurnOverTable)(h)=BBTurnOver[ie][in];
      (*theBBOffSetTable)(h)=BBOffSetPar[ie][in];
      (*theBBScaleTable)(h)=BBScalePar[ie][in];
      (*theBBSaturateTable)(h)=BBSaturate[ie][in];

  }

      calibFile.close();
    
 for (int h=0; h<thisEtaBins; h++){
   sigmaFile>>ie;  sigmaFile>>mSigmaOfSingleTrail[h];
 }

      sigmaFile.close();


   for (int pIdx=0; pIdx<thisPBins; pIdx++)
    for (int etaIdx=0; etaIdx<thisEtaBins; etaIdx++)
      for (int nhitsIdx=0; nhitsIdx<thisNHitsBins; nhitsIdx++){


     double pPosition=   (pIdx+0.5)*(thisPEnd-thisPStart)/float(thisPBins);             
     double etaPosition=  (etaIdx+0.5)*(thisEtaEnd-thisEtaStart)/float(thisEtaBins);
     double nhitsPosition=double((nhitsIdx+0.5)*(float(thisNHitsEnd-thisNHitsStart)/float(thisNHitsBins)));

   BandCenterPtr->SetParameter(0,BBPrePar[etaIdx][nhitsIdx]);
   BandCenterPtr->SetParameter(1,BBTurnOver[etaIdx][nhitsIdx]);
   BandCenterPtr->SetParameter(2,BBOffSetPar[etaIdx][nhitsIdx]);
   BandCenterPtr->SetParameter(5,BBScalePar[etaIdx][nhitsIdx]);
   BandCenterPtr->SetParameter(6,BBSaturate[etaIdx][nhitsIdx]);

        double theAmp;
        double theCenter;
        double theSigma;

	char *theAmpName = new char[80];
        sprintf(theAmpName,"%sAmp%d%d",PidTag,etaIdx,nhitsIdx);

     TH1F* theAmpHist=(TH1F *)AmpFile.Get(theAmpName);

        if (theAmpName) delete theAmpName;

    if (theAmpHist) {



     if (OutPutName.Contains("PiPIDTable")) 
          {PidTag="Pi"; BandCenterPtr=PiBandCenter;}
     if (OutPutName.Contains("EPIDTable"))  
          {PidTag="E";  BandCenterPtr=EBandCenter;}
     if (OutPutName.Contains("KPIDTable")) 
          {PidTag="K";  BandCenterPtr=KBandCenter;}
     if (OutPutName.Contains("PPIDTable"))  
          {PidTag="P";  BandCenterPtr=PBandCenter;}



     if (OutPutName.Contains("PiPIDTable")){//3 fcn for Pi

         theAmp = (theAmpHist->GetBinContent(pIdx+1)>0) ? 
                  (theAmpHist->GetBinContent(pIdx+1)) : 0.;

       TF1* PiFcnCenter = theAmpHist->GetFunction("PiFcnCenter");
       TF1* PiFcnLeft   = theAmpHist->GetFunction("PiFcnLeft");
       TF1* PiFcnRight  = theAmpHist->GetFunction("PiFcnRight");

       if (PiFcnRight){ 
          PiFcnRight->SetRange(PiFcnRight->GetXmin(),mPEnd);
       if (     pPosition<=PiFcnRight->GetXmax() 
	  && pPosition >PiFcnRight->GetXmin()&& PiFcnRight->GetParameter(1)<0 )
	 theAmp = PiFcnRight->Eval(pPosition,0.,0.);
       }

       if (PiFcnCenter){
       if (     pPosition<=PiFcnCenter->GetXmax() 
             && pPosition >PiFcnCenter->GetXmin() ) 
	 theAmp = PiFcnCenter->Eval(pPosition,0.,0.);
       }

       if (PiFcnLeft){
       if (     pPosition <= PiFcnLeft->GetXmax()
             && pPosition  > PiFcnLeft->GetXmin() )
	 theAmp = PiFcnLeft->Eval(pPosition,0.,0.);
       }


     } else if (OutPutName.Contains("EPIDTable")) { //1 fcn for E

         theAmp = (theAmpHist->GetBinContent(pIdx+1)>0) ? 
                  (theAmpHist->GetBinContent(pIdx+1)) : 0.;       

       TF1* EFcnLeft = theAmpHist->GetFunction("EFcnLeft");
       if (EFcnLeft){
       if (     pPosition<=EFcnLeft->GetXmax()
	     && pPosition >EFcnLeft->GetXmin() && EFcnLeft->GetParameter(1)<0 )
             theAmp = EFcnLeft->Eval(pPosition,0.,0.);
       }

       TF1* EFcnRight = theAmpHist->GetFunction("EFcnRight");
       if (EFcnRight){
	 //       EFcnRight->SetRange(EFcnRight->GetXmin(), mPEnd);
	 EFcnRight->SetRange(0.15, mPEnd); //extrapolate it to lower pt
       if (     pPosition<=EFcnRight->GetXmax()
	     && pPosition >EFcnRight->GetXmin() && EFcnRight->GetParameter(1)<0 )
             theAmp = EFcnRight->Eval(pPosition,0.,0.);
       }

      if (nhitsPosition<=5)  //ndedx<5, E fcn fitting not good. use hist.
         theAmp = (theAmpHist->GetBinContent(pIdx+1)>0) ? 
                  (theAmpHist->GetBinContent(pIdx+1)) : 0.;

     } else if (OutPutName.Contains("KPIDTable")) {//3 fcn for K

         theAmp = (theAmpHist->GetBinContent(pIdx+1)>0) ? 
                  (theAmpHist->GetBinContent(pIdx+1)) : 0.;


       TF1* KFcnCenter = theAmpHist->GetFunction("KFcnCenter");
       TF1* KFcnLeft   = theAmpHist->GetFunction("KFcnLeft");
       TF1* KFcnRight  = theAmpHist->GetFunction("KFcnRight");

       if (KFcnRight){
       if (     pPosition<=KFcnRight->GetXmax() 
	     && pPosition >KFcnRight->GetXmin() )
	 theAmp = KFcnRight->Eval(pPosition,0.,0.);
       }

       if (KFcnCenter){
       if (     pPosition<=KFcnCenter->GetXmax() 
             && pPosition >KFcnCenter->GetXmin() ) 
	 theAmp = KFcnCenter->Eval(pPosition,0.,0.);
       }

       if (KFcnLeft){
       if (     pPosition<=KFcnLeft->GetXmax()
             && pPosition >KFcnLeft->GetXmin() ) 
	 theAmp = KFcnLeft->Eval(pPosition,0.,0.);
       }

       if (nhitsPosition<=5)  //ndedx<5, K fcn fitting not good. use hist.
         theAmp = (theAmpHist->GetBinContent(pIdx+1)>0) ? 
                  (theAmpHist->GetBinContent(pIdx+1)) : 0.;

     } else if (OutPutName.Contains("PPIDTable")) { // 1 fcn for P

         theAmp = (theAmpHist->GetBinContent(pIdx+1)>0) ? 
                  (theAmpHist->GetBinContent(pIdx+1)) : 0.;

       TF1* PFcnRight = theAmpHist->GetFunction("PFcnRight");
       TF1* PFcnLeft  = theAmpHist->GetFunction("PFcnLeft");
     
       if (PFcnRight){              
       if (     pPosition<=PFcnRight->GetXmax() 
	     && pPosition >PFcnRight->GetXmin() )
	 theAmp = PFcnRight->Eval(pPosition,0.,0.);
       }

       if (PFcnLeft){
	 if (     pPosition<=PFcnLeft->GetXmax()
		  && pPosition >PFcnLeft->GetXmin() )
	   theAmp = PFcnLeft->Eval(pPosition,0.,0.);
       }

     }


     }   else theAmp=0;


     theCenter=BandCenterPtr->Eval(pPosition);
     theSigma=
    mSigmaOfSingleTrail[etaIdx]*theCenter/TMath::Sqrt(nhitsPosition);



     theAmp= (theAmp<FLT_MAX && theAmp>0.) ? theAmp : 0.;
     theCenter = (theCenter<FLT_MAX && theCenter>0.) ? theCenter : 0.;
     theSigma = (theSigma<FLT_MAX && theSigma>0. ) ? theSigma : 0.;

         (*theAmpTable)(objAryIdx)=(theAmp);
         (*theCenterTable)(objAryIdx)=(theCenter);
         (*theSigmaTable)(objAryIdx)=(theSigma);

	 //	 cout<<" mSigmaOfSingleTrail["<<etaIdx<<"] "<<mSigmaOfSingleTrail[etaIdx]<<" theAmp "<<theAmp<<" theCenter "<<theCenter<<" theSigma "<<theSigma<<endl;

      objAryIdx++;

      //    cout<<objAryIdx<<endl;

      }
    
      AmpFile.Close();
      
      }

    outFile.cd();
    

    char *ampNm = new char[80]; // name of amp. table
    sprintf(ampNm,"%sAmp",PidTag);
    char *ctrNm = new char[80]; // name of center table
    sprintf(ctrNm,"%sCenter",PidTag);
    char *sigmaNm = new char[80];// name of sigma table
    sprintf(sigmaNm,"%sSigma",PidTag);
 
    theAmpTable->Write(ampNm,TObject::kOverwrite | TObject::kSingleKey);
    theCenterTable->Write(ctrNm,TObject::kOverwrite | TObject::kSingleKey);
    theSigmaTable->Write(sigmaNm,TObject::kOverwrite | TObject::kSingleKey);

     theBBPreParTable->Write("BBPrePar",TObject::kOverwrite | TObject::kSingleKey);
     theBBTurnOverTable->Write("BBTurnOver",TObject::kOverwrite | TObject::kSingleKey);
     theBBOffSetTable->Write("BBOffSet",TObject::kOverwrite | TObject::kSingleKey);
     theBBScaleTable->Write("BBScale",TObject::kOverwrite | TObject::kSingleKey); 
     theBBSaturateTable->Write("BBSaturate",TObject::kOverwrite | TObject::kSingleKey); 
    
    
    outFile.Write();    
 
    if (theAmpTable)      delete theAmpTable;
    if (theCenterTable)   delete theCenterTable;
    if (theSigmaTable)    delete theSigmaTable;

    if (theBBScaleTable)  delete theBBScaleTable;
    if (theBBOffSetTable) delete theBBOffSetTable;
 
    if (ampNm)            delete ampNm;    
    if (ctrNm)            delete ctrNm;
    if (sigmaNm)          delete sigmaNm;

   outFile.Close();
}






























//submit.pl event /star/data12/reco/ProductionMinBias/ReversedFullField/P01gl/2001/269/ /star/data10/GC/aihong/P01glPIDFlowPicoDST/
