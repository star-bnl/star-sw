/***************************************************************************
 *
 * $Id: BetheBlochCalibrator.C,v 1.2 2000/08/10 20:52:56 aihong Exp $
 *
 * Author:Aihong Tang           Kent State University
 *        Send questions to aihong@cnr.physics.kent.edu 
 ***************************************************************************
 *
 *            A macro that calibrate the bethe-bloch curve.
 *
 ***************************************************************************
 *
 * $Log: BetheBlochCalibrator.C,v $
 * Revision 1.2  2000/08/10 20:52:56  aihong
 * change directory of Ntuples
 *
 * Revision 1.1  2000/08/09 15:55:23  aihong
 * initial version
 *
 *
 **************************************************************************/

// Algorithm and Usage:

// Currently BetheBlochFunction takes 7 aprameters:
// { pars[0],pars[1], pars[2], pars[3](charge), pars[4](mass), pars[5],pars[6]}

// where "charge" and "mass" are charge and mass for a perticular particle type
// by setting both of them to 1 will give you dE/dx_beta*gamma instead of 
// dE/dx_rigidity(p/z)


// Those parameters are got from fitting dE/dx_beta*gamma from the combination
// of clean pieces on the dE/dx_P plot.

// After the dE/dx_beta*gamma fitting is done, the shape of the curve 
// almost settles down except for two effects, which appear slightly
// different from run to run and  need to be adjudged on individual data set.
// The two effects are: dE/dx uniform shift,  dE/dx squeeze or enlarge. 

// There are two parameters (pars[2] & pars[5]) describing (controlling) these 
// two effects. What this macro does is to adjudge these two parameters 
// for a perticular data set.

// A set of calibrated parameters will appear at the end of the run.




// User might modify the code to let it run for his/her own NTuple.
// Two blocks in the code need to be modified for their own NTuple,
// they  are marked by "******"



#include <strstream.h>
#include "/afs/rhic/star/packages/DEV00/StRoot/StEventUtilities/BetheBlochFunction.hh"


void BetheBlochCalibrator(){



    gROOT->Reset();
 
    gSystem->Load("St_base");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StEvent");
    gSystem->Load("StEventUtilities");

    const int NParameters=7;
    bool  monitorFitting=false;

    //***********************************************************************
    //**********change the name to the corresponding name in your NTuple.****
    char* dedxMean="m_mean70";
    char* NTpcHitsOnTrack="m_tpcpoints";
    char* momentum="m_pmag";
    //***********end of block
    //***********************************************************************



 double pars[NParameters]={1.072,0.3199,2.028e-07,1,1,2.69555e-7,5.0e-4};//for mean70
    //we need to adjudge pars[2] and pars[5].
    //pars[2] describe uniform dE/dx shift.
    //pars[5] describe dE/dx squeeze or enlarge.

   //double pars[NParameters]={1.072,0.3199,1.26349e-07,1,1,2.39688e-07,5.0e-4};//for mean55
    double theCalib=pars[5];


    double minimumIonizingPionStart=0.475;
    double minimumIonizingPionEnd  =0.525;

    double mimimumIonizingPionPosition
           =(minimumIonizingPionEnd+minimumIonizingPionStart)/2.;


    double protonTestStart = 0.375;
    double protonTestEnd   = 0.425;
    double protonTestPosition
           =(protonTestStart+protonTestEnd)/2.;


  TF1* pionPlusBandCenter 
     =new TF1("pionPlusBandCenter",BetheBlochFunction, 0.02,5, NParameters); 
    pionPlusBandCenter->SetParameters(&pars[0]);
    pionPlusBandCenter->SetParameter(4,0.13957); //pion mass


  TF1* protonBandCenter 
     =new TF1("protonBandCenter",BetheBlochFunction, 0.02,5, NParameters); 
    protonBandCenter->SetParameters(&pars[0]);
    protonBandCenter->SetParameter(4, 0.93827); //proton mass
    

double delta(double calib, double pionPosition, double protonPosition){

    pionPlusBandCenter->SetParameter(5,calib);
    protonBandCenter->SetParameter(5,calib);

    return (protonBandCenter->Eval(protonPosition,0,0)-
            pionPlusBandCenter->Eval(pionPosition,0,0));
}


double minimumIonizingdEdx(double calib, double pionPosition){
    pionPlusBandCenter->SetParameter(5,calib);
    
    return pionPlusBandCenter->Eval(pionPosition,0,0);
}
    
double look4MinDeltaDiff(double calibStart, double calibEnd, int calibSteps, double pionPosition, double protonPosition, double DeltaRef){

     double calibSeg=(calibEnd-calibStart)/double(calibSteps);
     double thisCalib=calibStart;
     double minDeltaDiffCalib=5000;//calib associated with minDeltaDiff.
     double minDeltaDiff=5000;

     do {

        double myDelta=delta(thisCalib,pionPosition,protonPosition);
        double diff=TMath::Abs(myDelta-DeltaRef);
        if (diff<minDeltaDiff) {
            minDeltaDiff=diff;
            minDeltaDiffCalib=thisCalib;
	}
          
        thisCalib=thisCalib+calibSeg;
     }while(thisCalib<calibEnd);

     return minDeltaDiffCalib;
}


//now get delta and minimumIonizingdEdx from data. 
//then use them as our reference to adjudge pars[2] and pars[5].



  
 // initialize  chain.


//*************************************************************************
//************** change the file name and branch name  as yours ***********
  TChain chain("DeDxTree");

  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1176003.root");
 
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1176004.root");
 
  
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1176013.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1176014.root");
  
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1176015.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1177001.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1177002.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1177006.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1177009.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1177010.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1177013.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1179007.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1179010.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1183003.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1183006.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1183008.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1183009.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1183011.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1183013.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1183019.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1183020.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1184000.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1184001.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1184002.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1184004.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1184026.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1185001.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1185002.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1185003.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1185004.root");
  chain.Add("/star/rcf/pwg/spectra/mmiller/DeDxNtuples/07_26_00/st_physics_1185005.root");
  
  //**********end of block************************************
  //**********************************************************



  


 TH1D* pionHisto= new TH1D("pionHisto","dE/dx histo. for minimum ionizing pions",100,0.05e-5,0.18e-5);

 double protonHistoCenter=protonBandCenter->Eval(protonTestPosition,0,0);
 double protonHistoHalfRange=0.24e-5;


 TH1D* protonHisto= new TH1D("protonHisto","dE/dx histo. for proton",100,(protonHistoCenter-protonHistoHalfRange),(protonHistoCenter+protonHistoHalfRange));




  //dump to pionHisto
    strstream pionMtmCutStr;

    pionMtmCutStr<<momentum<<"<"<<minimumIonizingPionEnd<<" && "<<momentum<<">"<<minimumIonizingPionStart;
    //m_pmag is momentum.

    TCut   pionMtmCut=pionMtmCutStr.str();

    strstream pionMeanCutStr;
    pionMeanCutStr<<dedxMean<<"<5.0e-5";

    TCut   pionMeanCut=pionMeanCutStr.str();    

    strstream tpcpointsCutStr;
    tpcpointsCutStr<<NTpcHitsOnTrack<<">15";

     
    TCut   tpcpointsCut=tpcpointsCutStr.str();


    TCut   totalPionCut=pionMtmCut+pionMeanCut+tpcpointsCut;

    strstream pionHistoDump;
    pionHistoDump<<dedxMean<<">>pionHisto";

    chain->Draw(pionHistoDump.str(),totalPionCut.GetTitle()); 


  //dump to protonHisto
   strstream protonMtmCutStr;

   protonMtmCutStr<<momentum<<"<"<<protonTestEnd<<" && "<<momentum<<">"<<protonTestStart;

   TCut   protonMtmCut=protonMtmCutStr.str();

   strstream protonMeanCutStr;
   protonMeanCutStr<<dedxMean<<"<5.0e-5";

   TCut protonMeanCut=protonMeanCutStr.str();  

   TCut   totalProtonCut=protonMtmCut+protonMeanCut+tpcpointsCut;
   strstream protonHistoDump;
   protonHistoDump<<dedxMean<<">>protonHisto";

   chain->Draw(protonHistoDump.str(),totalProtonCut.GetTitle()); 
  
      
  //fitting.
TF1* protonGaus= new TF1("protonGaus","gaus",(protonHistoCenter-protonHistoHalfRange*0.7),(protonHistoCenter+protonHistoHalfRange*0.8));
    protonGaus->SetParLimits(1,protonHistoCenter*0.7,protonHistoCenter*1.3);
    protonHisto->Fit("protonGaus","R");
    double protonReference=protonGaus->GetParameter(1);


    double pionPeakPosition
           =pionHisto->GetBinCenter(pionHisto->GetMaximumBin());
    double pionFitHalfRange=0.025e-5;
TF1* pionGaus=new TF1("pionGaus","gaus",(pionPeakPosition-pionFitHalfRange),(pionPeakPosition+pionFitHalfRange));
    pionHisto->Fit("pionGaus","R");
    double pionReference=pionGaus->GetParameter(1);


    if (monitorFitting){
  TCanvas* pionCanvas=new TCanvas("pion");
        pionCanvas->cd();
        pionHisto->Draw();
  TCanvas* protonCanvas=new TCanvas("proton");
        protonCanvas->cd();
        protonHisto->Draw();
    }


    double deltaReference=protonReference-pionReference;


    //now do the calibration...

 double calibResult=look4MinDeltaDiff(theCalib*0.7, theCalib*1.3, 100,mimimumIonizingPionPosition ,protonTestPosition , deltaReference);



 double pars[2] = pars[2]+minimumIonizingdEdx(calibResult,mimimumIonizingPionPosition )-pionReference;

 double pars[5]=calibResult;

 cout<<endl;
 cout<<"               *************************************"<<endl;
 cout<<"               *   the calibrated parameters are:  *"<<endl;
 cout<<"               *************************************"<<endl;
 cout<<endl;
 cout<<"        ";
 cout<<"{ ";
 for (int j=0; j<3; j++)
   cout<<pars[j]<<", ";
 cout<<"charge, mass, ";
 cout<<pars[5]<<", "<<pars[6]<<" }"<<endl<<endl;

}
 
