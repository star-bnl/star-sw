void fitHisto(){


  for (int i=0; i<3; i++)// have to be consistent with StPidProbabilityConst.hh
    for (int j=0; j<2; j++)
      for (int k=0; k<2; k++) fit(i,j,k);


}


void fit(int i, int j, int k){ //mult, dca, charge
 
  gROOT->Reset();

 char* sigmaOfSigmTrialInputName = new char[80];
 sprintf(sigmaOfSigmTrialInputName,"PidHisto_%d00.root",i);

 char* sigmaOfSigmTrialOutputName = new char[80];
 sprintf(sigmaOfSigmTrialOutputName,"PidSigmaOfSingleTrail_%d%d%d_basedOn_%d00.txt",i,j,k,i);

 char* phaseSpaceCalibInputName = new char[80];
 sprintf(phaseSpaceCalibInputName,"./PidHisto_%d01.root",i);

 char* phaseSpaceCalibOutputName = new char[80];
 sprintf(phaseSpaceCalibOutputName, "./PhaseSpaceCalib%d%d%dButItisbasedOn_%d01.txt",i,j,k,i);

 char* gausFitInputName = new char[80];
 sprintf(gausFitInputName,"./PidHisto_%d%d%d.root",i,j,k);

 char* gausFitOutputName = new char[80];
  sprintf(gausFitOutputName,"./PidHistoFitted_%d%d%d.root",i,j,k);

 char* ampFitOutputName = new char[80];
  sprintf(ampFitOutputName,"./PidHistoAmp_%d%d%d.root",i,j,k);

  gSystem->Load("St_base");
  gSystem->Load("StChain");

  gSystem->Load("libgen_Tables");
  gSystem->Load("libsim_Tables");
  gSystem->Load("libglobal_Tables");

  gSystem->Load("StUtilities");
  gSystem->Load("StIOMaker");
  gSystem->Load("StTreeMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StEvent");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StMagF");

  gSystem->Load("StFlowMaker");

 gSystem->Load("PIDFitter");

  PIDFitter myfitter;



   myfitter.Process(   sigmaOfSigmTrialInputName,
                       sigmaOfSigmTrialOutputName, 
                       phaseSpaceCalibInputName,
                       phaseSpaceCalibOutputName, 
                       gausFitInputName,
                       gausFitOutputName,
                       ampFitOutputName );




  //    myfitter.Init();
  //    myfitter.GetSigmaOfSingleTrail(sigmaOfSigmTrialInputName,sigmaOfSigmTrialOutputName);
  //    myfitter.DoPhaseSpaceCalibration(phaseSpaceCalibInputName,phaseSpaceCalibOutputName);
  //    myfitter.FitMultiGaus(gausFitInputName,gausFitOutputName);
  //    myfitter.FitMultiGaus(gausFitInputName,gausFitOutputName);
  //    myfitter.ExtrapAmp(gausFitOutputName,ampFitOutputName);

}
