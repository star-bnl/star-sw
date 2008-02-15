//
// LoadLaserDriftVelocityToDb.C
// 
// Modified 31 Jan 2008 by G. Van Buren
// Account for case of single macro
// output of LoopOverLaserTrees.C
//

#include <iostream>
#include "TH1.h"
#include "TH1F.h"
#include "TH2F.h"
#include "TH3F.h"
//#include "TPad.h"
//#include "TVirtualPad.h"
//#include "TCanvas.h"
//#include "TText.h"
#include "TF1.h"
#include "TNtuple.h"
#include "TFile.h"
#include "TGraph.h"
#include "TGraphErrors.h"
#include "TSystem.h"
#include "TStyle.h"
#include "TMath.h"
#include "TProfile.h"
//#include "TPostScript.h"
//#include "TPaveLabel.h"
//#include "tpcDriftVelocity.h"
//#include "tables/St_tpcDriftVelocity_Table.h"
//#include "tpcDriftVelocity_st.h"
//#include "StDbManager.hh"
//#include "StDbLib/StDbTable.h"

int getDVInRun(int nInRun, int *runMacro, unsigned int *runTime, double *runDVE){

  const double maxRMS = 0.001;
  const int minFilesInRun=5;
  
  // Remove last time entry as it is usually bad, few entries
  // Will order by DVE value
  int nToUseInRun = nInRun-1;
  int iFirstToUseInRun=0;
  int *runDVEOrderI = new int[nToUseInRun];
  double *runDVEOrderV = new double[nToUseInRun];

  for(int iInRun=0; iInRun<nToUseInRun; iInRun++){
    runDVEOrderV[iInRun]=runDVE[iInRun];
    runDVEOrderI[iInRun]=iInRun;
  }
  
  // Ordering by DVE value
  bool bOrder = kFALSE;
  while(!bOrder){
    bOrder = kTRUE;
    for(int iInRun=0; iInRun<nToUseInRun-1; iInRun++){
      if(runDVEOrderV[iInRun]>runDVEOrderV[iInRun+1]){
	int tempMinI = runDVEOrderI[iInRun+1];
	double tempMinV = runDVEOrderV[iInRun+1];
	runDVEOrderI[iInRun+1]=runDVEOrderI[iInRun];
	runDVEOrderI[iInRun]=tempMinI;
	runDVEOrderV[iInRun+1]=runDVEOrderV[iInRun];
	runDVEOrderV[iInRun]=tempMinV;
	bOrder=kFALSE;
      }
    }
  }
  
  for(int iInRun=0; iInRun<nInRun; iInRun++){
    cout << "In run: " << iInRun << "/" << nInRun 
	 << " " << runMacro[iInRun]  
	 << " " << runTime[iInRun]  
	 << " " << runDVE[iInRun] << endl;
  }

  for(int iInRun=0; iInRun<nToUseInRun; iInRun++){
    cout << "Order: " << iInRun << "/" << nToUseInRun 
	 << " " << runDVEOrderV[iInRun] 
	 << " " << runDVEOrderI[iInRun] << endl;
  }

  int nInOrderU = 0;
  int nInOrderD = 0;
  for(int iInRun=0; iInRun<nToUseInRun; iInRun++){
    if(runDVEOrderI[iInRun]==iInRun){
      nInOrderU++;
    }
    if(runDVEOrderI[iInRun]==nToUseInRun-iInRun-1){
      nInOrderD++;
    }
  }

  if(nToUseInRun>=minFilesInRun-1){
    if(nInOrderU==nToUseInRun || nInOrderD==nToUseInRun){
      double *meanDiff = new double[nToUseInRun-2];
      if(nInOrderU==nToUseInRun){
	cout << "Drift Velocities East are ordered (U) in time. Check convergence ..." << endl;
	for(int i=0; i<nToUseInRun-2; i++){
	  meanDiff[i]=0.;
	  for(int j=i; j<nToUseInRun-1; j++){
	    meanDiff[i]+=(runDVEOrderV[j+1]-runDVEOrderV[j]);
	  }
	  meanDiff[i]/=(nToUseInRun-i-1);
	}
      }
      if(nInOrderD==nToUseInRun){
	cout << "Drift Velocities East are ordered (D) in time. Check convergence ..." << endl;
	for(int i=0; i<nToUseInRun-2; i++){
	  meanDiff[i]=0.;
	  for(int j=nToUseInRun-i-1; j>0; j--){
	    meanDiff[i]+=runDVEOrderV[j-1]-runDVEOrderV[j];
	  }
	  meanDiff[i]/=(nToUseInRun-i-1);
	}
      }
      int nDiffInOrder = 0;
      for(int i=0; i<nToUseInRun-3; i++){
	if(meanDiff[i]>meanDiff[i+1]){
	  nDiffInOrder++;
	}
      }
      if(nDiffInOrder>=nToUseInRun-3-1){
	if(nInOrderU==nToUseInRun) iFirstToUseInRun++;
	if(nInOrderD==nToUseInRun) nToUseInRun--;
	cout << "Convergence found! Will remove earliest entry and continue." << endl;
      }
      else {
	cout << "No convergence found! This run should be checked. No macro will be kept!" << endl;
	return -1;
      }
    }
  }

  int tryThreeRMS = 0;
  int iNotValidRMS1 = -1;
  int iNotValidRMS2 = -1;

  if(nToUseInRun==minFilesInRun-2){
    tryThreeRMS=2; // Only one chance
  }

  while(tryThreeRMS<3){
    double runMean = 0.;
    double runRMS = 0.;
    int nRMS = 0; 
    
    for(int iInRun=iFirstToUseInRun; iInRun<nToUseInRun; iInRun++){
      if(iInRun!=iNotValidRMS1 && iInRun!=iNotValidRMS2){
	runMean+=runDVEOrderV[iInRun];
	runRMS+=runDVEOrderV[iInRun]*runDVEOrderV[iInRun];
	nRMS++;
      }
    }
    runMean/=nRMS;
    runRMS/=nRMS;
    runRMS-=runMean*runMean;
    runRMS=TMath::Sqrt(runRMS);

    cout << "This run RMS = " << runRMS << endl;

    int tryTwiceLow = 0;
    int iNotValidLow = -1;
    
    if(runRMS<maxRMS){      
      tryTwiceLow = 0;
      iNotValidLow = -1;
      while(tryTwiceLow<2){	
	int iMeanOrder = 0;
	double diffToMean = 100.;
	for(int iInRun=iFirstToUseInRun; iInRun<nToUseInRun; iInRun++){
	  double lDiffToMean = TMath::Abs(runDVEOrderV[iInRun]-runMean); 
	  if(lDiffToMean<diffToMean && iInRun!=iNotValidLow && 
	     iInRun!=iNotValidRMS1 && iInRun!=iNotValidRMS2){
	    diffToMean=lDiffToMean;
	    iMeanOrder=iInRun;
	  }
	}
	int lowThird = nInRun/3;
	if(runDVEOrderI[iMeanOrder]/lowThird!=0 || (tryTwiceLow==1 && tryThreeRMS==2)){
	  int iKeepMacro = runMacro[runDVEOrderI[iMeanOrder]];      
	  cout << "Keep Macro: " << iKeepMacro << " " << runTime[runDVEOrderI[iMeanOrder]]
	       << " " << runDVE[runDVEOrderI[iMeanOrder]] << endl;
	  cout << runDVEOrderI[iMeanOrder] << "/" << nInRun << " " << iMeanOrder << endl;
	  
	  return iKeepMacro;
	}
	 
	tryTwiceLow++;
	iNotValidLow=iMeanOrder;
	cout << "Drift Velocity East value closer to mean was found in the first third of the run, is safer to try next closest value." << endl;
	cout << "tryTwiceLow= " <<  tryTwiceLow << " iNotValidLow= " << iNotValidLow << endl;
      }
    }
    
    int iFarOrder = 0;
    double diffToMean = 0.;      
    for(int iInRun=iFirstToUseInRun; iInRun<nToUseInRun; iInRun++){
      double lDiffToMean = TMath::Abs(runDVEOrderV[iInRun]-runMean); 
      if(lDiffToMean>diffToMean && iInRun!=iNotValidRMS1 && iInRun!=iNotValidRMS2){
	diffToMean=lDiffToMean;
	iFarOrder=iInRun;
      }
    }
    tryThreeRMS++;
    if (tryThreeRMS<2){
      iNotValidRMS1=iFarOrder;
      if (tryTwiceLow==2) 
	cout << "Will remove Further most value and try again." << endl;
      else
	cout << "RMS of Drift Velocity East is too large! Will remove Further most value." << endl;
      cout << "RMS " << runRMS << " > " << maxRMS << endl;
      cout << "tryThreeRMS= " <<  tryThreeRMS << " iNotValidRMS1= " << iNotValidRMS1 << endl;
    }
    else{
      iNotValidRMS2=iFarOrder;
      if (tryTwiceLow==2) 
	cout << "Will remove Further most value and try again." << endl;
      else
	cout << "RMS of Drift Velocity East is too large! Will remove Further most value." << endl;
      cout << "RMS " << runRMS << " > " << maxRMS << endl;
      cout << "tryThreeRMS= " <<  tryThreeRMS << " iNotValidRMS2= " << iNotValidRMS2 << endl;
    }
  }

  cout << "Couldn't find a good macro to be kept for this run! Run should be checked!" << endl;
  return -1;

}

int getDriftVelocityDB(unsigned int funixTime, unsigned int& uitimedb, double& ldvedb ){

  char *fDbName = "Calibrations_tpc";
  char *fTableName = "tpcDriftVelocity";
  char *fFlavorName = "laserDV";     // flavor name, like 'ofl', 'sim'  
  char *fTimestamp = "2004-10-31 00:00:00";      // Timestamp of the data requested
  //  unsigned int funixTime;  // unix timestamp
  
  StDbManager* mgr = StDbManager::Instance();               // Get the singleton manager

  StDbConfigNode* configNode = mgr->initConfig(fDbName);  // Connect to the db & get an empty container

  StDbTable* dbtable;
  dbtable = configNode->addDbTable(fTableName);
  // Add a table to the container with descriptor given by Database with wsing version name as "default" 
  
  if (dbtable == 0){  // If table asigned by fTableName does not exist in Dababase
    cout << " No Table : " << fTableName << endl;   // program is stoped and exit from this function.
    return 0;
  }
  
  if (fFlavorName != 0){
    dbtable -> setFlavor(fFlavorName);
    cout << "Flavor is set as " << fFlavorName << " by StDbTable::setFlavor." << endl;
  }else{
    cout << "Flavor is NOT assigned. Default value is set as 'ofl'. " << endl;
    dbtable -> setFlavor("ofl");
  }
  
  if(funixTime) {
    mgr->setRequestTime(funixTime);
  } else {
    mgr->setRequestTime(fTimestamp);
  }
  
  mgr->fetchDbTable(dbtable);              // Fetch the data from Database    
  void* cstruct = dbtable->GetTableCpy();  // Get pointer of table and copy to c-structure
  Int_t   nrows = dbtable->GetNRows();     // Get number of raws in the table
  
  St_tpcDriftVelocity * tpcDV = 
    (St_tpcDriftVelocity*)TTable::New(fTableName,fTableName,cstruct,nrows);
  
  if(!(tpcDV && tpcDV->HasData())) {
    cout << "No TTable returned!" << endl;
    return 0;
  }
  tpcDriftVelocity_st *ltpcDVs = (tpcDriftVelocity_st*)tpcDV->At(0);
  
  uitimedb = dbtable->getBeginTime();
  ldvedb = ltpcDVs->laserDriftVelocityEast;	  	  
  
  return 1;
}


//void ReadLaserDVListAuto(const char* listOfMacros, int nMacros){
//void LoadLaserDriftVelocityToDb(const char* listOfMacros){
//void LoadLaserDriftVelocityToDb(){
//void LoadLaserDriftVelocityToDb(const char* dirName, const char* listOfMacros, int nMacros, const char* baseName){

void LoadLaserDriftVelocityToDb(const char* dirName, const char* listOfMacros, const char* baseName, int mode=0){
  // 
  // Define some used values.
  // 

  // mode = 0 is the method of taking several macros and selecting a best one
  // mode = 1 uses just one macro to upload to DB

  //  const char* listOfMacros = "listOfLaserMacrosForTest.list";
  //   const int nMacros = 109; 
  //  const char* baesseName = "tpcDriftVelocity";
  //  const char* dirName = "dataLaserDVTestAuto";
  const int nMacrosMax = 800;
  int nMacros = 0;    

  TString fullListName(dirName);
  fullListName +="/";
  fullListName +=listOfMacros;

  const char* tmpLines = "/tmp/nLines.txt";
  TString myCount("/usr/bin/wc -l ");
  myCount += fullListName; 
  myCount += " > "; 
  myCount += tmpLines;


  // Prepare to read from the DB
  //gSystem->Setenv("DB_ACCESS_MODE","read");

  cout << "Executing: " << myCount.Data() << endl;

  gSystem->Exec(myCount.Data());
  ifstream dvc(tmpLines);
  dvc >> nMacros;
  TString myRm("/bin/rm ");
  myRm += tmpLines;
  gSystem->Exec(myRm.Data()); 

  cout << "Number of macros in list: " << nMacros << endl;
  nMacros = TMath::Min(nMacros,nMacrosMax);
  cout << "Number of macros to read: " << nMacros << endl;

  const double maxdDVdt = 3*0.000002;

  // macro to add a bunch of data from a list of *.C files
  //
  //-> not sure which to load so load a bunch of shared libs 
  //   
  gSystem->Load("St_base");                          // Standard Libraries
  gSystem->Load("StUtilities");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("St_Tables");
  gSystem->Load("StDbLib");                          // DB Libraries
  
  // Create tables to store unix time and dv values
  double *xLaserDVE = new double[nMacros];
  double *yLaserDVE = new double[nMacros];
  
  double *xLaserDVW = new double[nMacros];
  double *yLaserDVW = new double[nMacros];
  
  int *macroStatus = new int[nMacros];

  char nameMacro[100];
  TString fullNameMacro;
  unsigned int uidatetime;
  
  St_tpcDriftVelocity *tpcDV;
  tpcDriftVelocity_st *tpcDVs;
  
  FILE *theList;
  theList = fopen(fullListName.Data(),"r");

  // To Get Unix Time from DateTime
  StDbManager* mgr=StDbManager::Instance();
  
  
  const int maxDeltaT=120;     // Time 2 separate 2 runs
  const int maxFilesInRun=30;  // Maximum number of files in run
  const int minFilesInRun=5;   // Minimum number of files in run 

  int nInRun=0;
  int nMacroToKeep=0;
  int nTryToKeep=0;
  unsigned int lastTime=0; 
  int *macroToKeep = new int[nMacros];
  int *runMacro = new int[maxFilesInRun];
  unsigned int *runTime = new unsigned int[maxFilesInRun];
  double *runDVE = new double[maxFilesInRun];

  // Loop over macros
  for(int iMacro=0; iMacro<nMacros; iMacro++){
    if (!feof(theList)){
      cout << "\n" << "iMacro = " << iMacro << endl;
      macroStatus[iMacro]=6; // Base status
      fscanf(theList,"%s",&nameMacro);
      if(strstr(nameMacro,baseName)){
	fullNameMacro = TString(dirName);
	fullNameMacro+="/";
	fullNameMacro+=nameMacro;
	printf("Reading macro: %s\n",fullNameMacro.Data());
	
	char* sdatetime=strstr(nameMacro,".");  sdatetime++;
	TString tsdatetime(sdatetime);
	tsdatetime = tsdatetime.ReplaceAll(".C","");
	tsdatetime = tsdatetime.ReplaceAll("."," ");
	tsdatetime = tsdatetime.Insert(4,"-");
	tsdatetime = tsdatetime.Insert(7,"-");
	tsdatetime = tsdatetime.Insert(13,":");
	tsdatetime = tsdatetime.Insert(16,":");
	sdatetime = tsdatetime.Data();

	mgr->setRequestTime(sdatetime);
	uidatetime = mgr->getUnixRequestTime();
	cout << " " << sdatetime << " " << uidatetime << endl;
	if (uidatetime == 0){
	  cout << " Macro error " << endl;
	  return;
	}

	// Load Macro to get dv value
	gROOT->LoadMacro(fullNameMacro.Data());
	tpcDV = (St_tpcDriftVelocity*)CreateTable();
	if(tpcDV && tpcDV->HasData()) {
	  cout << " Macro succesfully loaded" << endl;
	  
	  tpcDVs = (tpcDriftVelocity_st*)tpcDV->At(0);
	  
	  xLaserDVE[iMacro]=uidatetime;
	  yLaserDVE[iMacro]=tpcDVs->laserDriftVelocityEast;
	  
	  xLaserDVW[iMacro]=uidatetime;
	  yLaserDVW[iMacro]=tpcDVs->laserDriftVelocityWest;
	  
	  cout << " Laser Drift Velocity East: " <<tpcDVs->laserDriftVelocityEast << endl;
	  
	  //
	  // Good DV selection starts here
	  //

          if (mode == 1) {
            // take all macros
            macroToKeep[nMacroToKeep++] = iMacro;
            macroStatus[iMacro] = iMacro;
            continue;
          }

	  // Need to select all macros in one laser run
	  // First macro in run
	  if (nInRun==0){   lastTime = uidatetime;}

	  if ((uidatetime-lastTime)<maxDeltaT && nInRun<maxFilesInRun ){ 
	    // A macro of the current run
	    cout << " Delta time passed & not enough count " 
		 << nInRun << " <=> " << maxFilesInRun << " yet" << endl;
	    runMacro[nInRun]=iMacro;
	    runTime[nInRun]=uidatetime;
	    runDVE[nInRun]=tpcDVs->laserDriftVelocityEast;
	    
	    nInRun++;
	    lastTime = uidatetime;

	  } else if (nInRun>=minFilesInRun-1){ 
	    // Have all macros for run, check if enough
	    nTryToKeep++;
	    int keepIt = getDVInRun(nInRun,runMacro,runTime,runDVE);
	    if (keepIt!=-1) {
	      macroToKeep[nMacroToKeep++] = keepIt; // Keep the right macro
	      for (int iInRun=0; iInRun<nInRun; iInRun++){
		macroStatus[runMacro[iInRun]]=keepIt; // Set Status to Macro To Keep
	      }
	    }
	    else {
	      for (int iInRun=0; iInRun<nInRun; iInRun++){
		macroStatus[runMacro[iInRun]]=2; // No good macro to keep found
	      }
	    }
	    // Current macro is the start of next run
	    nInRun=0;
	    
	    runMacro[nInRun]=iMacro;
	    runTime[nInRun]=uidatetime;
	    runDVE[nInRun]=tpcDVs->laserDriftVelocityEast;
	    nInRun++;
	    lastTime = uidatetime;	  

	  } else { 
	    // not enough
	    cout << " Not Enough macros!" << nInRun << " " << minFilesInRun << endl;
	    for (int iInRun=0; iInRun<nInRun; iInRun++){
	      macroStatus[runMacro[iInRun]]=1; // Not Enough Macros in Run
	      runMacro[nInRun]=0;
	      runTime[nInRun]=0;
	      runDVE[nInRun]=0;
	    }
	    // Current macro is the start of next run
	    nInRun=0;
	    
	    runMacro[nInRun]=iMacro;
	    runTime[nInRun]=uidatetime;
	    runDVE[nInRun]=tpcDVs->laserDriftVelocityEast;
	    nInRun++;
	    lastTime = uidatetime;
	  }
	}	
      }
    }
  } // End loop over macros


  cout << "\n" << "Processing information" << endl;
  if (mode==0) {
  if (nInRun>=minFilesInRun-1 || mode==1){ // Have all macros for run, check if enough
    nTryToKeep++;
    int keepIt = mode || getDVInRun(nInRun,runMacro,runTime,runDVE);
    if (keepIt!=-1) {
      macroToKeep[nMacroToKeep++] = keepIt; // Keep the right macro
      for (int iInRun=0; iInRun<nInRun; iInRun++){
	macroStatus[runMacro[iInRun]]=keepIt; // Set Status to Macro To Keep
      }
    }
    else {
      for (int iInRun=0; iInRun<nInRun; iInRun++){
	macroStatus[runMacro[iInRun]]=2; // No good macro to keep found
      }
    }

  } else {
    cout << "We concluded we do not have Enough macros at this stage" << endl;
    for (int iInRun=0; iInRun<nInRun; iInRun++){
      macroStatus[runMacro[iInRun]]=1; // Not Enough Macros in Run
    }
  }

  cout << "Selected " << nMacroToKeep << " out of " << nTryToKeep 
       << " runs with enough macros." << endl;

  } // mode==0

  TString LoadDone(dirName);    LoadDone   +="/Load/Done/";
  TString LoadFailed(dirName);  LoadFailed +="/Load/Failed/";
  TString LoadOthers(dirName);  LoadOthers +="/Load/Others/";
  TString CheckBadRun(dirName); CheckBadRun+="/Check/BadRun/";
  TString CheckVarSel(dirName); CheckVarSel+="/Check/VarSel/";
  TString CheckVarOth(dirName); CheckVarOth+="/Check/VarOth/";

  // 
  // One macro per run have been selected. Check whether each of those make sense.
  // Ddv/Dt < Max   
  // Need to query db for closest dv value. 

  unsigned int uidatetimedb =0;
  double laseredvdb =0.;

  double dDVdt = 0.;
  double dDV =0.;
  double dt =0.;
  int iMacroToKeep = 0;

  int nMacroKept=0;
  int *macroKept = new int[nMacros];

  int nSuccess = 0;
  int nFailed = 0;

  if (nMacroToKeep>0){
    rewind(theList);
    for(int iMacro=0; iMacro<nMacros; iMacro++){
      cout << "iMacro = " << iMacro << endl;
      if (!feof(theList)){
	fscanf(theList,"%s",&nameMacro);
	cout << "macroToKeep[" << iMacroToKeep << "]=" << macroToKeep[iMacroToKeep] << endl;
	if (iMacro==macroToKeep[iMacroToKeep]){
	  if(strstr(nameMacro,baseName)){
	    fullNameMacro = TString(dirName);
	    fullNameMacro+="/";
	    fullNameMacro+=nameMacro;
	    printf("Reading macro: %s\n",fullNameMacro.Data());
	  
	    char* sdatetime=strstr(nameMacro,".");  sdatetime++;
	    TString tsdatetime(sdatetime);
	    tsdatetime = tsdatetime.ReplaceAll(".C","");
	    tsdatetime = tsdatetime.ReplaceAll("."," ");
	    tsdatetime = tsdatetime.Insert(4,"-");
	    tsdatetime = tsdatetime.Insert(7,"-");
	    tsdatetime = tsdatetime.Insert(13,":");
	    tsdatetime = tsdatetime.Insert(16,":");
	    sdatetime = tsdatetime.Data();
	    mgr->setRequestTime(sdatetime);
	    uidatetime = mgr->getUnixRequestTime();
	    cout << "sdatetime=" << sdatetime << " uidatetime=" << uidatetime << endl;
	
	    if(iMacroToKeep==0){
	      // Get previous DV entry in DB
	      getDriftVelocityDB(uidatetime,uidatetimedb,laseredvdb);
	    
	      dt = -(double) uidatetimedb;
	      dDV = -laseredvdb;	  	  
	    }
	  
	    // Load Macro to get dv value
	    gROOT->LoadMacro(fullNameMacro.Data());
	    tpcDV = (St_tpcDriftVelocity*)CreateTable();	
	    if(!(tpcDV && tpcDV->HasData())) {
	      cout << "No TTable returned!" << endl;
	      continue;
	    }	  
	    tpcDVs = (tpcDriftVelocity_st*)tpcDV->At(0);
	
	    // although in double, there is something funny here
	    cout << "dt=" << dt << " uidatetime=" << uidatetime << " -> " << (double) uidatetime << endl;
	    dt  += (double) uidatetime;
	    cout << "Now dt=" << dt << endl;


	    dDV += tpcDVs->laserDriftVelocityEast;

	    //dDVdt = TMath::Abs(dDV/dt);
	    if (dt==0){
	      if(dDV==0)  dDVdt = 0;      
	      else        dDVdt = maxdDVdt+1;
	    } else {
	      dDVdt = TMath::Abs(dDV/dt);
	    }

	    cout << "dDV/dt=" << dDV << "/" << dt << "=" << dDVdt << endl;
	
	    if(dDVdt<maxdDVdt){
	      macroKept[nMacroKept++] = iMacro;
	      int jMacro=iMacro;
	      while(macroStatus[++jMacro]==iMacro){
		macroStatus[jMacro]=3; // A macro in run was selected for db loading
	      }
	      jMacro=iMacro;
	      while(macroStatus[--jMacro]==iMacro){
		macroStatus[jMacro]=3; // A macro in run was selected for db loading
	      }
	      macroStatus[iMacro]=0;  // Macro  selected for db loading
	      // This macro is the new reference for dDV/dt
	      dt=-(double)uidatetime;
	      dDV=-tpcDVs->laserDriftVelocityEast;	  
	    }
	    else{
	      int jMacro=iMacro;
	      while(macroStatus[++jMacro]==iMacro){
		macroStatus[jMacro]=4; // Selected macro in run show large dDV/dt
	      }
	      jMacro=iMacro;
	      while(macroStatus[--jMacro]==iMacro){
		macroStatus[jMacro]=4; // Selected macro in run show large dDV/dt
	      }
	      macroStatus[iMacro]=5;   // Macro show large dDV/dt
	      // Keep prevoius reference for dDV/dt
	      dt-=(double)uidatetime;
	      dDV-=tpcDVs->laserDriftVelocityEast;
	    }
	    //	  gROOT->DeleteAll();
	    gInterpreter->ResetGlobals();
	    iMacroToKeep++;
	  }
	}
      }
    }
  
  // Moved before ddv/dt check
//   TString LoadDone(dirName);    LoadDone   +="/Load/Done/";
//   TString LoadFailed(dirName);  LoadFailed +="/Load/Failed/";
//   TString LoadOthers(dirName);  LoadOthers +="/Load/Others/";
//   TString CheckBadRun(dirName); CheckBadRun+="/Check/BadRun/";
//   TString CheckVarSel(dirName); CheckVarSel+="/Check/VarSel/";
//   TString CheckVarOth(dirName); CheckVarOth+="/Check/VarOth/";


    // Prepare to write to the DB
    //gSystem->Setenv("DB_ACCESS_MODE","write");

    // --> create a modifier object and set up table definitions  
    StDbModifier *dm = new StDbModifier();
    dm->SetDbName("Calibrations_tpc");
    dm->SetFlavor("laserDV");
    dm->SetTableName("tpcDriftVelocity");


    // One more Loop!
    rewind(theList);
    for(int iMacro=0; iMacro<nMacros; iMacro++){
      cout << "iMacro = " << iMacro << endl;
      if (!feof(theList)){
	fscanf(theList,"%s",&nameMacro);
	cout << "macroStatus[" << iMacro << "]=" << macroStatus[iMacro] << endl;
	if(strstr(nameMacro,baseName)){
	  fullNameMacro = TString(dirName);
	  fullNameMacro+="/";
	  fullNameMacro+=nameMacro;
	  TString newFile;
	  switch (macroStatus[iMacro]) {
	  case 0:
	    dm->SetInputFileName(fullNameMacro.Data());
	    char* sdatetime=strstr(nameMacro,".");  sdatetime++;
	    TString tsdatetime(sdatetime);
	    tsdatetime = tsdatetime.ReplaceAll(".C","");
	    tsdatetime = tsdatetime.ReplaceAll("."," ");
	    tsdatetime = tsdatetime.Insert(4,"-");
	    tsdatetime = tsdatetime.Insert(7,"-");
	    tsdatetime = tsdatetime.Insert(13,":");
	    tsdatetime = tsdatetime.Insert(16,":");
	    sdatetime = tsdatetime.Data();
	    dm->SetDateTime(sdatetime);
	    cout << "Loading " << fullNameMacro.Data() << " ... ";	  
	    if(dm->WriteDataToDB()==1){
	      cout << " Succeeded!" << endl;
	      newFile+=LoadDone;
	      nSuccess++;
	    }     
	    else {	
	      cout << " Failed!" << endl;
	      newFile+=LoadFailed;
	      nFailed++;
	    }
	    newFile+=nameMacro;
	    break;
	  case 1:
	    newFile = TString(dirName);
	    newFile+="/";
	    newFile+=nameMacro;
	    break;
	  case 2:
	    newFile+=CheckBadRun;
	    newFile+=nameMacro;
	    break;
	  case 3:
	    newFile+=LoadOthers;
	    newFile+=nameMacro;
	    break;
	  case 4:
	    newFile+=CheckVarOth;
	    newFile+=nameMacro;
	    break;
	  case 5:
	    newFile+=CheckVarSel;
	    newFile+=nameMacro;
	    break;
	  default:
	    newFile = TString(dirName);
	    newFile+="/";
	    newFile+=nameMacro;	  
	  }

	  // Move macro to appropriate directory
	  cout << newFile.Data() << endl;
	  if(rename(fullNameMacro.Data(),newFile.Data()))
	    cout << "Error moving file to " << newFile.Data() << endl;
	  FILE *FI;
	  if( (FI = fopen(fullNameMacro.Data(),"r")) != NULL){
	    cout << "File " << fullNameMacro.Data() << " remained at its original place " << endl;
	    fclose(FI);
	  }	
	}
      }
    }
  }

  double *xLaserDVEk = new double[nMacroKept];
  double *yLaserDVEk = new double[nMacroKept];
  
  double *xLaserDVWk = new double[nMacroKept];
  double *yLaserDVWk = new double[nMacroKept];

  for(int iMacro=0; iMacro<nMacroKept; iMacro++){
    xLaserDVEk[iMacro] = xLaserDVE[macroKept[iMacro]];
    yLaserDVEk[iMacro] = yLaserDVE[macroKept[iMacro]];
    xLaserDVWk[iMacro] = xLaserDVW[macroKept[iMacro]];
    yLaserDVWk[iMacro] = yLaserDVW[macroKept[iMacro]];
  }

  TGraph *tpcLaserDriftVelocityEast = new TGraph(nMacros,xLaserDVE,yLaserDVE);
  TGraph *tpcLaserDriftVelocityWest = new TGraph(nMacros,xLaserDVW,yLaserDVW);

  TGraph *tpcLaserDriftVelocityEastK = new TGraph(nMacroKept,xLaserDVEk,yLaserDVEk);
  TGraph *tpcLaserDriftVelocityWestK = new TGraph(nMacroKept,xLaserDVWk,yLaserDVWk);

  TString Load(dirName); Load+="/Load/";
  Load+="tpcLaserDV_Auto.root";
  TFile *hFile = new TFile(Load.Data(),"RECREATE");
  
  tpcLaserDriftVelocityEast->Write("tpcLaserDriftVelocityEast");
  tpcLaserDriftVelocityWest->Write("tpcLaserDriftVelocityWest");
  tpcLaserDriftVelocityEastK->Write("tpcLaserDriftVelocityEastK");
  tpcLaserDriftVelocityWestK->Write("tpcLaserDriftVelocityWestK");

  hFile->Close();

  TCanvas cvn1;
  cvn1.cd();

  TH1F *oneDHisto = new TH1F("oneDHisto","oneDHisto",100,xLaserDVE[0]-100,xLaserDVE[nMacros-1]+100);

  oneDHisto->SetMaximum(5.6);
  oneDHisto->SetMinimum(5.4);
  oneDHisto->SetTitle("TPC Laser Drift Velocity");
  oneDHisto->SetXTitle("Time (unix)");
  oneDHisto->SetYTitle("dv (cm/#mus)");
  oneDHisto->Draw();
  
  tpcLaserDriftVelocityWest->SetMarkerStyle(23);
  tpcLaserDriftVelocityWest->SetMarkerColor(2);
  //tpcLaserDriftVelocityWest->Draw("PL");
  
  tpcLaserDriftVelocityEast->SetMarkerStyle(22);
  tpcLaserDriftVelocityEast->SetMarkerColor(4);
  tpcLaserDriftVelocityEast->Draw("PL");
  
  tpcLaserDriftVelocityEastK->SetMarkerStyle(22);
  tpcLaserDriftVelocityEastK->Draw("PL");
  //  tpcLaserDriftVelocityWest->Draw("PL");

  cvn1.Update();
  Load.ReplaceAll(".root",".eps");
  cvn1.SaveAs(Load.Data());
  
  cout << endl << endl << " ******************************************** " << endl;
  cout << " Number of Macros attempted = " << nMacros << endl;
  cout << nSuccess << " loaded to DB and " << nFailed << " failed to be load!" << endl;
  
}
   
