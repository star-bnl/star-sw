///////////////////////////////////////////////////////////////////////////////
//
// $Id: DoEqualDaq.C,v 1.2 2007/04/10 11:36:24 ogrebeny Exp $
// Author: Thomas Ullrich, Oct 2000
//
//////////////////////////////////////////////////////////////////////////////
//
// Description: 
//
//////////////////////////////////////////////////////////////////////////////
//
// $Log: DoEqualDaq.C,v $
// Revision 1.2  2007/04/10 11:36:24  ogrebeny
// Bug fixes
//
// Revision 3.1  2000/10/13 19:23:46  ullrich
// Initial Revision
//
///////////////////////////////////////////////////////////////////////////////
#include <string>
#include <typeinfo>
class    StChain;
StChain  *chain=0;

void DoEqualDaq(char* file="daq/*.daq",int nevents = 500000,char* name = "equal1.root")
{

    //
    // First load some shared libraries we need
    //
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
    gSystem->Load("StMagF");
    gSystem->Load("libgeometry_Tables");
    gSystem->Load("St_Tables");
    gSystem->Load("StEmcUtil");
  
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StDbUtilities");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker");
    gSystem->Load("St_db_Maker");
    gSystem->Load("libgeometry_Tables");
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StEpcMaker");
		     
    gSystem->Load("StEmcCalibrationMaker");


    chain=new StChain("StChain"); 
    chain->SetDebug();
    
    //
    //  Setup IO maker
    //
    StIOMaker *IOMk = new StIOMaker();
    IOMk->SetFile(file);
    IOMk->SetIOMode("r");
    IOMk->SetBranch("*",0,"0");                 //deactivate all branches
    IOMk->SetBranch("daqBranch",0,"r");
    IOMk->SetDebug();
    
    St_db_Maker *db = new St_db_Maker("StarDb","MySQL:StarDb");

    m = new StEmcADCtoEMaker();
    m->saveAllStEvent(kTRUE);
    m->setPrint(kFALSE);

    // This process the EMC event and fill the tables
    // necessary for calibration
    StEmcCalibrationMaker *cal = new StEmcCalibrationMaker();
    char outfile[300];

    // does equalization
    sprintf(outfile,"/home/emc/online/emc/pedestal/equal/%s",name);
    StEmcEqualMaker *equal = new StEmcEqualMaker();
    equal->setFile(outfile);

    //
    // Initialize chain
    //

    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    chain->PrintInfo();
    
    int istat=0,i=1;
    
    
     EventLoop: if (i <= nevents && istat!=2) 
     {
       
       cout << endl << "============================ Event " << i<< " start ============================" << endl;
       
       chain->Clear();
       istat = chain->Make();
       
       if (istat==2)  {cout << "Last  event processed. Status = " << istat << endl;}
       if (istat==3)  {cout << "Error event processed. Status = " << istat << endl;}
       i++;
       goto EventLoop;
     }
    
    i--;
    cout << endl << "============================ Event " << i << " finish ============================" << endl;
    
}
