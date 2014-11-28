///////////////////////////////////////////////////////////////////////////////
//
// $Id: doEvents.C,v 1.1 2002/04/02 20:51:16 jklay Exp $
//
// Description: 
// Chain to read events from files or database into StEvent and analyze.
// what it does: reads .dst.root or .xdf files and then runs StEventMaker
//          to fill StEvent and StAnalysisMaker to show example of analysis
//
// Environment:
// Software developed for the STAR Detector at Brookhaven National Laboratory
//
// Ways to run:
// If you specify a path, all DST files below that path will be
// found, and the first 'nevents' events in the file set will be
// analyzed.
// The type of DST files searched for is taken from the 'file' parameter.
// If 'file ends in '.dst.root', ROOT DSTs are searched for.
// If 'file ends in '.xdf', XDF DSTs are searched for.
//
// If path begins with '-', 'file' will be taken to be a single file
// to be processed.
//
// example invocation:
// .x doEvents.C(10,"-","some_directory/some_dst_file.xdf")
//
// example ROOT file invocation:
// .x doEvents.C(10,"-","some_directory/some_dst_file.root")
//
// example multi-ROOT file invocation:
// .x doEvents.C(9999,"some_directory","*.dst.root")
//
//////////////////////////////////////////////////////////////////////////////
//
// Author List: Torre Wenaus, BNL  2/99
//              Victor Perevoztchikov
//  
//  inputs:
//      nevents = # events to process
//      path = a. directory you want files from
//             b. "-" to get just the one file you want
//      file = a. file names in directory (takes all files)
//             b. the 1 particular full file name (with directory) you want
//      qaflag = "off"  - doesn't do anything now
//      wrStEOut = flag to turn on=1, off=0 writing of output test.event.root
//                 file --- set to off by default 
//      
///////////////////////////////////////////////////////////////////////////////
Int_t debug=1;
Int_t doIt=1;	//Yes, Store HITS!
Int_t    usePath = 0;
Int_t    nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
class    StChain;
StChain  *chain=0;
class StEventDisplayMaker;
StEventDisplayMaker *dsMaker = 0;
TBrowser *b=0;

const char *dstFile = 0;
const char *xdfFile = 0;
const char *mdcFile = 0;
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

void Help()
{
    cout << "Usage: doEvents.C(nevents,\"-\",\"some_directory/some_dst_file.xdf\")" << endl;
    cout << "       doEvents.C(nevents,\"-\",\"some_directory/some_dst_file.root\")" << endl;
    cout << "       doEvents.C(nevents,\"some_directory\",\"*.dst.root\")" << endl;	
}


void doEvents(Int_t, const Char_t **, const Char_t *qaflag = "");

void doEvents(Int_t nevents=5, 
              const Char_t *path=
	      "links/P02gc.productionCentral.FullField.2001.313.dst/",
              const Char_t *file=
	      "st_physics_2313048_raw_0237.event.root",
	      const Char_t* outDir = "./",
              const Char_t *qaflag = "off", 
              const Int_t wrStEOut = 0);

// ------------------ Here is the actual method -----------------------------------------
void doEvents(Int_t nevents, const Char_t **fileList,const Char_t* outDir, 
	      const Char_t *qaflag, const Int_t wrStEOut)
{

  cout <<  endl << endl <<" doEvents -  input # events = " << nevents << endl;
  Int_t ilist=0;
  while(fileList[ilist]){ 
      cout << " doEvents -  input fileList = " << fileList[ilist] << endl;
      ilist++; 
    }
  cout << " doEvents -  input qaflag   = " << qaflag << endl;
  cout << " doEvents -  input wrStEOut = " << wrStEOut << endl << endl << endl;
 
    //
    // First load some shared libraries we need
    //

    gSystem->Load("St_base");
    gSystem->Load("StChain");
   //    gSystem->Load("St_Tables"); 

    gSystem->Load("libgen_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libglobal_Tables");
 
    gSystem->Load("StUtilities");
    gSystem->Load("StarClassLibrary");

    gSystem->Load("StTpcDb");
    gSystem->Load("StEvent");
    gSystem->Load("StEventMaker");

    gSystem->Load("StIOMaker");
    gSystem->Load("StHiMicroEvent");
    gSystem->Load("StHiMicroMaker");

    // create a new instance of the chain
    chain = new StChain("StChain"); 
    chain->SetDebug();

    StFileI *setFiles = new StFile(fileList);

    TString mainBranch;
    if(fileList && fileList[0] && strstr(fileList[0],".root")){
      mainBranch = fileList[0];
      mainBranch.ReplaceAll(".root","");
      int idot = strrchr((char*)mainBranch,'.') - mainBranch.Data();
      mainBranch.Replace(0,idot+1,"");
      mainBranch+="Branch";
    }


    StIOMaker* ioMaker = new StIOMaker("IO","r",setFiles,"bfcTree");
    cout << "\t created" << endl;
    ioMaker->SetDebug();
    ioMaker->SetIOMode("r");
    ioMaker->SetBranch("*",0,"0");            //deactivate all branches
    //    ioMaker->SetBranch("geantBranch",0,"r");  //activate geant Branch
    //    ioMaker->SetBranch("dstBranch",0,"r");    //activate Event Branch
    //  ioMaker->SetBranch("runcoBranch",0,"r");  //activate runcoBranch
    if(!mainBranch.IsNull()) ioMaker->SetBranch(mainBranch,0,"r");

    if(mainBranch=="dstBranch"){
      cout << "Creating event.root" << endl;
      StEventMaker*       eventReader   = new StEventMaker("events","title");
      eventReader->doPrintEventInfo = 0;
    }

    //
    StHiMicroMaker* hiPt = new StHiMicroMaker;
    hiPt->setOutDir(outDir);
    hiPt->setDebug(debug);    
    hiPt->setHitLoop(doIt);
    

    //
    // Initialize chain
    //

    Int_t iInit = chain->Init();
    if (iInit) chain->Fatal(iInit,"on init");
    chain->PrintInfo();


    //
    // Event loop
    //
    int istat=0,i=1;
 EventLoop: if (i <= nevents && istat!=2) {

     cout << endl << "============================ Event " << i
	  << " start ============================" << endl;

     chain->Clear();
     istat = chain->Make(i);

     if (istat==2) 
         {cout << "Last  event processed. Status = " << istat << endl;}
     if (istat==3) 
         {cout << "Error event processed. Status = " << istat << endl;}


//------------------

     i++;
     goto EventLoop;
 }

    i--;
    cout << endl << "============================ Event " << i
	 << " finish ============================" << endl;

}

//--------------------------------------------------------------------------

void doEvents(const Int_t nevents, const Char_t *path, const Char_t *file,
	      const Char_t *outDir,
              const Char_t *qaflag, const Int_t wrStEOut)
{
    if (nevents==-1) { Help(); return;}
    const char *fileListQQ[]={0,0};
    if (strncmp(path,"GC",2)==0) {
      fileListQQ=0;
    } else if (path[0]=='-') {
	fileListQQ[0]=file;
    } else if (!file[0]) {
	fileListQQ[0]=path;
    } else {
	fileListQQ[0] = gSystem->ConcatFileName(path,file);
    }
    doEvents(nevents,fileListQQ,outDir,qaflag,wrStEOut);
}

