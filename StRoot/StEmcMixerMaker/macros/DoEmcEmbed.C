////////////////////////////////////////////////////////////////////////////////////////////////////
/*!\file DoEmcEmbed
\author Alexandre Suaide

This macro does the EMC embedding using the output of TPC
embeddig in which the TPC tracks are already embedded but EMC
information is not.

*/
////////////////////////////////////////////////////////////////////////////////////////////////////

class StChain;
StChain *chain=0;

////////////////////////////////////////////////////////////////////////////////////////////////////
/*!\fn FileType
\author Alexandre Suaide
This function checks the type of file it was requested to be open

\param file is the file name to be checked

It returns the file type and it should be:
0 if file is not identified
1 if .event.root
2 if .geant.root
3 if .MuDst.root
4 if .emcEvent.root
*/
Int_t FileType(char *file)
{
  TString f=file;
  Int_t type =0;
  if(f.EndsWith("event.root")) type = 1;
  if(f.EndsWith("geant.root")) type = 2;
  if(f.EndsWith("MuDst.root")) type = 3;
  if(f.EndsWith("emcEvent.root")) type = 4;
  return type;
}
////////////////////////////////////////////////////////////////////////////////////////////////////
/*!\fn CreateExtraChain
\author Alexandre Suaide

This function creates an extra chain for the embedding. This is necessary because
there are commom makers to the various file type options

\param id is 0 if first event chain, 1 if second event chain
\param type is related to the file type
\mode is related to the recalibration mode (see function DoEmcEmbed()
*/
void CreateExtraChain(Int_t id,Int_t type,Int_t mode)
{
  if(id==0) 
  {
    St_db_Maker *dbMk1 = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb","$PWD/StarDb");       
    if(mode==0 || mode==1) StEmcADCtoEMaker *adc = new StEmcADCtoEMaker(); 
  }
  if(id==1)
  {
    StEmcPreMixerMaker *preMixer = new StEmcPreMixerMaker();
    if(type!=2) if(mode==1 || mode==2)
    {
      StEmcADCtoEMaker *adc1 = new StEmcADCtoEMaker("EReadEmbed");
      adc1->setEmbeddingMode(kTRUE);
    }
  }
}
////////////////////////////////////////////////////////////////////////////////////////////////////
/*!\fn CreateExtraChain
\author Alexandre Suaide

This function creates the part of the chain that takes care of the IO of the event

\param id is the chain id (0 for the first file and 1 for the second file)
\param file is the file name
\param mode is related to the recalibration mode (see function DoEmcEmbed()
\param type is the file type return

This function returns 0 if the chain is NOT created properly and 1 if everything is ok.
*/
Int_t CreateChain(Int_t id, char* file,Int_t mode,Int_t &type)
{
  type=FileType(file);
  if(type==0) return 0;
  TString name = "IO";
  if(id==1) name ="embedIO"; // second chain should have IO makers with this name always
  if(type==1 || type==2) // .event.root or .geant.root file
  {
    StIOMaker* ioMaker = new StIOMaker(name.Data());
    CreateExtraChain(id,type,mode);
    ioMaker->SetFile(file);
    ioMaker->SetIOMode("r"); 
    ioMaker->SetBranch("*",0,"0");           //deactivate all branches
    if(type==1)ioMaker->SetBranch("eventBranch",0,"r"); //activate geant Branch
    if(type==2 && id==1) 
    {
      ioMaker->SetBranch("geantBranch",0,"r");
      StMcEventMaker *mcEvent = new StMcEventMaker();
      StEmcSimulatorMaker *emcSim = new StEmcSimulatorMaker();
    }
    if(type==2 && id==0) return 0;
  }
  if(type==3) // commom mu DST
  {
    StMuDstMaker *muDst = new StMuDstMaker(0,0,file,"","MuDst.root",20);
    StMuDbReader *muDb = StMuDbReader::instance();
    StMuDst2StEventMaker *muToStEvent = new StMuDst2StEventMaker(name.Data(),name.Data()); 
    CreateExtraChain(id,type,mode);
  }
  if(type==4)
  {
    StEmcMicroDstMaker *microDst = new StEmcMicroDstMaker(name.Data());
    microDst->addMicroEventFile(file);
    CreateExtraChain(id,type,mode);
  }
  return 1;
}

////////////////////////////////////////////////////////////////////////////////////////////////////
/*!\fn DoEmcEmbed
\author Alexandre Suaide

This function runs the EMC embedding chain. Based on the extensions of the input files it creates
the apropiate chain to run the embedding. Any combination of file types can be used. Files can
be .event.root, .geant.root, .MuDst.root and .emcEvent.root. The only option that is not possible
to run is when file0 is a .geant.root.

\param nevents is the total number of events requested
\param file0 is the first input file. 
\param file1 is the second input file
\param mode is the run mode

mode can be set with the following values and it is related with the way the EMC hits are recalibrated
mode = 0 recalibrate only hits in the first event
mode = 1 recalibrate hits in both events
mode = 2 recalibrate hits only in the first event
mode = 3 do not recalibrate hits

mode = 3 is particualary interesting when embedding simulated events into simulated data. There is no
need to recalibrate the events in that case.

If one of the input files is a MuDst.root or emcEvent.root the hits on that file(s) should be recalibrate.
This is even more important on MuDst.root because only tower ADC's are saved in that file. There is no
calibrated energy information for towers in that file.

You also need to edit this function in order to load your analysis library and you also should instanciate
your analysis maker. Your analysis maker should come after StEmcAssociationMaker. 

Note that the EMC association maker will only work if the second file is a .geant.root because it is
necessary to have monte carlo information to do association.

*/
void DoEmcEmbed(Int_t nevents=1,char *file0="*.event.root", char *file1="*.geant.root",Int_t mode=0)
{
// Load needed shared libs
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("libglobal_Tables");
    gSystem->Load("libsim_Tables");
    gSystem->Load("libgen_Tables");
    gSystem->Load("libgeometry_Tables");
    gSystem->Load("geometry");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");
    gSystem->Load("StBFChain");    
    gSystem->Load("StIOMaker");
    gSystem->Load("StMagF");
    gSystem->Load("StarClassLibrary");
    gSystem->Load("StAnalysisUtilities");
    gSystem->Load("StEvent");
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StAssociationMaker");
    gSystem->Load("StDaqLib");
    gSystem->Load("StDbLib");
    gSystem->Load("StDbBroker"); 
    gSystem->Load("St_db_Maker"); 
    gSystem->Load("StEmcUtil");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StEpcMaker");
    gSystem->Load("StEmcSimulatorMaker");     
		gSystem->Load("StStrangeMuDstMaker");
    gSystem->Load("StMuDSTMaker");
    gSystem->Load("StEmcMixerMaker");
    
    //load your libraries here

// create chain    
    chain = new StChain("bfc");    

// create basic chain to open correct files
    Int_t type0,type1;
    Int_t st0=CreateChain(0,file0,mode,type0);
    Int_t st1=CreateChain(1,file1,mode,type1);
    if(st0==0 || st1==0) { cout <<"Error creating embedding chains ... Exiting\n"; return; }

// create mixer maker and EMC reconstruction chain
    StEmcMixerMaker *emb = new StEmcMixerMaker();
    StPreEclMaker *pre = new StPreEclMaker();
    StEpcMaker *epc = new StEpcMaker();
    
// create EMC association maker 
    if(type1==2) // .geant.root file is being used. Can create Association Makers ...
    {
      StAssociationMaker    *association = new StAssociationMaker();       // TPC association maker
      StEmcAssociationMaker *emcAssociation = new StEmcAssociationMaker(); // EMC association maker
    }
    
//Your analysis maker should go here 
    
// now execute the chain Init functions
    chain->PrintInfo();
    Int_t initStat = chain->Init(); 
    if (initStat) chain->Fatal(initStat, "during Init()");    
    int istat=0,iev=1;

// do the event loop    
    EventLoop: 
     if (iev<=nevents && istat!=2 && istat!=3 && istat!=4) 
     {
       chain->Clear();
       cout << "---------------------- Processing Event : " << iev << " ----------------------" << endl;
       istat = chain->Make(iev); // This should call the Make() method in ALL makers
       if (istat == 2) { cout << "Last  Event Processed. Status = " << istat << endl; }
       if (istat == 3) { cout << "Error Event Processed. Status = " << istat << endl; }
       if (istat == 4) { cout << "Fatal Event Processed. Status = " << istat << endl; }
       iev++; 
       goto EventLoop;
     } 
}

