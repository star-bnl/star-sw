//  StMcEventReadMacro.C
//  StMcEventReadMacro.C
//
// 
// 
//
//======================================================================
class StChain;
class St_DataSet;
St_DataSet *Event;
StChain *chain;
TBrowser *brow=0;


// The acual file to be used is passed as an argument to the macro, or a default can be set



void StMcEventReadMacro(Int_t nevents=1,
     const char *MainFile="/disk00000/star/test/new/tfs_Solaris/year_2a/psc0210_01_40evts.geant.root")
    // /disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tfsr/psc0030_02_40evts.geant.root
    // /star/u2b/lisa/workdir/gtrack.PetersBranch.root
    // /disk00000/star/test/new/tfs_Solaris/year_2a/psc0210_01_40evts.geant.root

{
// Load all the System libraries
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    //gSystem->Load("StTreeMaker");
    gSystem->Load("StIOMaker");

    
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventReaderMaker"); // This is where I load my own maker


    gSystem->Load("StarClassLibrary");
    gSystem->Load("StRootEvent");
    
//	TOP maker
    chain = new StChain("StMcEventMainChain"); 
    chain->SetDebug();
   
//		Input Tree
//   StTreeMaker *treeMk = new StTreeMaker("StMcEventTree",MainFile); //The MainFile above is passed to the tree maker
//   treeMk->SetIOMode("r");
//   treeMk->SetDebug();
//   treeMk->SetBranch("*",0,"0");                 //deactivate all branches
//   treeMk->SetBranch("geantBranch",0,"r"); //activate EventBranch
  //treeMk->SetBranch("PetersBranch",0,"r");

    StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
    IOMk->SetDebug();

    
  
  // add makers to chain here:
  
  StMcEventReaderMaker  *mcEventReader  = new StMcEventReaderMaker; // Make an instance...

  // now execute the chain member functions
  
  chain->Init(); // This should call the Init() method in ALL makers
 

  for (int iev=0;iev<nevents; iev++) {
    chain->Clear();
    int iret = chain->Make(); // This should call the Make() method in ALL makers
    if (iret) break;
    
    // this next part is just for doing the browser:
    //create browser with name=BName,title=Btitle
    
//     Event = chain->GetDataSet("geant");
//     Event->ls(9);
//     brow = new TBrowser("BName","BTitle");    
    
//     // To view tables in Ntuple format
//     St_DataSetIter geantDstI(Event);
//     St_g2t_vertex  *g2t_vertexTablePointer  =  (St_g2t_vertex *)  geantDstI("g2t_vertex");
//     St_g2t_track   *g2t_trackTablePointer   =  (St_g2t_track *)   geantDstI("g2t_track");
//     St_g2t_tpc_hit *g2t_tpc_hitTablePointer =  (St_g2t_tpc_hit *) geantDstI("g2t_tpc_hit");
//     St_g2t_svt_hit *g2t_svt_hitTablePointer =  (St_g2t_svt_hit *) geantDstI("g2t_svt_hit");
//     St_g2t_ftp_hit *g2t_ftp_hitTablePointer =  (St_g2t_ftp_hit *) geantDstI("g2t_ftp_hit");
    
//     gSystem->Load("xdf2root"); // Needed for some reason
    
//     St_TableNtuple vertexNtuple(*g2t_vertexTablePointer);
//     St_TableNtuple trackNtuple(*g2t_trackTablePointer);
//     St_TableNtuple tpc_hitNtuple(*g2t_tpc_hitTablePointer);
//     St_TableNtuple svt_hitNtuple(*g2t_svt_hitTablePointer);
//     St_TableNtuple ftp_hitNtuple(*g2t_ftp_hitTablePointer);
    
//     vertexNtuple.Fill(*g2t_vertexTablePointer);
//     trackNtuple.Fill(*g2t_trackTablePointer);
//     tpc_hitNtuple.Fill(*g2t_tpc_hitTablePointer);
//     svt_hitNtuple.Fill(*g2t_svt_hitTablePointer);
//     ftp_hitNtuple.Fill(*g2t_ftp_hitTablePointer);
    
    
  } // Event Loop
  chain->Finish(); // This should call the Finish() method in ALL makers
}
