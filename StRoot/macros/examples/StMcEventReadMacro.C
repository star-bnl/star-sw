// $Id: StMcEventReadMacro.C,v 1.10 2000/01/12 20:29:15 calderon Exp $
// $Log: StMcEventReadMacro.C,v $
// Revision 1.10  2000/01/12 20:29:15  calderon
// Changed default file to the one produced weekly by Lidia in
// /star/rcf/test/dev/tfs_Linux/Tue/year_2a/hc_standard/
//
// Revision 1.9  1999/12/14 18:18:01  calderon
// using new StMcEvent, StEvent & StAssociationMaker
//
// Revision 1.8  1999/12/03 01:01:33  calderon
// Updated for new StMcEvent 2.0 and StMcEventMaker.
// Uses StTpcDb to get the geometry info (calib has some problems still).
//
// Revision 1.7  1999/11/03 22:47:33  calderon
// Changed default file.  Previous one no longer existed.
//
// Revision 1.6  1999/07/28 21:29:34  calderon
// Modified event loop: use 'if' and 'goto' to avoid using 'for'
//
// Revision 1.5  1999/07/28 20:27:46  calderon
// Version with SL99f libraries
//
// Revision 1.4  1999/07/23 19:57:26  calderon
// Load StarClassLibrary before loading StMcEvent
//
// Revision 1.3  1999/07/23 14:35:43  calderon
// Updated names of default files and of packages
//
// Revision 1.2  1999/07/23 10:53:52  kathy
// put in header info in Manuel's macros
//
//
//////////////////////////////////////////////////////////////////////
// owner: Manuel Calderon de la Barca Sanchez
//
// what it does: reads .geant.root file, 
//               loads StMcEvent by putting StMcEventMaker in chain
//
// note: for more info on StMcEvent and StAssociationMaker, do a 
//      cvs checkout and say "make" in the doc/tex directory - you'll
//      get a ps file with user guide and reference manual.
//////////////////////////////////////////////////////////////////////
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
const char *MainFile="/star/rcf/test/dev/tfs_Linux/Tue/year_2a/hc_standard/*.geant.root")
{
// Load all the System libraries
    gSystem->Load("St_base");
    gSystem->Load("StChain");
    gSystem->Load("St_Tables");
    gSystem->Load("StUtilities");

    gSystem->Load("StIOMaker");
    gSystem->Load("StarClassLibrary");
    
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    
    //	TOP maker
    chain = new StChain("StMcEventMainChain"); 
    chain->SetDebug();

    // IO Maker
    StIOMaker *IOMk = new StIOMaker("IO","r",MainFile,"bfcTree");
    IOMk->SetDebug();
    IOMk->SetIOMode("r");
    IOMk->SetBranch("*",0,"0");                 //deactivate all branches
    IOMk->SetBranch("geantBranch",0,"r");

    // StMcEvent
    StMcEventMaker  *mcEventReader  = new StMcEventMaker; // Make an instance...

    // now execute the chain member functions
    
    chain->PrintInfo();
    chain->Init(); // This should call the Init() method in ALL makers

    int istat=0,iev=1;
 EventLoop: if (iev<=nevents && !istat) {
     chain->Clear();
     cout << "---------------------- Processing Event : " << iev << endl;
     istat = chain->Make(iev); // This should call the Make() method in ALL makers
     if (istat) {
	 cout << "Last Event Processed. Status = " << istat << endl;
     }
     iev++; goto EventLoop;
     
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
    //chain->Finish(); // This should call the Finish() method in ALL makers
                   // Comment this line out if you want to access the information
                   // at the command line.
}
