enum dataFormat {dst, evt, tree};

// NOTE - chain needs to be declared global so for StHbtEventReader
class StChain;
StChain *chain=0;    

// File-scope stuff needed by setFiles, nextFile. Someone ambitious
// can clean this up by putting it all into a nice clean class.
Int_t usePath = 0;
Int_t nFile = 0;
TString  thePath;
TString  theFileName;
TString  originalPath;
//class StChain;
//StChain *chain=0;

TBrowser *b=0;
const char *venusFile ="set*geant.root";
const char *venusPath ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tfs_4/";
const char *dstFile ="/disk00001/star/auau200/two_photon/starlight/twogam/year_1b/hadronic_on/tfs/ric0022_01_14552evts.dst.root";
const char *xdfFile ="/afs/rhic.bnl.gov/star/data/samples/psc0054_07_40evts_dst.xdf";
const char *mdcFile ="/disk00001/star/auau200/venus412/default/b0_3/year_1b/hadronic_on/tss/psc0081_07_40evts.root";
const char *geantFile ="/disk00000/star/auau200/hijing135/jetq_off/b0_3/year_1b/hadronic_on/tfsr/set0041_01_53evts.geant.root";
const char *fileList[] = {dstFile,xdfFile,mdcFile,0};

//==========================================================================================
void StHbtMuDst2TTreeDstMaker(
			      const Int_t nevents=999999, int maxFiles=2,
			      const Char_t *path="",
			      const Char_t *fileName="AuAu200.lis",
			      const Char_t *filter="st_physics_2302016:MuDst.root",
			      const Char_t *outFile="MuDst2TTree.root")
			      
{

// Dynamically link needed shared libs
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("St_Tables");
  gSystem->Load("StMagF");
  gSystem->Load("StUtilities");  // new addition 22jul99
  gSystem->Load("StTreeMaker");
  gSystem->Load("StIOMaker");
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StEvent");
  gSystem->Load("StEventMaker");
  gSystem->Load("StEventUtilities");
  gSystem->Load("StEmcUtil");
  gSystem->Load("St_emc_Maker");
  gSystem->Load("StMcEvent"); 
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("StMcAnalysisMaker");
  gSystem->Load("StFlowMaker");
  gSystem->Load("StFlowTagMaker");
  gSystem->Load("StFlowAnalysisMaker");
  gSystem->Load("StStrangeMuDstMaker");
  gSystem->Load("StMuDSTMaker");   
  gSystem->Load("StHbtMaker");   


cout << " loading done " << endl;

chain = new StChain("StChain"); 
chain->SetDebug();


// **********
// MuDstMaker
// **********
StMuDebug::setLevel(0);
StMuDbReader* dbReader = StMuDbReader::instance();
dbReader->addDb("P02g.db");
StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,path,fileName,filter,maxFiles);

// ***************
// read from MuDst
// ***************



// *********
// Hbt Maker
// *********
StHbtMaker* hbtMaker = new StHbtMaker("HBT","title");
cout << "StHbtMaker instantiated"<<endl;
cout << "StHbtMaker::Init - setting up Reader and Analyses..." << endl;
StHbtManager* TheManager = hbtMaker->HbtManager();

// ******
// Reader
// ******
StHbtMuDstMakerReader* Reader = new StHbtMuDstMakerReader(muDstMaker);
Reader->setTrackType(1);
TheManager->SetEventReader(Reader);


// ***** set up event cut *****
 mikesEventCut* EventCut = new mikesEventCut;
 EventCut->SetEventMult(0,100000);      // selected multiplicity range
 EventCut->SetVertZPos(-75.0,75.0);     // selected range of vertex z-position
// ***** set up all track cut *****
 franksTrackCut* AllTrackCut = new franksTrackCut;
 AllTrackCut->SetNSigmaElectron(-1000.0,1000.0);
 AllTrackCut->SetNSigmaPion(-1000.0,1000.0);  
 AllTrackCut->SetNSigmaKaon(-1000.0,1000.0);  
 AllTrackCut->SetNSigmaProton(-1000.0,1000.0);
 AllTrackCut->SetNHits(10,1000);           // range on number of TPC hits on the track
 AllTrackCut->SetP(0.0,50.0);               // range in P
 AllTrackCut->SetPt(0.0,50.0);              // range in Pt
 AllTrackCut->SetRapidity(-15,15);       // range in rapidity
 AllTrackCut->SetDCA(0.0,200.);              // range in Distance of Closest Approach to primary vertex
 AllTrackCut->SetCharge(0);                // no cut on charge
 AllTrackCut->SetMass(0.139);              // pion mass
// ***** set up lambda cut *****
 helensV0Cut* V0Cut = new helensV0Cut;  // use V0 particle cut object
 V0Cut->SetdcaV0daughters(0.,1.0);      // DCA between 2 tracks
 V0Cut->SetdcaV0ToPrimVertex(0.,1.);    // DCA between V0 and event vertex
 V0Cut->SetdecayLengthV0(3.0,5000.);    // Decay length from prim. vertex
 V0Cut->SettpcHitsPos(15,50);           // Number of TPC hits on + track
 V0Cut->SettpcHitsNeg(15,50);           // Number of TPC hits on - track
 V0Cut->SetdcaPosToPrimVertex(1.8,50.); // Min. value + track at intersect
 V0Cut->SetdcaNegToPrimVertex(3.5,50.); // Min. value - track at intersect
 V0Cut->SetPt(0.,10.0);                 // range in Pt
// ***** set up xi cut *****
 franksXiCut* XiCut = new franksXiCut;
 XiCut->SetdcaXidaughters(0.,1.0);      // DCA between 2 tracks
 XiCut->SetdcaXiToPrimVertex(0.,1.);    // DCA between V0 and event vertex
 XiCut->SetdecayLengthXi(3.0,5000.);    // Decay length from prim. vertex
 XiCut->SetPt(0.,10.0);                 // range in Pt
cout << "READER SET UP.... " << endl;


  //   set up a microDstWriter 
 StHbtTTreeReader* allWriter = new StHbtTTreeReader(1,0,"./",outFile,".hbtTTreeMuDst","");
 // StHbtBinaryReader* allWriter = new StHbtBinaryReader(ioMaker,dirName,"dummy",".microDst");
 TheManager->AddEventWriter(allWriter);
 allWriter->SetEventCut(EventCut);
 allWriter->SetTrackCut(AllTrackCut);
 allWriter->SetV0Cut(V0Cut);
 allWriter->SetXiCut(XiCut);
 cout << "WRITER SET UP.... " << endl;

 // ------------------ end of setting up hbt stuff ------------------ //

 chain->Init(); // This should call the Init() method in ALL makers
 chain->PrintInfo();
 for (Int_t iev=0;iev<nevents; iev++) {
   cout << "StHbtExample -- Working on eventNumber " << iev << endl;
   chain->Clear();
   int iret = chain->Make(iev); // This should call the Make() method in ALL makers    
   if (iret) {
     cout << "Bad return code!" << endl;
     break;
   }
 } // Event Loop
 chain->Finish(); // This should call the Finish() method in ALL makers

 cout << " End of Analysis " << endl;
}



