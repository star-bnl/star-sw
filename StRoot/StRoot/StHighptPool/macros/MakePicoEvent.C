void MakePicoEvent(const char* inFilePath="links/P02gc.central.FullField.2001.mdst/", const char* inFileName="st_physics_2323002_raw_0127.himicro.root")
{
// Example of Root macro to copy a subset of a Tree to a new Tree

   gROOT->Reset();
   gSystem->Load("StHiMicroEvent");

   //Get old file, old tree and set top branch address
   char pathAndFile[150];
   strcat(pathAndFile,inFilePath);
   strcat(pathAndFile,inFileName);
   cout << "PathAndFile = " << pathAndFile << endl;

   TFile *oldfile = new TFile(pathAndFile);

   TString outFileName(inFileName);
   TString replace = ".himicro.root";
   outFileName.ReplaceAll(replace.Data(),".hipico.root");
   cout << "OutfileName = " << outFileName.Data() << endl;
   
//Get the Tree from the File
   TTree *oldtree = (TTree*)oldfile->Get("StHiMicroTree");
   mHiMicroEvent = new StHiMicroEvent;
   oldtree->SetBranchAddress("StHiMicroEvent",&mHiMicroEvent);
   //oldtree->Print();

//Remove the hits branch from the Tree
   oldtree->SetBranchStatus("mHits",0);
   oldtree->SetBranchStatus("mNHit",0);

//Create a new file + a clone of old tree in new file
   TFile *newfile = new TFile(outFileName.Data(),"recreate");
   TTree *newtree = oldtree->CloneTree();

   //newtree->Print();
   newfile->Write();
   delete oldfile;
   delete newfile;
}
