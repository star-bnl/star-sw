//=========================================================
// owner:  Gene Van Buren, BNL
// what it does:  read and list StrangeCuts in a micro DST
//=========================================================

TFile* filePtr;
TTree* treePtr;
TClonesArray* cutsArray;
TOrdCollection* cutsColl;

void listStrangeCuts(const char* fileName="evMuDst.root");

void listStrangeCuts(const char* fileName) {
  filePtr = new TFile(fileName);
  if (!filePtr) return;
  cout << "_______________________________________________" << endl;
  cout << "Opened file: " << fileName << endl;
  cout << "_______________________________________________" << endl;

  cutsColl = (TOrdCollection*) filePtr->Get("StrangeCuts");
  if (cutsColl) {
    cout << "Found cuts collection with " << cutsColl->GetSize()
         << " cuts." << endl;
    cout << "_______________________________________________" << endl;
    cutsColl->Print();
    cout << "_______________________________________________" << endl;
    return;
  }

  treePtr = (TTree*) filePtr->Get("StrangeMuDst");
  if (!treePtr) treePtr = (TTree*) filePtr->Get("MuDst");
  if (!treePtr) return;
  if (!(treePtr->GetBranch("StrangeCuts"))) return;

  cutArray = new TClonesArray("TCut",0);
  treePtr->SetBranchStatus("*",0);
  treePtr->SetBranchStatus("StrangeCuts.*",1);
  treePtr->SetBranchAddress("StrangeCuts",&cutArray);
  int ncuts=0;
  for (int i=0; i<treePtr->GetEntries(); i++) {
    treePtr->GetEvent(i);
    ncuts = cutArray->GetEntriesFast();
    if (ncuts) {
      cout << "Found cuts branch with " << ncuts << " cuts." << endl;
      cout << "_______________________________________________" << endl;
      cutArray->Print();
      break;
    }
  }
  if (!ncuts) cout << "Found cuts branch, but NO entries!!!" << endl;
  cout << "_______________________________________________" << endl;
}

//_____________________________________________________________
// $Id: listStrangeCuts.C,v 3.3 2002/05/23 02:58:36 genevb Exp $
// $Log: listStrangeCuts.C,v $
// Revision 3.3  2002/05/23 02:58:36  genevb
// Better search for cuts
//
// Revision 3.2  2002/05/10 20:59:31  genevb
// Fixed bug with branch status and changed cuts split level
//
// Revision 3.1  2002/04/30 01:29:17  genevb
// Updated macros for common micro DST
//
//
