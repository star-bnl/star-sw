// copy content of the binary data file to TTree
//aaaa
#include <iostream.h>

class    EEmcMCData;

// ______________________________________________
void bin2TTree(int Nevents=1, const Char_t *binFile ="data.bin"){
  Int_t  i=0;
  gSystem->Load("EEmc.so"); 

  FILE *fd=0;
  fd=fopen(binFile,"r");
  assert(fd);
  
  //create a Tree file tree4.root
  TFile f("tree4.root","RECREATE");
  
  // Create a ROOT Tree
  TTree t4("t4","A Tree with Events");

  // Create a pointer to an Event object

  EEevent *eve=new EEevent(); 
  t4.Branch("EEDst", "EEevent", &eve,16000,99);
  //return ;
  
  for (i=1; i<=Nevents; i++ ) {
    const int mx=1<<14;
    char hitBuf[mx];
    int mxh=sizeof(int);
    int j,len;
    // get buf len
    char *cLen=&len;
    for(j=0;j<sizeof(int);j++) cLen[j]=fgetc(fd);
    cerr << "actual data len=" << len << " bytes " << endl;
    // get data body
    for(j=0;j<len;j++) hitBuf[j]=fgetc(fd);

   // ..............   do sth with data locally
    EEmcMCData   data;
    int nbw = data.read(hitBuf,len);
    data.print();

    // generation of TTree
    data.write(eve); // Clear & Store EEevent
    eve->print();
    t4.Fill();  // Fill the tree
    // t4.Print();   // Print the tree contents
  } // end of loop over events

 Int_t nevent = (Int_t)t4->GetEntries();
  printf("Total events in TTree=%d\n",nevent);

  f.Write() ;  // Write the TTree file header

}

