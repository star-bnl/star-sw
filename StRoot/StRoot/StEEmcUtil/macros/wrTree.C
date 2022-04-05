
wrTree(){
  
 gSystem->Load("EEmc.so"); 

  //create a Tree file tree4.root
  TFile f("tree4.root","RECREATE");
  
  // Create a ROOT Tree
  TTree t4("t4","A Tree with Events");

  // Create a pointer to an Event object
  EEevent *event = new EEevent();

  //  Create DST for sectors
  int is;
  for (is=0;is<2;is++)
    event->addSectorDst(is+5);


  // Create two branches, split one.
  t4.Branch("EEDst", "EEevent", &event,16000,99);
//  return;

  for (Int_t ev = 0; ev <3; ev++) {

    event->clear();     // Clear the event before reloading it 
    event->setID(ev+80);
    for(is=0;is<2;is++) {
      int secID=is+5;

      EEsectorDst *sec= (EEsectorDst *)event->getSec(secID);
      
      int ih;
      int nh=100- ev*10-is;
      for(ih=0;ih<nh;ih++) {
	float energy=100+ih*100+nh;
	//	printf("bb=%d\n",ih);
	sec->addTwHit('A',1,energy);
      }
    }

    // Fill the tree
    t4.Fill(); 
  }// end of loop over events

 
  // Write the file header
  f.Write();
  
  // Print the tree contents
  t4.Print();
}
