// Author V.Fine 06/12/2001 BNL mailto:fine@bnl.gov
// This run Pythia using ROOT TPYthia6 interface
// Thanks Michael Bussmann <Michael.Bussmann@physik.uni-muenchen.de> for Pythia living example
void ReadPythia(const char *fileName="pythia.root")
{
  struct pythia_particle {
        Int_t status;        // status of particle          ( LUJETS K[1] )
        Int_t pdg_id;        // flavour code                ( LUJETS K[2] )
        Int_t parent;        // parrent's id                ( LUJETS K[3] )
        Int_t firstChild;    // id of first child           ( LUJETS K[4] )
        Int_t lastChild;     // id of last  child           ( LUJETS K[5] )
        Float_t momentum[4]; // X.Y,Z,energy momenta [GeV/c]( LUJETS P[1]=P[4] )
        Float_t mass;        // Mass      [Gev/c^2]         ( LUJETS P[5] )
        Float_t vertex[4];   // X,Y,Z vertex  [mm]; time of procuction [mm/c]( LUJETS V[1]-V[4] )
        Float_t lifetime;    // proper lifetime [mm/c]      ( LUJETS V[5] )
  };
  TFileIter readEvents(fileName);
  TObject *nextObject = 0;
  // the number of the object available directly from "MyDataSet.root"
  Int_t size = readEvents.TotalKeys();
  printf(" The total number of the events: %d\n",size);

  //-----------------------------------------------------------------------
  // Loop over all events, read them in to memory one by one

  printf(" -- > Loop over all events, read them in to memory one by one < -- \n");
  for( readEvents = 0; int(readEvents) < size; readEvents.SkipObjects() ){
      nextObject = *readEvents;
      printf(" %d bytes of the object \"%s\" of class \"%s\" written with TKey \"%s\"  has been read from file\n"
               ,readEvents.GetObjlen()
               ,nextObject->GetName()
               ,nextObject->IsA()->GetName()
               ,(const char *)readEvents
            );
      TDataSet *event = (TDataSet *)nextObject;
      event->ls(9);
      delete nextObject;
  }
}
