// Author V.Fine 06/12/2001 BNL mailto:fine@bnl.gov
// This reads Pythia file created by P2ATest.C macro
// Thanks Michael Bussmann <Michael.Bussmann@physik.uni-muenchen.de> for Pythia living example
// Thanks Christian Holm Christensen <cholm@nbi.dk> for Pythia histogramming pattern

#define HISTNAME   "ptSpectra"
#define PDGNUMBER  211

TH1D* ptSpectra = 0;
void PlotPythia(const char *fileName="pythia.root")
{
  struct pythia_particle {
        Int_t status;        // status of particle          ( LUJETS K[1] )
        Int_t pdg_id;        // flavour code                ( LUJETS K[2] )
        Int_t parent;        // parrent's id                ( LUJETS K[3] )
        Int_t firstChild;    // id of first child           ( LUJETS K[4] )
        Int_t lastChild;     // id of last  child           ( LUJETS K[5] )
        Float_t momentum[4]; // X.Y,Z,energy momenta [GeV/c]( LUJETS P[1]=P[4] )
        Float_t mass;        // Mass      [Gev/c^2]         ( LUJETS P[5] )
        Float_t vertex[4];   // X,Y,Z vertex  [mm]; time of production [mm/c]( LUJETS V[1]-V[4] )
        Float_t lifetime;    // proper lifetime [mm/c]      ( LUJETS V[5] )
  };
  // LoadPythia();
  printf("\nUsage: root LoadPythia.C PlotPythia.C(fileName=\"pythia.root\")\n");
  printf(  "-----\n\n");

  printf("\n       To create \"pythia.root\" input file run first");
  printf("\n     > root LoadPythia.C P2ATest.C\n");
  printf(  "-----\n\n");


  TFileIter readEvents(fileName);
  TObject *nextObject = 0;
  // the number of the object available directly from "MyDataSet.root"
  Int_t size = readEvents.TotalKeys();
  printf(" The total number of the events: %d\n",size);

  //-----------------------------------------------------------------------
  // Loop over all events, read them in to memory one by one
  // we may want to do some summary plots: 
  // The historamming was provided thanks Christian Holm Christensen mailto::cholm@nbi.dk 
  
  ptSpectra = new TH1D(HISTNAME, "p_{#perp} spectrum for #pi^{+}", 
			100, 0, 3);
  ptSpectra->SetXTitle("p_{#perp}");
  ptSpectra->SetYTitle("dN/dp_{#perp}");
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
      TGenericTable *particles = (TGenericTable *)event->FindByName("particle");
      if (particles) particles->Draw(Form("sqrt(pow(momentum[0],2)+pow(momentum[1],2))>>+%s", HISTNAME), 
                                    Form("pdg_id==%d", PDGNUMBER) );
      c1->Update();
      event->ls(9);
      delete nextObject;
  }
  // Normalise to the number of events, and the bin sizes.
  ptSpectra->SetDirectory(0); // We need this otherwise Rene BRun will kill our histogram shortly
  ptSpectra->Sumw2();
  ptSpectra->Scale(3 / 100. / ptSpectra->Integral());
  ptSpectra->Fit("expo", "QO+", "", .25, 1.75);
  TF1* func = ptSpectra->GetFunction("expo");
  func->SetParNames("A", "- 1 / T");
  c1->Update();
}
