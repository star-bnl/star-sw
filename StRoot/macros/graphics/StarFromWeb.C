//*-- Author :    Valery Fine   26/03/99  (E-mail: fine@bnl.gov)
// $Id: StarFromWeb.C,v 1.6 2005/05/02 16:55:33 fine Exp $
// $Log: StarFromWeb.C,v $
// Revision 1.6  2005/05/02 16:55:33  fine
// fix T=Star Geometry test
//
// Revision 1.5  2005/04/29 21:37:35  fine
// fix the  table library test
//
// Revision 1.4  2000/04/07 17:07:08  fine
// adjusted to the ROOT 2.24
//
// Revision 1.3  1999/05/21 15:33:53  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.2  1999/03/29 19:17:54  fine
// x3d view has been activated. Some improvement as well
//
//=======================================================================
// owner: Valery fine
// what it does: Shows the various view of the STAR geometry
//=======================================================================
void StarFromWeb(const char *geomFile ="/afs/rhic.bnl.gov/star/users/fine/WWW/star_year_2a.root")
{
 // To run this example one needs the access to Internet
 // To start this example launch ROOT as follows:
 //
 //*-*   root.exe StarFromWeb.C
 //

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/WebStar.gif"> </P> End_Html // 
 
  cout << " Loading share library" << endl;
  Bool_t NT=kFALSE;
  if (strcmp(gSystem->GetName(),"WinNT") == 0 ) NT=kTRUE;
  if (NT) gSystem->Load("Table");
  else  gSystem->Load("libTable");
  if (!gGeometry) new TGeometry;

  // Create canvas

  cout << " Creating an empty TCanvas object to draw in" << endl;
  TCanvas &starCanvas = *(new TCanvas("STAR","Star",400,600));
  
  Int_t  PadColumns = 2;
  Int_t  PadRows = 2;
  starCanvas.Divide(PadColumns,PadRows);

  // Open remote Webfile
  cout << " Open the remote Web file with STAR geometry database in ROOT format" << endl;
  //  TWebFile f("http://www.rhic.bnl.gov/~fine/star_year_2a.root");
  TFile f(geomFile);
  // read STAR geometry database remotely
  cout << " Reading STAR geometry database (the full size of this database 28K bytes - ROOT-object" << endl;
  TDataSetIter volume(HALL);
  volume.Cd("HALL/CAVE");

  const Char_t *parts[] = {"TPCE"
                           ,"MAGP"
                           ,"CALB/RICH/SRIC"
                           ,"UPST" };
  const Int_t numParts = sizeof(parts)/sizeof(void *);
  cout << numParts << " different parts of the STAR detector will be built" << endl;

  for (Int_t i =0; i< numParts; i++) {
    const Char_t *part = parts[i];
    cout << "Drawing \"" << part << "\"" << endl;
    TVolume *vol = (TVolume *)volume(part);
    starCanvas.cd(i+1);
    if (vol) vol->Draw();
    else printf(" Volume \"%s\"  was not found\n", part);
    gPad->Update();
  }
  
  Int_t ThreeDPart = 2;
  cout << " Plot  3D view of the " << ThreeDPart << " part: " << parts[ThreeDPart-1]              << endl << 
          " Note: Under Windows NT OpenGL will be used by default instead x3d for UNIX"           << endl << 
          " ================================================================"   << endl <<   
          " Select x3d windows and type \"m\" in there to get x3d HELP windows" << endl <<
          " ================================================================"   << endl <<   
          " DON NOT FORGET to type \"q\" letter to terminate x3d widget and"    << endl <<   
          " to continue this session"                                           << endl <<
          " ================================================================"   << endl;
  starCanvas.cd(ThreeDPart);
  gPad->x3d();

  cout << "Drawing ROOT TBrowser" << endl;
  new TBrowser("STAR",HALL);
  cout << "Now. Try yourself:" << endl <<
  "     1. Select any STAR geometry TVolume object with  \"double-left-mouse click\" on the TVolumePosition" << endl <<
  "               TVolume object has no \";<n>\" in its name." << endl <<
  "               The objects with trail  \";<n>\" are TVolumePosition ones" << endl <<
  "     2. Pop the context menu up with \"right-mouse click\" on the ROOT browser" << endl <<
  "     3. Select \"Draw\" position" << endl <<
  "     4. Click OK" << endl;
  cout << " Get this macro form $(STAR)/StRoot/macro/StarFromWeb.C and customize it for fun" << endl;



} 
