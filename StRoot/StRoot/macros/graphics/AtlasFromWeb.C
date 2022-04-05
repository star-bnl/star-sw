//*-- Author :    Valery Fine   26/03/99  (E-mail: fine@bnl.gov)
// $Id: AtlasFromWeb.C,v 1.2 2000/04/07 17:07:08 fine Exp $
// $Log: AtlasFromWeb.C,v $
// Revision 1.2  2000/04/07 17:07:08  fine
// adjusted to the ROOT 2.24
//
// Revision 1.1  1999/10/11 21:46:40  fine
// Macro to draw the ATLAS detector geometry
//
// Revision 1.2  1999/03/29 19:17:54  fine
// x3d view has been activated. Some improvement as well
//
{
 // To run this example one needs the access to Internet
 // To start this example launch ROOT as follows:
 //
 //*-*   root.exe StarFromWeb.C
 //

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/WebStar.gif"> </P> End_Html // 
 
  gROOT->Reset();
  cout << " Loading share library" << endl;
  gROOT->Reset();
  printf( " Loading share library\n");
  gSystem->Load("libSTAR");
  if (!gGeometry) new TGeometry;

  // Create canvas

  cout << " Creating an empty TCanvas object to draw in" << endl;
  TCanvas starCanvas("STAR","Star",400,600);
  Int_t  PadColumns = 2;
  Int_t  PadRows = 2;
  starCanvas.Divide(PadColumns,PadRows);

  // Open remote Webfile
  cout << " Open the remote Web file with STAR geometry database in ROOT format" << endl;
  //  TWebFile f("http://www.star.bnl.gov/~fine/atlas.root");
  TFile f("~fine/WWW/atlas.root");
  // read STAR geometry database remotely
  cout << " Reading STAR geometry database (the full size of this database 3Mb bytes - ROOT-object" << endl;
  TDataSetIter volume(HALL);
  volume.Cd("ATLS");

  const Char_t *parts[] = {"INNE"
                           ,"PIPE"
                           ,"OUTE" 
                           ,"CENT"
                          };
  const Int_t numParts = sizeof(parts)/sizeof(void *);
  cout << numParts << " different parts of the ATALS detector will be built" << endl;

  for (Int_t i =0; i< numParts; i++) {
    const Char_t *part = parts[i];
    cout << "Drawing \"" << part << "\"" << endl;
    TVolume *vol = volume(part);
    starCanvas.cd(i+1);
    if (vol) vol->Draw();
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
  TBrowser b("STAR",HALL);
  cout << "Now. Try yourself:" << endl <<
  "     1. Select any STAR geometry TVolume object with  \"double-left-mouse click\" on the TVolumePosition" << endl <<
  "               TVolume object has no \";<n>\" in its name." << endl <<
  "               The objects with trail  \";<n>\" are TVolumePosition ones" << endl <<
  "     2. Pop the context menu up with \"right-mouse click\" on the ROOT browser" << endl <<
  "     3. Select \"Draw\" position" << endl <<
  "     4. Click OK" << endl;
  cout << " Get this macro form $(STAR)/StRoot/macro/StarFromWeb.C and customize it for fun" << endl;
} 
