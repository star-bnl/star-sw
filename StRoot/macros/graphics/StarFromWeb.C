//*-- Author :    Valery Fine   26/03/99  (E-mail: fine@bnl.gov)
{
 // To run this example one needs the access to Internet
 // To start this example launch ROOT as follows:
 //
 //*-*   root.exe StarFromWeb.C
 //

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/WebStar.gif"> </P> End_Html // 
 
  gROOT->Reset();
  cout << " Loading share library" << endl;
  gSystem->Load("St_base");

  // Create canvas

  cout << " Creating an empty TCanvas object to draw in" << endl;
  TCanvas starCanvas("STAR","Star",400,600);
  Int_t  PadColumns = 2;
  Int_t  PadRows = 2;
  starCanvas.Divide(PadColumns,PadRows);

  // Open remote Webfile
  cout << " Open the remote Web file with STAR geometry database in ROOT format" << endl;
  TWebFile f("http://www.star.bnl.gov/~fine/star.root");
  // read STAR geometry database remotely
  cout << " Reading STAR geometry database (the full size of this database 28K bytes - ROOT-object" << endl;
  TGeometry *star = f.Get("STAR");

  TList *listOfNode = star.GetListOfNodes();
  St_Node *hall = listOfNode->First();
  St_DataSetIter volume(hall);

  cout << "Drawing \"HALL/CAVE/TPCE\"" << endl;
  St_Node *vol = volume("HALL/CAVE/TPCE");
  starCanvas.cd(1);
  if (vol) vol->Draw();
  gPad->Update();

  cout << "Drawing \"HALL/CAVE/BTOF\"" << endl;
  vol = (St_Node *)volume("HALL/CAVE/BTOF");
  starCanvas.cd(2);  
  if (vol) vol->Draw();
  gPad->Update();

  cout << "Drawing \"HALL/CAVE/CALB/RICH/SRIC\"" << endl;
  vol = (St_Node *)volume("HALL/CAVE/CALB/RICH/SRIC");
  starCanvas.cd(3);  
  if (vol) vol->Draw();
  gPad->Update();

  cout << "Drawing \"HALL/CAVE/UPST\"" << endl;
  vol = (St_Node *)volume("HALL/CAVE/UPST");
  starCanvas.cd(4);  
  if (vol) vol->Draw();
  gPad->Update();


  cout << "Drawing ROOT TBrowser" << endl;
  TBrowser b("STAR",hall);
  cout << "Now. Try yourself:" << endl <<
  "     1. Select any STAR geomentry St_Node object with  \"double-left-mouse click\" on the St_NodePosition" << endl <<
  "               St_Node object has no \";<n>\" in its name." << endl <<
  "               The objects with trail  \";<n>\" are St_NodePosition ones" << endl <<
  "     2. Pop the context menu up with \"right-mouse click\" on the ROOT browser" << endl <<
  "     3. Select \"Draw\" position" << endl <<
  "     4. Click OK" << endl;
  cout << " Get this macro form $(STAR)/StRoot/macro/StarFromWeb.C and customize it for fun" << endl;

} 
