//*-- Author :    Valery Fine   07/04/99  (E-mail: fine@bnl.gov)
// $Id: SubDetectorView.C,v 1.1 1999/04/08 19:21:19 fine Exp $
// $Log: SubDetectorView.C,v $
// Revision 1.1  1999/04/08 19:21:19  fine
// Macro to generate sub-detector and plot it
//
//
{

 // example to show how one can generate the STAR/GEANT geometry
 //                   sub-structure
 // To run this example one needs the access to Internet
 // To start this example launch ROOT as follows:
 //
 //*-*   root4star  SubDetectorView.C
 //

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/StarBMTC.gif"> </P> End_Html // 
 
  gROOT->Reset();
  printf( " Loading share library\n");
  gSystem->Load("St_base");


  // Open remote Webfile
  TWebFile f("http://www.star.bnl.gov/~fine/star.root");
  // read STAR geometry database remotely
  TGeometry *star = f.Get("STAR");
  if (!star) {
    printf("Sorry, STAR was not found !\n");
    return;
  }

  TList *listOfNode = star->GetListOfNodes();
  St_Node *hall = listOfNode->First();
  // Remove hall from the kist of ROOT nodes to make it free of ROOT control
  listOfNode->Remove(hall);
  listOfNode->Remove(hall);
  St_DataSetIter volume(hall);

  // Mark the pieces of the whole detector to create sub-structure
 
  volume("HALL")->Mark();
  volume("HALL")->Mark()->SetVisibility(1);;
  volume.Cd("HALL/CAVE");

   volume("SVTT/SCON")->Mark();
  ((St_Node*)volume("SVTT/SCON"))->SetVisibility(1);

   volume("SVTT/SCON/STAC")->Mark();
  ((St_Node*)volume("SVTT/SCON/STAC"))->SetVisibility(1);


   volume("SVTT/SCON/SHLA/SHMA/SHWA")->Mark();
  ((St_Node*)volume("SVTT/SCON/SHLA/SHMA/SHWA"))->SetVisibility(1);

   volume("SVTT/SCON/SHLB/SHMB/SHWA")->Mark();
  ((St_Node*)volume("SVTT/SCON/SHLA/SHMA/SHWA"))->SetVisibility(1);

   volume("MAGP/COIL/MCSE")->Mark();
  ((St_Node*)volume("MAGP/COIL/MCSE"))->SetVisibility(1);

  // Mark the whole structure excluding one node
  
  St_DataSetIter bmtc(volume("BTOF/BTOH/BSEC/BTRA/BXTR/BMTC"));
  St_Node *node =0;
  while( node = (St_Node *) bmtc())  {
    if( strcmp(node->GetName(),"BXSA"))   node->Mark();
  }
 
  // Create the "open" structure from the "closed" one
  St_NodeView *fullView = new St_NodeView(*hall);
  // Create the "open" sub-structure from the full one
  St_NodeView *s = new St_NodeView(fullView);

  printf( " Creating an empty TCanvas object to draw in\n");
//  TCanvas starCanvas("STAR","Star",400,600);
//  starCanvas.Divide(1,1);
//  starCanvas.cd(1);

 s->Draw();
 gPad->Update();
} 
