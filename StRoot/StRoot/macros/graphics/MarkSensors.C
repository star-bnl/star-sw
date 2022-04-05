//*-- Author :    Valery Fine   26/03/99  (E-mail: fine@bnl.gov)
// $Id: MarkSensors.C,v 1.4 2000/01/25 16:06:38 fisyak Exp $
// $Log: MarkSensors.C,v $
// Revision 1.4  2000/01/25 16:06:38  fisyak
// g2r -> g2t
//
// Revision 1.3  1999/05/21 15:33:52  kathy
// made sure Log & Id are in each file and also put in standard comment line with name of owner
//
// Revision 1.2  1999/05/06 03:21:24  fisyak
// synchronize FTPC and TPC slow/fast
//
// Revision 1.1  1999/04/15 20:37:49  fine
//  new macro MarkSensors.C to generate St_Node and St_NodeView by St_geant_Maker
//
// Revision 1.2  1999/04/08 21:12:24  fine
// SubDetectorView macro has been introduced
//
// Revision 1.2  1999/03/29 19:17:54  fine
// x3d view has been activated. Some improvement as well
//
//=======================================================================
// owner:  Valery Fine
// what it does: 
//=======================================================================
{
 // example to show how to generate the STAR/GEANT geometry sub-structure
 // To run this example one needs the access to Internet
 // To start this example launch ROOT as follows:
 //
 //*-*   root.exe SubDetectorView.C
 //

 //  Begin_Html <P ALIGN=CENTER> <IMG SRC="gif/StarBMTC.gif"> </P> End_Html // 
 
//  gROOT->Reset();
//  printf( " Loading share libraries:\n");
//  printf("St_base . . .");       gSystem->Load("St_base");    printf(" ..\n");
//  printf("StChain . . .");       gSystem->Load("StChain");    printf(" ..\n");
//  printf("St_Tables . . .");     gSystem->Load("St_Tables");  printf(" ..\n");
//  printf("geometry . . .");      gSystem->Load("geometry");   printf(" ..\n");
//  printf("St_g2t . . .");        gSystem->Load("St_g2t");     printf(" ..\n");
//  printf("St_geant_Maker . . .");gSystem->Load("St_geant_Maker"); printf(" ..\n");

  if (!geant) return;
  St_Node *hall = geant->Work();
  // Remove hall from the list of ROOT nodes to make it free of ROOT control
  St_DataSetIter volume(hall,0);

  // Get TGeant3 pointer:
  TGeant3 *gGeant3 = TGeant3::Geant3();
  if (!gGeant3) {
    printf(" No TGeant was loaded yet !\n");
    return;
  }
  // Mark the pieces of the whole detector to create sub-structure
 
  St_Node *node =0;
  Int_t iAcceptedCounter = 0;
  Int_t iRefusedCounter = 0;
  while( node = (St_Node *) volume())  {
    Char_t *nodeName = node->GetName();
    if (nodeName && gGeant3->Agsens(nodeName)) { 
      iAcceptedCounter++; 
      node->Mark(); 
      node->SetVisibility(1);
    }
    else iRefusedCounter++;
  }
 
  // Create the "open" structure from the "closed" one
  printf("%d nodes have been accepted and %d refused \n",iAcceptedCounter,iRefusedCounter);
  TFile f("STAR","RECREATE");
  gGeometry->Write();
  f.Write();
  f.Close();
  St_NodeView *fullView = new St_NodeView(*hall);
  // Create the "open" sub-structure from the full one
  St_NodeView *s = new St_NodeView(fullView);

  printf( " Creating an empty TCanvas object to draw in\n");
  
//  s->Draw();
//  gPad->Update();
} 
