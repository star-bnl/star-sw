{
//*-- Author :    Valery Fine(fine@bnl.gov)   25/12/98   
// Based on Begin_html <a href = "http://root.cern.ch/root/html/examples/shapes.C.html">shapes.C</a>
// end_html
//
// To see the output of this macro, click begin_html <a href="gif/shapes.gif" >here</a> end_html
//=======================================================================
// owner:  Valery Fine
// what it does: Use TVolume class to show "geant" based shapes
//=======================================================================
   //
   gROOT->Reset();
   gSystem->Load("libTable");
   c1 = new TCanvas("c1","Geometry Shapes",200,10,700,500);

   //  Define some volumes
   TBRIK *brik = new TBRIK("BRIK","BRIK","void",200,150,150);
   TTRD1 *trd1 = new TTRD1("TRD1","TRD1","void",200,50,100,100);
   TTRD2 *trd2 = new TTRD2("TRD2","TRD2","void",200,50,200,50,100);
   TTRAP *trap = new TTRAP("TRAP","TRAP","void",190,0,0,60,40,90,15,120,80,180,15);
   TPARA *para = new TPARA("PARA","PARA","void",100,200,200,15,30,30);
   TGTRA *gtra = new TGTRA("GTRA","GTRA","void",390,0,0,20,60,40,90,15,120,80,180,15);
   TTUBE *tube = new TTUBE("TUBE","TUBE","void",150,200,400);
   TTUBS *tubs = new TTUBS("TUBS","TUBS","void",80,100,100,90,235);
   TCONE *cone = new TCONE("CONE","CONE","void",100,50,70,120,150);
   TCONS *cons = new TCONS("CONS","CONS","void",50,100,100,200,300,90,270);
   TSPHE *sphe = new TSPHE("SPHE","SPHE","void",25,340, 45,135, 0,270);
   TSPHE *sphe1 = new TSPHE("SPHE1","SPHE1","void",0,140, 0,180, 0,360);
   TSPHE *sphe2 = new TSPHE("SPHE2","SPHE2","void",0,200, 10,120, 45,145);
 
   pcon = new TPCON("PCON","PCON","void",180,270,4);
   pcon->DefineSection(0,-200,50,100);
   pcon->DefineSection(1,-50,50,80);
   pcon->DefineSection(2,50,50,80);
   pcon->DefineSection(3,200,50,100);
 
   pgon = new TPGON("PGON","PGON","void",180,270,8,4);
   pgon->DefineSection(0,-200,50,100);
   pgon->DefineSection(1,-50,50,80);
   pgon->DefineSection(2,50,50,80);
   pgon->DefineSection(3,200,50,100);
 
   //  Set shapes attributes
   brik->SetLineColor(1);
   trd1->SetLineColor(2);
   trd2->SetLineColor(3);
   trap->SetLineColor(4);
   para->SetLineColor(5);
   gtra->SetLineColor(7);
   tube->SetLineColor(6);
   tubs->SetLineColor(7);
   cone->SetLineColor(2);
   cons->SetLineColor(3);
   pcon->SetLineColor(6);
   pgon->SetLineColor(2);
   sphe->SetLineColor(kRed);
   sphe1->SetLineColor(kBlack);
   sphe2->SetLineColor(kBlue);
 
 
   //  Build the geometry hierarchy
   node1 = new TVolume("NODE1","NODE1","BRIK");
 
   node1->Add(new TVolume("NODE2","NODE2","TRD1"),0,0,-1000);
   node1->Add(new TVolume("NODE3","NODE3","TRD2"),0,0,1000);
   node1->Add(new TVolume("NODE4","NODE4","TRAP"),0,-1000,0);
   node1->Add(new TVolume("NODE5","NODE5","PARA"),0,1000,0);
   node1->Add(new TVolume("NODE6","NODE6","TUBE"),-1000,0,0);
   node1->Add(new TVolume("NODE7","NODE7","TUBS"),1000,0,0);
   node1->Add(new TVolume("NODE8","NODE8","CONE"),-300,-300,0);
   node1->Add(new TVolume("NODE9","NODE9","CONS"),300,300,0);
   node1->Add(new TVolume("NODE10","NODE10","PCON"),0,-1000,-1000);
   node1->Add(new TVolume("NODE11","NODE11","PGON"),0,1000,1000);
   node1->Add(new TVolume("NODE12","NODE12","GTRA"),0,-400,700);
   node1->Add(new TVolume("NODE13","NODE13","SPHE"),10,-400,500);
   node1->Add(new TVolume("NODE14","NODE14","SPHE1"),10, 250,300);
   node1->Add(new TVolume("NODE15","NODE15","SPHE2"),10,-100,-200);
   
   node1->MarkAll();

   // Create "view" object of the "node1" to browse 
   TVolumeView view(*node1);   

   view.Draw();
  

   //
   //  Draw the geometry using the x3d viewer.
   //  Note that this viewver may also be invoked from the "View" menu in
   //  the canvas tool bar
   //  c1->x3d();
   //
   // once in x3d viewer, type m to see the menu.
   // For example typing r will show a solid model of this geometry.
}
