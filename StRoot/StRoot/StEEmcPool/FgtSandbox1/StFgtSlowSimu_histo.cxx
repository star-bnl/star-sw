// *-- Author : J.Balewski
// 
// $Id: StFgtSlowSimu_histo.cxx,v 1.1 2011/04/07 19:31:22 balewski Exp $

#include <TVector3.h>
#include <TH2.h>
#include <TF1.h>
#include <TFile.h>
#include <TLine.h>
#include <TPolyLine.h>
#include <TCrown.h>
#include <TRandom3.h>

#include "StFgtSlowSimuMaker.h"
#include "StFgtGeom.h"

//_______________________________________________
//________________________________________________
void
StFgtSlowSimuMaker::InitHisto1(){
  /* working arrays for slow simulator
     digXY is reset for every quart (24 times per event)
     digRad, digPhi are reset for every disk ( 6 times per event)
  */

  int digNx=2000, digNy=digNx;
  float maxR=40.;
  digXY=new TH2F("digXY"," 2D digitization response of FGT quart; local X(cm); local Y (cm)",digNx,0.,maxR,digNy,0.,maxR);  HList->Add(digXY);

  digRad=new TH1F("digRad","rad-strips response of FGT disk; radial-strip ID",geom->radStripGBLId_number(),0.,geom->radStripGBLId_number()); HList->Add(digRad);
  digPhi=new TH1F("digPhi","phi-strips response of FGT disk; phi-strip ID",geom->phiStripGBLId_number(),0.,geom->phiStripGBLId_number()); HList->Add(digPhi);

// The next 3 histograms are not reset for debugging purpose WMZ 
  digXYAll=new TH2F("digXYAll"," 2D digitization response of all FGT quart; local X(cm); local Y (cm)",digNx,0.,maxR,digNy,0.,maxR);  HList->Add(digXYAll);

  digRadAll=new TH1F("digRadAll","rad-strips response of all FGT disks; radial-strip ID",geom->radStripGBLId_number(),0.,geom->radStripGBLId_number()); HList->Add(digRadAll);
  digPhiAll=new TH1F("digPhiAll","phi-strips response of all FGT disks; phi-strip ID",geom->phiStripGBLId_number(),0.,geom->phiStripGBLId_number()); HList->Add(digPhiAll);

 
} 
//________________________________________________
//________________________________________________
void
StFgtSlowSimuMaker::InitHisto2(){
  
  // QA-histos
  hA[0]=new TH1F("ss_inDE","g2t  DE of hit; Elos (keV)",600,0.,5.);
  hA[1]=new TH1F("ss_inDS","g2t  path length of hit;  DS (cm)",60,0.,9.);
  hA[2]=new TH1F("ss_inZ","g2t  Z of hit (entrance);  Z(cm)",1000,50.,250.);
  
// # of disk increased to 9 from 6.
  hA[3]=new TH1F("ss_hitStat","Where hits are lost, 1-10 general; x=10+5*disk+quad",75, 0.5,75.5);
  hA[4]=new TH2F("ss_inXY","Entrance X-Y , accepted hits, all disks",90,-45.,45.,90,-45.,45.);
  
  float maxRb=45.;
  hA[5]=new TH1F("ss_cTof","g2t  TOF of hit (before cut);  TOF(ns)",1000,0.,200.);
  hA[6]=new TH1F("ss_inR","g2t  Rxy of hit, w=1.;  Rxy (cm)",10*(int)maxRb,0.,maxRb);
  hA[7]=new TH1F("ss_cPmag","g2t  P.Mag of hit (before cut);  log10(momentum/MeV)",160,-2,6.);



 // 8,9,10-free

  int i;
  for(i=0;i<kFgtMxDisk;i++) { // uses histos ID=[11,18]
    char tt1[100], tt2[500];
    sprintf(tt1,"ss_gXY%d",i);
    sprintf(tt2," hits accepted in Disk=%d; LAB X (cm) ; LAB Y (cm) ",i);
    hA[11+i]=new TH2F(tt1,tt2,25,-maxRb,maxRb,25,-maxRb,maxRb);
  }
 hA[20]=new TH1F("fr_pairEne", "Energy of pair per collision; Energy Loss per collision [eV]", 100, 0, 100);
   hA[21]=new TH1F("fr_nPrimPair", "No. of prim pairs per track; # of pairs", 35, -0.5, 34.5);
   hA[22]= new TH1F("fr_totEne", "Total energy deposit per track;  Energy Loss [keV]", 250, 0, 5.);

   hA[23]= new TH1F("fr_nTotPair", "Total No. of  pairs per track; # of pairs", 250, -0.5, 249.5);
   hA[24]= new TH1F("fr_pathL", "Total path length in gas per track; path (mm)",50, 0., 10.);
   hA[25]= new TH1F("fr_avrPath", "Average path length , w=nAnyEle; path (mm)",50, 0., 10.);
   hA[26]= new TH1F("fr_avrTPath", "Average transverse path length , w=nAnyEle; path (mm)",250, 0., 10.);
   hA[27]=new TH1F("fr_Zdrf","Zdrift of prim ele ;  Z(mm)",100,0.,5.);
   hA[28]=new TH2F("fr_Rdiff","Transverse diffusion; relative X(um); relative Y(um)",50,-250,250,50,-250,250);


   //..............add gadgets to histos
   TList *Lx;  TCrown *cr; //TLine *ln; 

   //....inXY....
   Lx=hA[4]->GetListOfFunctions();    assert(Lx);
   int iq=0;
   for(iq=0;iq<kFgtMxQuad;iq++) {
     float phi1=geom->phiQuadXaxis(iq)/3.1416*180.;
     cr=new TCrown(0.,0.,geom->Rin(),geom->Rout(),phi1,phi1+90);
     cr->SetLineColor(kRed); Lx->Add(cr);
   }
   cr=new TCrown(0.,0.,geom->Rmid(),geom->Rmid(),0,360);
   cr->SetLineColor(kMagenta); Lx->Add(cr);  cr->SetLineStyle(2);




  for(i=0;i<mxH;i++) 
    if(hA[i]) HList->Add(hA[i]);
  

}


//_______________________________________________
//________________________________________________
void
StFgtSlowSimuMaker::CloseHisto(){
  
    //..............add gadgets to histos
   TList *Lx;  TCrown *cr; //TLine *ln; 

  //....digXY....
   Lx=digXY->GetListOfFunctions();    assert(Lx);

   int iqq=0;
   for(iqq=0;iqq<2;iqq++) {
     float phi1=0;
     if(iqq<1) 
       cr=new TCrown(0.,0.,geom->Rmid(),geom->Rout(),phi1,phi1+90);
     else
       cr=new TCrown(0.,0.,geom->Rin(),geom->Rmid(),phi1,phi1+90);
     cr->SetLineColor(kGreen); 
     Lx->Add(cr);
   }
// WMZ
   cr=new TCrown(0.,0.,geom->Rmid(),geom->Rmid(),0,90);
   Lx->Add(cr);

   TList *Lx1;  
   Lx1=digXYAll->GetListOfFunctions();    assert(Lx1);

   for(iqq=0;iqq<2;iqq++) {
     float phi1=0;
     if(iqq<1) 
       cr=new TCrown(0.,0.,geom->Rmid(),geom->Rout(),phi1,phi1+90);
     else
       cr=new TCrown(0.,0.,geom->Rin(),geom->Rmid(),phi1,phi1+90);
     cr->SetLineColor(kGreen); 
     Lx1->Add(cr);
   }
// WMZ
   cr=new TCrown(0.,0.,geom->Rmid(),geom->Rmid(),0,90);
   Lx1->Add(cr);
}



/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////

// $Log: StFgtSlowSimu_histo.cxx,v $
// Revision 1.1  2011/04/07 19:31:22  balewski
// start
//


 


