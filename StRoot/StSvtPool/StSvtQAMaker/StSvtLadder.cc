/***************************************************************************
 *
 * $Id: StSvtLadder.cc,v 1.1 2004/02/06 02:30:35 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Ladder
 *
 ***************************************************************************
 *
 * $Log: StSvtLadder.cc,v $
 * Revision 1.1  2004/02/06 02:30:35  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
 
#include "TObjArray.h"
#include "TText.h"
#include "TButton.h"

//my classes
#include "StSvtLadder.hh"
#include "temp.hh"

ClassImp(StSvtLadder)

//--------------------------------------------------------------------------//
StSvtLadder::StSvtLadder(Int_t barrelID,Int_t ladderID){
  
  mBarrelNumber = barrelID ;
  mLadderNumber = ladderID ;
}

//--------------------------------------------------------------------------//
StSvtLadder::~StSvtLadder()
{ 
delete mSddArray;
}

//--------------------------------------------------------------------------//
void StSvtLadder::DrawVertical()
{  
  TText *text = new TText(0.3,0.97,"-z");
  text->SetTextSize(0.1);
  text->Draw();
  text = new TText(0.5,0.97,"East");
  text->SetTextSize(0.1);
  text->Draw();
  text = new TText(0.3,0.01,"+z");
  text->SetTextSize(0.1);
  text->Draw();
  text = new TText(0.5,0.01,"West");
  text->SetTextSize(0.1);
  text->Draw();

  TButton* sdd;
  TText *aNumber  = 0 ;
  TText *t  = 0 ;
  TText *tt  = 0 ;
  Char_t aLadderName[40];
  char command[200];

  switch(mBarrelNumber){
  case 1 :   /***************/
    
    aNumber = new TText(0.27,0.653,"1");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();
  
    aNumber = new TText(0.27,0.551,"2");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.449,"3");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.347,"4");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.38,0.71,"H1");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.53,0.71,"H2");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    mSddArray = new TObjArray(N_WAFER_LADDER_1);

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,1,1);aMonitor->setWaferID(1);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.603,0.50,0.703);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,1,2);aMonitor->setWaferID(1);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.603,0.65,0.703);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,2,1);aMonitor->setWaferID(2);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.501,0.50,0.601);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,2,2);aMonitor->setWaferID(2);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.501,0.65,0.601);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,3,1);aMonitor->setWaferID(3);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.399,0.50,0.499);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,3,2);aMonitor->setWaferID(3);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.399,0.65,0.499);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,4,1);aMonitor->setWaferID(4);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.297,0.50,0.397);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,4,2);aMonitor->setWaferID(4);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.297,0.65,0.397);
    mSddArray->Add(sdd);
    sdd->Draw();

    t = new TText(0.28,0.20,"inner barrel");
    t->SetTextSize(0.1);
    t->Draw();

    sprintf(aLadderName,"ladder %d",mLadderNumber); 
    tt =  new TText(0.32,0.15,aLadderName);
    tt->SetTextSize(0.1);
    tt->Draw();

    break;

  case 2 :    /*****************/
 
    aNumber = new TText(0.27,0.755,"1");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.653,"2");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.551,"3");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.449,"4");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.347,"5");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.245,"6");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();
 
    aNumber = new TText(0.38,0.81,"H1");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.53,0.81,"H2");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    mSddArray = new TObjArray(N_WAFER_LADDER_2);
      
    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,1,1);aMonitor->setWaferID(1);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.705,0.50,0.805);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,1,2);aMonitor->setWaferID(1);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.705,0.65,0.805);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,2,1);aMonitor->setWaferID(2);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.603,0.50,0.703);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,2,2);aMonitor->setWaferID(2);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.603,0.65,0.703);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,3,1);aMonitor->setWaferID(3);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.501,0.50,0.601);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,3,2);aMonitor->setWaferID(3);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.501,0.65,0.601);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,4,1);aMonitor->setWaferID(4);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.399,0.50,0.499);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,4,2);aMonitor->setWaferID(4);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.399,0.65,0.499);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,5,1);aMonitor->setWaferID(5);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.297,0.50,0.397);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,5,2);aMonitor->setWaferID(5);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.297,0.65,0.397);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,6,1);aMonitor->setWaferID(6);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.195,0.50,0.295);
    mSddArray->Add(sdd);
    sdd->Draw();
    
    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,6,2);aMonitor->setWaferID(6);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.195,0.65,0.295);
    mSddArray->Add(sdd);
    sdd->Draw();
    
    t = new TText(0.28,0.15,"middle barrel");
    t->SetTextSize(0.1);
    t->Draw();
    sprintf(aLadderName,"ladder %d",mLadderNumber); 
    tt =  new TText(0.32,0.10,aLadderName);
    tt->SetTextSize(0.1);
    tt->Draw();

    break;
    
  case 3 :    /*****************/

    aNumber = new TText(0.27,0.857,"1");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.755,"2");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.653,"3");
    aNumber->SetTextSize(0.1);
    aNumber->Draw(); 

    aNumber = new TText(0.27,0.551,"4");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.449,"5");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.27,0.347,"6");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();
  
    aNumber = new TText(0.27,0.245,"7");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();
 
    aNumber = new TText(0.38,0.91,"H1");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    aNumber = new TText(0.53,0.91,"H2");
    aNumber->SetTextSize(0.1);
    aNumber->Draw();

    mSddArray = new TObjArray(N_WAFER_LADDER_3);

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,1,1);aMonitor->setWaferID(1);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.807,0.50,0.907);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,1,2);aMonitor->setWaferID(1);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.807,0.65,0.907);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,2,1);aMonitor->setWaferID(2);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.705,0.50,0.805);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,2,2);aMonitor->setWaferID(2);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.705,0.65,0.805);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,3,1);aMonitor->setWaferID(3);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.603,0.50,0.703);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,3,2);aMonitor->setWaferID(3);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.603,0.65,0.703);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,4,1);aMonitor->setWaferID(4);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.501,0.50,0.601);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,4,2);aMonitor->setWaferID(4);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.501,0.65,0.601);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,5,1);aMonitor->setWaferID(5);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.399,0.50,0.499);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,5,2);aMonitor->setWaferID(5);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.399,0.65,0.499);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,6,1);aMonitor->setWaferID(6);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.297,0.50,0.397);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,6,2);aMonitor->setWaferID(6);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.297,0.65,0.397);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,7,1);aMonitor->setWaferID(7);aMonitor->setHybridID(1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.35,0.195,0.50,0.295);
    mSddArray->Add(sdd);
    sdd->Draw();

    sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(%d,%d,7,2);aMonitor->setWaferID(7);aMonitor->setHybridID(2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}",mBarrelNumber,mLadderNumber);
    sdd = new TButton("",command,0.50,0.195,0.65,0.295);
    mSddArray->Add(sdd);
    sdd->Draw();

    t = new TText(0.28,0.15,"outer barrel");
    t->SetTextSize(0.1);
    t->Draw();  
    sprintf(aLadderName,"ladder %d",mLadderNumber); 
    tt =  new TText(0.32,0.10,aLadderName);
    tt->SetTextSize(0.1);
    tt->Draw();

    break;    
  }
}
//--------------------------------------------------------------------------//
void StSvtLadder::DrawHorizontal(char* config)
{  
  TText *text = new TText(0.025,0.5,"-z");
  text->SetTextSize(0.08);
  text->Draw();
  text = new TText(0.01,0.42,"East");
  text->SetTextSize(0.08);
  text->Draw();
  text = new TText(0.925,0.5,"+z");
  text->SetTextSize(0.08);
  text->Draw();
  text = new TText(0.9,0.42,"West");
  text->SetTextSize(0.08);
  text->Draw();

  TButton* sdd;
  TText *aNumber  = 0 ;
  char command[200];

  if ( !strncmp(config, "Y1L", strlen("Y1L")) ) {

    aNumber = new TText(0.2,0.2,"1");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();

    aNumber = new TText(0.3,0.2,"2");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();

    aNumber = new TText(0.4,0.2,"3");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw(); 

    aNumber = new TText(0.5,0.2,"4");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();

    aNumber = new TText(0.6,0.2,"5");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();

    aNumber = new TText(0.7,0.2,"6");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();
  
    aNumber = new TText(0.8,0.2,"7");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();
 
    aNumber = new TText(0.1,0.375,"H1");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(2);
    aNumber->Draw();

    aNumber = new TText(0.1,0.575,"H2");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(2);
    aNumber->Draw();

      mSddArray = new TObjArray(7);
            
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,1,1);aMonitor->setHybridID(3,2,1,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.15,0.3,0.25,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,1,2);aMonitor->setHybridID(3,2,1,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.15,0.5,0.25,0.7);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,2,1);aMonitor->setHybridID(3,2,2,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.25,0.3,0.35,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,2,2);aMonitor->setHybridID(3,2,2,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.25,0.5,0.35,0.7);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,3,1);aMonitor->setHybridID(3,2,3,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.35,0.3,0.45,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,3,2);aMonitor->setHybridID(3,2,3,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.35,0.5,0.45,0.7);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,4,1);aMonitor->setHybridID(3,1,4,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.45,0.3,0.55,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,4,2);aMonitor->setHybridID(3,1,4,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.45,0.5,0.55,0.7);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,5,1);aMonitor->setHybridID(3,1,5,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.55,0.3,0.65,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,5,2);aMonitor->setHybridID(3,1,5,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.55,0.5,0.65,0.7);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,6,1);aMonitor->setHybridID(3,1,6,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.65,0.3,0.75,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,6,2);aMonitor->setHybridID(3,1,6,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.65,0.5,0.75,0.7);
      mSddArray->Add(sdd);
      sdd->Draw();

      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,7,1);aMonitor->setHybridID(3,1,7,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.75,0.3,0.85,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,7,2);aMonitor->setHybridID(3,1,7,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.75,0.5,0.85,0.7);
      mSddArray->Add(sdd);
      sdd->Draw();
    }

    else if ( !strncmp(config, "SYST", strlen("SYST")) ) {


    aNumber = new TText(0.18,0.1,"1");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();

    aNumber = new TText(0.26,0.1,"2");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();

    aNumber = new TText(0.34,0.1,"3");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw(); 

    aNumber = new TText(0.42,0.1,"4");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();

    aNumber = new TText(0.5,0.1,"5");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();

    aNumber = new TText(0.58,0.1,"6");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();
  
    aNumber = new TText(0.66,0.1,"7");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();
 
    aNumber = new TText(0.74,0.1,"8");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();
  
    aNumber = new TText(0.82,0.1,"9");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(4);
    aNumber->Draw();
 
    aNumber = new TText(0.09,0.31,"H1");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(2);
    aNumber->Draw();

    aNumber = new TText(0.09,0.61,"H2");
    aNumber->SetTextSize(0.08);
    aNumber->SetTextColor(2);
    aNumber->Draw();

      mSddArray = new TObjArray(9);

      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,1,1);aMonitor->setHybridID(3,1,7,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.14,0.2,0.22,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,1,2);aMonitor->setHybridID(3,1,7,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.14,0.5,0.22,0.8);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,2,1);aMonitor->setHybridID(3,1,6,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.22,0.2,0.3,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();

      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,2,2);aMonitor->setHybridID(3,1,6,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.22,0.5,0.3,0.8);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,3,1);aMonitor->setHybridID(1,1,4,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.3,0.2,0.38,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,3,2);aMonitor->setHybridID(1,1,4,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.3,0.5,0.38,0.8);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,4,1);aMonitor->setHybridID(1,1,3,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.38,0.2,0.46,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,4,2);aMonitor->setHybridID(1,1,3,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.38,0.5,0.46,0.8);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,5,1);aMonitor->setHybridID(3,2,7,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.46,0.2,0.54,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,5,2);aMonitor->setHybridID(3,2,7,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.46,0.5,0.54,0.8);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,6,1);aMonitor->setHybridID(3,2,6,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.54,0.2,0.62,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,6,2);aMonitor->setHybridID(3,2,6,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.54,0.5,0.62,0.8);
      mSddArray->Add(sdd);
      sdd->Draw();

      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,7,1);aMonitor->setHybridID(3,1,5,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.62,0.2,0.7,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,7,2);aMonitor->setHybridID(3,1,5,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.62,0.5,0.7,0.8);
      mSddArray->Add(sdd);
      sdd->Draw();

      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,8,1);aMonitor->setHybridID(3,1,4,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.7,0.2,0.78,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,8,2);aMonitor->setHybridID(3,1,4,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.7,0.5,0.78,0.8);
      mSddArray->Add(sdd);
      sdd->Draw();

      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,9,1);aMonitor->setHybridID(3,2,5,1);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.78,0.2,0.86,0.5);
      mSddArray->Add(sdd);
      sdd->Draw();
      
      sprintf(command,"{aGraphicMonitor->UpdateLadderButtons(0,0,9,2);aMonitor->setHybridID(3,2,5,2);if (menuBar->SetDefaultCanvas()) {menuBar->DrawHist(\"BUTTON\");menuBar->UpdateCanvas();}}");
      sdd = new TButton("",command,0.78,0.5,0.86,0.8);
      mSddArray->Add(sdd);
      sdd->Draw();
    }
}

//--------------------------------------------------------------------------//

void StSvtLadder::UpdateButtons(Int_t waferID, Int_t hybridID)
{
  TObjArrayIter iter(mSddArray);
  TButton* sdd;
  Int_t cur = 0 ;
  int iwafer = 0;
  int ihybrid = 0;

  while (iter.Next()){  

    sdd = (TButton*)mSddArray->At(cur);
    if (sdd->GetFillColor() == 2)
      sdd->SetFillColor(18);

    if ((iwafer == (waferID-1)) && (ihybrid == (hybridID-1)))
      sdd->SetFillColor(2);

    sdd->Modified();

    ihybrid++;
    if (ihybrid > 1) {
      iwafer++;
      ihybrid = 0;
    }

    cur++;
  }
}
