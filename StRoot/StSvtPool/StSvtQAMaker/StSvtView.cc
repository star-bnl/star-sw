/***************************************************************************
 *
 * $Id: StSvtView.cc,v 1.1 2004/02/06 02:30:36 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT View GUI Monitor
 *
 ***************************************************************************
 *
 * $Log: StSvtView.cc,v $
 * Revision 1.1  2004/02/06 02:30:36  munhoz
 * inserting SVT online monitor
 *
 **************************************************************************/
 
#include "TObject.h"
#include "TDialogCanvas.h"
#include "TPad.h"
#include "TButton.h"
#include "TEllipse.h"
#include "TOrdCollection.h"
#include "TPaveText.h"
#include "TText.h"

#include <string.h>

//my classes
#include "temp.hh"
#include "StSvtView.hh"
#include "StSvtBarrel.hh"
#include "StSvtLadder.hh"

#define N_BARRELS 3
#define N_HYBRIDS 2
#define N_ANODES 240

ClassImp(StSvtView)

//--------------------------------------------------------------------------//
StSvtView::StSvtView(char *config, TDialogCanvas *dcan)
{
  buttons = new TOrdCollection;
  fConfig = config;

  ReadGeometry();
 
  UInt_t Width = 800 ;
  UInt_t Height = 600 ; 
  mWHRatio = (Float_t) Width/Height ;
  
  if ( (!strncmp(config,"SYST", strlen("SYST"))) || (!strncmp(config,"Y1L", strlen("Y1L"))) )
    Height = 300 ; 

  if (dcan) {
    mDialogCanvas = dcan ;
    mDialogCanvas->SetCanvasSize(Width,Height);
  }
  else
    mDialogCanvas = new TDialogCanvas("svtCanvas","SVT Monitor",Width,Height);

  mButtonSize = new Float_t[2] ;
  mButtonSize[0] = 0.025;    // half of button size
  mButtonSize[1] = 0.025;    // half of button size

  mDialogCanvas->Draw();
  mDialogCanvas->cd();

  if (!strncmp(config,"FULL", strlen("FULL"))) {
    mBarrel1 = new StSvtBarrel(1);
    mBarrel2 = new StSvtBarrel(2);
    mBarrel3 = new StSvtBarrel(3);
  
    mBarrelPad = new TPad("SVT","SVT",0.01,0.25,(0.99/mWHRatio),0.99);
    mBarrelPad->Draw();  
    DrawBarrels();
  
    mDialogCanvas->cd();
    mLadderPad = new TPad("Ladder","Ladder",(0.99/mWHRatio + 0.01),0.25,0.99,0.99);
    mLadderPad->Draw();
  }
  else if ((!strncmp(config,"SYST", strlen("SYST"))) || (!strncmp(config,"Y1L", strlen("Y1L"))) ) {
    mLadder = new StSvtLadder();
    mLadderPad = new TPad("Ladder","Ladder",0.01,0.25,0.99,0.99);

    mLadderPad->Draw();
    DrawLadder(0,0);
    UpdateLadderButtons(0,0,1,1);
  }

  mDialogCanvas->cd();
  mInfoPad = new TPad("Info","Info",0.01,0.01,0.99,0.25);
  mInfoPad->Draw();
  mInfoPad->cd();

  TText *text = new TText(0.1,0.85,"Data File name:");
  text->SetTextSize(0.15);
  text->SetTextColor(4);
  text->Draw();
  mFilePave = new TPaveText(0.05,0.68,0.95,0.83);
  mFilePave->Draw();
  mFilePave->SetTextAlign(22);
  mFilePave->SetTextSize(0.12);
  mFilePave->SetFillColor(0);

  text = new TText(0.1,0.43,"Pedestal File name:");
  text->SetTextSize(0.15);
  text->SetTextColor(4);
  text->Draw();
  mFilePedPave = new TPaveText(0.05,0.26,0.95,0.41);
  mFilePedPave->Draw();
  mFilePedPave->SetTextAlign(22);
  mFilePedPave->SetTextSize(0.12);
  mFilePedPave->SetFillColor(0);

  text = new TText(0.13,0.04,"Event Number:");
  text->SetTextSize(0.15);
  text->SetTextColor(4);
  text->Draw();
  mEventPave = new TPaveText(0.35,0.01,0.45,0.15);
  mEventPave->Draw();
  mEventPave->SetTextAlign(22);
  mEventPave->SetTextSize(0.15);
  mEventPave->SetFillColor(0);

  text = new TText(0.55,0.04,"Events in buffer:");
  text->SetTextSize(0.15);
  text->SetTextColor(4);
  text->Draw();
  mEventStatPave = new TPaveText(0.80,0.01,0.90,0.15);
  mEventStatPave->Draw();
  mEventStatPave->SetTextAlign(22);
  mEventStatPave->SetTextSize(0.15);
  mEventStatPave->SetFillColor(0);
}
//--------------------------------------------------------------------------//
StSvtView::~StSvtView()
{
  delete    mBarrel1;
  delete    mBarrel2;
  delete    mBarrel3;
}
//--------------------------------------------------------------------------//

void StSvtView::DrawBarrels()
{  
  mBarrelPad->cd();

  int barrelID, ladderID, n_ladders;
  char title[10];
  char command[200];
  TButton* button;  
  Int_t scale_factor = 40;

  TEllipse* barrel1 = new TEllipse(0.5,0.5,0.15);
  barrel1->SetLineStyle(2);
  barrel1->SetLineColor(2);
  barrel1->SetLineWidth(3);
  TEllipse* barrel2 = new TEllipse(0.5,0.5,0.25);
  barrel2->SetLineStyle(2);
  barrel2->SetLineColor(2);
  barrel2->SetLineWidth(3);
  TEllipse* barrel3 = new TEllipse(0.5,0.5,0.35);
  barrel3->SetLineStyle(2);
  barrel3->SetLineColor(2);
  barrel3->SetLineWidth(3);

  barrel1->Draw();
  barrel2->Draw();
  barrel3->Draw();

  for (int i=0;i<N_BARRELS;i++) {

    barrelID = i+1;

    switch(barrelID) {
    case 1:
      n_ladders = N_LADDERS_BARREL_1;
      break;
    case 2:
      n_ladders = N_LADDERS_BARREL_2;
      break;
    case 3:
      n_ladders = N_LADDERS_BARREL_3;
      break;
    }

    for (int j=0;j<n_ladders;j++) {

      ladderID = j+1;

      sprintf(title,"%d",ladderID);
      sprintf(command,"{aGraphicMonitor->DrawLadder(%d,%d);aGraphicMonitor->UpdateBarrelButtons(%d,%d);aMonitor->setBarrelID(%d);aMonitor->setLadderID(%d);aMonitor->resetHist(-1);menuBar->DrawHist(\"LADDERBUTTON\");}",barrelID,ladderID,barrelID,ladderID,barrelID,ladderID);
      
      mLadderPositionX[i][j] = mLadderPositionX[i][j]/scale_factor + 0.5 ;
      mLadderPositionY[i][j] = mLadderPositionY[i][j]/scale_factor + 0.5 ;       

      button = new TButton(title,command,mLadderPositionX[i][j] - mButtonSize[0], mLadderPositionY[i][j] - mButtonSize[1], mLadderPositionX[i][j] + mButtonSize[0],  mLadderPositionY[i][j] + mButtonSize[1]  );
      button->Draw();

      buttons->Add(button);
    }
  }
}

//--------------------------------------------------------------------------//

void StSvtView::DrawLadder(Int_t barrelID, Int_t ladderID)
{
  mLadderPad->cd();
  mLadderPad->Clear();
  
  switch(barrelID){
  case 1:
    mBarrel1->DrawLadder(ladderID);
    break;
  case 2:
    mBarrel2->DrawLadder(ladderID);
    break;
  case 3:
    mBarrel3->DrawLadder(ladderID);
    break;
  default:
    mLadder->DrawHorizontal(fConfig);
    break;
  }
}

//--------------------------------------------------------------------------//
void StSvtView::SetInfoEvent(int event, int buffer)
{  
  char temp[5];

  if (event>=0) {
    sprintf(temp,"%d",event);
    mEventPave->Clear();
    mEventPave->AddText(temp);
  }
  
  if (buffer>=0) {
    sprintf(temp,"%d",buffer);
    mEventStatPave->Clear();
    mEventStatPave->AddText(temp);
  }

  mInfoPad->Paint();
  mInfoPad->Update();
}

//--------------------------------------------------------------------------//
void StSvtView::SetInfoFile(const char* fileName)
{  
  mFilePave->Clear();

  if (TString(fileName) != "NULL")
    mFilePave->AddText(fileName);

  mInfoPad->Paint();
  mInfoPad->Update();
}

//--------------------------------------------------------------------------//
void StSvtView::SetInfoPedFile(const char* fileName)
{  
  mFilePedPave->Clear();

  if (TString(fileName) != "NULL")
    mFilePedPave->AddText(fileName);

  mInfoPad->Paint();
  mInfoPad->Update();
}

//--------------------------------------------------------------------------//

void StSvtView::UpdateBarrelButtons(Int_t barrelID, Int_t ladderID)
{
  TButton* button;
  TIter get(buttons);
  int nbuttons = buttons->GetSize();

  int iladder = 0;
  int ibarrel = 0;

  for (int ibutton=0;ibutton<nbuttons;ibutton++) {

    button = (TButton*) get.Next();
    button->SetFillColor(17);

    if ((ibarrel == (barrelID-1)) && (iladder == (ladderID-1)))
      button->SetFillColor(2);

    button->Modified();

    iladder++;
    if (((ibarrel==0) && (iladder > 7)) ||
	((ibarrel==1) && (iladder > 11))) {
      ibarrel++;
      iladder = 0;
    }
  }

}

//--------------------------------------------------------------------------//

void StSvtView::UpdateLadderButtons(Int_t barrelID, Int_t ladderID, Int_t waferID, Int_t hybridID)
{
  
  switch(barrelID){
  case 1 :
    mBarrel1->UpdateLadderButtons(ladderID, waferID, hybridID);
    break;
  case 2 :
    mBarrel2->UpdateLadderButtons(ladderID, waferID, hybridID);
    break;
  case 3 :
    mBarrel3->UpdateLadderButtons(ladderID, waferID, hybridID);
    break;
  default:
    mLadder->UpdateButtons(waferID, hybridID);
    break;
  }
}

//--------------------------------------------------------------------------//

void StSvtView::ReadGeometry()
{
  mLadderPositionX[0][1] = 6.125000;
  mLadderPositionY[0][1] = 0.000000; 
  mLadderPositionX[0][7] = 0.000002;
  mLadderPositionY[0][7] = 6.125000;  
  mLadderPositionX[0][5] = -6.125000;
  mLadderPositionY[0][5] = -0.000002;  
  mLadderPositionX[0][3] = -0.000002;
  mLadderPositionY[0][3] = -6.125000;
  mLadderPositionX[0][0] = 5.080564;
  mLadderPositionY[0][0] = 5.080562;  
  mLadderPositionX[0][6] = -5.080560;
  mLadderPositionY[0][6] = 5.080562;  
  mLadderPositionX[0][4] = -5.080564;
  mLadderPositionY[0][4] = -5.080562;
  mLadderPositionX[0][2] = 5.080559;
  mLadderPositionY[0][2] = -5.080563; 

  mLadderPositionX[1][0] = 5.092503;
  mLadderPositionY[1][0] = 8.820468;
  mLadderPositionX[1][1] = 9.591233;
  mLadderPositionY[1][1] = 5.537499;
  mLadderPositionX[1][2] = 10.184999;
  mLadderPositionY[1][2] = 0.000000; 
  mLadderPositionX[1][3] = 9.591228;
  mLadderPositionY[1][3] = -5.537502;
  mLadderPositionX[1][4] = 5.092495;
  mLadderPositionY[1][4] = -8.820469;  
  mLadderPositionX[1][5] = -0.000003;
  mLadderPositionY[1][5] = -11.075000; 
  mLadderPositionX[1][6] = -5.092501; 
  mLadderPositionY[1][6] = -8.820470;
  mLadderPositionX[1][7] = -9.591233;
  mLadderPositionY[1][7] = -5.537500;
  mLadderPositionX[1][8] = -10.184999;
  mLadderPositionY[1][8] = -0.000003;
  mLadderPositionX[1][9] = -9.591228;
  mLadderPositionY[1][9] = 5.537503;
  mLadderPositionX[1][10] = -5.092496;
  mLadderPositionY[1][10] = 8.820468;
  mLadderPositionX[1][11] = 0.000005;
  mLadderPositionY[1][11] = 11.075000;

  mLadderPositionX[2][3] = 13.995000;
  mLadderPositionY[2][3] = 0.000000; 
  mLadderPositionX[2][1] = 9.895964;
  mLadderPositionY[2][1] = 9.895959; 
  mLadderPositionX[2][15] = 0.000006;
  mLadderPositionY[2][15] = 13.995000;
  mLadderPositionX[2][13] = -9.895955;
  mLadderPositionY[2][13] = 9.895959;
  mLadderPositionX[2][11] = -13.995000;
  mLadderPositionY[2][11] = -0.000004;
  mLadderPositionX[2][9] = -9.895964;
  mLadderPositionY[2][9] = -9.895959;
  mLadderPositionX[2][7] = -0.000004;
  mLadderPositionY[2][7] = -13.995000;
  mLadderPositionX[2][5] = 9.895954;
  mLadderPositionY[2][5] = -9.895961;
  mLadderPositionX[2][2] = 13.798141;
  mLadderPositionY[2][2] = 5.715377;
  mLadderPositionX[2][0] = 5.715384;
  mLadderPositionY[2][0] = 13.798139;
  mLadderPositionX[2][14] = -5.715369;
  mLadderPositionY[2][14] = 13.798141;
  mLadderPositionX[2][12] = -13.798136;
  mLadderPositionY[2][12] = 5.715380;
  mLadderPositionX[2][10] = -13.798141;
  mLadderPositionY[2][10] = -5.715380;
  mLadderPositionX[2][8] = -5.715382;
  mLadderPositionY[2][8] = -13.798140;
  mLadderPositionX[2][6] = 5.715375;
  mLadderPositionY[2][6] = -13.798138;
  mLadderPositionX[2][4] = 13.798138;
  mLadderPositionY[2][4] =  -5.715376;

}
//--------------------------------------------------------------------------//
//--------------------------------------------------------------------------//
//--------------------------------------------------------------------------//

