#include "StEmcCalc.h"
#include "TGMsgBox.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StDaqLib/EMC/StEmcDecoder.h"
#include <stdlib.h>
#include "TROOT.h"
#include "TRootHelpDialog.h"

ClassImp(StEmcCalc)

TROOT* root;

Int_t   flag[4][5]={{2,1,3,1,2}, {2,1,3,1,2}, {2,1,3,2,0}, {2,1,3,2,0}};

//--------------------------------------------------------

StEmcCalc::StEmcCalc(const TGWindow *p, UInt_t w, UInt_t h,TROOT* r):TGMainFrame(p, w, h)
{
	Int_t   detstatus[4]={1,0,1,1};

	TString labelRadio[4][5]={{"Eta, Phi","Software Id","Mod, Eta, Sub","Daq Index","Crate, index"},
                          	{"Eta, Phi","Software Id","Mod, Eta, Sub","Daq Index","Crate, index"},
                          	{"Eta, Phi","Software Id","Mod, Eta, Sub","Daq RDO,Index",""},
                          	{"Eta, Phi","Software Id","Mod, Eta, Sub","Daq RDO,Index",""}};

  root=r;
    
  SetWMSizeHints(w, h, w, h, 0, 0) ; // no resize ...
  help=new TGTextButton(this, "Help", 1001);
  exit=new TGTextButton(this, "Quit", 1002);
  AddFrame(help);
  AddFrame(exit);
  help->MoveResize(w/3-30,h-30,60,25);
  exit->MoveResize(2*w/3-30,h-30,60,25);
  
  for(Int_t det=0;det<4;det++)
  {
    group[det]=new TGGroupFrame(this,detname[det].Data());
    AddFrame(group[det]);
    Int_t framesizex=272,framesizey=142;
    Int_t x=6+(det/2)*(framesizex+2);
    Int_t y=6+(det%2)*(framesizey+2);
    group[det]->MoveResize(x,y,framesizex,framesizey); 
       
    button1[det]=new TGTextButton(this, "Calc", det+1);
    AddFrame(button1[det]);
    x+=framesizex/3-25;
    y+=framesizey-22;
    button1[det]->MoveResize(x,y,50,18);
    if(detstatus[det]==0) button1[det]->SetState(kButtonDisabled);

    button2[det]=new TGTextButton(this, "Clear ", det+11);
    AddFrame(button2[det]);
    x+=framesizex/3;
    button2[det]->MoveResize(x,y,50,18);
    if(detstatus[det]==0) button2[det]->SetState(kButtonDisabled);
    
    for(Int_t type=0;type<5;type++)
    {
      if(flag[det][type]>0)
      {
        radio[det][type]=new TGRadioButton(this,labelRadio[det][type].Data(),101+det*10+type);
        group[det]->AddFrame(radio[det][type]);
        x=10+(det/2)*(framesizex+2);
        y=22+(det%2)*(framesizey+2) + type*20;
        radio[det][type]->Move(x,y);
        radioSt[det][type]=kFALSE;
        if(type==1) {radio[det][type]->SetState(kButtonDown);radioSt[det][type]=kTRUE;}
        if(detstatus[det]==0) radio[det][type]->SetState(kButtonDisabled);
      }
      for(Int_t item=0;item<flag[det][type];item++)
      {
        coord[det][type][item]=new TGTextEntry(this,"",201+det*10+type*10+item);
        group[det]->AddFrame(coord[det][type][item]);
        x=10+(det/2)*(framesizex+2) + item*52 + 110;
        y=22+(det%2)*(framesizey+2) + type*20;
        coord[det][type][item]->MoveResize(x,y,50,20);
        if(detstatus[det]==0) coord[det][type][item]->SetEnabled(kFALSE);
        if(type!=1) coord[det][type][item]->SetEnabled(kFALSE);
      }     
    }
    geom[det]=StEmcGeom::instance(detname[det].Data());
  } 
           
  MapSubwindows();

  SetWindowName("EMC Calculator");
  SetIconName("EMC Calculator");

  MapWindow();
           
}
//-------------------------------------------------------------------------
StEmcCalc::~StEmcCalc()
{
}
//-------------------------------------------------------------------------
Bool_t StEmcCalc::ProcessMessage(Long_t msg, Long_t parm1, Long_t)
{
  Int_t det,type;
  switch (GET_MSG(msg)) 
  {
    case kC_COMMAND:
      switch (GET_SUBMSG(msg)) 
      {
         case kCM_BUTTON:
         
           if(parm1==1001) {Help(); return kTRUE;}
           if(parm1==1002) 
           {
             TString title="Quit Program";
             TString msg="Quit EMC Coordinate Calculator?";
             Int_t retCode;
             new TGMsgBox(fClient->GetRoot(),this,title.Data(),msg.Data(),kMBIconQuestion,kMBYes+kMBNo,&retCode);
             if(retCode==kMBYes) gROOT->ProcessLine(".q");
             return kTRUE;
           }
           if(parm1<11) det=parm1-1; else det=parm1-11;
           if(parm1>10) {ClearAll(det); return kTRUE;}
           type=0;
           for(Int_t t=0;t<5;t++) if(flag[det][t]>0) if(radioSt[det][t]) type=t;
           Convert(det,type);
           //cout <<"Detector "<<det<<"  type "<<type<<endl;
           break;
           
         case kCM_RADIOBUTTON:
          // get detector, type;
           Int_t tmp=parm1-101;
           det=tmp/10;
           type=tmp%10;
           //cout <<"Detector "<<det<<"  type "<<type<<endl;
           radioSt[det][type]=kTRUE;
           for(Int_t t=0;t<5;t++) 
           {
            if(t!=type && flag[det][t]>0) 
            {
              radio[det][t]->SetState(kButtonUp);
              radioSt[det][t]=kFALSE;
            }
            for(Int_t i=0;i<flag[det][t];i++)
              if(t!=type) coord[det][t][i]->SetEnabled(kFALSE);
              else coord[det][t][i]->SetEnabled(kTRUE);
           }
           break;
       }
  }
  return kTRUE;
}
//-------------------------------------------------------------------------
Bool_t StEmcCalc::ClearAll(Int_t det)
{
  Int_t type=0;
  radio[det][type]->SetState(kButtonDown);
  radioSt[det][type]=kTRUE;
  
  for(Int_t t=0;t<5;t++) 
  {
    if(t!=type && flag[det][t]>0) 
    {  radio[det][t]->SetState(kButtonUp);
      radioSt[det][t]=kFALSE;
    }        
    for(Int_t i=0;i<flag[det][t];i++)
    {
      if(t!=type) coord[det][t][i]->SetEnabled(kFALSE);
      else coord[det][t][i]->SetEnabled(kTRUE);
      coord[det][t][i]->SetText("");
    }
  }
  return kTRUE;
}
//-------------------------------------------------------------------------
Bool_t StEmcCalc::Convert(Int_t det,Int_t type)
{
  Float_t eta,phi;
  Int_t m=0,e=0,s=0,id=0,did=0,rdo=0,st=0,crate=0,crateIndex=0;
  if(!daqconv) daqconv=new StEmcDecoder(20300101,0);
	
  // eta, phi
  if(type==0)
  {
    eta = atof(coord[det][type][0]->GetText());
    phi = atof(coord[det][type][1]->GetText());
    // get m, e, s first
    st=geom[det]->getBin(phi,eta,m,e,s);
    if (e==-1 || s==-1) {BadCoord(det); return kFALSE;}
    if (st==1) {BadCoord(det); return kFALSE;}
    // get software id
    geom[det]->getId(m,e,s,id);
    // get daq id (detector dependent)
    if(!FindDaq(det,m,e,s,did,rdo,crate,crateIndex)) {did=0;rdo=0; NotPossible(det,"Not possible to get the Daq coordinates");}
    // fill text spaces
    FillSpaces(det,eta,phi,id,m,e,s,did,rdo,crate,crateIndex);
  }
  
  // software id
  if(type==1)
  {
    id = atoi(coord[det][type][0]->GetText());
    // get m,e,s
    st = geom[det]->getBin(id,m,e,s);
    if(st==1) {BadCoord(det); return kFALSE;}
    // get eta,phi
    geom[det]->getEtaPhi(id,eta,phi);
    // get daq id (detector dependent)
    if(!FindDaq(det,m,e,s,did,rdo,crate,crateIndex)) {did=0; rdo=0; NotPossible(det,"Not possible to get the Daq coordinates");}
    // fill text spaces
    FillSpaces(det,eta,phi,id,m,e,s,did,rdo,crate,crateIndex);
    
  }
  
  // module, eta, sub
  if(type==2)
  {
    m = atoi(coord[det][type][0]->GetText());
    e = atoi(coord[det][type][1]->GetText());
    s = atoi(coord[det][type][2]->GetText());
    //get id
    st = geom[det]->getId(m,e,s,id);
    if(st==1) {BadCoord(det); return kFALSE;}
    // get eta,phi
    geom[det]->getEtaPhi(id,eta,phi);
    // get daq id (detector dependent)
    if(!FindDaq(det,m,e,s,did,rdo,crate,crateIndex)) {did=0;rdo=0; NotPossible(det,"Not possible to get the Daq coordinates");}
    // fill text spaces
    FillSpaces(det,eta,phi,id,m,e,s,did,rdo,crate,crateIndex);
  }
  
  // daq id's
  if(type==3)
  {
    
    if(det==0 || det==1) // bemc or bprs
    {
      did = atoi(coord[det][type][0]->GetText());
      rdo = 0;
      st=daqconv->GetTowerIdFromDaqId(did,id);
      if(st==0) {BadCoord(det); return kFALSE;}
    }
    else // bsmde and bsmdp
    {
      rdo = atoi(coord[det][type][0]->GetText());
      
      if(rdo!=0) {NotImplemented(det); return kFALSE;} // just one fiber ready for y2001
      
      did = atoi(coord[det][type][1]->GetText());
      
      Int_t det1;
      st=daqconv->GetSmdCoord(rdo,did,det1,m,e,s);
      if(st==0) {BadCoord(det); return kFALSE;}
      if((det1-1)!=det) {NotPossible(3,"This is not a valid rdo and index"); return kFALSE;}
      st = geom[det]->getId(m,e,s,id);
      if(st==1) {BadCoord(det); return kFALSE;}
    }
    // get m,e,s
    geom[det]->getBin(id,m,e,s);
    // get eta,phi
    geom[det]->getEtaPhi(id,eta,phi);
    // fill text spaces
    FillSpaces(det,eta,phi,id,m,e,s,did,rdo,crate,crateIndex);   
  }

  // crate and position in the crate (tower and pre-shower only)
  if(type==4)
  {
    crate = atoi(coord[det][type][0]->GetText());
    crateIndex = atoi(coord[det][type][1]->GetText());
    // get id
    st=daqconv->GetTowerIdFromCrate(crate,crateIndex,id);
    if(st==0) {BadCoord(det); return kFALSE;}
    // get m,e,s
    geom[det]->getBin(id,m,e,s);
    // get eta,phi
    geom[det]->getEtaPhi(id,eta,phi);
    // get daq index
    Int_t id2;
    for(Int_t tmp=0;tmp<4800;tmp++)
    {
      st=daqconv->GetTowerIdFromDaqId(tmp,id2);
      if(id==id2) did=tmp;
    }
    FillSpaces(det,eta,phi,id,m,e,s,did,rdo,crate,crateIndex);
  }
  
  return kTRUE;
}
//-------------------------------------------------------------------------
Bool_t StEmcCalc::FindDaq(Int_t det,Int_t m,Int_t e,Int_t s,Int_t& index,Int_t& rdo,Int_t& crate,Int_t& crateIndex)
{
  if ((m<45 || m>60)&& det>1) return kFALSE; // only year2001 conf for SMD
  Int_t st,det1,m1,e1,s1,id1,id2;
  if (det==0 || det==1) // bemc or bprs
  {
    geom[det]->getId(m,e,s,id1);
    for(Int_t tmp=0;tmp<4800;tmp++)
    {
      st=daqconv->GetTowerIdFromDaqId(tmp,id2);
      if(id1==id2) {index=tmp; daqconv->GetTowerCrateFromDaqId(index,crate,crateIndex); return kTRUE;}
    }
    return kFALSE;
  }
  else
  {
    rdo=0; // only one fiber for year 2001
    for(Int_t tmp=1;tmp<=4800;tmp++)
    {
      st=daqconv->GetSmdCoord(rdo,tmp,det1,m1,e1,s1);
      if((det+1)==det1 && m==m1 && e==e1 && s==s1) {index=tmp; return kTRUE;}
    }
    return kFALSE;
  }
  return kFALSE;
}
//-------------------------------------------------------------------------
Bool_t StEmcCalc::BadCoord(Int_t det)
{
  TString title="Coordinate Error";
  TString msg="This is not a valid coordinate set\nfor detector "+detname[det];
  new TGMsgBox(fClient->GetRoot(),this,title.Data(),msg.Data(),kMBIconExclamation,kMBOk);
  return kTRUE;
}
//-------------------------------------------------------------------------
Bool_t StEmcCalc::NotImplemented(Int_t det)
{
  TString title="Value not corrected";
  TString msg="The value you entered is not implemented yet\nfor detector "+detname[det];
  new TGMsgBox(fClient->GetRoot(),this,title.Data(),msg.Data(),kMBIconExclamation,kMBOk);
  return kTRUE;
}
//-------------------------------------------------------------------------
Bool_t StEmcCalc::NotPossible(Int_t det,char* text)
{
  TString title="Warning !";
  char msg[80];
  sprintf(msg,"%s\nfor detector %s",text,detname[det].Data());
  new TGMsgBox(fClient->GetRoot(),this,title.Data(),msg,kMBIconExclamation,kMBOk);
  return kTRUE;
}
//-------------------------------------------------------------------------
Bool_t StEmcCalc::FillSpaces(Int_t det,Float_t eta,Float_t phi,Int_t id,
                           Int_t m,Int_t e,Int_t s,Int_t did,Int_t rdo,
                           Int_t crate,Int_t crateIndex)
{
  char text[20];
  
  sprintf(text,"%7.5f",eta); coord[det][0][0]->SetText(text);  // eta
  sprintf(text,"%7.5f",phi); coord[det][0][1]->SetText(text);  // phi
  
  sprintf(text,"%4d",id);    coord[det][1][0]->SetText(text);  // software id
    
  sprintf(text,"%3d",m);     coord[det][2][0]->SetText(text);  // module
  sprintf(text,"%2d",e);     coord[det][2][1]->SetText(text);  // eta
  sprintf(text,"%2d",s);     coord[det][2][2]->SetText(text);  // sub
    
  if(det>1) // smd
  {
    sprintf(text,"%2d",rdo); coord[det][3][0]->SetText(text);  // rdo
    sprintf(text,"%4d",did); coord[det][3][1]->SetText(text);  // daq id
  }
  else // tower and prs
  {
    sprintf(text,"%4d",did);        coord[det][3][0]->SetText(text);  // daqid
    sprintf(text,"%4d",crate);      coord[det][4][0]->SetText(text);  // crate
    sprintf(text,"%4d",crateIndex); coord[det][4][1]->SetText(text);  // crateIndex
  }
  return kTRUE;
}
//-------------------------------------------------------------------------
Bool_t StEmcCalc::Help()
{
  TRootHelpDialog* help=new TRootHelpDialog(this,"EMC Calculator Help",520,400);
  help->AddText("EMC Calculator                                                        ");
  help->AddText("======================================================================");
  help->AddText("by A. A. P. Suaide (suaide@physics.wayne.edu)                         ");
  help->AddText("                                                                      ");
  help->AddText("   This utility is designed to the general crew or EMC expert who     ");
  help->AddText("needs to convert any of the EMC identifycation numbers of any EMC     ");
  help->AddText("subdetector. To use it, just click on the type of identification      ");
  help->AddText("you would like to use as input, fill the corresponding input box(es)  ");
  help->AddText("and click on 'Calc' button. The 'Clear' button erases all numbers     ");
  help->AddText("for a given detector. The implemented identifications numbers are:    ");
  help->AddText("                                                                      ");
  help->AddText("     1. Eta, phi        - physical eta and phi numbers                ");
  help->AddText("                          -1  <= eta <= 1                             ");
  help->AddText("                          -pi <= phi <= pi                            ");
  help->AddText("                                                                      ");
  help->AddText("     2. Software Id     - the id used for offline software            ");
  help->AddText("                          1 <= id <= 4800  for bemc and bprs          ");
  help->AddText("                          1 <= id <= 18000 for bsmde and bsmdp        ");
  help->AddText("                                                                      ");
  help->AddText("     3. Mod, Eta, Sub   - module number, eta division and phi division");
  help->AddText("                          1 <= Mod <= 120                             ");
  help->AddText("                          1 <= Eta <= 20   for bemc and bprs          ");
  help->AddText("                          1 <= Eta <= 150  for bsmde                  ");
  help->AddText("                          1 <= Eta <= 10   for bsmdp                  ");
  help->AddText("                          1 <= Sub <= 2    for bemc and bprs          ");
  help->AddText("                               Sub  = 1    for bsmde                  ");
  help->AddText("                          1 <= Sub <= 15   for bsmdp                  ");
  help->AddText("                                                                      ");
  help->AddText("     4. Daq index       - The Daq identification for bemc and bprs    ");
  help->AddText("                          0 <= Daq Index <= 4799                      ");
  help->AddText("        Daq RDO,Index   - The Daq identification for bsmde and bsmdp  ");
  help->AddText("                          0 <= RDO   <= 7 (fiber number)              ");
  help->AddText("                          0 <= Index <= 4799                          ");        
  help->AddText("                                                                      ");
  help->AddText("     5. Crate,index     - The crate number and position inside it     ");
  help->AddText("                          for towers and pre-shower only              ");
  help->AddText("                          1 <= crate   <= 30                          ");
  help->AddText("                          0 <= Index   <= 159                         ");        
  help->AddText("======================================================================");
  help->Popup();
  return kTRUE;
}




