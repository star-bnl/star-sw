/**********************************************************************
 *
 * $Id: StEStructHAdd.cxx,v 1.1 2004/07/01 00:37:13 porter Exp $
 *
 * Author: Jeff Porter 
 *
 **********************************************************************
 *
 * Description: Simple helper class for calculating
 *              delta-rho, delta-rho/rho, and delta-rho/sqrt(rho)
 *              plus some other goodies
 *
 ***********************************************************************/
#include "StEStructHAdd.h"

#include "Stiostream.h"
#include "TH1.h"
#include "TFile.h"

ClassImp(StEStructHAdd)

  //------------------------------------------------------------------------
void StEStructHAdd::addCuts(const char* outfile, TFile* tf, int* nlist, int ntot){

  const char* base[]={"Sib","Mix"};
  const char* tpe[]={"pp","pm","mm"};
  const char* knd[]={"MtMt","EtaEta","PhiPhi","PtPt",
                     "SMtDMt","SEtaDEta","SPhiDPhi","SPtDPt",
                     "DMtDPhi","DEtaDMt","DEtaDPhi","IM",
                     "SMtSPhi","SEtaSMt","SEtaSPhi"};

  TFile* tfout=new TFile(outfile,"RECREATE");

  for(int i=0;i<2;i++){
    for(int j=0;j<3;j++){
      for(int k=0;k<15;k++){
        TString htype(base[i]);htype+=tpe[j];htype+=knd[k];
        TH1* outhist=0;
          for(int n=0;n<ntot;n++){
            TString fullName(htype.Data()); fullName+=nlist[n];
            tf->cd();
            TH1* tmp=(TH1*)tf->Get(fullName.Data());
            if(!tmp){
              cout<<"Cannot find "<<fullName.Data()<<endl;
              break;
            }
            if(n==0){
              outhist=(TH1*)tmp->Clone();
              outhist->SetName(htype.Data());
              outhist->SetTitle(tmp->GetTitle());
            } else {
              outhist->Add(tmp);
            }
          }
          tfout->cd();
          if(outhist){
            outhist->Write();
            delete outhist;
	  }
      }
    }
  }

  tfout->Close();
};

//--------------------------------------------------------------------------
void StEStructHAdd::addCuts(const char* outfile, const char* infile, int* nlist, int ntot){

  TFile* tf=new TFile(infile);
  if(tf) addCuts(outfile,tf,nlist,ntot);
  return;
};

/***********************************************************************
 *
 * $Log: StEStructHAdd.cxx,v $
 * Revision 1.1  2004/07/01 00:37:13  porter
 * new code previously my StEStructHelper. Takes hists from correltation
 * pass and builds final ressults.  Also the StEStructHAdd.h is a simple
 * replacemnt for my sumyt.C macro which could be expanded later as needed.
 *
 *
 *
 *********************************************************************/



