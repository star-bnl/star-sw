/**********************************************************************
 *
 * $Id: StEStructHAdd.cxx,v 1.3 2005/03/08 21:56:42 porter Exp $
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
  const char* knd[]={"YtYt","PtPt",
                     "EtaEta","PhiPhi",
                     "PrEtaEta","PrPhiPhi",
                     "SuEtaEta","SuPhiPhi",
                     "SYtDYt","SPtDPt",
                     "DEtaDPhi","SEtaDPhi",
                     "PrDEtaDPhi","PrSEtaDPhi",
                     "SuDEtaDPhi","SuSEtaDPhi",
                     "Qinv","pt"};

  TFile* tfout=new TFile(outfile,"RECREATE");

  for(int i=0;i<2;i++){
    for(int j=0;j<3;j++){
      for(int k=0;k<18;k++){
        TString htype;
        if(k<17){
         htype+=base[i];htype+=tpe[j];
	} else if(i>0 || j>0){
          continue;
	}
        htype+=knd[k];        
	  
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

  tf->cd();
  TH1* tmp=(TH1*)tf->Get("NEventsSame");
  tfout->cd();
  tmp->Write();

  tf->cd();
  tmp=(TH1*)tf->Get("NEventsMixed");
  tfout->cd();
  tmp->Write();

  /*
  tf->cd();
  tmp=(TH1*)tf->Get("pt");
  tfout->cd();
  tmp->Write();
  */
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
 * Revision 1.3  2005/03/08 21:56:42  porter
 * fixed bug in StEStructHAdd.cxx and added diagnostic option in ptcorrelations to
 * view individual terms separately
 *
 * Revision 1.2  2005/03/03 01:33:04  porter
 * Added pt-correlations method to support and included
 * these histograms to the HAdd routine
 *
 * Revision 1.1  2004/07/01 00:37:13  porter
 * new code previously my StEStructHelper. Takes hists from correltation
 * pass and builds final ressults.  Also the StEStructHAdd.h is a simple
 * replacemnt for my sumyt.C macro which could be expanded later as needed.
 *
 *
 *
 *********************************************************************/



