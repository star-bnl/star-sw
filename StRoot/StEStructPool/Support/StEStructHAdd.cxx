/**********************************************************************
 *
 * $Id: StEStructHAdd.cxx,v 1.5 2006/04/06 01:09:46 prindle Exp $
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
void StEStructHAdd::addCuts(const char* outfile, TFile* inFile,
                            int* nlist, int ntot, int all ){

  const char* base[]={"Sib","Mix"};
  const char* tpe[]={"pp","pm","mp","mm"};
  const char* knd[]={"YtYt","PtPt", "XtXt",
                     "EtaEta",    "PhiPhi",
                     "PrEtaEta",  "PrPhiPhi",
                     "PaEtaEta",  "PaPhiPhi",
                     "PbEtaEta",  "PbPhiPhi",
                     "SYtDYt",    "SPtDPt",
                     "DEtaDPhi",  "SEtaDPhi",
                     "DYtDEta",   "DYtDPhi",
                     "PrDEtaDPhi","PrSEtaDPhi",
                     "PaDEtaDPhi","PaSEtaDPhi",
                     "PbDEtaDPhi","PbSEtaDPhi",
                     "Qinv","pta","ptb"};

  TFile* outFile = new TFile(outfile,"RECREATE");

  for(int i=0;i<2;i++){
    for(int j=0;j<4;j++){
      for(int k=0;k<26;k++){
        TString htype;
        if (k<24) {
         htype+=base[i];htype+=tpe[j];
	} else if(i>0 || j>0){
          continue;
	}
        htype+=knd[k];        
	  
        TH1* outhist=0;
        for(int n=0;n<ntot;n++){
          TString fullName(htype.Data()); fullName+=nlist[n];
          inFile->cd();
          TH1* tmp=(TH1*)inFile->Get(fullName.Data());
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
        if ((k>=24)  && all) {
            TH1* tmp=(TH1*)inFile->Get("ptAll");
            if (tmp) {
                outhist = (TH1*)tmp->Clone();
            }
        }
        outFile->cd();
          if(outhist){
            outhist->Write();
            delete outhist;
	  }
      }
    }
  }

  inFile->cd();
  TH1* tmp=(TH1*)inFile->Get("NEventsSame");
  outFile->cd();
  tmp->Write();

  inFile->cd();
  tmp=(TH1*)inFile->Get("NEventsMixed");
  outFile->cd();
  tmp->Write();

  outFile->Close();
};

//--------------------------------------------------------------------------
void StEStructHAdd::addCuts(const char* outfile, const char* infile,
                            int* nlist, int ntot, int all){

  TFile* tf=new TFile(infile);
  if(tf) addCuts(outfile,tf,nlist,ntot,all);
  return;
};

/***********************************************************************
 *
 * $Log: StEStructHAdd.cxx,v $
 * Revision 1.5  2006/04/06 01:09:46  prindle
 * Calculating pt for each cut bin caused changes in HAdd.
 * The splitting of +- into +- and -+ caused changes in Support.
 *
 * Revision 1.4  2005/09/29 17:42:14  msd
 * Added Xt to hadd
 *
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



