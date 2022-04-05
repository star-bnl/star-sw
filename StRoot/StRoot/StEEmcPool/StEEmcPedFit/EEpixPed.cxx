// \class  EEpixPed
// \author Jan Balewski
// \edited Justin Stevens

#include <stdio.h>
#include <assert.h>
#include <string.h>


#include <TH1.h>
#include <TF1.h>
#include <TFile.h>

#include <TCanvas.h>

#include "StEEmcUtil/EEfeeRaw/EEdims.h"
#include "EEpixPed.h"

ClassImp(EEpixPed)

//-------------------------------------------
//-------------------------------------------
EEpixPed::EEpixPed(TString fname) {
 tFd=new TFile(fname);
  if(tFd==0 || !tFd->IsOpen()) {
    printf("file not opened, Ignore\n");
    return;
  }
  //t-d>ls();
  printf("file '%s' opened\n",fname.Data());
  HList=new TObjArray(0);
 
}  

//-----------------------------------------
//-------------------------------------------
void EEpixPed::findTowerHisto() {
  assert(tFd->IsOpen());
  //printf("AA c_x1=%d c_x2=%d cMinInt=%d c_xFit=%d\n",c_x1,c_x2,c_minInt,c_xFit);
  // assert(1==2);
  int ntot_hist=0;
  int i,j,k;
  for(i=0;i<MaxSectors ;i++) { // sectors
    int n_hist=0;
    for(j=0;j<MaxSubSec;j++) { // subsectors
      for(k=0;k<MaxEtaBins;k++) { // pseudorapidity
        char tt1[100];
        sprintf(tt1,"a%2.2dT%c%2.2d",i+1,'A'+j,k+1);
	//printf("tryJ2 %s ...\n",tt1);
        TH1F* h= (TH1F*) tFd->Get(tt1); 
	if(h==0) {
	  //printf("histo zero !!!  \n");
	  continue;
	}
	//printf("S %s nEnt=%f  cMinInt=%d\n",tt1, h->GetEntries(),c_minInt);
	// h->Draw();
	if (h->GetEntries() < c_minInt)  {
	  //printf("Not reaching bottom line !!! \n");
	  continue;  
	}
	// 3 channels with known (unfixable) stuck bits
	bool A=strstr(tt1,"a04TB07")>0;
	bool B=strstr(tt1,"a06TA07")>0;
	bool C=strstr(tt1,"a08TB07")>0;
	if (A||B||C){
	  h->Rebin(4);
	}
	HList->Add(h);
	n_hist++;
	//	break;
      }
      // break;
    }
    printf("sector %d found histo %d\n ",i+1,n_hist);
    ntot_hist+=n_hist;
    n_hist=0;
  }
  printf("\n");

  printf("%s() Tower histo initialized, %d found\n",GetName(),ntot_hist);
  
}

//-----------------------------------------
//-------------------------------------------
void EEpixPed::findMapmtHisto() {
  assert(tFd->IsOpen());
  int ntot_hist=0;
  int i=0,ist;
  int uv; 

  for(i=0;i<MaxSectors ;i++) { // sectors
    int secID=i+1;
    printf("searching MAMT histsos for sect %d\n",secID);
    int n_hist=0;
    for(uv=0;uv<MaxSmdPlains;uv++){
      for(ist=0;ist<MaxSmdStrips;ist++) {
	char tt1[100];
	sprintf(tt1,"a%2.2d%c%3.3d",secID,'U'+uv,ist+1);
	//printf("try %s ... \n",tt1);
        TH1F* h= (TH1F*) tFd->Get(tt1); 
	if(h==0){
	  // printf("histo zero!!!\n");
	  continue;
	}
	//printf("S %s %f\n",tt1, h->GetEntries());
	
	//if (h->GetEntries() < c_minInt)  continue;  
	HList->Add(h);
	n_hist++;
	//break;
	}

    }

    // pre/post
    int j,k;
    for(j=0;j<MaxSubSec;j++) { // subsectors
      for(k=0;k<MaxEtaBins;k++) { // pseudorapidity
        char tt1[100];
	int l;
	char preL[4]="PQR";
	for(l=0;l<3;l++) {
	  sprintf(tt1,"a%2.2d%c%c%2.2d",i+1,preL[l],'A'+j,k+1);
	  TH1F* h= (TH1F*) tFd->Get(tt1); 
	  if(h==0) continue;
	  //printf("S %s %f\n",tt1, h->GetEntries());
	  //if (h->GetEntries() < c_minInt)  continue;  
	  HList->Add(h);
	  n_hist++;
	}
       	//break;
      }
    }

    printf("sector %d found histo %d  \n ",i+1,n_hist);
    ntot_hist+=n_hist;
    n_hist=0;
    }
  printf("\n");
  
  printf("%s() MAPMT histo initialized, %d found\n",GetName(),ntot_hist);
  
}

//-------------------------------------------
//-------------------------------------------
void EEpixPed::fitHisto(TString fPath){
  printf("Find pedestal within ADC limits [%d,%d], nMin>%d max+/-%d\n",c_x1,c_x2,c_minInt,c_xFit);
  assert(c_x1>0);
  assert(c_x2>c_x1);
  assert(c_x2<4096);
  assert(c_xFit>0);
  static int xfile= 0;
  FILE *fout;
  TString ouT=fPath+"T.log";
  TString ouM=fPath+"Mpt.log";
  printf("Achtung!!! - static variable xfile = %d\n",xfile);
  printf(" outT='%s' outM='%s'\n",ouT.Data(), ouM.Data());

  if(xfile % 2 == 0)
    { fout=fopen(ouT.Data(),"w");}
  else
    { fout=fopen(ouM.Data(),"w");}
   assert(fout); 

  // printf("name     Nentries chi2/ndf gaus_cons gaus_mean gaus_sig sig_con sig_mean sig_sig \n"); 
  fprintf( fout,"name   QA[ILHNWX]  Nintegr chi2/ndf amplitude   mean     sigma   erAmpl   erMean  erSigma\n"); 

  TCanvas *c=new TCanvas(); c=c;// just to get nice printout
  
  TF1* fit = new TF1("fPeak","gaus");
  fit->SetLineColor(kGreen);
  
  //............................... LOOP over histo  .........
  int ja;
  for(ja=0;ja<HList->GetEntries();ja++) {
        TH1F* h=(TH1F*)HList->UncheckedAt(ja);     
    assert(h);
    int nb=h->GetNbinsX();   
    h->SetAxisRange(c_x1,c_x2);
    // printf("F: %s -->Int=%.1f Ent=%1.f\n",h->GetName(),  h->Integral(),h->GetEntries() );
    
    if (h->Integral() < c_minInt) {
      fprintf(fout,"%s %8.0f  failed\n",h->GetName(), h->Integral());
      continue;  
    }
    //this histogram has some "reasonable number of entries"
    
    // localize pedestal peak 
    float *x=h->GetArray();
    
    float ym=0,ymax=0;
    int i;
    int j=-1,jfind=-1;
    
    for(i=c_x1;i<=c_x2;i++){
      if(ym>x[i]) continue;
      ym=x[i];
      j=i;
    }

    float x0=h->GetBinCenter(j);
     
    float x1=x0-c_xFit;
    if(x1<c_x1) x1=c_x1;
    float x2=x0+c_xFit;
    if(x2>c_x2) x2=c_x2;
    
    for(i=0;i<=nb;i++)
    {  if(ymax < x[i])
      {  ymax = x[i];
         jfind= i;   }
    }
    float sum = 0;
    for(i=jfind-5;i<=jfind+5;i++){
      sum +=x[i];
    }

    //h->Fit(fit,"OQN+","",x1,x2); // fit hist with gaussian 
    //h->Fit(fit,"OQ+W","",x1,x2); // fit hist with gaussian 
    // fit->Print();
    bool A=strstr(h->GetName(),"a04TB07")>0;
    bool B=strstr(h->GetName(),"a06TA07")>0;
    bool C=strstr(h->GetName(),"a08TB07")>0;
    if (A||B||C){
      //printf("\n\n Target chan=%s\n\n", h->GetName());
      //printf("x1=%f,     x2=%f\n",x1, x2);
      //printf("x0=%f,     j=%d\n",x0, j);
      x1=0;
      x2=60;
      //h->Fit(fit,"O+W","",x1,x2);
    }
    h->Fit(fit,"OQ+W","",x1,x2);
     
    char qaString[7]="......";
    if(h->Integral()<qa_minInt) qaString[0]='I';
    if(fit->GetParameter(1)<qa_pedLow) qaString[1]='L';
    if(fit->GetParameter(1)>qa_pedHigh) qaString[2]='H';
    if(fit->GetParameter(2)<qa_pedSigMin) qaString[3]='N';
    if(fit->GetParameter(2)>qa_pedSigMax) qaString[4]='W';
  
    fprintf(fout,"%s  %6s %8.0f  %8.3f %9.3f %9.3f %8.3f %7.3f %7.3f %7.3f \n", 
	    h->GetName(),
	    qaString,
	    h->Integral(),
	    fit->GetChisquare()/fit->GetNDF(),
	    fit->GetParameter(0),   // amplitude
	    fit->GetParameter(1),   // pedestal centroid
	    fit->GetParameter(2),   // sigma of gaussian
	    fit->GetParError(0),    // error in amplitude
	    fit->GetParError(1),    // error in centroid
	    fit->GetParError(2)    // error in sigma
	    //sum
	    );
      
    //      h->Print();
    //      fit->Print();
    //    break;
  }// end of loop over histos
  fclose(fout);
  xfile++;
}

//-------------------------------------------
//-------------------------------------------
void EEpixPed:: savePedTable( char *mode, TString fname){
  //....write pedestal values to ped.sector files
  printf(" write file of pedestal values to '%s'\n",fname.Data());
  int secID;
  for(secID=1;secID<=MaxSectors ;secID++) { // sectors
    char txt[100];
    sprintf(txt,"%s.sector%2.2d",fname.Data(),secID);
    printf("%s ....",txt);
    FILE *fd = fopen(txt,mode);
    assert(fd);
    if(mode[0]=='w')  fprintf(fd,"#sector%2.2d/eemcPMTped\n",secID);
    sprintf(txt,"a%2.2d",secID);
    
    //............................... LOOP over histo  .........
    int j;
    int nOK=0;
    for(j=0;j<HList->GetEntries();j++) {
      TH1F* h=(TH1F*)HList->UncheckedAt(j);   
      assert(h);
      if(strncmp(txt,h->GetName(),3) ) continue;
      TF1* ff=h->GetFunction("fPeak");
      //printf("cc %s \n",h->GetName());
      if(ff==0) continue;
      // Write values to a text file
      fprintf(fd,"%s  %6.2f  %6.2f\n", h->GetName()+1,ff->GetParameter(1), ff->GetParameter(2));
      nOK++;
    }// end of loop within a sector
    fclose(fd);
    printf(" nOK=%d\n",nOK);
  } //end of loop over sectors

  
#if 0
  //.......write pedestal values to ped.crate files
  printf(" write file of pedestal values by crate to '%s'\n",fname.Data());
  int crateID;
  int MaxCrates=111;
  int crate;
  int chan;
  int nOK=0;
  for(crateID=1;crateID<=MaxCrates ;crateID++) { // crates
    char txt[100];
    sprintf(txt,"%s.crate",fname.Data());
    if(crateID==1) printf("%s ....",txt);
    else mode="a";
    FILE *fd = fopen(txt,mode);
    assert(fd);
    sprintf(txt,"%03d",crateID);
    
    //............................... LOOP over histo  .........
    int j;
    for(j=0;j<HList->GetEntries();j++) {
      TH1F* h=(TH1F*)HList->UncheckedAt(j);   
      assert(h);
      
      char title[200];
      strcpy(title,h->GetTitle());
      //grab crate # and channel # from title
      char *p = strchr(title,'=');
      crate=atoi(p+1);
      chan=atoi(p+5);
      
      if(crateID != crate) continue;
      //if(chan==1) printf("t=%s= cr=%d ch=%d\n",title,crate,chan);
      TF1* ff=h->GetFunction("fPeak");
      //printf("cc %s \n",h->GetName());
      if(ff==0) continue;
      // Write values to a text file
      fprintf(fd,"%03d  %03d  %6.2f  %6.2f\n", crate, chan, ff->GetParameter(1), ff->GetParameter(2));
      nOK++;
      
    }// end of loop within a crate
    fclose(fd);
  } //end of loop over crate
  printf(" nOK=%d\n",nOK);
#endif

}

//-------------------------------------------
//-------------------------------------------
void EEpixPed::saveHisto(TString fname) {
  fname+=".hist.root";
  
  TFile f(fname.Data(),"update");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),fname.Data());
  HList->Write();
  TH1F* h= (TH1F*) tFd->Get("info"); 
  if(h) h->Write();
  f.Close();
  assert(!f.IsOpen());
  
  printf("                      , save Ok \n");
}
