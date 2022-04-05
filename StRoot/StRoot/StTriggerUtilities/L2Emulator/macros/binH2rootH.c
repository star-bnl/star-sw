/* to compile:
g++ -m32 -Wall -g -O -I `root-config --incdir` -c binH2rootH.c -o binH2rootH.o
g++ -m32 -Wall -g -O -I `root-config --incdir` -c L2Histo.cxx -o L2Histo.o
g++ -m32 -o binH2rootH-exe binH2rootH.o L2Histo.o -l m `root-config --libs`

*/

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
/*********************************************************************
 * $Id: binH2rootH.c,v 1.2 2011/01/30 03:02:52 balewski Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * Converts binary histos produced by L2 to root histos
 *********************************************************************
 */

#include "L2Histo.h"

#include "TH1F.h"
#include "TH2F.h"
#include "TFile.h"
#include "TString.h"

//=================================
void create1D( L2Histo & h, char *hname) {   
    TString name="h";name+=h.getId(); 
    if(hname[0]) name=hname;
    TString title=h.getTitle();
    int nbin=h.getNbin();
    int sum=h.getNunder()+h.getNover();

    TH1F *hout=new TH1F(name,title,nbin,0,nbin);
    float * y=hout->GetArray();
    const int *data=h.getData();
    for ( int ii=0;ii<nbin;ii++ ){
      y[ii+1]= data[ii];
      sum+= data[ii];
    }
    y[0]=h.getNunder();
    y[nbin+1]=h.getNover();
    hout->SetEntries(sum);
    hout->SetLineColor(kBlue);
    hout->Write();
}

//=================================
void create2D( L2Histo & h) {   
    TString name="h";name+=h.getId(); 
    TString title=h.getTitle();
    int nbin=h.getNbin();
    int nbinX=h.getNbinX();
    int nbinY=h.getNbin()/nbinX;
    int sum=h.getNunder()+h.getNover();

    TH2F *hout=new TH2F(name,title,nbinX,0,nbinX,nbinY,0,nbinY);

    const int *data=h.getData();
    for ( int i =0;i <nbin;i ++ ){
      int ix=i%nbinX;
      int iy=i/nbinX;
      // printf("ix=%d  iy=%d  val=%d\n", ix,iy,data[i]);
      hout->SetBinContent(ix+1,iy+1,data[i]); // why root is counting bins from 1 ?
      sum+=data[i];
    }
    //    y[0]=h.getNunder(); // not implemented yet
    // y[nbin+1]=h.getNover(); // not implemented yet
    hout->SetEntries(sum);

    hout->Write();
}


//===================================
int main(int argc, char ** argv ) {
  printf("Converting L2-bin-histos to root histos  START...");

  if(argc!=3) {
    printf("\nERROR: must specify an input and output file, e.g.:\n %s  inp.hist.bin  out.hist.root\n STOP\n",argv[0]);
    exit(1);
  }

  char *histFname=argv[1];
  //char *histFname="janH.hist.bin";

  FILE *histFd=fopen(histFname,"r");
  assert(histFd);

  TString ofilename=argv[2];
  printf("outfile=%s\n",ofilename.Data());

  TFile *ofile=new TFile(ofilename,"recreate");
  
  int nh=0;
  while(1) {
    L2Histo h;
    int ret=h.read(histFd);
    if(ret) break;
    nh++;
    int iMax=-3, iFWHM=-4;
    h.findMax( &iMax, &iFWHM);
    if(h.getId()<1000) printf("h%d, '%s'   binMax=%d \n",h.getId(),h.getTitle(),iMax);

    char hname[100];
    hname[0]=0;

    // assume for high ID are tower histos
    if(h.getId()>9999) { 
      // h.print(0);
      hname[0]='a';
      strncpy(hname+1,h.getTitle()+5,6);
      hname[7]=0;
      // printf("=%s=\n",hname);
    }

    if(h.is1D()) create1D(h,hname); 
    else  create2D(h); 

    // if(h.getId()==10) h.print(1); break;
    //if(nh>10) break;
  }

  ofile->Close();
  delete ofile; 

} 

/**********************************************************************
  $Log: binH2rootH.c,v $
  Revision 1.2  2011/01/30 03:02:52  balewski
  fixed instruction how to compile it

  Revision 1.1  2007/10/11 00:33:29  balewski
  L2algo added

  Revision 1.2  2006/03/11 17:11:34  balewski
  now CVS comments should work

*/

