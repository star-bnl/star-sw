#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>

#include "TROOT.h"
#include "TFile.h"
#include "TDirectory.h"
#include "TTree.h"
#include "TBranch.h"


#include "EEfeeDataBlock.h"
#include "EEfeeRawEvent.h"
#include "EEfeeRunDescr.h"


#define DEBUG 1
const int MaxCommentLen=1024;

static TFile   *file  = NULL ;
static TTree   *tree  = NULL ;

static TBranch *beve  = NULL ;
static TBranch *bdes  = NULL ;

static EEfeeDataBlock *b   = NULL ;
static EEfeeRawEvent  *eve = NULL ;
static EEfeeRunDescr  *des = NULL ;

static int   evnum                   = 0;
static char *filename = NULL;


extern "C" void
rootopen_(long& run, long& runtime, char *chfile, int len)
{ 
  char *comment  = new char[MaxCommentLen];
  filename = new char[len];
  memcpy(filename,chfile,len);
  char *isp  = strchr(filename,' ');  *isp = 0x0;

  //sprintf(comment,"file:%s, run:%05ld, time:%s ",filename,run,ctime(&runtime));
  sprintf(comment,"%s",filename);
  char *idt  = strchr(filename,'.');  *idt = 0x0;
  strcat(filename,".root");

  file  = new TFile(filename,"RECREATE");
  tree  = new TTree("fee","A tree with FEE events");
  eve   = new EEfeeRawEvent();
  des   = new EEfeeRunDescr();
  b     = new EEfeeDataBlock();

  bdes  = tree->Branch("desc","EEfeeRunDescr",&des,10000,99);
  beve  = tree->Branch("evt" ,"EEfeeRawEvent",&eve,10000,99);

  des->clear();
  des->setProcessingTime(time(0));
  des->setTimeStamp(runtime);
  des->setComment(comment);

  fprintf(stderr,"rootopen: %s\n",comment);

  return;
}


extern "C" void 
rootfill_(unsigned short& evtype, unsigned short& size , unsigned short *e, int *ierr)
{ 
  UShort_t *head = new UShort_t[EEfeeDataBlock::DefaultMaxHead];

  *ierr=0;
  b->clear();
  eve->clear();
  eve->setID(++evnum);

  if(evnum%10000==0) 
    fprintf(stderr,"EVENT %6dk %6d 0x%04hx\r",evnum/10000,size,evtype);

#ifdef DEBUG
  fprintf(stderr,"\nEVENT %08d %06d 0x%04hx\n",evnum,size,evtype);
#endif

#ifdef DEBUG
  int i=0;
  while(i<size) {
    for(int k=0; i<size && k<2  ; k++,i++) fprintf(stderr,"[%03d] 0x%04hx ",i,e[i]);
    fprintf(stderr,"\n");
    for(int k=0; i<size && k<4  ; k++,i++) fprintf(stderr,"[%03d/%02d]=0x%04hx ",i,k,e[i]);
    fprintf(stderr,"\n");
    for(int k=0; i<size && k<128; k++,i++);
  }
  fprintf(stderr,"i=%d\n",i);
#endif  

  for(unsigned short *p = e; (p-e)<size; ) {
    b->clear();
    *p++;                 // skip marker
    int wordcnt   = *p++;    

    *p++;                    // 0 resword1 
    *p++;                    // 1 resword2 
    int token     = *p++;    // 2
    int cratrig   = *p++;    // 3

    if(wordcnt>4) wordcnt -= 4;
    head[EEfeeDataBlock::EVTYPE] = evtype;
    head[EEfeeDataBlock::WRDCNT] = wordcnt;
    head[EEfeeDataBlock::TOKEN]  = token;
    head[EEfeeDataBlock::CRATE]  = cratrig;
    b->setHead(head);
    b->setDataArray(p,wordcnt);

#if DEBUG
    b->print(0);
#endif    
    if(cratrig) eve->addFeeDataBlock(b);
    p += wordcnt;
  }


  tree->Fill();  
  *ierr=0;
  return;
}



extern "C" void
rootclose_()
{ 
  if(file) { 
    file->Write();
    delete file;
    file=NULL;
    fprintf(stderr,"rootclose: file %s written\n",filename);
  }
  fprintf(stderr,"rootclose: OK (total events=%8d)\n",evnum); 
  return;
}
