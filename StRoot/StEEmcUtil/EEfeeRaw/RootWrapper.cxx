#include <cstdlib>
#include <cstdio>

#include "TROOT.h"
#include "TFile.h"
#include "TDirectory.h"
#include "TTree.h"
#include "TBranch.h"


#include "EEfeeDataBlock.h"
#include "EEfeeRawEvent.h"
#include "EEmcEventHeader.h"


// A simple wrapper for Fortran xsort program
// works for Linux and possibly other Unices
// Author Piotr A. Zolnierczuk / IUCF
// 2002-2003


//#define DEBUG 0
static const int MaxCommentLen=1024;

static TFile   *file  = NULL ;
static TTree   *tree  = NULL ;

static TBranch *beve  = NULL ;
static TBranch *bhead  = NULL ;
 
static EEfeeDataBlock *b   = NULL ;
static EEfeeRawEvent  *eve = NULL ;
static EEmcEventHeader *ehead = NULL ;

static int   evnum                   = 0;
static int   nAutoSave =10000;
static char *filename = NULL;


extern "C" void
eemcfeerootopen_(long& run, long& runtime, char *chfile, int &nAuto, int len)
{ 
  char *comment  = new char[MaxCommentLen];
  char *basefile = new char[MaxCommentLen];
  const char *rootdir  = getenv("MINIROOTDIR");
  if ( rootdir == NULL ) rootdir=".";
  filename = new char[(len<MaxCommentLen)?MaxCommentLen:len]; // ???
  
  // a hack for now
  if(strstr(chfile,"[BSND ALL]")!=NULL) { // we have an online case
    fprintf(stderr,"rootopen: on-line data <[BSND ALL]>\n");
    sprintf(basefile,"run%03ld",run);
  } else {
    memcpy(basefile,chfile,len);
    char *isp  = strchr(basefile,' ');  *isp = 0x00; // locate first space
    fprintf(stderr,"rootopen: off-line data <%s>\n",basefile);
    char *idt  = strchr(basefile,'.');  *idt = 0x00; // locate first dot
  }
  sprintf(filename,"%s/%s.ez.root",rootdir,basefile); 
  sprintf(comment,"run:%05ld, time:%s ",run,ctime((time_t *)&runtime));

  file  = new TFile(filename,"RECREATE");
  tree  = new TTree("ezstar","A tree with FEE events");
  eve   = new EEfeeRawEvent();
  ehead   = new EEmcEventHeader();
  b     = new EEfeeDataBlock();

  bhead  = tree->Branch("head","EEmcEventHeader",&ehead,10000,99);
  beve  = tree->Branch("eemc" ,"EEfeeRawEvent",&eve,10000,99);

  ehead->clear();
  ehead->setRunNumber(run);
  ehead->setProcessingTime(time(0));
  ehead->setTimeStamp(runtime);
  ehead->setComment(comment);
  evnum=1; // reset event counter

  nAutoSave=nAuto;

  fprintf(stderr,"rootopen: file=%s\n",filename);
  fprintf(stderr,"rootopen: comment=%s\n",comment);
  fprintf(stderr,"rootopen: nAutoSave=%d\n",nAutoSave);
  fflush(stderr);
  if(basefile) delete [] basefile;
  if(comment ) delete [] comment;
  return;
}


extern "C" void 
eemcfeerootfill_(unsigned short& evtype, unsigned short& evtoken, unsigned short& size , unsigned short *e, int &eveID, int *ierr)
{ 
  UShort_t *head = new UShort_t[EEfeeDataBlock::DefaultMaxHead];
  static int nTotErr=0;

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
    unsigned int wordcnt   = *p++;    
    *p++;                    // 0 resword1 
    *p++;                    // 1 resword2 
    unsigned short token   = *p++;    // 2
    unsigned short cratrig = *p++;    // 3
    if(token!=evtoken) {
      nTotErr++;
      if(nTotErr%10000==0) {
	fprintf(stderr,"eemcfeerootfill: *** token mismatch, \n  so far   %d-th data blocks  in %d events\n",nTotErr, eveID);
	fprintf(stderr," (event token=%hd crate token=%hd)\n",evtoken,token);
      }
    }
    if(wordcnt>4) wordcnt -= 4;
    head[EEfeeDataBlock::ERRFLG] = evtype; // use free slot
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


  ehead->setProcessingTime(time(0));
  ehead->setToken(evtoken);
  ehead->setEventNumber(eveID);
  tree->Fill();  

  if(evnum%nAutoSave==0) {
    tree->AutoSave();
    file->SaveSelf();
    printf("====>  ezTree AutoSave done, nEve=%d\n",evnum);
  }


  *ierr=0;
  return;
}



extern "C" void
eemcfeerootclose_()
{ 
  if(file) { 
    file->Write();
    delete file;
    file=NULL;
    fprintf(stderr,"eemcfeerootclose: file %s written\n",filename);
  }
  fprintf(stderr,"eemcfeerootclose: OK (total events=%8d)\n",evnum); 
  if(filename) delete [] filename;
  return;
}
