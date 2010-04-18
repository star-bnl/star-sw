#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>

/*********************************************************************
 * $Id: L2EmcDb.cxx,v 1.6 2010/04/18 06:05:32 pibero Exp $
 * \author Jan Balewski, IUCF, 2006 
 *********************************************************************
 * Descripion:
 * StRoot-free DB container , common for BTOW + ETOW + ESMD
 * see also: http://www.star.bnl.gov/protected/spin/balewski/2006-L2JetAlgo/algo-L2EmcDb/
 *********************************************************************
 */


#include "L2EmcDb.h"

//=====================================
//=====================================
L2EmcDb::L2EmcDb(char *inpP, char *logP) {
  printf("L2EmcDb::constr: ver20, inpPath='%s', logPath='%s'\n",inpP,logP);
  strncpy(inpPath,inpP,mxTxt);
  strncpy(logPath,logP,mxTxt);
  clearTables();
  setPedFile("pedestal.current");// default ped file
  setMaskFile("towerMask.current");// default mask file
}

void L2EmcDb::setPedFile(const char *c){ strncpy(pedFile,c,mxTxt); }
void L2EmcDb::setMaskFile(const char *c){ strncpy(maskFile,c,mxTxt); }
 
//==============================
//==============================
int
L2EmcDb::initRun(int runNo){
  if(runNo==run_number) return 0; // re-initialization is not needed
  if(runNo>0) finishRun(); // save final DB for previous run - if any 
  clearTables();
  run_number=runNo;
  char fname[1000];
  
  printf("L2EmcDb::initRun, inpPath='%s', logPath='%s'\n",inpPath,logPath);
  int err=0;
  sprintf(fname,"%s/btowDb.current",inpPath);           err+=readAsciiDb(fname,db_labels[0]);
  sprintf(fname,"%s/etowDb.current",inpPath);           err+=readAsciiDb(fname,db_labels[1]);

  sprintf(fname,"%s/btowCrateMask.current",inpPath); err+=changeMaskFullCrate(fname,'B',db_labels[2]);
  sprintf(fname,"%s/etowCrateMask.current",inpPath); err+=changeMaskFullCrate(fname,'E',db_labels[3]);

  sprintf(fname,"%s/%s",inpPath,maskFile);     err+=changeMaskByName(fname,db_labels[4]);
  sprintf(fname,"%s/%s",inpPath,pedFile);            err+=changePedsByName(fname,db_labels[5]);

  if(err) {
    printf("\n\n total CRASH of L2EmcDb::initRun(%d), reason=some error in ASCII DB\n\n",runNo);
    return err;
  }
  printf("L2EmcDb::initRun(%d) done\n", runNo);
  return 0;
}

//==============================
//==============================
void
L2EmcDb::finishRun(){
  //discards all DB info for the first time
  if(run_number<0) return;

  // dumping final DB config

  char dbFname[1000];
  sprintf(dbFname,"%s/run%d.l2db.out",logPath,run_number);
    

  FILE *dbFd=fopen(dbFname,"w");
  if(dbFd) { 
    fprintf(dbFd,"#L2EmcDb  finishRun(%d), compiled: %s , %s\n",run_number,__DATE__,__TIME__);
    // dums lables of all corrections
    int i;
    for(i=0;i<txMxLbl;i++) fprintf(dbFd,"#%d-%s",i,db_labels[i]);
    fprintf(dbFd,"#\n");
    writeAsciiDb(dbFd);
    fclose(dbFd);
    printf("L2EmcDb:finishRun()  dbDump='%s' ...\n",dbFname);
  } else {
    if (dbFd)
      fprintf(dbFd,"L2EmcDb:FinishRun(%d)  dbDump  NOT saved, I/O error\n",run_number);
  }

  clearTables();
}


//=====================================
//=====================================
L2EmcDb::~L2EmcDb(){
  finishRun();
}

//=====================================
//=====================================
void
L2EmcDb::clearTables(){
  run_number=-4;
  int i;
  for(i=0; i<EmcDbIndexMax; i++)
    clearItem(dbByIndex+i);

  memset(db_labels,0,sizeof(db_labels));
}
  
//=====================================
//=====================================
void 
L2EmcDb::clearItem(struct EmcCDbItem *x) {
  x->name[0]=0;
  x->tube[0]=0;
  x->crate=-1; x->chan=-1; 
  x->gain=-2;
  x->ped=-3;
  x->sec=-4;
  x->sub='Z';
  x->eta=-5;  
  x->thr=-6;
  x->sigPed=-7; 
  x->strip=-299;
  x->plane='X';	
  x->stat=0xaa; x->fail=0xbb;
  x->rdo=123456;
  x->key=-999;
}


//=====================================
//=====================================
void 
L2EmcDb::printItem(const EmcCDbItem *x) {
  printf("gEmcCDb:");
  if(x==0) { printf(" NULL pointer\n"); return; }

  if(x->name[0]==0) {
    printf(" item not defined ???\n");
    return;
  }

  if(strchr(x->name,'U') || strchr(x->name,'V') )
    printf(" %s crate=%d chan=%3d sec=%2d plane=%c strip=%3d gain=%.3f  ped=%.2f sPed=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x pix=%s rdo=%d  key=%d\n",x->name,x->crate,x->chan,x->sec,x->plane,x->strip,x->gain,x->ped,x->sigPed,x->thr,x->stat,x->fail,x->tube,x->rdo,x->key);
  else
    printf(" %s crate=%d chan=%3d sec=%2d sub=%c eta=%2d gain=%.3f  ped=%.2f sPed=%.2f ADC_thr=%.2f stat=0x%4.4x fail=0x%4.4x tube=%s rdo=%d  key=%d\n",x->name,x->crate,x->chan,x->sec,x->sub,x->eta,x->gain,x->ped,x->sigPed,x->thr,x->stat,x->fail,x->tube,x->rdo,x->key);
}

//=====================================
//=====================================
const L2EmcDb::EmcCDbItem *
L2EmcDb::getByIndex(int i){
  if(i<0 || i>=EmcDbIndexMax) return 0;
  return  dbByIndex+i;
}

//=====================================
//=====================================
int 
L2EmcDb::importItem(EmcCDbItem *x, FILE *fd){
  /* return:
    <0 : error in input
     0 : EOF
     1 : line ignored
     2 : valid input
  */
 
  clearItem(x);
  const int mx=1000;
  char buf[mx];
  
  char * ret=fgets(buf,mx,fd);
 
  if(ret==0) return 0;  

  if(buf[0]=='#') return 1; 

  char name0[mx];
  int ret1=sscanf(buf,"%s",name0);


  if(ret1==0) return -1;

  int n=0; 
  if(name0[2]=='U' || name0[2]=='V') {     
    n=sscanf(buf,"%s %d %d %d %c %d %f %f %f %x %x %s %d",x->name,&x->crate,&x->chan,&x->sec,&x->plane,&x->strip,&x->gain,&x->ped,&x->thr,&x->stat,&x->fail,x->tube,&x->rdo);
  }
  else if (name0[2]=='T' || name0[2]=='P' || name0[2]=='Q' || name0[2]=='R'||  name0[2]=='t' ) {    
    n=sscanf(buf,"%s %i %i %d %c %d %f  %f %f %x %x %s %d",x->name,&x->crate,&x->chan,&x->sec,&x->sub,&x->eta,&x->gain,&x->ped,&x->thr,&x->stat,&x->fail,x->tube,&x->rdo);
  }
  else 
    return -3;

  /*  printf("aaa name='%s'  n=%d\n",name0,n);  */


  if(n!=13) return -1000-n;

  return 2;
}


//=====================================
//=====================================
int 
L2EmcDb::name2index(char *name){

  int index=-1;
  int sec=atoi(name);
  assert(sec>0 && sec<=12);
  
  index=(sec-1)*1000;

  if(sec<10 && name[0]!='0') name--; /* compensate for missing proceeding zero in sectorID */
  char key=name[2];
  
  switch(key) {
  case 'R': index+=100;
  case 'Q': index+=100;
  case 'P': index+=100;
  case 'T': {
    int sub =name[3];
    assert(sub>='A' && sub<='E');
    int eta=atoi(name+4);
    assert(eta>0 && eta <=12);
    index+=(sub-'A')*12 +eta;
  }  break;
  case 'V': index+=300;
  case 'U': {index+=400;
    int strip=atoi(name+3);
    assert(strip>0 && strip<=288);
    index+=strip;
  } break;
  /* extension for BTOW towers */
  case 't' : index= EindexMax+BtowName2Index(sec,name+3); break;
 
  default:
    printf("EEname2Index('%s')  Logical Error3: invalid index=%d\n",name,index);
    exit(-1);
  }

  /*  printf("EmcName2Index(%s)-->%d\n",name,index); */
  
  assert(index>=0);
  assert(index<EmcDbIndexMax);
  
  return index;
  
}

//=====================================
//=====================================
bool  
L2EmcDb::isEmpty(const EmcCDbItem *x){
  if(x==0) return true;
   return x->name[0]==0;
}

//=====================================
//=====================================
bool  
L2EmcDb::isBTOW(const EmcCDbItem *x){
  if (isEmpty( x)) return false;
  if (x->name[2]!='t') return false;
  if (x->crate<1 || x->crate> BTOW_MAXFEE) return false;
  if (x->chan<0  || x->chan > BTOW_DATSIZE ) return false;
  return true;
}

//=====================================
//=====================================
bool  
L2EmcDb::isETOW(const EmcCDbItem *x){
 if (isEmpty( x)) return false;
  if (x->name[2]!='T') return false;
  if (x->crate<1 || x->crate> ETOW_MAXFEE) return false;
  if (x->chan<0  || x->chan > ETOW_DATSIZE ) return false;
  return true;
}

//=====================================
//=====================================
int 
L2EmcDb::BtowName2Index(int sect, char *xee){
  /* RETURN index range [0,4799] */

  int index=-1;
  /*printf("in BtowName2Index, sect=%d, xee=%s=\n",sect,xee);*/
  char sub=xee[0];
  int etaB=atoi(xee+1);


  if(etaB<1 || etaB>40) {
    printf("BtowName2Index()  Logical Error5: invalid sect=%d, xee=%s=\n",sect,xee);  
    exit(-1);
  }

  if(sub<'a' || sub>'j') {
    printf("BtowName2Index()  Logical Error6: invalid sect=%d, xee=%s=\n",sect,xee);  
    exit(-1);
  }
  index=etaB +400*(sect-1) + (sub-'a')*40;
  /* printf("sub=%c etaB=%d index=%d\n",sub,etaB,index); */


  return index;
}


//==============================
//==============================
int  
L2EmcDb::readAsciiDb(char *fname, char *lbl) {
  int err=77;
  FILE *fd=0;
  int nL=0;
  int nR=0; /* count good records */

  fd=fopen(fname,"r");  
  if(fd==0) { err=-1; goto crashIt;}
  /* printf(" readAsciiDb(%s) ...\n",fname); */
  {
    const int mx=1000;
    char buf[mx];
    if(fgets(buf,mx,fd)) strncpy(lbl,buf,txMxSize-1); //preserve label
  }
  while (1) {
    /* -- Read in each individual entry.  The return value is checked
       -- for success or failure */
    struct EmcCDbItem x;
    
    err = importItem(&x,fd);
    nL++;
    if(err==0) break;
    if(err==1) continue;
    if(err <0) goto crashIt;

    /* -- recover Index for the specified (named) item */
    int key= name2index(x.name);
    /*  printf(" name='%s' key=%d, inpKey=%d, inpKey \n",x.name,key,x.key); */
    
    x.key=key;
    
    if(key<1 || key>=EmcDbIndexMax) { err=-22; goto crashIt;}
    
    
    /* -- Copy the database record read in from the file into
       local lookup table */
    
    struct EmcCDbItem  *y=dbByIndex+key;
    
    if(!isEmpty(y)) { err=999; goto crashIt; }
    *y=x; /* save content of this DB record & update lookup tables */
    nR++;
    /* tmp, do not verify duplicated mapping problems , fix it later*/
    
    /*  EmcDbItemStruct_print(&x);  */
  }
  
  printf("L2EmcDb::readAsciiDb(%s) ... nL=%d  nR=%d\n",fname,nL,nR);
  return 0;
  
 crashIt:
  printf("\n\n total CRASH of L2EmcDb::readAsciiDb(%s) err=%d  nL=%d\n\n",fname,err,nL);
  return err;
}



/*--------------------------------------------------
  --------------------------------------------------*/
void 
L2EmcDb::exportItem(struct EmcCDbItem *x, FILE *fd) {
  
  if(x->name[0]==0) return; /* item not defined */

  if(strchr(x->name,'U') || strchr(x->name,'V') )
    fprintf(fd,"%s %3d %3d %2d %c %4d %.3f %.2f %.2f 0x%4.4x 0x%4.4x %s %d\n",x->name,x->crate,x->chan,x->sec,x->plane,x->strip,x->gain,x->ped,x->thr,x->stat,x->fail,x->tube,x->rdo);
  else
    fprintf(fd,"%s %d %3d %2d %c %2d %.3f  %.2f %.2f 0x%4.4x 0x%4.4x %s %d\n",x->name,x->crate,x->chan,x->sec,x->sub,x->eta,x->gain,x->ped,x->thr,x->stat,x->fail,x->tube,x->rdo);
}


/* ------------------------------------
   ------------------------------------ */
void 
L2EmcDb::writeAsciiDb(FILE *fd) {
  fprintf(fd,"#------------------------------\n");
  fprintf(fd,"#TOWERS: name crate chan sec sub etaBin gain ped thres stat fail tube rdo\n"); 
  int i;
  int n1=0,n2=0;
  for(i=0; i<EmcDbIndexMax ; i++) {
    EmcCDbItem  *x=dbByIndex+i;
    n1++;
    if(isEmpty(x)) continue;
    exportItem(x,fd);
    // if(strstr(x->name,"01TA")) printf("c %s %f\n",x->name,x->ped);
    n2++;
  }
  fprintf(fd,"# total %d items out of %d possible\n",n2,n1);
}


//==============================
//==============================
int
L2EmcDb::changeMaskFullCrate(const char *fname, char BEflag, char *lbl) {

  /* Replace stat&fatal masks for channels already initialized from the DB
     in specified crate
     format : {hexCrateID hexStat hexFatal ,  anythingElse}
     lines starting with '#' are ignored
     empty lines are not permitted
  */
  printf("L2EmcDb::changeMaskFullCrate(%s)\n",fname);
  FILE *fd=fopen(fname,"r");
  int nd=0,nl=0;
  const int mx=1000;
  char buf[mx];
  int err=-1;

  if(fd==0) goto crashIt;

  if(fgets(buf,mx,fd)) strncpy(lbl,buf,txMxSize-1); //preserve label

  uint crate, stat, fail;

  while(1) {
    char *ret=fgets(buf,mx,fd);
    if(ret==0) break;

    nl++;
    if(buf[0]=='#') continue;
    int n=sscanf(buf,"%x %x %x",&crate, &stat, &fail);
    if(n!=3) { err=-2;  goto crashIt; }
    printf("# crate=0x%02x  set:  stat=0x%x  fail=0x%x\n",crate,stat,fail);
    int i;
    for(i=0; i<EmcDbIndexMax; i++) {
      struct EmcCDbItem  *x=dbByIndex+i;
      if(isEmpty(x)) { continue; }
      if(x->crate!=(int)crate) continue;
      if ((BEflag=='B' && isBTOW(x)) ||
          (BEflag=='E' && isETOW(x))) {
        x->stat=stat;
        x->fail=fail;
        nd++;
      }
    }

  }
  fclose(fd);

  printf("    change stat & fail bits done inpLine=%d nChanged=%d\n",nl,nd);  return 0;

 crashIt:
  printf("\n\n total CRASH of L2EmcDb::changeMaskFullCrate(%s) err=%d  nL=%d\n\n",fname,err,nl);
  return err;
}

//==============================
//==============================
int
L2EmcDb::changePedsByName(const char *fname, char *lbl) {

  /* Replace peds only for channels already initialized from the DB 
     format : {name, ped, sigPed  anythingElse}
     lines starting with '#' are ignored
     empty lines are not permitted
  */
  printf("L2EmcDb::changePedsByName(%s)\n",fname);
  FILE *fd=fopen(fname,"r");
  int nd=0,nl=0,ne=0;
  const int mx=1000;
  char buf[mx];
  int err=-1;

  if(fd==0) goto crashIt;

  if(fgets(buf,mx,fd)) strncpy(lbl,buf,txMxSize-1); //preserve label

  char cVal[100];
  float pVal,sVal;

  while(1) {
    char *ret=fgets(buf,mx,fd);
    if(ret==0) break;
    nl++;
    if(buf[0]=='#') continue;
    int n=sscanf(buf,"%s %f %f",cVal,&pVal,&sVal);
    if(n!=3) { err=-2;  goto crashIt; }
    int key= name2index(cVal);
    struct EmcCDbItem  *x=dbByIndex+key;

    if(isEmpty(x)) { ne++; continue; } // skip unmapped channels
     // printf("%s %p\n",cVal,x);

    // replace only initialized channels
    if(pVal<0)  { printf("Ignore: %s",buf); continue; }
    x->ped=pVal;


     nd++;
  }
  fclose(fd);

  printf("    changePedsByName() done inpLine=%d nChanged=%d nNotMappedAndIgnored=%d \n",nl,nd,ne);  return 0;

 crashIt:
  printf("\n\n total CRASH of L2EmcDb::changePedsByName(%s) err=%d  nL=%d\n\n",fname,err,nl);
  return err;
}

//==============================
//==============================
int
L2EmcDb::changeMaskByName(const char *fname, char *lbl) {

/* Replace stat&fail masks only for channels already initialized from the DB 
     format : {name, hexStat, hexFail,  anythingElse}
     lines starting with '#' are ignored
     empty lines are not permitted
*/
  printf("L2EmcDb::changeMaskByName(%s)\n",fname);
  FILE *fd=fopen(fname,"r");
  int nd=0,nl=0,ne=0;
  const int mx=1000;
  char buf[mx];
  int err=-1;
  
  if(fd==0) goto crashIt;
  char cVal[100];
  uint  stat, fail;
  if(fgets(buf,mx,fd)) strncpy(lbl,buf,txMxSize-1); //preserve label

  while(1) {
    char *ret=fgets(buf,mx,fd);
    if(ret==0) break;
    
    nl++;
    if(buf[0]=='#') continue;
    int n=sscanf(buf,"%s %x %x",cVal,&stat, &fail);
    if(n!=3) { err=-2;  goto crashIt; }
    int key= name2index(cVal);
    struct EmcCDbItem  *x=dbByIndex+key;
    
    if(isEmpty(x)) { ne++; continue; } // skip unmapped channels
    // printf("%s %p\n",cVal,x);
    
    // replace only initialized channels
    x->stat=stat;
    x->fail=fail;
    nd++;
  }
  fclose(fd);
  
  printf("    changeMaskByName() done inpLine=%d nChanged=%d nNotMappedAndIgnored=%d \n",nl,nd,ne);  return 0;

 crashIt:
  printf("\n\n total CRASH of L2EmcDb::changeMaskByName(%s) err=%d  nL=%d\n\n",fname,err,nl);
  return err;
}


/*
*********************************************************************
  $Log: L2EmcDb.cxx,v $
  Revision 1.6  2010/04/18 06:05:32  pibero
  Address compiler warnings.

  Revision 1.5  2007/11/06 22:07:24  balewski
  added timeStamp controlled L2 setup from Jason

  Revision 1.4  2007/10/25 23:00:30  balewski
  logic bug fix

  Revision 1.2  2007/10/22 23:10:03  balewski
  split L2 to generic and year specific, not finished

  Revision 1.1  2007/10/11 00:33:13  balewski
  L2algo added

  Revision 1.4  2006/03/28 19:33:22  balewski
  ver16b , in L2new

  Revision 1.3  2006/03/11 17:08:32  balewski
  now CVS comments should work

*/

