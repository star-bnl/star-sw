/* This program reads an xdf data file and produces C++ and C code.
** It then compiles, links (to the root library), and runs this code,
** resulting in a root data file, and shared object library, some
** example macros, and a few other goodies.
*/
/**************************************************  INCLUDES  ************/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mode.h>
#include "dstype.h"
#include "dsxdr.h"
/****************************************************  DEFINES  **********/
#define APFIF 100  /* max len of Abs Path For Input xdf File */
#define BUF 2000
#define PP printf(
#define FF fprintf(ff,
#define ERR  FatalError(0,__LINE__)
#define DERR FatalError(7,__LINE__)
#define PASS 300
#define DIRSIZE 150
#define DIR     94
#define FN      80
#define FREE 1000 /* sum of number of tables and number of directories */
/****************************************************  GLOBALS  **********/
int gEventCnt,gOptionC,gOptionE,gOptionG,gOptionEIndex;
int gNfreeOld,gNfree;
char *gFree[FREE];
char *gFreeOld[FREE];
char gBaseDir[DIRSIZE];
char gDir[2][DIR+1];
char gPass[PASS]; /* To pass a string w/o declaring memory in calling fnct. */
FILE *gXdfIn;
XDR gXdrPtr;
int gNtype,gNtable,gNtableOld;
#define NTABLE 500
#define NTYPE  400  /* less than or equal to NTABLE */
DS_DATASET_T *gTablePtr[NTABLE];
char         *gTableType[NTABLE];
char         *gTableName[NTABLE];
char         *gTableNameOld[NTABLE];
char         *gTypesList[NTYPE]; /* non-redundant copy of gTableType */
char         *gTableSpec[NTYPE];
DS_DATASET_T *gPtrCurrentEvent;
/*****************************************************  PROTOTYPES  ******/
void EnvValNotDefined(char *evname);
int FileExists(char *fn);
void WriteRecipe(void);
int IndexOfSomeTableWhichHasThisType(int i);
char *ColName(int whichTable,int icol);
int Dimensions(int whichTable,int whichCol);
int NumberColumns(int whichTable);
char *Slash2X(char *fn);
void ConvertStructToMyRoot(int maxlen,char *in,char *out);
void Ose(void); void FatalError(int isDslError,int ln);
/*****************************************************  FUNCTIONS  ******/
void Exit(int ln) {
  if(ln>0) PP"Abnormal exit from line %d of %s.\n",ln,__FILE__);
  PP"ABORTED.   ABORTED.   ABORTED.   ABORTED.   ABORTED.   ABORTED.\n");
  exit(2);
}
DS_DATASET_T *DsPointer(int callingLineNum,char *tableName) {
  int i;
  for(i=gNtable-1;i>=0;i--) {
    if(!strcmp(tableName,gTableName[i])) return gTablePtr[i];
  }
  Ose();
  PP"callingLineNum = %d\n",callingLineNum);
  PP"Fatal error 88u in %s.\n",__FILE__);
  PP"I can't find table name '%s' in my list:\n",tableName);
  for(i=0;i<gNtable;i++) {
    if(i>30) { PP"List truncated.\n"); break; }
    PP"%2d '%s'\n",gTableName[i]);
  }
  if(7) Exit(__LINE__);
  return 0;
}
int NumberOfRows(int callingLineNumber,char *tableName) {
  size_t nrow;
  if(!dsTableRowCount(&nrow,DsPointer(callingLineNumber,tableName))) ERR;
  return (int)nrow;
}
FILE *Fopen(char *fn,char *mode) {
  char buf[123]; FILE *ff;
  if(strcmp(mode,"w")) ERR;
  sprintf(buf,"%s/%s",gDir[1],fn);
  ff=fopen(buf,mode);
  if(!ff) { Ose(); PP">> Can't write %s.\n",buf); Exit(__LINE__); }
  return ff;
}
void FatalError(int isDslError,int ln) {
  Ose();
  if(isDslError) { PP">> The last dsl error was:\n"); dsPerror(""); }
  PP">> Error in file %s on line :%d\n",__FILE__,ln);
  exit(-10);
}
void OpenTheXdfFile(char *filename) {
  if((gXdfIn=fopen(filename,"rb"))==NULL) {
    PP">> can't read file '%s'.\n",filename); Exit(__LINE__);
  }
  xdrstdio_create(&gXdrPtr,gXdfIn,XDR_DECODE);
}
char *Path(int addDotC,char *oldPath,char *newEnd) {
  char *rv; int extra;
  if(addDotC) extra=2; else extra=0;  /* two extra bytes for the .C */
  rv=(char*)malloc((size_t)(strlen(oldPath)+strlen(newEnd)+2+extra));
  if(!rv) { PP">> Fatal error malloc() failed.\n"); Exit(__LINE__); }
  strcpy(rv,oldPath);
  if(strlen(rv)>0) strcat(rv,"/"); strcat(rv,newEnd);
  if(addDotC) strcat(rv,".C");
  if(gNfree>=FREE) ERR; gFree[gNfree++]=rv;
  return rv;
}
void HandleADataset(DS_DATASET_T *pDslPtr,char *inPath) {
  /* If pDslPtr is a dataset, this function calls itself, else if it
  ** is a table, it writes a single ROOT macro file. */
  long entryIndex; char *extendedName,*nameOfDataset;
  bool_t isDataset;
  DS_DATASET_T *pDslPtrNew;
  size_t numEntries;
  if(!dsIsDataset(&isDataset,pDslPtr)) DERR;
  if(isDataset) {
    if(!dsDatasetName(&nameOfDataset,pDslPtr)) DERR;
    if(!dsDatasetEntryCount(&numEntries,pDslPtr)) DERR;
    for(entryIndex=0;entryIndex<numEntries;entryIndex++) {
      if(!dsDatasetEntry(&pDslPtrNew,pDslPtr,entryIndex)) DERR;
      HandleADataset(pDslPtrNew,Path(0,inPath,nameOfDataset));
    }
  } else { /* is a table */
    if(!dsTableName(&nameOfDataset,pDslPtr)) DERR;
    extendedName=Path(0,inPath,nameOfDataset);
    if(gNtable>=NTABLE) ERR;
    gTablePtr[gNtable]=pDslPtr;
    gTableName[gNtable]=extendedName;
    gNtable++;
  }
}
void FillListOfTables(void) {
  gNtable=0;
  HandleADataset(gPtrCurrentEvent,"");
}
int GetNextEvent(void) {
  int rv=7;
  if(!xdr_dataset(&gXdrPtr,&gPtrCurrentEvent)) rv=0;
  if(rv) {
    ++gEventCnt;
    /* PP">> Event number %d.\n",eventCnt); */
    FillListOfTables();
    /* -------------------
    PP">> This event has %d tables:\n",gNtable);
    for(ii=0;ii<gNtable;ii++) PP">> %s\n",gTableName[ii]);
    ------------------------------ */
  }
  return rv;
}
void Ose(void) {
  PP"+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
}
void Usage() {
  Ose();
  PP">> usage: xdf2root [options] file.xdf\n");
  PP">> options:\n");
  PP">> -c   Do not compress the root data file.\n");
  PP">> -g   Prepare the auto-gen code, then immediately stop.\n");
  PP">> -eN  Select only event number N (count from 1, like FORTRAN).\n");
  PP">>      Example to select event 3: xdf2root -e3 file.xdf\n");
  exit(2);
}
void AllEventsNotSame(void) {
  Ose();
  PP">> Fatal error:  all events must have the same tables.\n");
  PP">> You can work around this limitation by using the -e option.\n");
  PP">> Type 'xdf2root -h' for help.\n");
  Exit(__LINE__);
}
void CheckAllEventsHaveSameTables(void) {
  int ii,cnt=0,foundTheEvent=0;
  gNfree=0;
  if(!GetNextEvent()) ERR; cnt++;
  for(;;) {
    if(gOptionE&&cnt==gOptionEIndex) { foundTheEvent=7; break; }
    gNtableOld=gNtable;
    for(ii=gNtable-1;ii>=0;ii--) gTableNameOld[ii]=gTableName[ii];
    gNfreeOld=gNfree;
    for(ii=gNfree-1;ii>=0;ii--) gFreeOld[ii]=gFree[ii]; 
    gNfree=0;
    if(!GetNextEvent()) break; cnt++;
    if(!gOptionE) {
      if(gNtable!=gNtableOld) AllEventsNotSame();
      for(ii=gNtable-1;ii>=0;ii--) 
            if(strcmp(gTableNameOld[ii],gTableName[ii])) AllEventsNotSame();
    }
    /* Free the memory malloc()ed during the GetNextEvent() before the
    ** previous one, since all valuable info is safe in gTableName[i],
    ** which is included in gFree, not gFreeOld (used below).  */
    for(ii=gNfreeOld-1;ii>=0;ii--) free(gFreeOld[ii]); gNfreeOld=0;
  }
  if(gOptionE&&!foundTheEvent) {
    Ose();
    PP"I did not find event number %d.  Count from 1, like FORTRAN.\n",
      gOptionEIndex); Exit(__LINE__);
  }
}
void PrintListOfTablesToScreen(void) {
  int ii;
  Ose();
  PP">> Each event in this xdf file has %d tables:\n",gNtable);
  for(ii=0;ii<gNtable;ii++) {
    PP">>   %3d %s\n",ii+1,gTableName[ii]);
    if(ii>10) { PP">> List truncated.\n"); break; }
  }
}
void GenerateListOfTableTypes(void) {
  char alreadyInList,*tableType; int ii,jj;
  char *mytableSpec;
  gNtype=0;
  for(ii=gNtable-1;ii>=0;ii--) {
    if(!dsTableTypeName(&tableType,gTablePtr[ii])) DERR;
    if(!dsTableTypeSpecifier(&mytableSpec,gTablePtr[ii])) DERR;
    gTableType[ii]=tableType;
    alreadyInList=0;
    for(jj=gNtype-1;jj>=0;jj--) {
      if(!strcmp(tableType,gTypesList[jj])) alreadyInList=7;
    }
    if(!alreadyInList) {
      if(gNtype>=NTYPE) ERR;
      gTableSpec[gNtype  ]=mytableSpec;
      gTypesList[gNtype++]=tableType;
    }
  }
}
int PartOfVar(char x) {
  if(x==' ') return 0;
  if(x>='a'&&x<='z') return 7;
  if(x>='A'&&x<='Z') return 7;
  if(x>='0'&&x<='9') return 7;
  if(x=='_') return 7;
  return 0;
}
void ConvertStructToMyRoot(int maxlen,char *in,char *out) {
  char buf[BUF],*lastBrack,*thisbrack,*lookHere; char changeOk1,changeOk2;
  char *corbaInBuf,*chngToFloat,*aCorbaType,dobreak; int len,jj;
  char *corbaList[]={ "unsigned short", "unsigned long", "long", "short", 
                      "octet", "int", "float", "double", "char", NULL };
  if(strlen(in)>maxlen-10) ERR;
  if(strlen(in)>BUF-10) ERR;
  strcpy(out,in);
  lastBrack=strstr(out,"}"); if(!lastBrack) ERR;
  for(;;) {
    thisbrack=strstr(lastBrack+1,"}"); if(!thisbrack) break;
    lastBrack=thisbrack;
  }
  strcpy(lastBrack,"};\n");
  for(;;) { /* every loop replaces one corba type with Float_t */
    dobreak=7;
    for(jj=0;;jj++) {
      aCorbaType=corbaList[jj]; if(!aCorbaType) break; lookHere=out;
      for(;;) {
        chngToFloat=strstr(lookHere,aCorbaType);
        if(!chngToFloat) break;
        /* From here to the bottom of the for(;;) loop ensures that we
        ** do not leave chngToFloat pointing to within a variable with 
        ** a CORBA type in its name (eg, shortSectors or ped_float) */
        if(chngToFloat==lookHere)                      changeOk1=7;
        else if(PartOfVar(chngToFloat[-1]))            changeOk1=0;
        else                                           changeOk1=7;
        if(PartOfVar(chngToFloat[strlen(aCorbaType)])) changeOk2=0;
        else                                           changeOk2=7;
        if(changeOk1&&changeOk2) break;
        lookHere=chngToFloat+1;
      }
      if(chngToFloat) { /*replace, eg, 'unsigned short' with 'Float_t'*/
        strcpy(buf,out);
        corbaInBuf=strstr(buf,aCorbaType); if(!corbaInBuf) ERR;
        corbaInBuf[0]=0; 
        strcat(buf,"Float_t"); len=strlen(aCorbaType);
        strcat(buf,chngToFloat+len); 
        strcpy(out,buf); dobreak=0; 
        break;
      }
    }
    if(dobreak) break; /* have replaced all CORBA data types with Float_t */
  }
}
void PrintListOfTypes() {
  int i;
  Ose();
  PP">> Non-redundant list of table types:\n");
  for(i=0;i<gNtype;i++) {
    PP">>     %2d %s\n",i+1,gTypesList[i]);
    if(i>10) { PP">> List truncated.\n"); break; }
  }
}
void WriteMakefile_sun4os5pc() {
  FILE *ff=Fopen("Makefile","w"); if(!ff) ERR;
  FF"ObjSuf        = o\n");
  FF"SrcSuf        = C\n");
  FF"ExeSuf        =\n");
  FF"DllSuf        = so\n");
  FF"EVENTLIB      = $(EVENTO)\n");
  FF"OutPutOpt     = -o\n");
  FF" \n");
  FF"# Solaris\n");
  FF"CXX           = CC\n");
  FF"CXXFLAGS      = -w -KPIC -I$(ROOTSYS)/include\n");
  FF"LD            = CC\n");
  FF"LDFLAGS       = -g\n");
  FF"SOFLAGS       = -G\n");
  FF"ROOTLIBS      = $(ROOTSYS)/lib/*.so\n");
  FF"LIBS          = $(ROOTLIBS) -L/usr/dt/lib -L/usr/openwin/lib \\\n");
  FF"                -L/usr/ccs/lib -lXm -lXt -lX11 -lm -lgen\n");
  FF" \n");
  FF" \n");
  FF"#------------------------------------------------------------------\n");
  FF" \n");
  FF"EVENTO        = Event.$(ObjSuf) \\\n");
  FF"                EventCint.$(ObjSuf)\n");
  FF" \n");
  FF"EVENTS        = Event.$(SrcSuf) \\\n");
  FF"                EventCint.$(SrcSuf)\n");
  FF" \n");
  FF"MAINEVENTS    = MainEvent.$(SrcSuf)\n");
  FF" \n");
  FF"MAINEVENTO    = MainEvent.$(ObjSuf)\n");
  FF" \n");
  FF"EVENT         = Event$(ExeSuf)\n");
  FF"EVENTSO       = libEvent.$(DllSuf)\n");
  FF" \n");
  FF"HSIMPLEO      = hsimple.$(ObjSuf)\n");
  FF"HSIMPLES      = hsimple.$(SrcSuf)\n");
  FF"HSIMPLE       = hsimple$(ExeSuf)\n");
  FF" \n");
  FF"MINEXAMO      = minexam.$(ObjSuf)\n");
  FF"MINEXAMS      = minexam.$(SrcSuf)\n");
  FF"MINEXAM       = minexam$(ExeSuf)\n");
  FF" \n");
  FF"TSTRINGO      = tstring.$(ObjSuf)\n");
  FF"TSTRINGS      = tstring.$(SrcSuf)\n");
  FF"TSTRING       = tstring$(ExeSuf)\n");
  FF" \n");
  FF"TCOLLEXO      = tcollex.$(ObjSuf)\n");
  FF"TCOLLEXS      = tcollex.$(SrcSuf)\n");
  FF"TCOLLEX       = tcollex$(ExeSuf)\n");
  FF" \n");
  FF"OBJS  = $(EVENTO) $(HSIMPLEO) $(MINEXAMO) $(TSTRINGO) $(TCOLLEXO)\n");
  FF" \n");
  FF"PROGRAMS      = $(EVENT)\n");
  FF" \n");
  FF"all:            $(PROGRAMS)\n");
  FF" \n");
  FF"$(EVENT):       $(EVENTO) $(MAINEVENTO) dataset_access.o\n");
  FF"\t\t$(LD) $(SOFLAGS) $(LDFLAGS) $(EVENTO) $(OutPutOpt) $(EVENTSO)\n");
  FF"\t\t$(LD) $(LDFLAGS) dataset_access.o $(MAINEVENTO) \\\n");
  FF"\t\t-lnsl $(STAR_LIB)/$(STAR_SYS_LEVEL)/sys/lib/libdsl.a \\\n");
  FF"\t\t$(EVENTLIB) $(LIBS) $(OutPutOpt) $(EVENT)\n");
  FF"\t\t@echo \"$(EVENT) done\"\n");
  FF" \n");
  FF"dataset_access.o:\tdataset_access.c\n");
  FF"\tcc -c -I$(STAR_LIB)/$(STAR_SYS_LEVEL)/sys/inc dataset_access.c\n");
  FF"\n");
  FF"Event.o: Event.h\n");
  FF" \n");
  FF"EventCint.$(SrcSuf): Event.h LinkDef.h\n");
  FF"\t@echo \"Generating dictionary, ignore the two \\\"Note:\\\"s:\"\n");
  FF"\t@$(ROOTSYS)/bin/rootcint EventCint.$(SrcSuf) -c Event.h LinkDef.h\n");
  FF"\t@echo ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo\n");
  FF"\t@echo Compile of EventCint.C takes around 3 minutes per megabyte.\n");
  FF"\t@ls -l EventCint.C\n");
  FF" \n");
  FF".$(SrcSuf).$(ObjSuf):\n");
  FF"\t$(CXX) $(CXXFLAGS) -c $<\n");
  fclose(ff);
}
void WriteMakefile() {
  char ok=0;
#ifdef aix
  PP"Note to programmer:  use Root/Aix/Makefile as a template\n");
  PP"when writing"); WriteMakefile_aix(); Exit(__LINE__);
#endif
#if defined(sun)
  WriteMakefile_sun4os5pc(); ok=7;
#endif
  if(!ok) {
    Ose(); 
    PP"Fatal error, the code for xdf2root does not support\n");
    PP"this architecture.  You need to add another ifdef to function\n");
    PP"WriteMakefile().\n");
    Exit(__LINE__);
  }
}
#define STRUCTSIZE 3000
void WriteStrucFile() {
  char rootstruct[STRUCTSIZE],*cc; FILE *ff; int ii;
  ff=Fopen("table_structs.h","w"); if(!ff) ERR;
  for(ii=0;ii<gNtype;ii++) {
    FF"struct struc_%s { /* one struc per table type */\n",
          gTypesList[ii]);
    cc=strstr(gTableSpec[ii],"\n"); if(!cc) ERR;
    ConvertStructToMyRoot(STRUCTSIZE,cc+1,rootstruct);
    FF"%s\n",rootstruct);
  }
  fclose(ff);
}
void WriteLinkDef() {
  FILE *ff; int ii;
  ff=Fopen("LinkDef.h","w"); if(!ff) ERR;
  FF"#ifdef __CINT__\n");
  FF"\n");
  FF"#pragma link off all globals;\n");
  FF"#pragma link off all classes;\n");
  FF"#pragma link off all functions;\n");
  FF"\n");
  FF"#pragma link C++ class Event;\n");
  for(ii=0;ii<gNtype;ii++) 
      FF"#pragma link C++ class %s;  // one line per table type\n",
            gTypesList[ii]);
  FF"\n");
  FF"#endif\n");
  fclose(ff);
}
void WriteDataset_access() {
  FILE *ff;
  ff=Fopen("dataset_access.c","w"); if(!ff) ERR;
  FF"#include <stdio.h>\n");
  FF"#include <stdlib.h>\n");
  FF"#define PP printf(\n");
  FF"#define ERR FatalError(__LINE__)\n");
  FF"#define FREE %d\n",FREE);
  FF"#include \"dstype.h\"\n");
  FF"#include \"dsxdr.h\"\n");
  FF"int gEventNumber;\n");
  FF"FILE *gXdfIn;\n");
  FF"XDR gXdrPtr;\n");
  FF"int gNfree,gNtable;\n");
  FF"char *gFree[FREE];\n");
  FF"#define NTABLE 500\n");
  FF"DS_DATASET_T *gTablePtr[NTABLE];\n");
  FF"char         *gTableName[NTABLE];\n");
  FF"DS_DATASET_T *gPtrCurrentEvent;\n");
  FF"/*-----------------------------------------------  PROTOTYPES  --*/\n");
  FF"void FatalError(int ln);\n");
  FF"/*-----------------------------------------------  FUNCTIONS  --*/\n");
  FF"DS_DATASET_T *DsPointer(char *table) {\n");
  FF"  int ii;\n");
  FF"  for(ii=gNtable-1;ii>=0;ii--) {\n");
  FF"    if(!strcmp(table,gTableName[ii])) return gTablePtr[ii];\n");
  FF"  }\n");
  FF"  ERR; /* non-existent table */\n");
  FF"  return 0; /* suppress compiler message */\n");
  FF"}\n");
  FF"void Ose(void) {\n");
  FF"  PP\"+++++++++++++++++++++++++++++++++++++++++++++++++++++\\n\");\n");
  FF"}\n");
  FF"void FatalError(int ln) {\n");
  FF"  PP\"The last dsl error was\\n\");\n");
  FF"  dsPerror(\"\");\n");
  FF"  PP\"Error in file %%s on line :%%d\\n\",__FILE__,ln);\n");
  FF"  exit(2);\n");
  FF"}\n");
  FF"void OpenTheXdfFile(char *filename) {\n");
  FF"  if((gXdfIn=fopen(filename,\"rb\"))==NULL) {\n");
  FF"    PP\"can't read file '%%s'.\\n\",filename); exit(2);\n");
  FF"  }\n");
  FF"  xdrstdio_create(&gXdrPtr,gXdfIn,XDR_DECODE);\n");
  FF"  gEventNumber=0;\n");
  /* FF"  PP\"OpenTheXdfFile() was successful.\\n\");\n"); */
  FF"}\n");
  FF"float TableValue(char *table,int row,int col,int ss) {");
  FF"/*ss=vec.subscript*/\n");
  FF"  DS_DATASET_T *tp; char *tn; float rv;\n");
  FF"  char *pData;\n");
  FF"  tp=DsPointer(table);\n");
  FF"  if(ss<0) ss=0; /* Not a vector column. */\n");
  FF"  if(!dsColumnTypeName(&tn,tp,(size_t)col)) ERR;\n");
  FF"  if(!dsCellAddress(&pData,tp,(size_t)row,(size_t)col)) ERR;\n");
  FF"  if(!strcmp(tn,\"long\")) rv=*((  long*)(pData+ss*sizeof(long)));\n");
  FF"  else if(!strcmp(tn,\"short\")) rv=*((   short*)(pData+ss*sizeof(short)));\n");
  FF"  else if(!strcmp(tn,\"octet\")) rv=*((unsigned char*)(pData+ss));\n");
  FF"  else if(!strcmp(tn,\"int\")) rv=*((   int*)(pData+ss*sizeof(int)));\n");
  FF"  else if(!strcmp(tn,\"float\")) rv=*(( float*)(pData+ss*sizeof(float)));\n");
  FF"  else if(!strcmp(tn,\"double\")) rv=*((double*)(pData+ss*sizeof(double)));\n");
  FF"  else if(!strcmp(tn,\"char\")) rv=0;\n");
  FF"  else if(!strcmp(tn,\"unsigned short\"))\n");
  FF"                   rv=*((unsigned short*)(pData+ss*sizeof(unsigned short)));\n");
  FF"  else if(!strcmp(tn,\"unsigned long\")) \n");
  FF"                     rv=*((unsigned long*)(pData+ss*sizeof(unsigned long)));\n");
  FF"  else ERR;\n");
  FF"  return rv;\n");
  FF"}\n");
  FF"int NumberOfRows(char *tableName) {\n");
  FF"  size_t nrow;\n");
  FF"  if(!dsTableRowCount(&nrow,DsPointer(tableName)))  ERR;\n");
  FF"  return (int)nrow;\n");
  FF"}\n");
  FF"char *Path(int addDotC,char *oldPath,char *newEnd) {\n");
  FF"  char *rv; int extra;\n");
  FF"  if(addDotC) extra=2; else extra=0;  /* two extra bytes for the .C */\n");
  FF"  rv=(char*)malloc((size_t)(strlen(oldPath)+strlen(newEnd)+2+extra));\n");
  FF"  if(!rv) { PP\"Fatal error malloc() failed.\\n\"); exit(2); }\n");
  FF"  strcpy(rv,oldPath);\n");
  FF"  if(strlen(rv)>0) strcat(rv,\"/\"); strcat(rv,newEnd);\n");
  FF"  if(addDotC) strcat(rv,\".C\");\n");
  FF"  if(gNfree>=FREE) ERR;\n");
  FF"  gFree[gNfree++]=rv;\n");
  FF"  return rv;\n");
  FF"}\n");
  FF"void HandleADataset(DS_DATASET_T *pDslPtr,char *inPath) {\n");
  FF"  /* If pDslPtr is a dataset, this function calls itself, else if it\n");
  FF"  ** is a table, it writes a single ROOT macro file. */\n");
  FF"  long entryIndex; char *extendedName,*nameOfDataset;\n");
  FF"  bool_t isDataset;\n");
  FF"  DS_DATASET_T *pDslPtrNew;\n");
  FF"  size_t numEntries;\n");
  FF"  if(!dsIsDataset(&isDataset,pDslPtr)) ERR;\n");
  FF"  if(isDataset) {\n");
  FF"    if(!dsDatasetName(&nameOfDataset,pDslPtr)) ERR;\n");
  FF"    if(!dsDatasetEntryCount(&numEntries,pDslPtr)) ERR;\n");
  FF"    for(entryIndex=0;entryIndex<numEntries;entryIndex++) {\n");
  FF"      if(!dsDatasetEntry(&pDslPtrNew,pDslPtr,entryIndex)) ERR;\n");
  FF"      HandleADataset(pDslPtrNew,Path(0,inPath,nameOfDataset));\n");
  FF"    }\n");
  FF"  } else { /* is a table */\n");
  FF"    if(!dsTableName(&nameOfDataset,pDslPtr)) ERR;\n");
  FF"    extendedName=Path(0,inPath,nameOfDataset);\n");
  FF"    if(gNtable>=NTABLE) ERR;\n");
  FF"    gTablePtr[gNtable]=pDslPtr;\n");
  FF"    gTableName[gNtable]=extendedName;\n");
  FF"    gNtable++;\n");
  FF"  }\n");
  FF"}\n");
  FF"void FillListOfTables(void) {\n");
  FF"  gNtable=0;\n");
  FF"  HandleADataset(gPtrCurrentEvent,\"\");\n");
  FF"}\n");
  FF"int GetNextEvent(int doTheFree) {\n");
  FF"  int ii;\n");
  FF"  if(!xdr_dataset(&gXdrPtr,&gPtrCurrentEvent)) return 0;\n");
  FF"  gEventNumber++; /* is this used? */\n");
  FF"  /* Skip free once, when gNfree does not have a meaningful value. */\n");
  FF"  if(doTheFree) { for(ii=0;ii<gNfree;ii++) free(gFree[ii]); }\n");
  FF"  gNfree=0;\n");
  FF"  FillListOfTables();\n");
  /* FF"  PP\"This event has %%d tables:\\n\",gNtable);\n"); */
  /* FF"  for(ii=0;ii<gNtable;ii++) PP\"%%s\\n\",gTableName[ii]);\n"); */
  FF"  return 7;\n");
  FF"}\n");
  fclose(ff);
}
int NumberColumns(int whichTable) {
  size_t nc;
  if(!dsTableColumnCount(&nc,gTablePtr[whichTable])) DERR;
  return (int)nc;
}
int Dimensions(int whichTable,int whichCol) {
  size_t dimensions,dimensionality;
  DS_DATASET_T *pTbl;
  pTbl=gTablePtr[whichTable];
  if(!dsColumnDimensions(&dimensions,pTbl,(size_t)whichCol)) DERR;
  if(!dsColumnDimCount(&dimensionality,pTbl,(size_t)whichCol)) DERR;
  if(dimensionality==0) dimensions=1;
  return (int)dimensions;
}
char *ColName(int whichTable,int icol) {
  char *retValue;
  if(!dsColumnName(&retValue,gTablePtr[whichTable],(size_t)icol)) DERR;
  return retValue;
}
void ErrorCheck(FILE *ff) {
  FF"// I am neglecting the return value of GetVal().\n");
  FF"// This is not so bad here, since this code is automatically\n");
  FF"// generated.  In your code, you should check that GetVal returns\n");
  FF"// true.  It returns false in case of \n");
  FF"// error (eg, row number out of range).\n");
  
}
void PrintStuffForColumns(int itbl,FILE *ff) {
  int idim,dim,ncol,icol; char *cn; /* cn = col name */
  ncol=NumberColumns(itbl);
  for(icol=0;icol<ncol;icol++) {
    dim=Dimensions(itbl,icol);
    cn=ColName(itbl,icol);
    if(dim==1) {                                   /* scalar column */
      ErrorCheck(ff);
      FF"    event->GetVal(&(newStruct[row].%s),\"%s\",\"%s\",row);\n",
                 cn,gTableName[itbl],cn);
    } else if(dim>1) {                             /* vector column */
      if(dim>10) dim=10;
      ErrorCheck(ff);
      for(idim=0;idim<dim;idim++) {
        FF"   event->GetVal(&(newStruct[row].%s[%d]),\"%s\",\"%s\",row,%d);\n",
                 cn,idim,gTableName[itbl],cn,idim);
      }
    } else ERR;
  }
}
char *Slash2X(char *fn) {
  int i;
  if(strlen(fn)>PASS-3) ERR;
  strcpy(gPass,fn);
  for(i=0;gPass[i];i++) if(gPass[i]=='/') gPass[i]='X';
  return gPass;
}
FILE *PointerForTableNumber(int whichtable) {
  char fn[123]; FILE *ff;
  sprintf(fn,"st_%s.C",Slash2X(gTableName[whichtable]));
  ff=Fopen(fn,"w"); if(!ff) ERR;
  return ff;
}
void WriteStrucAllocFiles() {
  FILE *ff; int itable;
  for(itable=0;itable<gNtable;itable++) {
    ff=PointerForTableNumber(itable);
    FF"{\n");
    FF"// This macro may not work as compiled code, I had trouble\n");
    FF"// passing C structures from compiled code to Cint.  However,\n");
    FF"// it runs fast as a macro, (on rscr12, a Pentium pro machine,\n");
    FF"// it fills 17000 cells per second).\n");
    FF"#include \"table_structs.h\"\n");
    FF"#include <stdlib.h>\n");
    FF"struc_%s *struc_%s(Event *event,Int_t nrows) {\n",
        gTableType[itable],Slash2X(gTableName[itable]));
    FF"  Int_t row;\n");
    FF"  size_t neededMemory; struc_%s *newStruct;\n",gTableType[itable]);
    FF"  neededMemory=sizeof(struc_%s)*nrows;\n",gTableType[itable]);
    FF"  printf(\"Needed for struct: %%d bytes.\\n\",neededMemory);\n");
    FF"  newStruct=(struc_%s*)malloc(neededMemory);\n",gTableType[itable]);
    FF"  if(!newStruct) return 0;\n");
    FF"  for(row=nrows-1;row>=0;row--) {\n");
    PrintStuffForColumns(itable,ff);
    FF"  }\n");
    FF"  return newStruct;\n");
    FF"}\n");
    FF"}\n");
    fclose(ff);
  }
}
void WriteEventC() {
  char *scalVec,*typename,*cn,*cc; int iotwhtt,i,ncol,icol,ndim,idim;
  FILE *ff=Fopen("Event.C","w");
  if(!ff) ERR;
  FF"#include \"TRandom.h\"\n");
  FF"#include \"Event.h\"\n");
  FF"#include <stdlib.h>\n");
  FF"ClassImp(Event)\n");
  for(i=0;i<gNtype;i++) FF"ClassImp(%s)\n",gTypesList[i]);
  for(i=gNtable-1;i>=0;i--) {
    FF"TClonesArray *gRows_%s;\n",Slash2X(gTableName[i]));
  }
  FF"#define PP printf(\n");
  FF"extern \"C\" float TableValue(char *tableName,");
  FF"int row,int col,int ss);\n");
  FF"Event::Event() {\n");
  for(i=0;i<gNtable;i++) {
    cc=Slash2X(gTableName[i]);
    FF"  if(!gRows_%s) gRows_%s = \n",cc,cc);
    FF"            new TClonesArray(\"%s\", 1000);  \n",gTableType[i]);
    FF"  nRows_%s=0; \n",cc);
    FF"  rows_%s=gRows_%s;\n",cc,cc);
  }
  FF"}\n");
  FF"Event::~Event() {\n");
  for(i=0;i<gNtable;i++) {
    cc=Slash2X(gTableName[i]);
    FF"  rows_%s->Clear();\n",cc);
  }
  FF"}\n");
  FF"void Event::GetCol(char *tbl,char **col,char **dtype,int wc) {\n");
  FF"  char *v=\"vector\",*s=\"scalar\";\n");
  for(i=0;i<gNtable;i++) {
    FF"  if(!strcmp(tbl,\"%s\")) {\n",gTableName[i]);
    ncol=NumberColumns(i);
    FF"    switch(wc) {\n");
    for(icol=0;icol<ncol;icol++) {
      if(Dimensions(i,icol)>1) scalVec="v"; else scalVec="s";
      cn=ColName(i,icol);
      FF"    case %2d: *col=\"%s\"; *dtype=%s; break;\n",icol,cn,scalVec);
    }
    FF"      default: *col=0; *dtype=0; break;\n");
    FF"    } // end switch\n");
    FF"  } // end if\n");
  }
  FF"}\n");
  FF"void Event::GetTableName(char **tableName,int whichTable) {\n");
  FF"  switch(whichTable) {\n");
  for(i=0;i<gNtable;i++) {
    FF"    case %d: *tableName=\"%s\"; break;\n",i,gTableName[i]);
  }
  FF"    default: *tableName=0; break;\n");
  FF"  }\n");
  FF"}\n");
  FF"Int_t Event::GetVal(Float_t *val,char *table,char *col,Int_t irow) {\n");
  FF"  // Returns non-zero for error.\n");
  for(i=0;i<gNtable;i++) {
    cc=Slash2X(gTableName[i]);
    FF"  if(!strcmp(table,\"%s\")) {\n",gTableName[i]);
    FF"    if(irow>=nRows_%s) return 0;  ",cc);
    FF"// irow out of range\n");
    FF"    if(irow<0) return 0;  // irow out of range\n");
    FF"    %s *aRow = (%s*)",gTableType[i],gTableType[i]);
    FF"rows_%s->UncheckedAt(irow);\n",cc);
    FF"    if(!aRow) return 0; // bug in program, or in ROOT\n");
    ncol=NumberColumns(i);
    for(icol=0;icol<ncol;icol++) {
      if(Dimensions(i,icol)==1) { /* vectors are handled ~30 lines below */
        cn=ColName(i,icol);
        FF"    if(!strcmp(col,\"%s\")) { *val=aRow->Get_%s(); ",cn,cn);
        FF"      return 7; }\n");
      }
    }
    FF"    return 0; // non-existent column\n");
    FF"  }\n");
  }
  FF"  return 0; // non-existent table\n");
  FF"}\n");
  FF"Int_t Event::GetVal(Float_t *val,char *table,");
  FF"char *col,Int_t irow,Int_t vecIndex) {\n");
  FF"  // Returns non-zero for error.\n");
  for(i=0;i<gNtable;i++) {
    cc=Slash2X(gTableName[i]);
    FF"  if(!strcmp(table,\"%s\")) {\n",gTableName[i]);
    FF"    if(irow>=nRows_%s) return 0;  ",cc);
    FF"// irow out of range\n");
    FF"    if(irow<0) return 0;  // irow out of range\n");
    FF"    %s *aRow = (%s*)",gTableType[i],gTableType[i]);
    FF"rows_%s->UncheckedAt(irow);\n",cc);
    FF"    if(!aRow) return 0; // bug in program, or in ROOT\n");
    ncol=NumberColumns(i);
    for(icol=0;icol<ncol;icol++) {
      if(Dimensions(i,icol)>1) { /* scalars handled ~30 lines above */
        cn=ColName(i,icol);
        FF"    if(!strcmp(col,\"%s\")) { ",cn);
        FF"*val=aRow->Get_%s(vecIndex); return 7; }\n",cn);
      }
    }
    FF"    return 0; // non-existent column\n");
    FF"  }\n");
  }
  FF"  return 0; // non-existent table\n");
  FF"}\n");
  FF"Int_t Event::Nrow(char *table) {\n");
  for(i=0;i<gNtable;i++) {
    FF"  if(!strcmp(table,\"%s\")) {\n",gTableName[i]);
    FF"    // printf(\"r %%d.\\n\",nRows_%s);\n",Slash2X(gTableName[i]));
    FF"    return nRows_%s;\n",Slash2X(gTableName[i]));
    FF"  }\n");
  }
  FF"  printf(\"Nonexistent table: %%s.\\n\",table);\n");
  FF"  return 0; // non-existent table\n");
  FF"}\n");
  FF"void Event::AddRow(int row,char *tableName) {\n");
  for(i=0;i<gNtable;i++) {
    cc=Slash2X(gTableName[i]);
    FF"  if(!strcmp(tableName,\"%s\")) {\n",gTableName[i]);
    FF"    TClonesArray &rows%d = *rows_%s;\n",i,cc);
    FF"    new(rows%d[nRows_%s++]) %s(row,tableName);\n",i,cc,gTableType[i]);
    FF"  }\n");
  }
  FF"}\n"); 
  FF"void Event::Clear() {   // Does this get called?\n");
  for(i=0;i<gNtable;i++) {
    cc=Slash2X(gTableName[i]);
    FF"  rows_%s->Clear();\n",cc);
  }
  FF"}\n");
  FF"void Event::Finish() {  // Does this get called?\n");
  for(i=0;i<gNtable;i++) {
    cc=Slash2X(gTableName[i]);
    FF"  gRows_%s->Delete();\n",cc);
    FF"  delete gRows_%s;\n",cc);
    FF"  gRows_%s=0;\n",cc);
    FF"  rows_%s=0;\n",cc);
  }
  FF"}\n");
  for(i=0;i<gNtype;i++) {
    iotwhtt=IndexOfSomeTableWhichHasThisType(i);
    typename=gTypesList[i];
    FF"%s::%s(int row,char *tableName) :TObject() {\n",typename,typename);
    FF"  Int_t uu;\n");
    ncol=NumberColumns(iotwhtt);
    for(icol=0;icol<ncol;icol++) {
      ndim=Dimensions(iotwhtt,icol);
      cn=ColName(iotwhtt,icol);
      if(ndim==1) {
        FF"  %s=   TableValue(tableName,row,%d,-1);\n",cn,icol);
      } else {
        FF"  for(uu=%d-1;uu>=0;uu--) {\n",ndim);
        FF"    %s[uu]=TableValue(tableName,row,%d,uu);\n",cn,icol);
        FF"  }\n");
      }
    }
    FF"}\n"); /* One ctor in generated code per table type */
  }
  fclose(ff);
}
int IndexOfSomeTableWhichHasThisType(int itype) {
  int rv;
  for(rv=0;rv<gNtable;rv++) {
    if(!strcmp(gTableType[rv],gTypesList[itype])) return rv;
  }
  ERR;
  return 0; /* to keep compiler quiet */
}
void WriteEventH() {
  int itype,itbl,ncol,icol,ndim,idim,iotwhtt; char *xx,*tname,*cn;
  FILE *ff=Fopen("Event.h","w");
  FF"#ifndef XDF2ROOT_H\n");
  FF"#define XDF2ROOT_H\n");
  FF"#include \"TObject.h\"\n");
  FF"#include \"TClonesArray.h\"\n");
  FF"#include <iostream.h>\n");
  for(itbl=0;itbl<gNtable;itbl++) {
    xx=Slash2X(gTableName[itbl]);
    FF"#define MAXROWS_%s 100\n",xx);
  }
  FF"/* max over all the events */\n");
  for(itype=0;itype<gNtype;itype++) {
    iotwhtt=IndexOfSomeTableWhichHasThisType(itype);
    tname=gTypesList[itype];
    FF"class %s : public TObject {  // corresponds to a table type\n",tname);
    FF"private:  // public doesn't work for ");
    FF"array types as data members (core dumps).\n");
    ncol=NumberColumns(iotwhtt);
    for(icol=0;icol<ncol;icol++) {
      ndim=Dimensions(iotwhtt,icol);
      cn=ColName(iotwhtt,icol);
      if(ndim==1) {
        FF"        Float_t      %s;\n",cn);
      } else if(ndim>1) {
        FF"        Float_t      %s[%d];\n",cn,ndim);
      } else ERR;
    }
    FF"public:\n");
    for(icol=0;icol<ncol;icol++) {
      ndim=Dimensions(iotwhtt,icol);
      cn=ColName(iotwhtt,icol);
      if(ndim==1) {
        FF"        Float_t    Get_%s() { return %s; }\n",cn,cn);
      } else if(ndim>1) {
        FF"        Float_t    Get_%s(Int_t icomp) { return %s[icomp]; }\n",
        cn,cn);
      }
    }
    FF"	%s() {;}\n",tname);
    FF"	%s(int row,char *tableName);\n",tname);
    FF"        virtual ~%s() {;}\n",tname);
    FF"        ClassDef(%s,1)\n",tname);
    FF"};\n");
  }
  FF"class Event : public TObject {\n");
  FF"private:\n");
  for(itbl=0;itbl<gNtable;itbl++) {
    xx=Slash2X(gTableName[itbl]);
    FF"   Int_t nRows_%s;        // 1 line/table\n",xx);
    FF"   TClonesArray *rows_%s; // 1 line/table\n",xx);
  }
  FF"public:\n");
  FF"   Event();\n");
  FF"   virtual ~Event();\n");
  FF"   void    Clear();\n");
  FF"   void    Finish();\n");
  FF"   void    AddRow(int row,char *table);\n");
  FF"   void    GetCol(char *table,char **col,char **dtype,int icol);\n");
  FF"   void    GetTableName(char **tableName,int whichTable);\n");
  FF"Int_t GetVal(Float_t *val,char *tbl,char *col,Int_t irow,Int_t ivec);\n");
  FF"Int_t GetVal(Float_t *val,char *tbl,char *col,Int_t irow);\n");
  FF"   Int_t   Nrow(char *table);\n");
  FF"   ClassDef(Event,1)  //Event structure\n");
  FF"};\n");
  FF"#endif\n");
  fclose(ff);
}
void WriteMainEventC() {
  int itbl;
  FILE *ff=Fopen("MainEvent.C","w");
  FF"#include <stdlib.h>\n");
  FF"#include \"TROOT.h\"\n");
  FF"#include \"TFile.h\"\n");
  FF"#include \"TTree.h\"\n");
  FF"#include \"TBranch.h\"\n");
  FF"#include \"TClonesArray.h\"\n");
  FF"#include \"TStopwatch.h\"\n");
  FF"#include \"Event.h\"\n");
  FF"int Error;\n");
  FF"#define PP printf(\n");
  FF"extern \"C\" void OpenTheXdfFile(char *filename);\n");
  FF"extern \"C\" int GetNextEvent(int skipTheFree);\n");
  FF"extern \"C\" int NumberOfRows(char *tableName);\n");
  FF"main(int nnn, char **aaa) {\n");
  FF"  char *table;\n");
  FF"   TROOT simple(\"simple\",\"From xdf2root\");\n");
  if(gOptionC) FF"   Int_t nevent=0,comp=0,split=0; ");
  else         FF"   Int_t nevent=0,comp=1,split=0; ");
  FF"// comp=0 -> no compress split=0 -> no branches\n");
  FF"   Int_t nb=0,ev,t,bufs,nrow;\n");
  FF"   Event *event; TBranch *b=0;\n");
  FF"   TStopwatch timer; timer.Start();\n");
  FF" \n");
  FF"   if(nnn!=2) { PP\"Usage: %%s file.xdf\\n\",aaa[0]); return 1; }\n");
  FF"   OpenTheXdfFile(aaa[1]);\n");
  FF"   TFile hfile(\"Event.root\",\"RECREATE\",\"");
  FF"xdf2root\");\n");
  FF"   hfile.SetCompressionLevel(comp);\n");
  FF"   TTree *tree=new TTree(\"T\",\"An example of a ROOT tree\");\n");
  FF"   tree->SetAutoSave(1000000000);  ");
  FF"// autosave when 1 Gbyte written\n");
  FF"   bufs=256000; if (split) bufs /= 4;\n");
  FF"\n");
  FF"   for(ev=0;ev<9999;ev++) {\n");
  FF"      if(!GetNextEvent((int)ev)) break; nevent++;\n");
  FF"      printf(\">> Reading event %%d from xdf file.\\n\",ev+1);\n");
  if(gOptionE) {
    FF"      if(ev!=%d) continue;\n",gOptionEIndex-1);
  }
  /* FF"      cout<<\"event=\"<<ev<<endl;\n"); */
  FF"      event=new Event();\n");
  FF"      if(b) b->SetAddress(event); ");
  FF"else b=tree->Branch(\"event\",event,bufs,split);\n");
  FF"\n");
  for(itbl=0;itbl<gNtable;itbl++) {
    FF"      table=\"%s\"; nrow=NumberOfRows(table);\n",gTableName[itbl]);
    FF"      for(t=0;t<nrow;t++) event->AddRow(t,table);\n");
    FF"      printf(\">> %3d percent done,  event %%d.\\n\",1+ev);\n",
          (100*(1+itbl))/gNtable);
    FF"\n");
  }
  FF"      nb += tree->Fill();\n");
  FF"      delete event;\n");
  FF"   }\n");
  FF"   hfile.Write(); /* tree->Print(); */ hfile.Close();\n");
  FF" \n");
  FF"   //  Stop timer and print results\n");
  FF"   timer.Stop();\n");
  FF"   Float_t mbytes=0.000001*nb;\n");
  FF"   Double_t rtime=timer.RealTime();\n");
  FF"   Double_t ctime=timer.CpuTime();\n");
  FF" \n");
  /*
  FF"   printf(\"\\n%%d events and %%d bytes ");
  FF"processed.\\n\",nevent,nb);\n");
  FF"   printf(\"RealTime=%%f seconds, ");
  FF"CpuTime=%%f seconds\\n\",rtime,ctime);\n");
  FF"   printf(\"compression level=%%d, split=%%d.\\n\",comp,split);\n");
  FF"   printf(\"You wrote %%f ");
  FF"Mbytes/Realtime seconds\\n\",mbytes/rtime);\n");
  FF"   printf(\"You wrote %%f ");
  FF"Mbytes/Cputime seconds\\n\",mbytes/ctime);\n");
  FF"   //printf(\"file compression factor ");
  FF"= %%f\\n\",hfile.GetCompressionFactor());\n");
  */
  FF"   fprintf(stderr,\"Normal end of ");
  FF"automatically generated code..\\n\");\n");
  FF" \n");
  FF"   return 0;\n");
  FF"}\n");
  fclose(ff);
}
void WriteTestGetVal(int macroNumber) {
  int itbl,icol,ncol,ndim,idim; char *tn,*cn; char didOne=0;
  FILE *ff;
  if(macroNumber==1) ff=Fopen("small.C","w");
  if(macroNumber==2) ff=Fopen("large.C","w");
  FF"{\n");
  FF"// This ROOT macro shows how to access the table values from the\n");
  FF"// xdf file using class member functions.  If you prefer a\n");
  FF"// more STAR-like syntax (eg, to import STAR PAM C code), see\n");
  FF"// the other sample macro made by xdf2root (struc.C).\n");
  FF"//\n");
  if(macroNumber==2) {
    FF"// You may want to comment or delete many of the lines\n");
    FF"// (eg, you don't want to look at all columns of all tables).\n");
    FF"//\n");
    FF"// You may not want to look at all rows.    Search for 'nrows'.\n");
    FF"//\n");
    FF"// You may not want to look at all events.  Search for 'nevent'.\n");
    FF"//\n");
  }
  FF"// Note that GetVal() has an extra argument for vector columns.\n");
  FF"//\n");
  FF"   gROOT->Reset();\n");
  FF"   TFile f(\"Event.root\");\n");
  FF"   TTree *T = (TTree*)f.Get(\"T\");\n");
  FF"   TStopwatch timer; timer.Start();\n");
  FF"   Event *event = new Event();   ");
  FF"//create the event object outside the loop\n");
  FF"   TBranch *branch  = T.GetBranch(\"event\");\n");
  FF"   branch->SetAddress(event);\n");
  FF"   Int_t nevent = T.GetEntries(); Int_t nb = 0;\n");
  FF"   printf(\"nevent = %%d\\n\",nevent);\n");
  FF"   for (Int_t i=0;i<nevent;i++) {\n");
  if(macroNumber==1) {
    FF"     if(i!=nevent/2) continue;\n");
  }
  FF"      Int_t nrows;\n");
  FF"      nb += T.GetEvent(i);\n");
  FF"      event = (Event*)branch->GetAddress();\n");
  FF"      printf(\"Event:%%d\\n\",i);\n");
  for(itbl=0;itbl<gNtable;itbl++) {
    tn=gTableName[itbl];
    if( macroNumber==1 && itbl<gNtable-1 ) {  /* do at least one */
      if(itbl<gNtable/4) continue;
      if(NumberOfRows(__LINE__,tn)<3) continue;
    }
    if(didOne&&macroNumber==1) continue; didOne=7;
    if(macroNumber==1) PP">> Using %s for small.C.  It has %d rows.\n",tn,
          NumberOfRows(__LINE__,tn));
    FF"      /////////////////////////// table %s\n",tn);
    FF"      nrows=event->Nrow(\"%s\");\n",tn);
    FF"      printf(\"Table %s ",tn);
    FF"has %%d rows.\\n\",nrows);\n");
    if(macroNumber==1) {
      FF"      Int_t lower = nrows/2;\n");
      FF"      Int_t upper = lower+2; if(upper>nrows) upper=nrows;\n");
      FF"      for(Int_t row=lower;row<upper;row++) {\n");
    } else if(macroNumber==2) {
      FF"      for(Int_t row=0;row<nrows;row++) {\n");
    }
    FF"        Float_t val;\n");
    ncol=NumberColumns(itbl);
    for(icol=0;icol<ncol;icol++) {
      ndim=Dimensions(itbl,icol);
      cn=ColName(itbl,icol);
      if(ndim==1) {
        ErrorCheck(ff);
        FF"        event->GetVal(&val,\"%s\",\"%s\",row);\n",tn,cn);
        FF"        printf(\"%s[%%d].%s = %%g\\n\",row,val);\n",tn,cn);
      } else {
        ErrorCheck(ff);
        for(idim=0;idim<ndim;idim++) {
          if(idim>5) { FF"        // vector elementation truncated\n");break;}
          FF"        event->GetVal(&val,\"%s\",\"%s\",row,%d);\n",tn,cn,idim);
          FF"        printf(\"%s[%%d].%s[%d] = %%g\\n\",row,val);\n",
                tn,cn,idim);
        }
      }
    }
    FF"      }\n");
  }
  FF"      event->Clear();                    //clear array\n");
  FF"   }\n");
  FF"   timer.Stop();\n");
  FF"   Float_t mbytes = T.GetTotBytes()*1.e-6;\n");
  FF"   Double_t rtime = timer.RealTime();\n");
  FF"   Double_t ctime = timer.CpuTime();\n");
  FF"   printf(\"Number of events = %%d\\n\",nevent);\n");
  FF"   printf(\"RealTime=%%f seconds, ");
  FF"CpuTime=%%f seconds\\n\",rtime,ctime);\n");
  FF"   printf(\"You have scanned %%f ");
  FF"Mbytes/Realtime seconds\\n\",mbytes/rtime);\n");
  FF"   printf(\"You have scanned %%f ");
  FF"Mbytes/Cputime seconds\\n\",mbytes/ctime);\n");
  FF"//  Delete ClonesArray and histogram objects\n");
  FF"   event->Finish();\n");
  FF"   f.Close();\n");
  FF"}\n");
  fclose(ff);
}
void WriteTestStruct() {
  char *tx,*ttype,*cn,*tn; int itbl,ncol,icol,idim,ndim;
  FILE *ff=Fopen("struc.C","w");
  FF"{\n");
  FF"// THIS MACRO IS NOT GUARANTEED TO RUN.  It is supplied to\n");
  FF"// you, along with the st_*.C load files, to serve as a guide\n");
  FF"// in case you insist on using star-like codisms such as\n");
  FF"// tableName[rowNumber].columnName.  YOU WILL PROBABLY BE\n");
  FF"// BETTER OFF TO IGNORE THIS FILE AND THE st_*.C FILES,\n");
  FF"// and simply use small.C and large.C.  Your root code\n");
  FF"// will be much simpler and more maintainable.\n");
  FF"//\n");
  FF"// The STAR-like structures used in this file are malloc()'d by\n");
  FF"// functions in the st_*.C files.  You must load all of these\n");
  FF"// that correspond to the functions that you leave uncommented\n");
  FF"// in this file.  To load, type at the root prompt, eg,\n");
  FF"//       .L st_dslTASdataXtsspar.C\n");
  FF"//\n");
  FF"  #include \"table_structs.h\"\n");
  FF"  printf(\"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX");
  FF"XXXXXXXXXXXXXXXXXXXXXXXXXXXX\\n\");\n");
  FF"  gROOT->Reset(); TFile f(\"Event.root\"); ");
  FF"TTree *T = (TTree*)f.Get(\"T\");\n");
  FF"  TStopwatch timer; timer.Start();\n");
  FF"  printf(\"Starting timer.\\n\");\n");
  FF"  Event *event = new Event();   ");
  FF"//create the event object outside the loop\n");
  FF"  TBranch *branch  = T.GetBranch(\"event\");\n");
  FF"  branch->SetAddress(event);\n");
  FF"  Int_t nevent = T.GetEntries(); Int_t nb = 0;\n");
  FF"  printf(\"nevent = %%d\\n\",nevent);\n");
  FF"  for (Int_t i=0;i<nevent;i++) {\n");
  FF"     Int_t nrows;\n");
  FF"     nb += T.GetEvent(i);               ");
  FF"//read complete event in memory\n");
  FF"     printf(\"Event:%%d\\n\",i);\n");
  for(itbl=0;itbl<gNtable;itbl++) {
    tn=gTableName[itbl]; ttype=gTableType[itbl]; tx=Slash2X(tn);
    FF"     nrows=event->Nrow(\"%s\");\n",tn);
    FF"     printf(\"Table %s has %%d rows.\\n\",nrows);\n",tn);
    FF"     struc_%s *st_%s=struc_%s(event,nrows);\n",ttype,tx,tx);
    FF"     if(!st_%s) {\n",tx);
    FF"       printf(\"Structure alloc failed.\\n\");\n");
    FF"     } else {\n");
    FF"       for(Int_t row=0;row<nrows;row++) {\n");
    FF"printf(\"-------------------------------");
    FF"------------------\\n\");\n");
    ncol=NumberColumns(itbl);
    for(icol=0;icol<ncol;icol++) {
      cn=ColName(itbl,icol);
      ndim=Dimensions(itbl,icol);
      if(ndim==1) {
        FF"printf(\"%s[%%d].%s = %%g\\n\",row,st_%s[row].%s);\n",
        tn,cn,tx,cn);
      } else {
        for(idim=0;idim<ndim;idim++) {
          if(idim>5) { FF"        // vector elementation truncated\n");break;}
          FF"printf(\"%s[%%d].%s[%d] = %%g\\n\",",tn,cn,idim);
          FF"row,st_%s[row].%s[%d]);\n",tx,cn,idim);
        }
      }
    }
    FF"       if(st_%s) free(st_%s);\n",tx,tx);
    FF"       }\n");
    FF"     } /* End of else */\n");
  }
  FF"     event->Clear();                    //clear array\n");
  FF"  }\n");
  FF"  timer.Stop();\n");
  FF"  printf(\"Stopping timer.\\n\");\n");
  FF"  Float_t mbytes = T.GetTotBytes()*1.e-6;\n");
  FF"  Double_t rtime = timer.RealTime();\n");
  FF"  Double_t ctime = timer.CpuTime();\n");
  FF"  printf(\"Number of events = %%d\\n\",nevent);\n");
  FF"  printf(\"RealTime=%%f seconds, ");
  FF"CpuTime=%%f seconds\\n\",rtime,ctime);\n");
  FF"  printf(\"You have scanned %%f ");
  FF"Mbytes/Realtime seconds\\n\",mbytes/rtime);\n");
  FF"  printf(\"You have scanned %%f ");
  FF"Mbytes/Cputime seconds\\n\",mbytes/ctime);\n");
  FF"//  Delete ClonesArray and histogram objects\n");
  FF"  event->Finish();\n");
  FF"  f.Close();\n");
  FF"}\n");
  fclose(ff);
}
void WriteHist() {
  FILE *ff=Fopen("hist.C","w");
  FF"{\n");
  FF"  gROOT->Reset();\n  Int_t isScalar,vecIdx,nbin; Float_t val;\n");
  FF"  char title[150];\n");
  FF"  char ans[10],*tableName,*colName,*scalvec;\n");
  FF"  Axis_t xmax,xmin;\n");
  FF"  TFile f(\"Event.root\"); TTree *T = (TTree*)f.Get(\"T\");\n");
  FF"  Event *event = new Event();\n  Int_t ii,nrow;\n");
  FF"  TBranch *branch  = T.GetBranch(\"event\");\n");
  FF"  branch->SetAddress(event);\n  Int_t nevent = T.GetEntries();\n");
  FF"  Int_t whichEvent=nevent/2;\n  T.GetEvent(whichEvent);\n");
  FF"  event = (Event*)branch->GetAddress();\n  printf(\"Event number %%d ");
  FF"(count from one, like FORTRAN).\\n\",\n      1+whichEvent);\n");
  FF"  printf(\"oooooooooooooooooooooooooooooooooooooooooooooooooo\\n\");\n");
  FF"  for(Int_t cnt=0;cnt<9999;cnt++) {\n");
  FF"    Int_t nr;\n");
  FF"    event->GetTableName(&tableName,cnt);\n");
  FF"    if(!tableName) break; nr=event->Nrow(tableName);\n");
  FF"    printf(\"%%2d %%33s  %%7d rows\\n\",cnt+1,tableName,nr);\n");
  FF"  }\n");
  FF"  do {\n");
  FF"    printf(\"Type a NUMBER from the above list:\\n\"); gets(ans);\n");
  FF"  } while(ans[0]>'9'||ans[0]<'0');\n");
  FF"  event->GetTableName(&tableName,(int)(atoi(ans)-1));\n");
  FF"  printf(\"oooooooooooooooooooooooooooooooooooooooooooooooooo\\n\");\n");
  FF"  for(Int_t cnt=0;cnt<9999;cnt++) {\n");
  FF"    event->GetCol(tableName,&colName,&scalvec,cnt);\n");
  FF"    if(!colName) break;  printf(\"%%2d %%s\\n\",cnt+1,colName);\n");
  FF"  }\n");
  FF"  do {\n");
  FF"    printf(\"Type a NUMBER from the above list:\\n\"); gets(ans);\n");
  FF"  } while(ans[0]>'9'||ans[0]<'0');\n");
  FF"  event->GetCol(tableName,&colName,&scalvec,(int)(atoi(ans)-1));\n");
  FF"  printf(\"Column %%s is of data type %%s.\\n\",colName,scalvec);\n");
  FF"  if(!strcmp(scalvec,\"scalar\")) isScalar=7; else isScalar=0;\n");
  FF"  if(!isScalar) {\n");
  FF"    printf(\"Type index of vector ");
  FF"element (start from 0, like C)\\n\");\n");
  FF"    gets(ans); vecIdx=atoi(ans); if(vecIdx<0) vecIdx=0;\n");
  FF"    printf(\"Vector index = %%d\\n\",vecIdx);\n");
  FF"  }\n");
  FF"  nrow=event->Nrow(tableName);\n");
  FF"  printf(\"There are %%d rows.\\n\",nrow);\n");
  FF"  printf(\"How many bins do you want?\\n\"); ");
  FF"gets(ans); nbin=atoi(ans);\n");
  FF"  printf(\"If drawing window over net, have patience.\\n\");\n");
  FF"\n");
  FF"  if(isScalar) {\n");
  FF"    sprintf(title,\"%%s,  %%s\",tableName,colName);\n");
  FF"  } else {\n");
  FF"    sprintf(title,\"%%s,  %%s(%%d)\",tableName,colName,vecIdx);\n");
  FF"  }\n");
  FF"  c1 = new TCanvas(\"c1\",\"xdf2root\",200,10,700,500);\n");
  FF"  c1->SetFillColor(42);\n");
  FF"  c1->GetFrame()->SetFillColor(21);\n");
  FF"  c1->GetFrame()->SetBorderSize(6);\n");
  FF"  c1->GetFrame()->SetBorderMode(-1);\n");
  FF"  xmax=-1e20; xmin=1e20;\n");
  ErrorCheck(ff);
  FF"  for(ii=0;ii<nrow;ii++) {\n");
  FF"    if(isScalar) event->GetVal(&val,tableName,colName,ii);\n");
  FF"    else         event->GetVal(&val,tableName,colName,ii,vecIdx);\n");
  FF"    if(xmax<val) xmax=val; if(xmin>val) xmin=val;\n");
  FF"  }\n");
  FF"  printf(\"The minimum value is %%g, ");
  FF"and the max is %%g.\\n\",xmin,xmax);\n");
  FF"  xmax+=(xmax-xmin)*0.05;\n");
  FF"  xmin-=(xmax-xmin)*0.05;\n");
  FF"  hpxpy  = new TH1F(title,title,nbin,xmin,xmax);\n");
  FF"  hpxpy->SetFillColor(48);\n");
  FF"  gRandom->SetSeed();\n");
  FF"  Float_t px;\n");
  FF"  const Int_t kUPDATE = 1000;\n");
  ErrorCheck(ff);
  FF"  for ( Int_t i=0; i<nrow; i++) {\n");
  FF"     if(isScalar) event->GetVal(&px,tableName,colName,i);\n");
  FF"     else         event->GetVal(&px,tableName,colName,i,vecIdx);\n");
  FF"     // printf(\"Adding %%g to the histogram.\\n\",px);\n");
  FF"     hpxpy->Fill(px);\n");
  FF"  }\n");
  FF"  hpxpy->Draw();\n");
  FF"  c1->Modified();\n");
  FF"  c1->Update();\n");
  FF"  printf(\" * * * * * * * * * * * * * * * * * * * * * * * *\\n\");\n");
  FF"  printf(\" *   You may have to use the Refresh menu item  * \\n\");\n");
  FF"  printf(\" *   under the Options menu to get the stuff    * \\n\");\n");
  FF"  printf(\" *   to appear in the new window.               * \\n\");\n");
  FF"  printf(\" * * * * * * * * * * * * * * * * * * * * * * * *\\n\");\n");
  FF"}\n");
  fclose(ff);
}
void WriteTheOutputFiles() {
  WriteMakefile();
  WriteStrucFile();
  WriteLinkDef();
  WriteDataset_access();
  WriteStrucAllocFiles();
  WriteEventC();
  WriteEventH();
  WriteMainEventC();
  WriteTestGetVal(1);
  WriteTestGetVal(2);
  WriteTestStruct();
  WriteRecipe();
  WriteHist();
}
void MakeTwoSubdirectories() {
  char com[123];
  int ii;
  for(ii=0;ii<2;ii++) {
    sprintf(com,"rm -rf %s",gDir[ii]); system(com);
    if(mkdir(gDir[ii],S_IRWXU|S_IRWXG|S_IRWXO)) {
      PP">> Cannot create directory '%s'.\n",gDir[ii]); Exit(__LINE__);
    } else {
      PP">> Made directory %s.\n",gDir[ii]);
    }
  }
}
void CheckStderr() {
  FILE *ff; int ok=7; char line[103];
  ff=fopen("Event.stderr","r"); if(!ff) ERR;
  while(fgets(line,100,ff)) {
    if(strstr(line,"mmzip")) ok=0;
  } fclose(ff);
  if(!ok) {
    Ose(); 
    PP">> Fatal error.\n");
    PP">> There is an error during ROOT's file compression.\n");
    PP">> Please re-run xdf2root with the -c option, which\n");
    PP">> turns off compression.\n");
    Exit(__LINE__);
  }
}
void CheckForNormalEnd() {
  FILE *ff; int ok=0; char line[103];
  ff=fopen("Event.stderr","r"); if(!ff) ERR;
  while(fgets(line,100,ff)) {
    if(strstr(line,"Normal end of automatically generated code")) ok=7;
  } fclose(ff);
  if(!ok) {
    Ose(); 
    PP">> Fatal error.\n");
    PP">> The automatically generated code did not run correctly.\n");
    PP">> See files %s/Event.std* for details.\n",gDir[1]);
    Exit(__LINE__);
  }
}
void CompileAndRunTheOutputFiles(char *absolutePathForInputFile) {
  char com[123];
  PP">> Making root dictionary; compiling/linking generated code.\n");
  if(chdir(gDir[1])) ERR;
  /* The sleep() is because I get error message from make
  ** like "File `Event.C' has modification time in the future.  */
  sleep(2); /* See comment above. */
  system("/usr/local/bin/make");
  if(!FileExists("Event")) {
    Ose();
    PP"The make did not make the executable.\n");
    PP"Check your klog (afs token).\n");
    PP"Check that the STAR stuff is installed:\n");
    PP"    ls $STAR_LIB/$STAR_SYS_LEVEL/sys/lib/libdsl.a\n");
    PP"    ls $STAR_LIB/$STAR_SYS_LEVEL/sys/inc/ds*.h\n");
    PP"If these are ok, then either call the programmer, or apply the \n");
    PP"above make messages to the code in subdirectory %s.\n",
            gDir[1]);
    Exit(__LINE__);
  }
  PP">> Running the new executable.\n");
  sprintf(com,"./Event %s 2> Event.stderr",
  absolutePathForInputFile);
  system(com);
  CheckForNormalEnd();
  CheckStderr();
  PP">> The new executable finished normally.\n");
  if(chdir(gBaseDir)) ERR;
}
int FileExists(char *fn) {
  char com[150];
  sprintf(com,"ls %s 2> /dev/null > /dev/null",fn);
  if(system(com)) return 0;
  return 7;
  /*------------------------
  FILE *ff; int rv;
  if(strstr(fn,"*")||strstr(fn,"?")) return 7;
  ff=fopen(fn,"rb");
  if(ff) { fclose(ff); rv=7; }
  else { rv=0; }
  return rv;
  -------------------------------*/
}
void MoveFilesFromScratchToOutputDir() {
  char *cc,com[222]; int ii;
  /*-----------------------------  old 
  char *files[]={ "Event.root", "small.C", "large.C", "libEvent.so",
  "st_*.C", "help.txt", "table_structs.h", "struc.C", NULL };
  -------------------------------------*/
  char *files[]={ "hist.C", "Event.root", "small.C", "large.C", "libEvent.so",
  "help.txt", NULL };
  Ose();
  for(ii=0;;ii++) {
    cc=files[ii]; if(!cc) break;
    sprintf(com,"mv %s/%s %s",gDir[1],cc,gDir[0]); system(com);
    sprintf(com,"%s/%s",gDir[0],cc);
    if(!FileExists(com)) {
      Ose(); PP">> Fatal error: failed to make %s/%s.\n",gDir[1],cc);
      Exit(__LINE__);
    } else {
      PP">> Final output file:   %s\n",com);
    }
  }
}
void WriteRecipe() {
  FILE *ff; int ii;
  ff=Fopen("help.txt","w");
  FF"The files ending in .C are root macro files.  They\n");
  FF"are offered\n");
  FF"      1. to help root beginners\n");
  FF"      2. to save typing for root experts\n");
  FF"      3. to document use of the data classes that I made\n");
  FF"Their use (except as documenation) is optional.\n");
  FF"\n");
  FF"The file named Event.root is the root data file.\n");
  FF"You will need the file libEvent.so to read the data file.\n");
  FF"\n");
  FF"The file small.C shows how to access the data.\n");
  FF"It does only one table and produces a moderate amount of screen\n");
  FF"output.\n");
  FF"\n");
  FF"The file large.C is the same as small.C, except\n");
  FF"it prints all columns of all rows of all tables for all events.\n");
  FF"\n");
  /*
  FF"The files st_*.C must be loaded before running struc.C,\n");
  FF"they are not needed for the other two macro files.\n");
  */
  /*------------------------------------------
  for(ii=0;ii<gNtable;ii++) FF"       .L st_%s.C\n",Slash2X(gTableName[ii]));
  FF"       .x struc.C  // SUPPLIED AS DOCUMENTATION, MAY NOT RUN.\n");
  ---------------------------------------------------*/
  FF"\n");
  fclose(ff);
}
void DestroyScratchDir() {
  char com[123];
  sprintf(com,"/usr/bin/rm -rf %s",gDir[1]); system(com);
}
#define BLANK 77
void EndingBlurb() {
  char *rr; int ii,end;
  Ose();
  PP">> The output is ");
  PP"in a subdirectory named %s.\n",gDir[0]);
  PP">>\n");
  PP">> Warning: I will erase the directories %s and %s\n",gDir[0],gDir[1]);
  PP">> next time I run.  You may want to rename them.\n");
  PP">> Directory %s contains waste of no interest to typical users.\n",
  gDir[1]);
  PP">>\n");
  if(gOptionE) PP">> Because of option -e, I looked only at event %d.\n",
  gOptionEIndex);
  else PP">> There were %d events.\n", gEventCnt);
  if(gOptionE) rr="the"; else rr="each";
  PP">> There %d tables in %s event.\n",gNtable,rr);
  PP">>\n");
  PP">> Here are a few commands for mouse capture:\n");
  PP">>    For the UNIX prompt:\n");
  PP"         cd %s/%s ; ls -l ; root\n", gBaseDir,gDir[0]);
  PP">>    For the root prompt.\n");
  PP"         gSystem.Load(\"libEvent.so\");\n");
  PP"         .x small.C\n");
  PP"         .x hist.C\n");
  PP"         .q\n");
  PP">>\n");
}
/* 
root
gSystem.Load("libEvent.so"); 
.x hist.C
*/
char *ReadOptions(int nnn, char *aaa[]) {
  int ifc=0,ii; char *cc,*dd,*retVal=0;
  gOptionG=0; gOptionC=0; gOptionE=0;
  for(ii=1;ii<nnn;ii++) {
    cc=aaa[ii];
    if(cc[0]=='-') {
      switch(cc[1]) {
        case 'g': gOptionG=7; break;
        case 'c': gOptionC=7; break;
        case 'h': Usage(); break;
        case 'e': gOptionE=7; gOptionEIndex=atoi(cc+2); break;
        default: Usage();
      }
    } else {
      retVal=cc; ifc++;
    }
  }
  if(ifc>1) Usage();
  if(ifc<1) Usage();
  if(!retVal) Usage();
  if(gOptionE) PP">> Selecting event number %d.\n",gOptionEIndex);
  return retVal;
}
#define COPY 900
void CheckLoadPath(char *fn,char *loadPath) {
  char *cc,ok=0;
  char copy[COPY];
  if(strlen(loadPath)>COPY-3) ERR;
  strcpy(copy,loadPath);
  cc=strtok(copy,":");
  while(cc) {
    sprintf(fn,"%s/libCint*",cc);
    if(FileExists(fn)) { ok=7; break; }
    cc=strtok(NULL,":");
  }
  
  if(!ok) {
    Ose();
    PP"Your LD_LIBRARY_PATH is bad.  It does not have libCint.\n");
    PP"This should have been taken care of in SetEnvironmentalVariables().\n");
    PP"LD_LIBRARY_PATH=\n%s\n",loadPath);
    Exit(__LINE__);
  }
}
void SetEnvironmentalVariables() {
  char ok=0;
  char *ldl;
#if defined(sun)
  ldl="LD_LIBRARY_PATH=/usr/lib:/usr/openwin/lib:/usr/dt/lib:/opt/SUNWsprob\
/lib:/usr/local/src/root/lib";
  if(putenv(ldl)) ERR;
  ok=7;
#endif
  if(!ok) ERR;
}
void CheckForDslHeaders() {
  char *fn="$STAR_LIB/$STAR_SYS_LEVEL/sys/inc/dstype.h";
  if(!FileExists(fn)) {
    Ose();
    PP"I can't read %s.\nPlease check your afs token (klog), and the\n",fn);
    PP"values of the env vars STAR_LIB and STAR_SYS_LEVEL.\n");
    PP"You might try\n");
    PP"STAR_LIB = /afs/rhic/star/arch/sun4os5pc\n");
    PP"STAR_SYS_LEVEL = dev\n");
    Exit(__LINE__);
  }
}
void EnvValNotDefined(char *evname) {
  Ose(); 
  PP"Fatal error:  environmental variable %s is not defined\n",evname);
  PP"for me.  Did you export it?\n");
  if(!strcmp(evname,"LD_LIBRARY_PATH")) {
    PP"In ksh, you would type\n");
    PP"export LD_LIBRARY_PATH=$ROOTSYS/lib:$LD_LIBRARY_PATH\n");
  }
  Exit(__LINE__);
}
void CheckEnvironmentalVariables() {
  char fn[183],*evvalue,*evname,ok;  int ii; FILE *ff;
  char *evlist[]={ "ROOTSYS", "LD_LIBRARY_PATH", NULL };
  CheckForDslHeaders();
  for(ii=0;;ii++) {
    evname=evlist[ii]; if(!evname) break;
    evvalue=getenv(evname);
    if(!evvalue) EnvValNotDefined(evname);
    if(!strcmp(evname,"LD_LIBRARY_PATH")) CheckLoadPath(fn,evvalue);
    if(!strcmp(evname,"ROOTSYS")) {
      sprintf(fn,"%s/include/TROOT.h",evvalue);
      ff=fopen(fn,"r");
      if(ff) {
        fclose(ff);
      } else {
        Ose();
        PP"Fatal error:  I can't read %s.\n",fn);
        PP"Check value of your enenvironmental variable %s.\n",evname);
        PP"Also check that ROOT is installed on this machine.\n");
        Exit(__LINE__);
      }
    }
  }
}
void Set_gDir(char *fullpath) {
  char fn[FN+1]; int i;
  char *prefix1="dir.good";
  char *prefix2="dir.junk";
  for(i=strlen(fullpath)-1;i>=0;i--) if(fullpath[i]=='/') break;
  if(strlen(fullpath+i+1)>FN) ERR;
  strcpy(fn,fullpath+i+1);
  for(i=strlen(fn)-1;i>=0;i--) if(fn[i]=='.') break;
  if(i<0) {
    Ose();
    PP"Fatal error.  I was expecting a period in this: '%s'.\n",fn);
    PP"fullpath=%s.\n",fullpath); exit(2);
  }
  fn[i]=0;
  if(strlen(prefix1)+1+strlen(fn)>DIR) ERR;
  if(strlen(prefix2)+1+strlen(fn)>DIR) ERR;
  sprintf(gDir[0],"%s.%s",prefix1,fn);
  sprintf(gDir[1],"%s.%s",prefix2,fn);
}
void main(int nnnn,char *aaaa[]) {
  char *inFile,absolutePathForInputFile[APFIF+1];
  inFile=ReadOptions(nnnn,aaaa);
  gNfree=0; gEventCnt=0;
  if(!getcwd(gBaseDir,DIRSIZE-3)) ERR;
  if(inFile[0]=='/') {
    if(strlen(inFile)>APFIF) ERR;
    strcpy(absolutePathForInputFile,inFile);
  } else {
    if(strlen(gBaseDir)+1+strlen(inFile)>APFIF) ERR;
    sprintf(absolutePathForInputFile,"%s/%s",gBaseDir,inFile);
  }
  Set_gDir(absolutePathForInputFile);
  SetEnvironmentalVariables();
  CheckEnvironmentalVariables();
  PP">> Input file = %s\n",absolutePathForInputFile);
  PP">> Current directory is %s.\n",gBaseDir);
  OpenTheXdfFile(inFile);
  CheckAllEventsHaveSameTables();            /* Fills gNtable & gTableName. */
                   /* PrintListOfTablesToScreen(); */
  GenerateListOfTableTypes();               /* Fills gNtype and gTypesList. */
                   /* PrintListOfTypes(); */
  MakeTwoSubdirectories();
  PP">> Writing some code.\n");
  WriteTheOutputFiles();
  if(gOptionG) {
    PP"Your -g files are in\n     cd %s/%s\n",gBaseDir,gDir[1]);
    exit(2);
  }
  CompileAndRunTheOutputFiles(absolutePathForInputFile);
  MoveFilesFromScratchToOutputDir();
                   /* DestroyScratchDir(); */
  EndingBlurb();
  PP">> Normal end, %s.\n",inFile);
}
