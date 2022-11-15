#include <assert.h>
#include "StEnumerations.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TString.h"

#include "St_base/StMessMgr.h"

int   ids[100]={0};
char *cds[100]={0};
static void detectorIdInit();
//_____________________________________________________________________________
void detectorId(int *ids=0, char** cds=0)
{
 int   myIds[100];
 char *myCds[100];
 if (!ids) { ids = myIds; cds = myCds; }

 memset(ids,0,sizeof(ids[0])*100);
 memset(cds,0,sizeof(cds[0])*100);

 TString myPath("$STAR/StRoot/StEvent/StEnumerations.h");
 gSystem->ExpandPathName(myPath);

 int notExi = gSystem->AccessPathName(myPath.Data(),kFileExists);
 if (notExi)    { ids[0]=-1; return;}
 FILE *fp = fopen(myPath.Data(),"r");
 if (!fp)       { ids[0]=-1; return;}
 char buf[400];

 int kase = 0;
 //
 // Process the StEnumerations header file.  Once StDetectorId is found
 // we begin the work.  Each instance of k<DetectorName> will be used to
 // build a map, associating the enum value to the string name.  The loop
 // will be terminated once the enumeration block is ended (the "}") is
 // found.
 //
 while (true) {          
   fgets(buf,200,fp);
   int eof = feof(fp);
   if (eof)  break;
   TString tb(buf);

   if(!kase) {
     if (tb.Index("enum")<0)            continue;
     if (tb.Index("StDetectorId")<0)    continue;
     if (tb.Index("=")<0)               continue;
     if (tb.Index("kUnknownId")<0)      continue;
     kase = 1;
   }
   tb.ReplaceAll(" ","");
   if (tb.Index("//")==0)               continue;
   int myK = tb.Index("k"); if (myK <0) break;
   int myEq= tb.Index("="); if (myEq<0) break;
   int myE = tb.Index(",");
   if (myE<0) myE = tb.Index("}");
   if (myE<0)                           break;
   TString com(tb.Data()+myK,myEq-myK);
   int id = gROOT->ProcessLineFast(com);
   ids[0]++; // counts total number of entries
   ids[ids[0]] = id;
   cds[ids[0]] = new char[com.Length()+1];
   strcpy(cds[ids[0]],com.Data());

   if (tb[myE]=='}')                    break;
 }
 fclose(fp);
 for (int i=1;i<=ids[0];i++) {
   printf("%d = %s\n",ids[i],cds[i]);
 }
}
//_____________________________________________________________________________
const char *detectorNameById(StDetectorId id)
{
  if (ids[0]<0) return "Unknown";
  if (!ids[0] ) detectorIdInit();

    for (int i=1;i<=ids[0];i++) { if (ids[i]==id) return cds[i]+1;}
    return "Unknown";
}
//_____________________________________________________________________________
StDetectorId detectorIdByName(const char *name)
{
  if (ids[0]<0) return kUnknownId;
  if (!ids[0] ) detectorIdInit();
  TString tName(name); tName.ReplaceAll("Id","");  
    for (int i=1;i<=ids[0];i++){
      TString tds(cds[i]+1); tds.ReplaceAll("Id","");
      if (tName.Contains(tds,TString::kIgnoreCase)) return (StDetectorId)ids[i];
    }
    return kUnknownId;
}
//_____________________________________________________________________________
void detectorIdInit()
{
  detectorId( ids, cds );
  if ( ids[0] <= 0 ) {
    LOG_FATAL << "Failed to parse StEnumerations.h / StDetectorId enumeration.  Kaboom." << endm;
    assert( 0 == 2015 );
  };
}  
