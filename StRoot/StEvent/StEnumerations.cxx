#include <assert.h>
#include "StEnumerations.h"
#include "TROOT.h"
#include "TSystem.h"
#include "TString.h"
int         ids[100]={0};
const char *cds[100]={0};
static void detectorIdInit();
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
const char *paths[] = {
                  "./detectorId.C",
    "./StRoot/macros/detectorId.C",
"$STAR/StRoot/macros/detectorId.C",
                                0};

  ids[0] = -1;
  for (int ifa=0;paths[ifa];ifa++) {
    TString path(paths[ifa]);
    gSystem->ExpandPathName(path);
    int notExi = gSystem->AccessPathName(path.Data());
    if (notExi) continue;
    ids[0]=0;
    TString com;
    com.Form("%s((int*)%p,(const char**)%p)",path.Data(),ids,cds);
    gROOT->Macro(com.Data());
    assert(ids[0]);
    break;
  }
}  
