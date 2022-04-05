/***************************************************************************
 *
 * $Id: TDirIter.cxx,v 1.14 2009/10/06 21:01:30 fine Exp $
 *
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 **************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "TSystem.h"
#include "TObjArray.h"
#include "TError.h"

#include "TDirIter.h"


//ClassImp(TDirIter)
//______________________________________________________________________________
TDirIter::TDirIter(const char *path,Int_t maxlev):fRegx("",0)
{
  fArr = new TObjArray;
  Reset(path,maxlev);
}
//______________________________________________________________________________
TDirIter::~TDirIter()
{
   fArr->Delete();
   delete fArr;
   fArr=0;
}
//______________________________________________________________________________
void TDirIter::Reset(const char *path,Int_t maxlev)
{
  fIter = -1;
  fArr->Delete();
  fFull = "";
  fMaxLev = maxlev;
  if (*path == '@') { //read path's from file
    FILE *in = fopen(path+1,"r");
    if (!in) { 
      fprintf(stderr,"*** TDirIter::Reset failed to open %s ***\n",path+1);
      fSele = -2; 
      return;
    }

    char buf[1024];
    while ( fgets(buf,1024,in) ) {
      if (*buf == '!') continue;	//commented
      if (*buf == '#') continue;	//commented

      char *c = strchr(buf,' ');
      if (c){
	// If space, take first part but don't forget to restore \n
	*c= '\0';
	fFull += buf; 
	fFull += "\n"; 
      } else {
	fFull += buf; 
      }
    }
    fclose(in);

  } else {
    fFull = path;

  }
  // printf("DEBUG --> [%s] [%s]\n",path,fFull.Data());
      
  fSkip = strspn(fFull.Data()," \t\n");
  ResetQQ(fFull.Data()+fSkip);
}
//______________________________________________________________________________
void TDirIter::ResetQQ(const char *path)
{
  TString machine("root://");
  machine += gSystem->HostName();
  machine += "/";
  fLevel = 0; fState = 1; fSele=0; fTop = 0;
  fMaxLevQQ = fMaxLev;
  int n = strcspn(path," \t\n");
  TString myPath; myPath.Insert(0,path,n);
  gSystem->ExpandPathName(myPath);

  fEntrStk[0]=0;
  const char *p = myPath.Data();
  if (*p == '^') p++;
  fFile = p; 
  if (fFile.BeginsWith(machine)) {
    fFile.ReplaceAll(machine,"");
  } else {
    fFile.ReplaceAll(".gov//",".gov:1095//");
  }
  const char *c,*f=fFile.Data();
  c = strpbrk(f,"*#?[]\\");
  if (c) {
    fSele=1;fFile.Remove(c-f,9999);
    int s = fFile.Last('/');
    if (s>0) {fFile.Remove(s,9999);}
    else     {fFile = "";}
  }
  if (fFile.Length()==0) f = ".";
  if (fSele==0) {
    Long_t flags = 0, id = 0, modtime = 0; Long64_t size=1;
    int noexi = 0;
    if (! strstr(f,"://")) 
      noexi = gSystem->GetPathInfo(f,&id,&size,&flags,&modtime);
    if (noexi) { 
      fSele = -2;
      Warning("TDirIter","*** File %s does not exist ***",f);}
    else if (size==0) {
      fSele = -2;
      Warning("TDirIter","*** File %s is empty ***",f);}
    else  if ((flags&2)==0) {
      fSele = -1;
       Warning("TDirIter","*** File %s is special (like socket) ***",f);}
    }
  
  fTop = fFile.Length();
  if (myPath[0]=='^') fTop = 0;
  TString QWE = MakeWild(myPath.Data()+fTop);
  fRegx=TRegexp(QWE);

  p = myPath.Data();
  int lP = myPath.Length();
  if (lP && *p!='^' && strstr(p,"#")==0) {//calculate maxlevQQ
     fMaxLevQQ=1;
     for(int i=fTop+1;i<lP;i++) {  if (p[i]=='/') fMaxLevQQ++; } }
}
//______________________________________________________________________________
const char *TDirIter::NextFile()
{
   if (fIter == -1) {
     const char *name=0;
     while((name=NextFileQ())) {fArr->Add(new TNamed(name,""));}
     fArr->Sort();
   }
   fIter++;
   if (fIter > fArr->GetLast()) return 0;
   return fArr->At(fIter)->GetName();
}

//______________________________________________________________________________
const char *TDirIter::NextFileQ()
{
  
  const char *name = NextFileQQ();
  if (name) return name;
  const char *full = fFull.Data();
  if (full[fSkip]==0) return 0;
  fSkip += strcspn(full+fSkip," \t\n");
  if (full[fSkip]==0) return 0;
  fSkip += strspn(full+fSkip," \t\n");
  if (full[fSkip]==0) return 0;

  ResetQQ(full+fSkip);
  return NextFileQ();
}
//______________________________________________________________________________
const char *TDirIter::NextFileQQ()
{
  if (fSele == -2) return 0;
  if (fSele == -1) {fSele = -2; return fFile.Data();}
  while(2002) {
    if (fState && fLevel < fMaxLevQQ) {	//Last name was directory
      fLevel++; fState=0;
      const char *f = fFile.Data();
      if (*f==0) f=".";
      fEntrStk[fLevel] = gSystem->OpenDirectory(f);     
      fLengStk[fLevel] = fFile.Length();
    }

    const char *name;
    while ((name = gSystem->GetDirEntry(fEntrStk[fLevel])))
    {  
      if (strcmp("." ,name)==0) continue;
      if (strcmp("..",name)==0) continue;
      break;
    }
    if  (name==0) { 
      gSystem->FreeDirectory(fEntrStk[fLevel]);
      if (fLevel<=0) 		return 0;
      fLevel--; fState=0; 
      return NextFileQ();
    }

    fFile.Remove(fLengStk[fLevel],999);
    if (fFile.Length()) fFile += "/"; fFile += name;
    Long_t flags=0; fState=0;
    Long_t id = 0, modtime = 0;Long64_t size=0;
    if (strstr(fFile.Data(),"://")==0)
      gSystem->GetPathInfo(fFile.Data(),&id,&size,&flags,&modtime);
    if (flags & 2) 	fState=1;
    if (fSele==0) 	break;
    int len;
    TString qwe(fFile.Data()+fTop);
    if (fRegx.Index(qwe,&len) >=0) break;
    
 }
   return fFile.Data();
}   
//______________________________________________________________________________
TString TDirIter::MakeWild(const char *re)
{
  TString ts;
  if (re[0]=='^') { ts = re; return ts;}

  for (int i=0;re[i];i++)
  {
    if (i == 0)		{ts+="^" ;}
    if (re[i]=='*')	{ts+="[a-zA-Z0-9_\\.,-= ]*"; 	continue;}
    if (re[i]=='#')	{ts+=".*"; 			continue;}
    if (re[i] == '.')	{ts+="\\.";			continue;}		
    ts += re[i];
  }
  ts += "$";
  return ts;
}
   





       
