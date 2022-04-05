// this is auxiliary macro used in StEvent/StEnumerations
// It is invoked once to provide relations between detector id & detector name
// Author V.Perev



void detectorId(int *ids=0,const char** cds=0)
{
 int         myIds[100];
 const char *myCds[100];
 if (!ids) { ids = myIds; cds = myCds; }


 memset(ids,0,sizeof(ids[0])*100);
 memset(cds,0,sizeof(cds[0])*100);

 TString myPath("$STAR/StRoot/StEvent/StEnumerations.h");
 gSystem->ExpandPathName(myPath);
// printf ("=%s\n",myPath.Data());
 int notExi = gSystem->AccessPathName(myPath.Data(),kFileExists);
 if (notExi)    { ids[0]=-1; return;}
 FILE *fp = fopen(myPath.Data(),"r");
 if (!fp)       { ids[0]=-1; return;}
 char buf[400];

 int kase = 0;
 while (2015) {
   fgets(buf,200,fp);
   int eof = feof(fp);
   if (eof)  break;
   TString tb(buf);

   if(!kase) {
//   enum StDetectorId {kUnknownId   = kUnknownIdentifier,
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
//   printf("%s",buf);
   TString com(tb.Data()+myK,myEq-myK);
   int id = gROOT->ProcessLineFast(com);
   ids[0]++;
   ids[ids[0]] = id;
   cds[ids[0]] = new char[com.Length()+1];
   strcpy(cds[ids[0]],com.Data());
//   printf("%d = %s\n",ids[ids[0]],cds[ids[0]]);
   if (tb[myE]=='}')                    break;
 }
 fclose(fp);
 for (int i=1;i<=ids[0];i++) {
   printf("%d = %s\n",ids[i],cds[i]);
}





#if 0

  const char *res =0;
  int nG=0,     nGtot=0,        nGmax=0;
  int nG15=0,   nG15tot=0,      nG15max=0;
  int nP=0,     nPtot=0,        nPmax=0;
  int nP15=0,   nP15tot=0,      nP15max=0;
  int nFile=0,  nEv=0;
  double times[4] = {0};
  const char* file=0;
  int num=0;




  FILE *fp = fopen(file,"r");






  while ((file = it.NextFile())) {
  nFile++;
  num++;
  if ((num%100)==1)  printf("%5d - InputFile = %s\n",num,file);
  FILE *fp = fopen(file,"r");
    if (!fp) { printf("Wrong file %s\n",file); continue;}
    char buf[400];

    while (2007) {
      fgets(buf,200,fp);
      int eof = feof(fp);
      if (eof)  break;
      res = strstr(buf,"StvStEventFiller::fillEvent");
      if (!res) res = strstr(buf,"StiStEventFiller::fillEvent");
      if (res) {                //it is EventFiller
        res = strstr(buf,"global(2):");
        if (res) {      //count globals
           if (nEv == nEvt) break;
           nEv++;
           sscanf(res,"global(2):%d",&nG);
           nGtot+=nG; if (nGmax<nG) nGmax=nG;
        }
        res = strstr(buf,"GOOD globals:");
        if (res) {      //count globals
           sscanf(res,"GOOD globals:%d",&nG15);
           nG15tot+=nG15;if (nG15max<nG15) nG15max=nG15;
        }
        res = strstr(buf,"fillEventPrimaries");
        if (res) {
          res = strstr(buf,"(2):");
          if (res) {
            sscanf(res,"(2):%d",&nP);
           nPtot+=nP;if (nPmax<nP) nPmax=nP;
          }
          res = strstr(buf,"GOOD:");
          if (res) {
            sscanf(res,"GOOD:%d",&nP15);
            nP15tot+=nP15;if (nP15max<nP15) nP15max=nP15;
          }
        }

      }
      const char *cpu = strstr(buf,"Ast =");
      if (cpu) {                //Cpu count
      int kase=0;
      if (strstr(buf,"StBFChain::bfc")) kase = 1;
      if (strstr(buf,"StiMaker::Sti" )) kase = 2;
      if (strstr(buf,"StvMaker::Stv" )) kase = 3;
      double cpuTime = 0;
      if (kase) { // It is our case
        cpuTime = atof(cpu+5);
        times[kase]+=cpuTime;
    } }
    }

    fclose(fp);
  }//end file iter
  printf("In File(s) %s\n",myfile);
  printf("nFiles =%g nEvents = %d <nEvents>=%d\n",nFile,nEv,nEv/nFile);
  printf("nGlob = %10d <nGlob> = %5d nGlobMax=%5d\n"            ,nGtot  ,nGtot  /nEv,nGmax  );
  printf("nGgoo = %10d <nGgoo> = %5d nGgooMax=%5d Ggoo/Gtot=%5.3f\n",nG15tot,nG15tot/nEv,nG15max,double(nG15tot)/nGtot  );
  printf("nPrim = %10d <nPrim> = %5d nPrimMax=%5d Ptot/Gtot=%5.3f\n",nPtot  ,nPtot  /nEv,nPmax  ,double(nPtot  )/nGtot  );
  printf("nPgoo = %10d <nPgoo> = %5d nPgooMax=%5d Pgoo/Ggoo=%5.3f\n",nP15tot,nP15tot/nEv,nP15max,double(nP15tot)/nG15tot);


  printf("CpuTot/Event = %g  CpuTot/tracks = %g\n",times[1]/nEv,times[1]/nGtot);
  if (times[1]>0) {
    if (times[2]) {
      printf("StiTot/Event = %g  StiTot/tracks = %g StiPct=%g%%\n"
     ,times[2]/nEv,times[2]/nGtot,times[2]/(times[1]-times[2])*100+1e-10);}
     else {
      printf("StvTot/Event = %g  StvTot/tracks = %g StvPct=%g%%\n"
     ,times[3]/nEv,times[3]/nGtot,times[3]/(times[1]-times[3])*100+1e-10);}
  }


#endif
}

