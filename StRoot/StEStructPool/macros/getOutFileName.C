char* getOutFileName(const char* baseDir, const char* jobName,
                     const char* type,    const char* centTag){

    char* jobid=gSystem->Getenv("JOBID");

    if(!jobid){
      jobid=new char[3];
      strcpy(jobid,"A_00");
    }

    char* JobID=new char[strlen(jobid)+1];

    strcpy(JobID,jobid);   
    char* ptr;
    if(ptr=strstr(JobID,"_")){
      *ptr='\0';
      ptr++;
    }
  
    TString outArea(baseDir);
    outArea+="/";
    if(!jobName){
      outArea+=JobID; 
    } else {
      outArea+=jobName;
    }
    outArea+="/";
    outArea+=type; outArea+="/";

    TString createDir("mkdir -p ");
    createDir+=outArea.Data();
    system(createDir.Data());
 
    char* exten[]={".root",".root",".txt"};
    int iext = 0;
       
    TString outputFileFile(outArea.Data());
    if(strstr(type,"cut")){
      outputFileFile+="cutHists";
      outputFileFile+=centTag;
    } else if(strstr(type,"data")){
      outputFileFile+="dataHists";
      outputFileFile+=centTag;
    } else if(strstr(type,"QA")){
      outputFileFile+="QA";
      outputFileFile+=centTag;
    } else if(strstr(type,"stats")){
      outputFileFile+="stats";
      iext=2;
    } else {
      outputFileFile+="events";
      iext=1;
    }

    outputFileFile+="_";
    if(ptr)outputFileFile+=ptr;
    outputFileFile+=exten[iext];

    delete [] JobID;
    char* retVal=new char[strlen(outputFileFile.Data())+1];
    strcpy(retVal,outputFileFile.Data());
  
    return retVal;
};



