char* getOutFileName(const char* baseDir, const char* jobName,
                     const char* type,    int icent = -1){

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
    if (strstr(type,"centralityDefs")) {
      outArea+="cuts";
    } else if (strstr(type,"EStruct")) {
      outArea+="data";
    } else {
      outArea+=type;
    }
    outArea+="/";

    TString createDir("mkdir -p ");
    createDir+=outArea.Data();
    system(createDir.Data());
 
    char* exten[]={".root",".root",".txt"};
    int iext = 0;

    const char* centTag=NULL;
    TString TcentTag;
    if(icent>=0){
      TcentTag+="_M";
      TcentTag+=icent;
      centTag=TcentTag.Data();
    }
       
    TString outputFileFile(outArea.Data());
    if(strstr(type,"cuts")){
      outputFileFile+="cutHists";
    } else if(strstr(type,"data")){
      outputFileFile+="dataHists";
    } else if(strstr(type,"EStruct")){
      outputFileFile+="EStructEvents";
    } else if(strstr(type,"QA")){
      outputFileFile+="QA";
    } else if(strstr(type,"stats")){
      outputFileFile+="stats";
      iext=2;
    } else if(strstr(type,"centralityDefs")){
      outputFileFile+="centralityDefs";
    } else {
      outputFileFile+="data";
      iext=1;
    }

    if(centTag)outputFileFile+=centTag;
    outputFileFile+="_";
    if(ptr)outputFileFile+=ptr;
    outputFileFile+=exten[iext];

    delete [] JobID;
    char* retVal=new char[strlen(outputFileFile.Data())+1];
    strcpy(retVal,outputFileFile.Data());
  
    return retVal;
};



