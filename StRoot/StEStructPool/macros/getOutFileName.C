char* getOutFileName(const char* baseDir, const char* jobName, const char* type){

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
 
    char* exten[]={".root",".estruct.root"};
    int iext = 0;
       
    TString outputFile(outArea.Data());
    if(strstr(type,"cut")){
      outputFile+="cutHists";
    } else if(strstr(type,"data")){
      outputFile+="dataHists";
    } else {
      outputFile+="events";
      iext=1;
    }

    outputFile+="_";
    if(ptr)outputFile+=ptr;
    outputFile+=exten[iext];

    delete [] JobID;
    char* retVal=new char[strlen(outputFile.Data())+1];
    strcpy(retVal,outputFile.Data());
  
    return retVal;
};




