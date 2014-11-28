int getRandomSeed() {

    char* jobid = gSystem->Getenv("JOBID");
    float fjobid=0.9999;
    if(jobid){ // reset fjobid from job number after _ in the envvar JOBID
        char* ptr=strstr(jobid,"_");
        if(ptr){
            ptr++;
            fjobid=atof(ptr)+1;
        }
    }
    int timestamp=time(NULL); 
    int iseed = (int)((float)timestamp/fjobid);

    return iseed;
};
