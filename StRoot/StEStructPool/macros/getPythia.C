TPythia6* getPythia(int jobId, const char* jobid=NULL){

  int mjobid=1;
  if(jobid){
    char* ptr=strstr(jobid,"_");
    if(ptr){
       ptr++;
       jobId=atoi(ptr);
    }  
  } else {
    mjobid=jobId;
  }

  TPythia6* retVal=new TPythia6();
  retVal->Initialize("CMS","p","p",130.);

  int timestamp=time(NULL);
  int iseed = (int)((float)timestamp/(float)mjobid);

  retVal->SetMRPY(1,iseed);
  retVal->SetMRPY(2,0); 

  cout<<"<seedValue>"<<iseed<<"</seedValue>"<<endl;

  return retVal;

}

  
