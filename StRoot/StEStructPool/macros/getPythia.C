TPythia6* getPythia(int jobId){


  TPythia6* retVal=new TPythia6();
  retVal->Initialize("CMS","p","p",200.);

  int timestamp=time(NULL);
  int iseed = (int)((float)timestamp/(float)jobId);

  retVal->SetMRPY(1,iseed);
  retVal->SetMRPY(2,0); 

  cout<<"<seedValue>"<<iseed<<"</seedValue>"<<endl;

  return retVal;

}

  
