/************************************************************************
 * $Id: support.C,v 1.2 2004/06/26 16:28:42 porter Exp $
 *
 * Author: Jeff Porter 
 *
 *   Many of the macros have common features which when they change
 *   they typically change for all. So I put them here in one place
 *   and call from the main macro
 *
 *************************************************************************/

//----------------------------------------------------------------
char** getDirNames(const char* type, int nset){

  char** retVal=new char*[nset];

  for(int i=0;i<nset;i++){
    TString ndir(type);
    ndir+="/0";
    ndir+=(i+1);
    retVal[i]=new char[strlen(ndir.Data())+1];
    strcpy(retVal[i],ndir.Data());
  };

  return retVal;
}


//------------------- helper macro for numTracks cut -----
char** getNumTracksStrings(int min, int max){

      TString nchMin; 
      nchMin+=min;
      TString nchMax; 
      nchMax+=max;
      char** retVal=new char*[2];
      retVal[0]=new char[strlen(nchMin.Data())+1];
      strcpy(retVal[0],nchMin.Data());
      retVal[1]=new char[strlen(nchMax.Data())+1];
      strcpy(retVal[1],nchMax.Data());

      return retVal;
}

//-------------------------------------------------------------------

void doTheWork(StEStructAnalysisMaker* estructMaker, int maxNumEvents){

    int istat=0,i=1;
    estructMaker->Init();
    estructMaker->startTimer();

    int counter=0;
  while(istat!=2){

      istat=estructMaker->Make();
      i++; counter++;
      if(counter==200){ 
        cout<<"doing event ="<<i<<endl;
        counter=0;
      }
      if( maxNumEvents!=0 && i>=maxNumEvents )istat=2;
   }
  estructMaker->stopTimer();


}


/**********************************************************************
 *
 * $Log: support.C,v $
 * Revision 1.2  2004/06/26 16:28:42  porter
 * fixed typo in getDirNames.C
 *
 * Revision 1.1  2004/06/25 03:14:56  porter
 * modified basic macro to take only 1 cutfile and moved some common
 * features into a new macro=support.C.....   this cleaned up the
 * doEStruct macro somewhat
 *
 *
 **********************************************************************/
