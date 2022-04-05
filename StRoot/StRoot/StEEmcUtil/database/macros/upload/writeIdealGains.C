void writeIdealGains(){

  const int etabin=12;
  const float maxADC=4095;
  const float maxEt=60;
  const int strip=288;
  const float eta[etabin]={1.95,1.855,1.765,1.675,1.59,1.51,1.435,1.365,1.3,1.235,1.17,1.115};
  int tower[etabin];
  float gain[etabin];
  float err[etabin];
  char mapt[100];
  char title[100];
  char tow[100];

  system("mkdir -p idealGain/");

  for (UInt_t i=0;i<etabin;i++){
    int hold=i-etabin;
    tower[i]=i+1;
    gain[i]=maxADC/maxEt/cosh(eta[i]);
    err[i]=0.0;
   printf("eta bin %d is tower %d with eta = %f, gain =%f\n",i,tower[i],eta[i],gain[i]);
  }
  
  //tower files
  for (int k=0;k<etabin;k++){
    sprintf(title,"sector%02d.Tgain",k+1);
    FILE *file=fopen(title,"w"); assert(file);
    fprintf(file,"#sector%02d/eemcPMTcal\n",k+1);
    for (char zub='A';zub<='E';zub++){
      for (int j=0;j<etabin;j++){
	sprintf(tow,"%02dT%c%02d",k+1,zub,j+1);
	fprintf(file,"%s %f %f\n",tow,gain[j],err[j]);
	
      }
     }
    fclose(file);
  }


  //smd,pre1,pre2,post (U/V,P,Q,R)
  for (int k=0;k<etabin;k++){
    sprintf(title,"sect%02dPIXgains.dat",k+1);
    FILE *file2=fopen(title,"w"); assert(file2);
    fprintf(file2,"#sector%02d/eemcPIXcal\n",k+1);
    for (char zub='U';zub<='V';zub++){
      for (int j=0;j<strip;j++){
	sprintf(mapt,"%02d%c%03d",k+1,zub,j+1);
	fprintf(file2,"%s %2.1f %f\n",mapt,23000,0.0);
      }
    }
    for (char zub='P';zub<='R';zub++){
      for (char sub='A';sub<='E';sub++){
	for (int j=0;j<etabin;j++){
	  sprintf(mapt,"%02d%c%c%02d",k+1,zub,sub,j+1);
	  fprintf(file2,"%s %2.1f %f\n",mapt,23000.0,0.0);
	}
      }
    }
    fclose(file2);
  }

  system("mv sect* idealGain/");
  
}
