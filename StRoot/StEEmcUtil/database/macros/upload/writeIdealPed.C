void writeIdealPed(){

  const int etabin=12;
  const int strip=288;
  char title[100], tow[100], mapt[100];

  system("mkdir -p idealPedStatus/");
  
  for (int k=0;k<etabin;k++){
    sprintf(title,"ped.sector%02d",k+1);
    FILE *file=fopen(title,"w"); assert(file);
    fprintf(file,"#sector%02d/eemcPMTped\n",k+1);
    //tower files
    for (char zub='A';zub<='E';zub++){
      for (int j=0;j<etabin;j++){
	sprintf(tow,"%02dT%c%02d",k+1,zub,j+1);
	fprintf(file,"%s 0.0 1.0\n",tow);	
      }
    }
    // SMD
    for (char zub='U';zub<='V';zub++){
      for (int j=0;j<strip;j++){
	sprintf(mapt,"%02d%c%03d",k+1,zub,j+1);
	fprintf(file,"%s 0.0 0.7\n",mapt);
      }
    }
    // pre/post
    for (char zub='P';zub<='R';zub++){
      for (char sub='A';sub<='E';sub++){
	for (int j=0;j<etabin;j++){
	  sprintf(mapt,"%02d%c%c%02d",k+1,zub,sub,j+1);
	  fprintf(file,"%s 0.0 0.7\n",mapt);
	}
      }
    }
          
      fclose(file);
  }// end of sector
  
  system("mv ped* idealPedStatus/");

}
