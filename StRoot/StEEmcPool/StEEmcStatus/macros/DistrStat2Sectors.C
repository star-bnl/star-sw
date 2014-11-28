DistrStat2Sectors(char *filename){
  FILE *fout1=fopen("stat-01","w");assert(fout1);
  FILE *fout2=fopen("stat-02","w");assert(fout2);
  FILE *fout3=fopen("stat-03","w");assert(fout3);
  FILE *fout4=fopen("stat-04","w");assert(fout4);
  FILE *fout5=fopen("stat-05","w");assert(fout5);
  FILE *fout6=fopen("stat-06","w");assert(fout6);
  FILE *fout7=fopen("stat-07","w");assert(fout7);
  FILE *fout8=fopen("stat-08","w");assert(fout8);
  FILE *fout9=fopen("stat-09","w");assert(fout9);
  FILE *fout10=fopen("stat-10","w");assert(fout10);
  FILE *fout11=fopen("stat-11","w");assert(fout11);
  FILE *fout12=fopen("stat-12","w");assert(fout12);
  FILE *fd2=fopen(filename,"r");assert(fd2);
  fprintf(fout1,"#sector01/eemcPMTstat\n");
  fprintf(fout2,"#sector02/eemcPMTstat\n");
  fprintf(fout3,"#sector03/eemcPMTstat\n");
  fprintf(fout4,"#sector04/eemcPMTstat\n");
  fprintf(fout5,"#sector05/eemcPMTstat\n");
  fprintf(fout6,"#sector06/eemcPMTstat\n");
  fprintf(fout7,"#sector07/eemcPMTstat\n");
  fprintf(fout8,"#sector08/eemcPMTstat\n");
  fprintf(fout9,"#sector09/eemcPMTstat\n");
  fprintf(fout10,"#sector10/eemcPMTstat\n");
  fprintf(fout11,"#sector11/eemcPMTstat\n");
  fprintf(fout12,"#sector12/eemcPMTstat\n");
  while(1){
    char txtx[10];
    //n1 is the fail word, n2 is the status word
    //they are read in as n1 and n2 from the .errs file
    //but they should be output as n2 n1
    char n1[10];char n2[10];
    int ret0=fscanf(fd2,"%s %s %s",txtx,n1,n2);
    if(ret0!=3) break;
    if(strncmp(txtx,"a01",3)==0){
      fprintf(fout1,"%s %s %s\n",txtx+1,n2,n1);
    }
    if(strncmp(txtx,"a02",3)==0){
      fprintf(fout2,"%s %s %s\n",txtx+1,n2,n1); 
    }
    if(strncmp(txtx,"a03",3)==0){
      fprintf(fout3,"%s %s %s\n",txtx+1,n2,n1); 
    }
    if(strncmp(txtx,"a04",3)==0){
      fprintf(fout4,"%s %s %s\n",txtx+1,n2,n1);   
    }
    if(strncmp(txtx,"a05",3)==0){
      fprintf(fout5,"%s %s %s\n",txtx+1,n2,n1);  
    }
    if(strncmp(txtx,"a06",3)==0){
      fprintf(fout6,"%s %s %s\n",txtx+1,n2,n1);  
    }
     if(strncmp(txtx,"a07",3)==0){
       fprintf(fout7,"%s %s %s\n",txtx+1,n2,n1);  
    }
     if(strncmp(txtx,"a08",3)==0){
       fprintf(fout8,"%s %s %s\n",txtx+1,n2,n1);  
    }
     if(strncmp(txtx,"a09",3)==0){
       fprintf(fout9,"%s %s %s\n",txtx+1,n2,n1);  
    }
     if(strncmp(txtx,"a10",3)==0){
       fprintf(fout10,"%s %s %s\n",txtx+1,n2,n1);       
    }
     if(strncmp(txtx,"a11",3)==0){
       fprintf(fout11,"%s %s %s\n",txtx+1,n2,n1);     
    }
     if(strncmp(txtx,"a12",3)==0){
       fprintf(fout12,"%s %s %s\n",txtx+1,n2,n1);      
    }
  }
  
}
