// retrieves  P & Q for a given fill

//==========================================
//==========================================
void getPol(int xFill, float &P_blue, float &Q_yell, float &eP, float &eQ){
  char *inpF="OsamuPol/polVsFill.dat";
  
  FILE *fd=fopen(inpF,"r");  assert(fd);
  const int len=1000;
  char buf[len], cdum[100];
  float dum;
  int nY,nB;

  int k=1,i=0;
  while(k) {
    k=fgets(buf,len,fd);    i++;
    if(buf[0]=='#') continue;
    //printf("i=%d =%s=\n\n",i,buf);
    int j=sscanf(buf,"%s %f  %f %f %d  %f %f %d",
	     cdum, &dum, &P_blue ,&eP, &nB, &Q_yell ,&eQ, &nY);
    assert(j==8);
    int fill=atoi(cdum+1);
    if(fill!=xFill) continue;

    if(nB<1) {P_blue=0.0001,eP=2.;}
    if(nY<1) {Q_yell=0.0001,eQ=2.;}
    fclose(fd);
    goto end;
  }
  P_blue=0.0001,eP=3.;
  Q_yell=0.0001,eQ=3.;
  return;
 
 end:  
  printf("%s  blue=%f /- %f  yell=%f +/- %f\n",cdum,P_blue, eP, Q_yell,eQ);

}

  //#FILL   dayIN2002    BLUE(pol,err,np)      YELL(pol,err,np)
  // F2036  -9.970      0.125  0.027  1       -0.015  0.026  1
