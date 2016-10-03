#include <math.h>
#include <stdio.h>
#include <time.h>

extern void upup_(int *L1,int *L2, int *L3,int *L4,double *s,double *t,double *u,int *iz,double *har,double *hai);
extern void downdown_(int *L1,int *L2, int *L3,int *L4,double *s,double *t,double *u,int *iz,double *har,double *hai);
extern void flagset_(int *iqedx,int *iewx,int *ibornx,int *gfschemex,int *ifggx,double *ncx,double *fcx,double *tlmu2x);
extern void paraget_(double  *mtax, double  *conhcx, double *pix);
extern void printconsts_(int *pmg);


int iqed=0,iew=1,iborn=0,gfscheme=0,ifgg=1;
double nc=1.0,fc=3.0,tlmu2=1e-5;
int buf=0;
double R[4][4];
double divv;
double borndivv=123.456;

struct compl {double im,re;};

struct compl mul(struct compl x, struct compl y)
{
  struct compl c ;
  c.re=x.re*y.re-x.im*y.im;
  c.im=x.re*y.im+x.im*y.re;
  return c;
}

struct compl conju(struct compl x)
{
  struct compl c ;
  c.re=x.re;
  c.im=-x.im;
  return c;
}

double abso(struct compl x)
{
  return sqrt(x.re*x.re+x.im*x.im);
}


void Rcalc(int flav, double sloop,double costhetloop)
{
	int L1,L2,L3,L4,iz,i,j,M3,M4;
	double s,t,u;
	double cosf,betaf,har,hai,MM,sinf,xnorm,xx;

	double mta,conhc,pi;

	double sum,sig[4][4][2], Bench[4][4]={0};
	double ha[2];
      	struct compl amp[2][2][2][2][2]={0}, sigma[4][2][2]={0}, c;
        sigma[0][0][0].re=1;
        sigma[0][1][1].re=1;

        sigma[1][0][1].re=1;
        sigma[1][1][0].re=1;

        sigma[2][0][1].im=-1;
        sigma[2][1][0].im= 1;

        sigma[3][0][0].re=1;
        sigma[3][1][1].re=-1;

	s=sloop;
	cosf=costhetloop;
// --------------------------
	paraget_(&mta,&conhc,&pi);
	betaf = sqrt(1e0-4e0*mta*mta/s);
	t = mta*mta - s/2*(1e0-betaf*cosf);
	u = mta*mta - s/2*(1e0+betaf*cosf);
	for(iz = 0;iz<2;iz++)
	{
		for(L1=1;L1<=2;L1++)
		{
			for(L2=1;L2<=2;L2++)
			{
				for(L3=1;L3<=2;L3++)
				{
					for(L4=1;L4<=2;L4++)
					{
					  if(flav==1)	downdown_(&L1,&L2,&L3,&L4,&s,&t,&u,&iz,&har,&hai);
					  else  	upup_(&L1,&L2,&L3,&L4,&s,&t,&u,&iz,&har,&hai);
                                                amp[L1-1][L2-1][L3-1][L4-1][iz].im=hai;
                                                amp[L1-1][L2-1][L3-1][L4-1][iz].re=har;
					}
				}
			}
		}
	}
	for(i=0;i<4;i++){
	for(j=0;j<4;j++){
	for(iz = 0;iz<2;iz++)
	{
	        sum = 0e0;
		for(L1=1;L1<=2;L1++)
		{
			for(L2=1;L2<=2;L2++)
			{
				for(L3=1;L3<=2;L3++)
				{
					for(L4=1;L4<=2;L4++)
	                                for(M3=1;M3<=2;M3++)
				        for(M4=1;M4<=2;M4++)
					{       
					  c=mul(mul(mul(amp[L1-1][L2-1][L3-1][L4-1][iz],conju(amp[L1-1][L2-1][M3-1][M4-1][iz])),
						    sigma[i][L3-1][M3-1]),(sigma[j][M4-1][L4-1])); 
					  sum=sum+ c.re;
					}
				}
			}
		}
		sig[i][j][iz]=sum;
	}
	R[i][j] = conhc *                            // to pbarn
                  nc/fc*1e0/2.0/s *
	          1e0/4 *                            // spin sum
                  (sig[i][j][1] - sig[i][j][0]) *    // |Amp|^2 - linearized
	          betaf/16/pi;                       // phase_space/dcos{theta}
        if(i==0 && j==0)
	    divv=R[0][0];

	R[i][j]=R[i][j]/divv;  
	        if(i!=0) R[i][j] =-  R[i][j]; // necessary for tau- but further investigation needed

	}
	}

	/*	// there should be rotation like this one, however we do not need that. 
                // Wrong definition of Pauli matrix for antiparticle, missing conjugation?
	for(j=0;j<4;j++)
	  {
	    xx=       R[1][j];
           R[3][j] =-  R[3][j];
           R[1][j] =  -xx;
	  }
	
	*/
	
						
	for(i=0;i<4;i++)      // rotations  around z axis  must be checked
	{
	  xx=       R[i][1];
          R[i][1] =-  R[i][2];
          R[i][2] =  xx;
	}
	
	
	for(j=0;j<4;j++)    // rotations  around z axis  must be checked
        {
	  xx=       R[1][j];
          R[1][j] =  R[2][j];
          R[2][j] =-  xx;
	}
		
				
	/*	 	
	  printf("\n");
          printf("d_sigma/d_cos{theta} = %.9f \n",divv);
	  printf("\n");
	  printf("R(i,j)= \n");
	  printf("\n");
          for(i=0;i<4;i++)
	  {
	    for(j=0;j<4;j++)
	      printf("%.5f ",R[i][j]);
	    printf("\n");
	  }
	 
	  sinf=sqrt(1-cosf*cosf);
          MM=sqrt(4e0*mta*mta/s);
          xnorm=1+cosf*cosf + MM*MM*sinf*sinf;
          Bench[0][0]=(1+cosf*cosf + MM*MM*sinf*sinf)/xnorm;
          Bench[1][1]=(-(1- MM*MM)*sinf*sinf)/xnorm;
          Bench[2][2]=( (1+ MM*MM)*sinf*sinf)/xnorm;
          Bench[3][3]=(1+cosf*cosf - MM*MM*sinf*sinf)/xnorm;
          Bench[2][3]=(2*MM*sinf*cosf)/xnorm;
          Bench[3][2]=(2*MM*sinf*cosf)/xnorm;
	  printf("\n");
	  printf("Bench(i,j)= \n");
	  printf("\n");

	  for(i=0;i<4;i++)
	  {
	    for(j=0;j<4;j++)
	      printf("%.5f ",Bench[i][j]);
	    printf("\n");
	  }

	  printf("\n");
	  printf("   \n");
	  printf("Bench(i,j)=R(i,j) at low energies, where ew corr and Z contribute little \n");
	  printf("\n");
	*/	  

}

int main(int argc, char **argv){
  double sloop=100;
  double costhetloop=0.0;
  int i,j,k,l;
  int NS1=100, NS2=100, NS3=100, NCOS=21;
  double smin1=log(6*6);
  double smax1=log(17000*17000);

  double smin2=85*85;
  double smax2=110*110;

  double smin3=160*160;
  double smax3=220*220;
  int    flav;
  double step;
  FILE *f=NULL,*d=NULL,*dd=NULL;
  unsigned char c;
  char   ftime[255],path[255];
  time_t utime;
  if (argc > 1) flav=atoi(argv[1]);
  else flav=1;
  if (flav==1) f=fopen("table1-1.txt","w");
  else         f=fopen("table2-2.txt","w");

  flagset_(&iqed,&iew,&iborn,&gfscheme,&ifgg,&nc,&fc,&tlmu2);
  printconsts_(&buf);
  dd=fopen("SancLib_v1_02/lib.txt","r");
  if(!dd) { printf("No info file  on electrowek library SANC (file SancLib_v1_02/lib.txt missing), we better stop \n"); return -1; }
  d=fopen("var.dump","r");
  if(!d) { printf("No initialization variables of SANC (file var.dump missing), we better stop \n"); return -1; }

  fprintf(f,"Dimensions: %i %i %i %i\n",NS1,NS2,NS3,NCOS);
  fprintf(f,"Ranges: %.5f %.5f %.5f %.5f %.5f %.5f \n",smin1,smax1,smin2,smax2,smin3,smax3);
  getcwd(path,255);
  time(&utime);
  strftime(ftime,255,"%d %b %Y, %H:%M:%S, GMT%z",localtime(&utime));
  fprintf(f,"Timestamp: %s\n",ftime);
  fprintf(f,"Path: %s\n\n",path);

  while(1)
  {
    c=0;
    fscanf(dd,"%c",&c);
    if(c==0) break;
    fprintf(f,"%c",c);
    }

  while(1)
  {
    c=0;
    fscanf(d,"%c",&c);
    if(c==0) break;
    fprintf(f,"%c",c);
    }
  fprintf(f,"\nBeginRange1\n");  
  printf("\nBeginRange1\n");  

  step=(smax1-smin1)/(NS1-1);
  for(i=0;i<NS1;i++)
  {
    sloop=exp(smin1+i*step);
    printf("%.5f \n",sloop);
    for(j=0;j<NCOS;j++)
    {
      costhetloop=-1.+1.0/NCOS+j*2./(NCOS);

      iew=0;
      flagset_(&iqed,&iew,&iborn,&gfscheme,&ifgg,&nc,&fc,&tlmu2);
      Rcalc(flav,sloop,costhetloop);
      borndivv=divv;

      iew=1;
      flagset_(&iqed,&iew,&iborn,&gfscheme,&ifgg,&nc,&fc,&tlmu2);
      Rcalc(flav,sloop,costhetloop);

      for(k=0;k<4;k++)
	for(l=0;l<4;l++)
	  fprintf(f,"%.5f ",R[k][l]);
      fprintf(f,"%.8f ",divv);
      fprintf(f,"%.8f\n",borndivv);
      // fprintf(f,"%.5f \n",sloop);
    }
  }


  fprintf(f,"\nBeginRange2\n");  
  printf("\nBeginRange2\n");  

  step=(smax2-smin2)/(NS2-1);
  for(i=0;i<NS2;i++)
  {
    sloop=(smin2+i*step);
    printf("%.5f \n",sloop);
    for(j=0;j<NCOS;j++)
    {
      costhetloop=-1.+1.0/NCOS+j*2./(NCOS);
 
      iew=0;
      flagset_(&iqed,&iew,&iborn,&gfscheme,&ifgg,&nc,&fc,&tlmu2);
      Rcalc(flav,sloop,costhetloop);
      borndivv=divv;

      iew=1;
      flagset_(&iqed,&iew,&iborn,&gfscheme,&ifgg,&nc,&fc,&tlmu2);
      Rcalc(flav,sloop,costhetloop);

      for(k=0;k<4;k++)
	for(l=0;l<4;l++)
	  fprintf(f,"%.5f ",R[k][l]);
      fprintf(f,"%.5f ",divv);
      fprintf(f,"%.5f \n",borndivv);
    }
  }


  fprintf(f,"\nBeginRange3\n");  
  printf("\nBeginRange3\n");  

  step=(smax3-smin3)/(NS3-1);
  for(i=0;i<NS3;i++)
  {
    sloop=(smin3+i*step);
    printf("%.5f \n",sloop);
    for(j=0;j<NCOS;j++)
    {
      costhetloop=-1.+1.0/NCOS+j*2./(NCOS);

      iew=0;
      flagset_(&iqed,&iew,&iborn,&gfscheme,&ifgg,&nc,&fc,&tlmu2);
      Rcalc(flav,sloop,costhetloop);
      borndivv=divv;

      iew=1;
      flagset_(&iqed,&iew,&iborn,&gfscheme,&ifgg,&nc,&fc,&tlmu2);
      Rcalc(flav,sloop,costhetloop);

      for(k=0;k<4;k++)
	for(l=0;l<4;l++)
	  fprintf(f,"%.5f ",R[k][l]);
      fprintf(f,"%.5f ",divv);
      fprintf(f,"%.5f \n",borndivv);
    }
  }
  fprintf(f,"End\n");
  fclose(f);
  return 0;
}
