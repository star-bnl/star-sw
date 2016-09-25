#include "SANCtable.h"

int    SANCtable::ns1=0,SANCtable::ns2=0,SANCtable::ns3=0,SANCtable::ncos=0;
double SANCtable::smin1=0,SANCtable::smax1=0;
double SANCtable::smin2=0,SANCtable::smax2=0;
double SANCtable::smin3=0,SANCtable::smax3=0;
int    SANCtable::iqed=0,SANCtable::iew=0,SANCtable::iborn=0;
int    SANCtable::gfscheme=0,SANCtable::ifgg=0;
double SANCtable::nc=0,SANCtable::fc=0,SANCtable::tlmu2=0;

//-----------------------------------------------------------------------------
//-Static part-----------------------------------------------------------------
//-----------------------------------------------------------------------------
void SANCtable::setDimensions(int n1, int n2, int n3, int nc)
{
	ns1=n1;
	ns2=n2;
	ns3=n3;
	ncos=nc;
}
void SANCtable::setRanges(double sn1, double sx1, double sn2, double sx2, double sn3, double sx3)
{
	smin1=sn1*sn1;
	smax1=sx1*sx1;
	smin2=sn2*sn2;
	smax2=sx2*sx2;
	smin3=sn3*sn3;
	smax3=sx3*sx3;
}
void SANCtable::setFlags()
{
	iqed=0;iew=1;iborn=0;gfscheme=0;ifgg=1;
	nc=1.0;fc=3.0;tlmu2=1e-5;
	flagset_(&iqed,&iew,&iborn,&gfscheme,&ifgg,&nc,&fc,&tlmu2);
	int buf=0;
	printconsts_(&buf);
}
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
void SANCtable::setFixedLength(int prc)
{
	if(prc==0)
	{
		f.precision(6);
		f.unsetf(ios::fixed);
	}
	else
	{
		f.precision(prc);
		f.setf(ios::fixed);
	}
}

bool SANCtable::addHeader()
{
	char path[255],ftime[255];
	time_t utime;
	if(ns1==0 || smin1==0) return false;
	f<<"Dimensions: "<<ns1<<" "<<ns2<<" "<<ns3<<" "<<ncos<<endl;
	f<<"Ranges: "<<log(smin1)<<" "<<log(smax1)<<" "<<smin2<<" "<<smax2<<" "<<smin3<<" "<<smax3<<endl;
	getcwd(path,255);
	time(&utime);
	strftime(ftime,255,"%d %b %Y, %H:%M:%S, GMT%z",localtime(&utime));
	f<<"Timestamp: "<<ftime<<endl;
	f<<"Path: "<<path<<endl<<endl;
	return true;
}

bool SANCtable::addFile(char *name)
{
	ifstream d(name);
	unsigned char c;
	if(!d.is_open()) return false;
	while(!d.eof())
	{
		d>>noskipws>>c;
		f.put(c);
	}
	return true;
}

void SANCtable::addRange(int rangeNo,bool isLog)
{
	double min=0,max=0,steps=0;
	ofstream::fmtflags last = cout.setf(ios::fixed);
	f<<"\nBeginRange"<<rangeNo<<endl;
	cout<<"\nBeginRange"<<rangeNo<<endl;
	switch(rangeNo)
	{
		case 1: min=smin1; max=smax1; steps=ns1; break;
		case 2: min=smin2; max=smax2; steps=ns2; break;
		case 3: min=smin3; max=smax3; steps=ns3; break;
	}
	double step= (isLog) ? (log(max)-log(min))/(steps-1) : (max-min)/(steps-1);
	double sloop=0;
	for(int i=0;i<steps;i++)
	{
		sloop= (isLog) ? exp(log(min)+i*step) : min+i*step;
		cout<<sloop<<endl;
		for(int j=0;j<ncos;j++)
		{
			double costhetloop=-1.+1.0/ncos+j*2./ncos;
			iew=0;
			flagset_(&iqed,&iew,&iborn,&gfscheme,&ifgg,&nc,&fc,&tlmu2);
			double borndivv = Rcalc(flav,sloop,costhetloop);
			
			iew = (born) ? 0 : 1;
			flagset_(&iqed,&iew,&iborn,&gfscheme,&ifgg,&nc,&fc,&tlmu2);
			double divv = Rcalc(flav,sloop,costhetloop);
			
			for(int k=0;k<4;k++)
				for(int l=0;l<4;l++)
					f<<R[k][l]<<" ";
			f<<divv<<" "<<borndivv<<endl;
		}
	}
	cout.flags(last);
}

void SANCtable::open(char *name)
{
	f.open(name);
	isOpen = f.is_open();
}

void SANCtable::close()
{
	f<<"End"<<endl;
	f.close();
	isOpen=false;
}
//-----------------------------------------------------------------------------
//-The main computation module-------------------------------------------------
//-----------------------------------------------------------------------------
double SANCtable::Rcalc(int flav, double s,double cosf)
{
	double sig[4][4][2],sum=0.,divv=0.;
	complex<double> amp[2][2][2][2][2]={0}, sigma[4][2][2]={0}, c;

	sigma[0][0][0] = complex<double>(1,0);
	sigma[0][1][1] = complex<double>(1,0);

	sigma[1][0][1] = complex<double>(1,0);
	sigma[1][1][0] = complex<double>(1,0);

	sigma[2][0][1] = complex<double>(0,-1);
	sigma[2][1][0] = complex<double>(0,1);

	sigma[3][0][0] = complex<double>(1,0);
	sigma[3][1][1] = complex<double>(-1,0);

	double mta,conhc,pi;
	paraget_(&mta,&conhc,&pi);

	double betaf = sqrt(1e0-4e0*mta*mta/s);
	double t = mta*mta - s/2*(1e0-betaf*cosf);
	double u = mta*mta - s/2*(1e0+betaf*cosf);
	for(int iz = 0;iz<2;iz++)
	{
		for(int L1=1;L1<=2;L1++)
		{
			for(int L2=1;L2<=2;L2++)
			{
				for(int L3=1;L3<=2;L3++)
				{
					for(int L4=1;L4<=2;L4++)
					{
						double har=0,hai=0;
						if(flav==1) downdown_(&L1,&L2,&L3,&L4,&s,&t,&u,&iz,&har,&hai);
						else        upup_(&L1,&L2,&L3,&L4,&s,&t,&u,&iz,&har,&hai);
						amp[L1-1][L2-1][L3-1][L4-1][iz] = complex<double>(har,hai);
					}
				}
			}
		}
	}

	for(int i=0;i<4;i++){
	for(int j=0;j<4;j++){
	for(int iz = 0;iz<2;iz++)
	{
	        sum = 0.0;
		for(int L1=1;L1<=2;L1++)
		{
			for(int L2=1;L2<=2;L2++)
			{
				for(int L3=1;L3<=2;L3++)
				{
					for(int L4=1;L4<=2;L4++)
	                                for(int M3=1;M3<=2;M3++)
				        for(int M4=1;M4<=2;M4++)
					{
						c =      amp[L1-1][L2-1][L3-1][L4-1][iz]  *
						    conj(amp[L1-1][L2-1][M3-1][M4-1][iz]) *
						    sigma[i][M3-1][L3-1] *
						    sigma[j][M4-1][L4-1];
						sum+=real(c);
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

        if(i==0 && j==0) divv=R[0][0];

	R[i][j]=R[i][j]/divv;

	}
	}
	
	
	for(int i=0;i<4;i++)    // rotations  for tau+  see tau+  KW and SANC frames
	{
		double xx=  R[i][1];
		R[i][1]  =  R[i][2];
		R[i][2]  = -xx;
	}

	for(int j=0;j<4;j++)    // rotations  for tau-  see tau-  KW and SANC frames
        {
		double xx=  R[1][j];
		R[1][j]  = -R[2][j];
		R[2][j]  = -xx;
		R[3][j]  = -R[3][j];
	}
	for(int i=0;i<4;i++)    // rotations  for tau+/tau- see reaction  KW and SANC frames
	{
		R[i][1]  = -R[i][1];
		R[i][2]  = -R[i][2];
	}

	for(int j=0;j<4;j++)    // rotations  for tau+/tau- see reaction KW and SANC frames
        {
		R[1][j]  = -R[1][j];
		R[2][j]  = -R[2][j];
	}
	/*		 	
	  printf("\n");
          printf("d_sigma/d_cos{theta} = %.9f \n",divv);
          printf("s    = %.9f \n" ,s);
          printf("cosf = %.9f \n",cosf);
	  printf("\n");
	  printf("R(i,j)= \n");
	  printf("\n");
          for(int i=0;i<4;i++)
	  {
	    for(int j=0;j<4;j++)
	      printf("%.5f ",R[i][j]);
	    printf("\n");
	  }
	  double sinf;
	  double MM;
	  double xnorm;
          double Bench[4][4]={0.};      	 
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

	  for(int i=0;i<4;i++)
	  {
	    for(int j=0;j<4;j++)
	      printf("%.5f ",Bench[i][j]);
	    printf("\n");
	  }

	  printf("\n");
	  printf("   \n");
	  printf("Bench(i,j)=R(i,j) at low energies, where ew corr and Z contribute little \n");
	  printf("\n");
		  
	*/

	return divv;
}
