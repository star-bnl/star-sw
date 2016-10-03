#include <iostream>
#include <fstream>
#include <complex>
using namespace std;

extern "C"
{
	void upup_(int *L1,int *L2, int *L3,int *L4,double *s,double *t,double *u,int *iz,double *har,double *hai);
	void downdown_(int *L1,int *L2, int *L3,int *L4,double *s,double *t,double *u,int *iz,double *har,double *hai);
	void flagset_(int *iqedx,int *iewx,int *ibornx,int *gfschemex,int *ifggx,double *ncx,double *fcx,double *tlmu2x);
	void paraget_(double  *mtax, double  *conhcx, double *pix);
	void printconsts_(int *pmg);
}

class SANCtable
{
//-----------------------------------------------------------------------------
//-STATIC PART-----------------------------------------------------------------
//-----------------------------------------------------------------------------
public:
	static void setDimensions(int n1, int n2, int n3, int nc);
	static void setRanges(double sn1, double sx1, double sn2, double sx2, double sn3, double sx3);
	static void setFlags();
private:
	static int ns1,ns2,ns3,ncos;
	static double smin1,smax1,smin2,smax2,smin3,smax3;
	static int iqed,iew,iborn,gfscheme,ifgg;
	static double nc,fc,tlmu2;
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
public:
	SANCtable():flav(1),born(false),isOpen(false) {}
	SANCtable(char *filename):flav(1),born(false) { open(filename); }
	void setFixedLength(int prc=8);
	void setBornLevel(bool bn) { born=bn; }
	void setFlavor(int flv)    { flav=flv; }
	bool addHeader();
	bool addFile(char *name);
	void addRange(int rangeNo,bool isLog=false);
	void open(char *name);
	void close();
	~SANCtable() { if(isOpen) close(); }
protected:
	bool isOpen,born;
	int flav;
private:
	double Rcalc(int flav, double sloop,double costhetloop);
	double R[4][4];
	ofstream f;
};
