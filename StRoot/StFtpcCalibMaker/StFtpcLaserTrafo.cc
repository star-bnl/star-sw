#include "StFtpcLaserTrafo.hh"
#include "TSystem.h"
#include "StMessMgr.h"
#include "TMath.h"
#include "PhysicalConstants.h"
 
const float normgas = 50.0;
const Float_t Grad2Rad=1/(180/TMath::Pi());
//---------------------------------------------------------------

StFtpcLaserTrafo::StFtpcLaserTrafo()
{
  // default constructor !!!
}
//---------------------------------------------------------------

StFtpcLaserTrafo::StFtpcLaserTrafo(StFtpcDbReader *getdb,StFtpcParamReader *getparam,float getdeltat0,float getdeltagas, float getmicropertime, float getdeltap, float getmbfield, float getTZero)
{
  //cout<<"StFtpcLaserTrafo..."<<endl; 
  mDb=getdb; mParam=getparam;
  
  mBField = getmbfield;

  deltat0=getdeltat0;
  deltagas=getdeltagas;
  tZero = getTZero;

  cout<<"d_t0 = "<<deltat0<<" | d_gas(Ar) = "<<(float) deltagas<<endl;
  micropertime = getmicropertime;
  cout<<"Microseconds per timebin = "<<micropertime<<endl;
  cout<<"==============================="<<endl;
  cout<<endl;

  deltap = getdeltap;

  pradius = new Double_t[mParam->numberOfDriftSteps()
			 *mDb->numberOfPadrowsPerSide()];
  pdeflection = new Double_t[mParam->numberOfDriftSteps()
			     *mDb->numberOfPadrowsPerSide()];
}

//---------------------------------------------------------------

StFtpcLaserTrafo::~StFtpcLaserTrafo()
{
  delete[] pradius;        // release the pradius array
  delete[] pdeflection;   // release the pdeflection array
  //cout<<"StFtpcLaserTrafo deconstructed"<<endl; 
}

//---------------------------------------------------------------

//double StFtpcLaserTrafo::vd_gas(float rad, float gasmix)
//{
  //return vd_gas_slope(rad)*(gasmix-normgas)+vd_gas_y(rad);
  //return vd_gas_slope(rad)*(gasmix-normgas);
//}

//---------------------------------------------------------------

double StFtpcLaserTrafo::vd_gas(float rad)
{
  //return vd_gas_slope(rad)*(gasmix-normgas)+vd_gas_y(rad);
  return vd_gas_slope(rad)*deltagas;
}

//---------------------------------------------------------------

double StFtpcLaserTrafo::vd_gas_slope(float rad)
{
  double p0, p1, p2;
  if ( mBField == 0) {
     p0=0.8327;
     p1=1.613;
     p2=0.00378;
  }
  if ( abs(mBField) == 1) {
     p0=0.7775;
     p1=1.598;
     p2=0.003616;
   }

  return (p0*1/TMath::Power(rad,p1)+p2);
}

//---------------------------------------------------------------
// B=1
double StFtpcLaserTrafo::vd_gas_y(float rad)
{
  double p0=13.05;
  double p1=1.08;

  return p0*1/TMath::Power(rad,p1);
}

//---------------------------------------------------------------

//double StFtpcLaserTrafo::lor_gas(float rad, float gasmix)
//{
  //return lor_gas_slope(rad)*(gasmix-normgas)+lor_gas_y(rad);
  //return lor_gas_slope(rad)*(gasmix-normgas);
//}

//---------------------------------------------------------------

double StFtpcLaserTrafo::lor_gas(float rad)
{
  //return lor_gas_slope(rad)*(gasmix-normgas)+lor_gas_y(rad);
  return lor_gas_slope(rad)*deltagas;
}

//---------------------------------------------------------------

double StFtpcLaserTrafo::lor_gas_slope(float rad)
{
  double p0, p1, p2;
  if ( mBField == 0 ) {
     p0=0.0001995;
     p1=0.1211;
     p2=-0.0001324;
  }
  if ( abs(mBField) == 1) {
     p0=8.916;
     p1=2.723;
     p2=0.0775;
  }

  return (p0*1/TMath::Power(rad,p1)+p2);
}


//---------------------------------------------------------------
// B=1
double StFtpcLaserTrafo::lor_gas_y(float rad)
{

  // nicht besonders gut der fit !!!)

  double p0=8.005;
  double p1=-0.9733;
  double p2=0.09855;
  double p3=-0.005031;
  double p4=0.0001277;
  double p5=-1.281/1000000;

  return p0+p1*rad+p2*TMath::Power(rad,2)+p3*TMath::Power(rad,3)+p4*TMath::Power(rad,4)+p5*TMath::Power(rad,5);
}
//---------------------------------------------------------------

// include Gas variation !!!

int StFtpcLaserTrafo::calcpadtrans()
{
  int i, j, v_buf, padrow;
  double t_last, t_next, r_last, r_next, e_now, v_now, psi_now;
  double step_size;
  
  cout<<"calcpadtrans ..."<<endl;

  step_size=((float) mDb->numberOfTimebins()
	     / (float) mParam->numberOfDriftSteps());


  cout<<"Pressure & temperature correction  = "<<deltap<<endl;
  cout<<"======================================================"<<endl;


  cout<<"RadiusTimesField = "<<mDb->radiusTimesField()<<endl;
  cout<<"vdrift(0,0)   = "<<mDb->magboltzVDrift(0,0)<<endl;
  cout<<"lorangle(0,0) = "<<mDb->magboltzDeflection(0,0)<<endl;
  cout<<endl;

  for (padrow=0; padrow<mDb->numberOfPadrowsPerSide(); padrow++)
    {
      /* determine starting values */
      t_last=0;
      v_buf=0;
      r_last=mDb->sensitiveVolumeOuterRadius();
      pradius[padrow]=mDb->sensitiveVolumeOuterRadius();
      pdeflection[padrow]=0;
      e_now = mDb->radiusTimesField() / (0.5*r_last);
      for(j=v_buf; mDb->magboltzEField(j) < e_now
	    && j<(mDb->numberOfMagboltzBins()-1); j++);
      if(j<1 || j>mDb->numberOfMagboltzBins())
	{
	  gMessMgr->Message("", "E", "OST") << "Error 1: j=" << j << ", v_buf=" << v_buf << " e_drift=" << mDb->magboltzEField(j) << ", e_now=" << e_now << endm;
	  return FALSE;
	}
      v_buf=j-1;
      v_now=((mDb->magboltzVDrift(v_buf, padrow)
	      +deltap*mDb->magboltzdVDriftdP(v_buf, padrow))
	     *(mDb->magboltzEField(j)-e_now)
	     +(mDb->magboltzVDrift(j, padrow)
	       +deltap*mDb->magboltzdVDriftdP(j, padrow))
	     *(e_now-mDb->magboltzEField(v_buf)))
	/(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
      psi_now=((mDb->magboltzDeflection(v_buf,padrow)
		+deltap*mDb->magboltzdDeflectiondP(v_buf,padrow))
	       *mParam->lorentzAngleFactor()
	       *(mDb->magboltzEField(j)-e_now)
	       +(mDb->magboltzDeflection(j,padrow)
		 +deltap*mDb->magboltzdDeflectiondP(j,padrow))
	       *mParam->lorentzAngleFactor()
	       *(e_now-mDb->magboltzEField(v_buf)))
	/(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));

      //cout<<psi_now<<endl;
      
      // gas correction (only use by fullfield !)
      if (deltagas!=0)
	{
	  //cout<<v_now<<endl;
	  v_now=v_now+vd_gas(r_last);
	  //cout<<v_now<<endl;
	  //cout<<psi_now<<endl;
	  //cout<<Grad2Rad*lor_gas(r_last)<<" "<<lor_gas(r_last)<<endl;
	  //psi_now=psi_now+lor_gas(r_last);
	  // 1. Neherung reversed FullField richtig !????
	  //psi_now=-(psi_now+lor_gas(r_last));

	  if (mBField == -1 ) 
             psi_now=psi_now-lor_gas(r_last); // ???? B=-1 !!!
          else if (mBField == 0 || mBField == 1 )
	     psi_now=psi_now+lor_gas(r_last);   // B=0,B=+1
           else {
             cout<<"psi_now not defined for mBField = "<<mBField<<endl;
           }  

	  //cout<<psi_now<<endl;
	  //v_now=v_now*cos(Grad2Rad*psi_now); 

	  //***************************
	  //* ??? so 1. Naeherung ??? *
	  //***************************
	  //cout<<v_now<<endl;
	}

      for (i=0; i<mParam->numberOfDriftSteps() 
	     && e_now < mDb->magboltzEField(mDb->numberOfMagboltzBins()-2)
	     ; i++) 
	{
	  t_next = t_last + step_size;
	  /* first guess for r_next: */
	  //r_next = r_last - v_now * step_size * mDb->microsecondsPerTimebin();
	  r_next = r_last - v_now * step_size * micropertime;
	  e_now = mDb->radiusTimesField() / (0.5*(r_last+r_next));
	  
	  for(j=v_buf; mDb->magboltzEField(j) < e_now 
		       && (j<mDb->numberOfMagboltzBins()-1); j++);
	  
	  if(j<1 || j>mDb->numberOfMagboltzBins())
	    {
	      gMessMgr->Message("", "E", "OST") << "Error 2: j=" << j << ", v_buf=" << v_buf << " e_drift=" << mDb->magboltzEField(j) << ", e_now=" << e_now << endm;
	      return FALSE;
	    }
	  
	  v_buf=j-1;
	  v_now=((mDb->magboltzVDrift(v_buf, padrow)
		  +deltap*mDb->magboltzdVDriftdP(v_buf, padrow))
		 *(mDb->magboltzEField(j)-e_now)
		 +(mDb->magboltzVDrift(j, padrow)
		   +deltap*mDb->magboltzdVDriftdP(j, padrow))
		 *(e_now-mDb->magboltzEField(v_buf)))
	  /(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
	  psi_now=((mDb->magboltzDeflection(v_buf,padrow)
		    +deltap*mDb->magboltzdDeflectiondP(v_buf,padrow))
		   *mParam->lorentzAngleFactor()
		   *(mDb->magboltzEField(j)-e_now)
		   +(mDb->magboltzDeflection(j,padrow)
		     +deltap*mDb->magboltzdDeflectiondP(j,padrow))
		   *mParam->lorentzAngleFactor()
		   *(e_now-mDb->magboltzEField(v_buf)))
	  /(mDb->magboltzEField(j)-mDb->magboltzEField(v_buf));
	 
	  // gas correction (only use by fullfield !)
	  if (deltagas!=0)
	    {

	      v_now=v_now+vd_gas(r_last); // aenderung von rlast -> rnext !!!???
	      //psi_now=psi_now+lor_gas(r_last);
	      // 1. Neherung reversed FullField richtig !????
	      //psi_now=-(psi_now+lor_gas(r_last));

	      //psi_now=psi_now-lor_gas(r_last); // ???? B=-1 !!!

	      psi_now=psi_now+lor_gas(r_last);   // B=0,B=+1
	      //v_now=v_now*cos(Grad2Rad*psi_now);
	      //cout<<(lor_gas(r_next)-lor_gar_nexts(r_last))*180/TMath::Pi()<<endl;
	      //cout<<"r_last = "<<r_last<<" | r_next = "<<r_next<<endl;
	    }
 
	  /* correct r_next: */
	  //r_next = r_last - v_now * step_size *mDb->microsecondsPerTimebin();
	  //cout<<"r_last = "<<r_last<<" | r_next = "<<r_next<<" | r_next_new = "<< r_last - v_now * step_size * micropertime<<" | psi = "<<psi_now<<endl;
	  r_next = r_last - v_now * step_size * micropertime;
	  pradius[padrow+mDb->numberOfPadrowsPerSide()*(i+1)]=r_next;
	  //cout<<padrow+mDb->numberOfPadrowsPerSide()*(i+1)<<endl;
	  pdeflection[padrow+mDb->numberOfPadrowsPerSide()*(i+1)]
	    =pdeflection[padrow+mDb->numberOfPadrowsPerSide()*i]
	    +((r_last-r_next)*tan(degree * psi_now)/r_last);

	  t_last=t_next;
	  r_last=r_next;
	}     
    }
  //int wait;
  //cin>>wait;
  //cout<<wait<<endl;
  return true;
}

//---------------------------------------------------------------

int StFtpcLaserTrafo::padtrans(int iRow,int iSec,float timepos, float padpos,float *x1,float *y1)
{  
  int PadtransPerTimebin;
  int PadtransLower;
  float PhiDeflect, TimeCoordinate;
  
  TimeCoordinate = timepos; 

  // Laser t0 = 1.0
  // Data  t0 from Calibrations_ftpc/ftpcElectronics

  TimeCoordinate += (tZero+deltat0)/micropertime;
  
  //cout<<TimeCoordinate<<" "<<padpos<<" "<<(*x1)<<" "<<(*y1)<<endl;

  PadtransPerTimebin = (int) mParam->numberOfDriftSteps() / mDb->numberOfTimebins();
  PadtransLower= (int) (TimeCoordinate*PadtransPerTimebin);

  float Rad=pradius[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower]
  -(pradius[iRow + mDb->numberOfPadrowsPerSide()*(PadtransLower)]
           -pradius[iRow + mDb->numberOfPadrowsPerSide() * (PadtransLower+1)])/2;

  
  PhiDeflect=pdeflection[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower]
   +(pdeflection[iRow + mDb->numberOfPadrowsPerSide() * (PadtransLower+1)]
    -pdeflection[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower])/2;

  // Aenderungen damit Koord. richtig (vgl. chain+Frank)

  //PhiDeflect=0.0; // ? genauer vgl auch ZerfieldMaker !???
  
  float Phi;

  Phi = mDb->radiansPerBoundary() / 2 
    + (padpos + 0.5) * mDb->radiansPerPad()
    + PhiDeflect + iSec * (mDb->numberOfPads() * mDb->radiansPerPad()
    + mDb->radiansPerBoundary())+halfpi;

  //cout<<"mDb->radiansPerBoundary() = "<<mDb->radiansPerBoundary()<<endl;
  //cout<<"mDb->radiansPerPad() = "<<mDb->radiansPerPad()<<endl;
  //cout<<"mDb->numberOfPads() = "<<mDb->numberOfPads()<<endl;

  /* 
  // macht Probleme bei fit usw. !??? auch wenn nur West !????
  // Wie !!!!!!!!!!???????????????????
   if (iRow>=10)
     { 
     Phi = mDb->radiansPerBoundary() / 2 
     + (159.5-padpos) * mDb->radiansPerPad()
     -PhiDeflect + iSec * (mDb->numberOfPads() * mDb->radiansPerPad()
     + mDb->radiansPerBoundary())+halfpi;
     }
  */

  // debug
  //cout<<"====================================="<<endl;
  //cout<<"Radius = "<<Rad<<" | Phi = "<<Phi<<endl;
  //cout<<"====================================="<<endl;

  // Anm : Pointer genauer !!!!!!!??????????

  (*x1) = -Rad*cos(Phi); 

  /*
  if (iRow <10) {
   (*x1) = -Rad*cos(Phi);
  } 
  */

  (*y1) = Rad*sin(Phi);

  //float z = mDb->padrowZPosition(iRow)

  /*
  if (iRow<10)
    {
      cout<<iRow<<" "<<iSec<<" "<<timepos<<" "<<padpos<<endl;
      cout<<pdeflection[iRow + mDb->numberOfPadrowsPerSide() * (PadtransLower+1)]<<" "<<pdeflection[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower]<<endl;
      cout<<PhiDeflect<<" "<<Phi<<" "<<Rad<<endl;
      cout<<(*x1)<<" "<<(*y1)<<endl;
    }
  */

  return true;
}
