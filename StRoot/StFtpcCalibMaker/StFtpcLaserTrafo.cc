// $Id: StFtpcLaserTrafo.cc,v 1.8 2010/01/14 18:44:28 jcs Exp $
//
// $Log: StFtpcLaserTrafo.cc,v $
// Revision 1.8  2010/01/14 18:44:28  jcs
// change padtrans so that the code is the same as StFtpcClusterFinder::padtrans
//
// Revision 1.7  2009/12/09 14:40:01  jcs
// update comment
//
// Revision 1.6  2007/01/22 13:08:15  jcs
// replace cout with LOG
//
// Revision 1.5  2007/01/19 08:23:00  jcs
// replace true, false with kTRUE, kFALSE
//
// Revision 1.4  2007/01/16 15:04:17  jcs
// reolace "return FALSE" with "return false" as temporary fix so that dev compiles
//
// Revision 1.3  2006/04/12 08:46:01  jcs
// initialize p0,p1,p2
//
// Revision 1.2  2006/03/15 15:13:57  jcs
// add lines for listing CVS update info
//

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
  LOG_INFO<<"StFtpcLaserTrafo..."<<endm; 
  mDb=getdb; mParam=getparam;
  
  mBField = getmbfield;

  deltat0=getdeltat0;
  deltagas=getdeltagas;
  tZero = getTZero;

  LOG_INFO<<"d_t0 = "<<deltat0<<" | d_gas(Ar) = "<<(float) deltagas<<endm;
  micropertime = getmicropertime;
  LOG_INFO<<"Microseconds per timebin = "<<micropertime<<endm;
  LOG_INFO<<"==============================="<<endm;
  LOG_INFO<<" "<<endm;

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
  //LOG_DEBUG<<"StFtpcLaserTrafo deconstructed"<<endm; 
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
  double p0 = 0.0;
  double p1 = 0.0;
  double p2 = 0.0;

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
  double p0 = 0.0;
  double p1 = 0.0;
  double p2 = 0.0;

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
  
  LOG_INFO<<"calcpadtrans ..."<<endm;

  step_size=((float) mDb->numberOfTimebins()
	     / (float) mParam->numberOfDriftSteps());


  LOG_INFO<<"Pressure & temperature correction  = "<<deltap<<endm;
  LOG_INFO<<"======================================================"<<endm;


  LOG_INFO<<"RadiusTimesField = "<<mDb->radiusTimesField()<<endm;
  LOG_INFO<<"vdrift(0,0)   = "<<mDb->magboltzVDrift(0,0)<<endm;
  LOG_INFO<<"lorangle(0,0) = "<<mDb->magboltzDeflection(0,0)<<endm;
  LOG_INFO<<" "<<endm;

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
	  return kFALSE;
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

      //LOG_DEBUG<<psi_now<<endm;
      
      // gas correction (only use by fullfield !)
      if (deltagas!=0)
	{
	  //LOG_DEBUG<<"v_now before "<<v_now<<endm;
	  v_now=v_now+vd_gas(r_last);
	  //LOG_DEBUG<<"v_now after "<<v_now<<endm;
	  //LOG_DEBUG<<"pnow "<<psi_now<<endm;
	  //LOG_DEBUG<<Grad2Rad*lor_gas(r_last)<<" "<<lor_gas(r_last)<<endm;
	  //psi_now=psi_now+lor_gas(r_last);
	  // Is 1st approximation reversed FullField correct !????
	  //psi_now=-(psi_now+lor_gas(r_last));

	  if (mBField == -1 ) 
             psi_now=psi_now-lor_gas(r_last); // ???? B=-1 !!!
          else if (mBField == 0 || mBField == 1 )
	     psi_now=psi_now+lor_gas(r_last);   // B=0,B=+1
           else {
             LOG_ERROR<<"psi_now not defined for mBField = "<<mBField<<endm;
           }  

	  //LOG_DEBUG<<"psi_now "<<psi_now<<endm;
	  //v_now=v_now*cos(Grad2Rad*psi_now); 

	  //***************************
	  //* ??? so 1. Naeherung ??? *
	  //***************************
	  //LOG_DEBUG<<"v_now "<<v_now<<endm;
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
	      return kFALSE;
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
	      //LOG_DEBUG<<(lor_gas(r_next)-lor_gar_nexts(r_last))*180/TMath::Pi()<<endm;
	      //LOG_DEBUG<<"r_last = "<<r_last<<" | r_next = "<<r_next<<endm;
	    }
 
	  /* correct r_next: */
	  //r_next = r_last - v_now * step_size *mDb->microsecondsPerTimebin();
	  //LOG_DEBUG<<"r_last = "<<r_last<<" | r_next = "<<r_next<<" | r_next_new = "<< r_last - v_now * step_size * micropertime<<" | psi = "<<psi_now<<endm;
	  r_next = r_last - v_now * step_size * micropertime;
	  pradius[padrow+mDb->numberOfPadrowsPerSide()*(i+1)]=r_next;
	  //LOG_DEBUG<<padrow+mDb->numberOfPadrowsPerSide()*(i+1)<<endl;
	  pdeflection[padrow+mDb->numberOfPadrowsPerSide()*(i+1)]
	    =pdeflection[padrow+mDb->numberOfPadrowsPerSide()*i]
	    +((r_last-r_next)*tan(degree * psi_now)/r_last);

	  t_last=t_next;
	  r_last=r_next;
	}     
    }
  return kTRUE;
}

//---------------------------------------------------------------

int StFtpcLaserTrafo::padtrans(int iRow,int iSec,float timepos, float padpos,float *x1,float *y1)
{  
  int PadtransPerTimebin;
  int PadtransLower;
  float PhiDeflect, TimeCoordinate;
  iRow = iRow - 1;
  iSec = iSec - 1;
  
  TimeCoordinate = timepos + 0.5; /*time start at beginning of bin 0*/; 

  // Laser t0 from Calibrations_ftpc/ftpcElectronics 
  // Data  t0 from Calibrations_ftpc/ftpcElectronics

  TimeCoordinate += (tZero+deltat0)/micropertime;
  
  //LOG_DEBUG<<TimeCoordinate<<" "<<padpos<<" "<<(*x1)<<" "<<(*y1)<<endm;

  PadtransPerTimebin = (int) mParam->numberOfDriftSteps() / mDb->numberOfTimebins();
  PadtransLower= (int) (TimeCoordinate*PadtransPerTimebin);

  float Rad=pradius[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower]
  -(pradius[iRow + mDb->numberOfPadrowsPerSide()*(PadtransLower)]
           -pradius[iRow + mDb->numberOfPadrowsPerSide() * (PadtransLower+1)])/2;

  
  PhiDeflect=pdeflection[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower]
   +(pdeflection[iRow + mDb->numberOfPadrowsPerSide() * (PadtransLower+1)]
    -pdeflection[iRow + mDb->numberOfPadrowsPerSide() * PadtransLower])/2;

  // Aenderungen damit Koord. richtig (vgl. chain+Frank)

  //PhiDeflect=0.0; // ? genauer vgl auch ZerofieldMaker !???
  
  float Phi;

  /* calculate phi angle from pad position */
    // for FTPC West
  if (iRow <10) {
     Phi = mDb->radiansPerBoundary() / 2 
       + ((padpos-1) + 0.5) * mDb->radiansPerPad()
       + PhiDeflect + iSec * (mDb->numberOfPads() * mDb->radiansPerPad()
       + mDb->radiansPerBoundary())+halfpi;
  }

  //LOG_DEBUG<<"mDb->radiansPerBoundary() = "<<mDb->radiansPerBoundary()<<endm;
  //LOG_DEBUG<<"mDb->radiansPerPad() = "<<mDb->radiansPerPad()<<endm;
  //LOG_DEBUG<<"mDb->numberOfPads() = "<<mDb->numberOfPads()<<endm;

   // for FTPC East
  /* Invert pad number (== Peak->PadPosition) for FTPC East  */
  /* (not yet understood where and why pad numbers were inverted) */
   if (iRow>=10) { 
     Phi = mDb->radiansPerBoundary() / 2 
       + (159.5-(padpos-1)) * mDb->radiansPerPad()
       -PhiDeflect + iSec * (mDb->numberOfPads() * mDb->radiansPerPad()
       + mDb->radiansPerBoundary())+halfpi;
     }

  // debug
  //LOG_DEBUG<<"====================================="<<endm;
  //LOG_DEBUG<<"Radius = "<<Rad<<" | Phi = "<<Phi<<endl;
  //LOG_DEBUG<<"====================================="<<endm;

  // Anm : Pointer genauer !!!!!!!??????????

  /* transform to cartesian */
  (*x1) = Rad*cos(Phi); 
  if (iRow <10) {
    (*x1) = -(*x1);
  } 

  (*y1) = Rad*sin(Phi);

  return kTRUE;
}
