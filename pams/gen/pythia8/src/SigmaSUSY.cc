// SigmaSUSY.cc is a part of the PYTHIA event generator.
// Copyright (C) 2008 Torbjorn Sjostrand.
// PYTHIA is licenced under the GNU GPL version 2, see COPYING for details.
// Please respect the MCnet Guidelines, see GUIDELINES for details.

// Function definitions (not found in the header) for the 
// supersymmetry simulation classes. 

#include "SigmaSUSY.h"

namespace Pythia8 {

//**************************************************************************

// Sigma2qqbar2gauginogaugino "mother" class.
// Cross section for gaugino pair production: neutralino pair, 
// neutralino-chargino, and chargino pair production all inherit from this.

//*********

// Initialize process. 
  
void Sigma2qqbar2gauginogaugino::initProc() {

  // Construct name of process. 
  nameSave = "q qbar -> " + ParticleDataTable::name(id3) + " " 
    + ParticleDataTable::name(id4);

  // Count number of final-state charged particles (default = 0)
  // (useful since charginos inherit from this class.)
  nCharged=0;
  if (abs(id3) == 1000024 || abs(id3) == 1000037) nCharged++;
  if (abs(id4) == 1000024 || abs(id4) == 1000037) nCharged++;

  // Set up couplings
  mZpole    = ParticleDataTable::m0(23);
  wZpole    = ParticleDataTable::mWidth(23);
  double mWpole = ParticleDataTable::m0(24);

  // Running masses and weak mixing angle 
  // (default to pole values if no running available)
  double mW = mWpole;
  double mZ = mZpole;
  sin2W     = 1.0 - pow(mW/mZ,2);  
  if (slhaPtr->gauge.exists(1) && slhaPtr->gauge.exists(2) 
      && slhaPtr->hmix.exists(3)) {
    double gp=slhaPtr->gauge(1);
    double g =slhaPtr->gauge(2);
    double v =slhaPtr->hmix(3);
    mW      = g * v / 2.0;
    mZ      = sqrt(pow(gp,2)+pow(g,2)) * v / 2.0;
    sin2W   = pow2(gp)/(pow2(g)+pow2(gp));  
  }

  // Shorthand for SUSY couplings
  // By default, use the running one in HMIX
  // If not found, use the MZ one in MINPAR
  double tanb = slhaPtr->hmix.exists(2) ? 
    slhaPtr->hmix(2) : slhaPtr->minpar(3);
  double sinW=sqrt(sin2W);
  double cosW=sqrt(1.0-sin2W);
  double cosb = sqrt( 1.0 / (1.0 + tanb*tanb) );
  double sinb = sqrt(max(0.0,1.0-cosb*cosb));
  SusyLesHouches::matrixblock<6> Ru(slhaPtr->usqmix);
  SusyLesHouches::matrixblock<6> Rd(slhaPtr->dsqmix);
  SusyLesHouches::matrixblock<6> imRu(slhaPtr->imusqmix);
  SusyLesHouches::matrixblock<6> imRd(slhaPtr->imusqmix);

  // Local complex copies of neutralino mixing matrix entries. 
  // (incl NMSSM for future use)
  complex ni1,nj1,ni2,nj2,ni3,nj3,ni4,nj4,ni5,nj5;
  if (slhaPtr->modsel(3) != 1) {
    ni1=complex( slhaPtr->nmix(id3chi,1), slhaPtr->imnmix(id3chi,1) );
    nj1=complex( slhaPtr->nmix(id4chi,1), slhaPtr->imnmix(id4chi,1) );
    ni2=complex( slhaPtr->nmix(id3chi,2), slhaPtr->imnmix(id3chi,2) );
    nj2=complex( slhaPtr->nmix(id4chi,2), slhaPtr->imnmix(id4chi,2) );
    ni3=complex( slhaPtr->nmix(id3chi,3), slhaPtr->imnmix(id3chi,3) );
    nj3=complex( slhaPtr->nmix(id4chi,3), slhaPtr->imnmix(id4chi,3) );
    ni4=complex( slhaPtr->nmix(id3chi,4), slhaPtr->imnmix(id3chi,4) );
    nj4=complex( slhaPtr->nmix(id4chi,4), slhaPtr->imnmix(id4chi,4) );
    ni5=complex( 0.,0.);
    nj5=complex( 0.,0.);
  } else {
    ni1=complex( slhaPtr->nmnmix(id3chi,1), slhaPtr->imnmnmix(id3chi,1) );
    nj1=complex( slhaPtr->nmnmix(id4chi,1), slhaPtr->imnmnmix(id4chi,1) );
    ni2=complex( slhaPtr->nmnmix(id3chi,2), slhaPtr->imnmnmix(id3chi,2) );
    nj2=complex( slhaPtr->nmnmix(id4chi,2), slhaPtr->imnmnmix(id4chi,2) );
    ni3=complex( slhaPtr->nmnmix(id3chi,3), slhaPtr->imnmnmix(id3chi,3) );
    nj3=complex( slhaPtr->nmnmix(id4chi,3), slhaPtr->imnmnmix(id4chi,3) );
    ni4=complex( slhaPtr->nmnmix(id3chi,4), slhaPtr->imnmnmix(id3chi,4) );
    nj4=complex( slhaPtr->nmnmix(id4chi,4), slhaPtr->imnmnmix(id4chi,4) );
    ni5=complex( slhaPtr->nmnmix(id3chi,5), slhaPtr->imnmnmix(id3chi,5) );
    nj5=complex( slhaPtr->nmnmix(id4chi,5), slhaPtr->imnmnmix(id4chi,5) );
  }    
  
  // Change to positive mass convention.
  complex iRot( 0., 1.);
  if (slhaPtr->mass(abs(id3)) < 0.) {
    ni1 *= iRot;
    ni2 *= iRot;
    ni3 *= iRot;
    ni4 *= iRot;
    ni5 *= iRot;
  };
  if (slhaPtr->mass(abs(id4)) < 0.) {
    nj1 *= iRot;
    nj2 *= iRot;
    nj3 *= iRot;
    nj4 *= iRot;
    nj5 *= iRot;
  };

  // Local copies of Chargino mixing
  complex ui1,ui2,vi1,vi2,uj1,uj2,vj1,vj2;
  if (id3chi <= 2) {
    ui1=complex( slhaPtr->umix(id3chi,1), slhaPtr->imumix(id3chi,1) );
    ui2=complex( slhaPtr->umix(id3chi,2), slhaPtr->imumix(id3chi,2) );
    vi1=complex( slhaPtr->vmix(id3chi,1), slhaPtr->imvmix(id3chi,1) );
    vi2=complex( slhaPtr->vmix(id3chi,2), slhaPtr->imvmix(id3chi,2) );
  }
  if (id4chi <= 2) {
    uj1=complex( slhaPtr->umix(id4chi,1), slhaPtr->imumix(id4chi,1) );
    uj2=complex( slhaPtr->umix(id4chi,2), slhaPtr->imumix(id4chi,2) );
    vj1=complex( slhaPtr->vmix(id4chi,1), slhaPtr->imvmix(id4chi,1) );
    vj2=complex( slhaPtr->vmix(id4chi,2), slhaPtr->imvmix(id4chi,2) );    
  }

  // Z chi_i chi_j 
  OLpp = -0.5 * ni3 * conj(nj3) + 0.5 * ni4 * conj(nj4);
  ORpp =  0.5 * conj(ni3) * nj3 - 0.5 * conj(ni4) * nj4;

  // Z cha_i cha_j
  OLp = -vi1*conj(vj1) - 0.5*vi2*conj(vj2) 
    + ( (id3chi == id4chi) ? sin2W : 0.0);
  ORp = -conj(ui1)*uj1 - 0.5*conj(ui2)*uj2 
    + ( (id3chi == id4chi) ? sin2W : 0.0);
    
  // W chi_i cha_j
  OL = -1.0/sqrt(2.0)*ni4*conj(vj2)+ni2*conj(vj1);
  OR = 1.0/sqrt(2.0)*conj(ni3)*uj2+conj(ni2)*uj1;
    
  // Z q_{idq} q_{idq} (def with extra factor 2 compared to [Okun])
  for (int idq = 1; idq <= 5; ++idq) {
    // No FCNC in Zqq, so here sum over diagonal only
    // No t in beam, so only sum up to 5
    LqqZ[idq] = CoupEW::lf(idq);
    RqqZ[idq] = CoupEW::rf(idq);
  }
 
  // ~chi^0_i ~q_jsq idq
  for (int jsq = 1; jsq<=6; jsq++) {
    // Sum jsq over all squarks
    for (int idq = 1; idq<=5; idq++) {
      // No t in beam, so only sum iq over 5 lightest quarks.

      // quark index 
      int k=(idq+1)/2;

      // Set quark mass for use in couplings
      // Initial guess 0,0,0,mc,mb, with the latter from the PDT
      double mq = ParticleDataTable::m0(idq);
      if (idq <= 3) mq=0.0;

      // Treat u and d quarks separately
      if (idq % 2 == 1) {

	// idq = d quark
	double eq = -1.0/3.0;
	double T3q = -0.5;

	// Compute running mass from Yukawas and vevs if possible.
	if (slhaPtr->yd.exists() && slhaPtr->hmix.exists(3)) {
	  double ykk=slhaPtr->yd(k,k);
	  double v1=slhaPtr->hmix(3)/sqrt(1+pow(tanb,2));
	  if (ykk != 0.0) mq = ykk * v1 / sqrt(2.0) ;
	  //	  cout <<scientific<<" q "<<idq << "  (k="<<k<<")  Y = "
	  //     <<ykk<<"  v = "<<v1<<"  m = "<<mq<<endl;
	}

	// Shorthand for squark mixing matrices
	complex RdjkL(Rd(jsq,k),imRd(jsq,k));
	complex RdjkR(Rd(jsq,k+3),imRd(jsq,k+3));

	// L(~d_jsq,d_k,~chi^0_i)
	LsqXi[jsq][idq]=((eq-T3q)*sinW*ni1+T3q*cosW*ni2) * conj(RdjkL)
	  + mq*cosW*ni3*conj(RdjkR)/2.0/mW/cosb ;

	// L(~d_jsq,d_k,~chi^0_j)
	LsqXj[jsq][idq]=((eq-T3q)*sinW*nj1+T3q*cosW*nj2) * conj(RdjkL) 
	  + mq*cosW*nj3*conj(RdjkR)/2.0/mW/cosb ;

	// R(~d_jsq,d_k,~chi^0_i)
	RsqXi[jsq][idq]=eq*sinW*ni1*RdjkR - mq*cosW*ni3*RdjkL/2.0/mW/cosb ;
	RsqXi[jsq][idq]=-conj(RsqXi[jsq][idq]) ;

	// R(~d_jsq,d_k,~chi^0_j)
	RsqXj[jsq][idq]=eq*sinW*nj1*RdjkR - mq*cosW*nj3*RdjkL/2.0/mW/cosb ;
	RsqXj[jsq][idq]=-conj(RsqXj[jsq][idq]) ;
	
	// Chargino couplings (initialize)
	LsqCi[jsq][idq]=0.0;
	RsqCi[jsq][idq]=0.0;
	LsqCj[jsq][idq]=0.0;
	RsqCj[jsq][idq]=0.0;

	// Sum over flavours (off-diagonal mixings)
	// (note l <-> k with respect to [Klasen et al])
	for (int l=1; l<=3; l++) {

	  // Set quark masses for use in couplings
	  // Initial guess 0,0,0,mc,mb, with the latter from the PDT
	  double mu = ParticleDataTable::m0(2*l);
	  if (2*l <= 3) mu=0.0;	  

	  // Compute running u mass from Yukawas and vevs if possible.
	  if (slhaPtr->yu.exists() && slhaPtr->hmix.exists(3)) {
	    double yll=slhaPtr->yu(l,l);
	    double v2=slhaPtr->hmix(3)/sqrt(1.0+1.0/pow(tanb,2));
	    if (yll != 0.0) mu = yll * v2 / sqrt(2.0) ;
	  }

	  // Shorthand for u squark mixing 
	  complex RujlL(Ru(jsq,l),imRu(jsq,l));
	  complex RujlR(Ru(jsq,l+3),imRu(jsq,l+3));

	  // CKM matrix (use Pythia-8 one if no SLHA)
	  complex Vlk=VCKM::Vid(l,k);
	  if (slhaPtr->vckm.exists()) 
	    Vlk=complex(slhaPtr->vckm(l,k),slhaPtr->imvckm(l,k));

	  // L(~u_jsq,d_k,~chi^+/-_i)
	  LsqCi[jsq][idq] += Vlk*(conj(vi1)*RujlL
				  -mu*conj(vi2)*RujlR/sqrt(2.0)/mW/sinb );

	  // L(~u_jsq,d_k,~chi^+/-_j)
	  LsqCj[jsq][idq] += Vlk*(conj(vj1)*RujlL
				  -mu*conj(vj2)*RujlR/sqrt(2.0)/mW/sinb ); 
	  
	  // R(~u_jsq,d_k,~chi^+/-_i)
	  RsqCi[jsq][idq] += - Vlk*mq*ui2*RujlL/sqrt(2.0)/mW/cosb ;

	  // R(~u_jsq,d_k,~chi^+/-_j)
	  RsqCj[jsq][idq] += - Vlk*mq*uj2*RujlL/sqrt(2.0)/mW/cosb ;

	}
	
      } else {

	// idq = u quark
	double eq = 2.0/3.0;
	double T3q = 0.5;

	// Compute mass from Yukawas and vevs if possible.
	if (slhaPtr->yu.exists() && slhaPtr->hmix.exists(3)) {
	  double ykk=slhaPtr->yu(k,k);
	  double v2=slhaPtr->hmix(3)/sqrt(1.0+1.0/pow(tanb,2));
	  if (ykk != 0.0) mq = ykk * v2 / sqrt(2.0) ;
	  //	  cout <<scientific<<" q "<<idq << "  (k="<<k<<")  Y = "
	  //     <<ykk<<"  v = "<<v2<<"  m = "<<mq<<endl;
	}

	// Shorthand for squark mixing matrices
	complex RujkL(Ru(jsq,k),imRu(jsq,k));
	complex RujkR(Ru(jsq,k+3),imRu(jsq,k+3));

	// L(~u_jsq,u_k,~chi^0_i)
	LsqXi[jsq][idq]=((eq-T3q)*sinW*ni1+T3q*cosW*ni2) * conj(RujkL)
	  + mq*cosW*ni4*conj(RujkR)/2.0/mW/sinb ;

	// L(~u_jsq,u_k,~chi^0_j)
	LsqXj[jsq][idq]=((eq-T3q)*sinW*nj1+T3q*cosW*nj2) * conj(RujkL)
	  + mq*cosW*nj4*conj(RujkR)/2.0/mW/sinb ;

	// R(~u_jsq,u_k,~chi^0_i)
	RsqXi[jsq][idq]=eq*sinW*ni1*RujkR - mq*cosW*ni4*RujkL/2.0/mW/sinb ;
	RsqXi[jsq][idq]=-conj(RsqXi[jsq][idq]);

	// R(~u_jsq,u_k,~chi^0_i)
	RsqXj[jsq][idq]=eq*sinW*nj1*RujkR - mq*cosW*nj4*RujkL/2.0/mW/sinb ;
	RsqXj[jsq][idq]=-conj(RsqXj[jsq][idq]);

	// Chargino couplings (initialize)
	LsqCi[jsq][idq]=0.0;
	RsqCi[jsq][idq]=0.0;
	LsqCj[jsq][idq]=0.0;
	RsqCj[jsq][idq]=0.0;

	// Sum over flavours (off-diagonal mixings)
	// (note l <-> k with respect to [Klasen et al])
	for (int l=1; l<=3; l++) {

	  // Set quark masses for use in couplings
	  // Initial guess 0,0,0,mc,mb, with the latter from the PDT
	  double md = ParticleDataTable::m0(2*l-1);
	  if (2*l-1 <= 3) md=0.0;	  

	  // Compute running d mass from Yukawas and vevs if possible.
	  if (slhaPtr->yd.exists() && slhaPtr->hmix.exists(3)) {
	    double yll=slhaPtr->yd(l,l);
	    double v1=slhaPtr->hmix(3)/sqrt(1+pow(tanb,2));
	    if (yll != 0.0) md = yll * v1 / sqrt(2.0) ;
	  }

	  // Shorthand for d squark mixing 
	  complex RdjlL(Rd(jsq,l),imRd(jsq,l));
	  complex RdjlR(Rd(jsq,l+3),imRd(jsq,l+3));

	  // CKM matrix (use Pythia-8 one if no SLHA)
	  complex Vkl=VCKM::Vid(k,l);
	  if (slhaPtr->vckm.exists()) 
	    Vkl=complex(slhaPtr->vckm(k,l),slhaPtr->imvckm(k,l));

	  // L(~d_jsq,u_k,~chi^+/-_i)
	  LsqCi[jsq][idq] += Vkl*(ui1*conj(RdjlL)
				  -md*ui2*conj(RdjlR)/sqrt(2.0)/mW/cosb) ;

	  // L(~d_jsq,u_k,~chi^+/-_j)
	  LsqCj[jsq][idq] += Vkl*(uj1*conj(RdjlL)
				  -md*uj2*conj(RdjlR)/sqrt(2.0)/mW/cosb) ; 
	  
	  // R(~d_jsq,u_k,~chi^+/-_i)
	  RsqCi[jsq][idq] += - Vkl*mq*conj(vi2*RdjlL)/sqrt(2.0)/mW/sinb ;

	  // R(~d_jsq,u_k,~chi^+/-_j)
	  RsqCj[jsq][idq] += - Vkl*mq*conj(vi2*RdjlL)/sqrt(2.0)/mW/sinb ;
	  
	}
	
      }
    }
  }
}

//*********

// Evaluate d(sigmaHat)/d(tHat), part independent of incoming flavour. 

void Sigma2qqbar2gauginogaugino::sigmaKin() {

  // Common flavour-independent factor.
  sigma0 = (M_PI / sH2) * pow2(alpEM) ; 

  // Factor 1/2 for identical final particles.
  if (id3 == id4 && nCharged == 0) sigma0 *= 0.5;

  // Auxiliary factors for use below
  ui     = uH - s3;
  uj     = uH - s4;
  ti     = tH - s3;
  tj     = tH - s4;
  sz     = sH - pow2(mZpole);
  d      = pow2(sz) + pow2(mZpole * wZpole);
  propZ  = complex( sz / d, mZpole * wZpole / d);

}

//*********

// Evaluate d(sigmaHat)/d(tHat), including incoming flavour dependence. 

// neutralino pairs:

double Sigma2qqbar2gauginogaugino::sigmaHat() {

  // Only allow quark-antiquark incoming states
  if (id1*id2 >= 0) {
    return 0.0;    
  }
  
  // Only allow incoming states with sum(charge) = 0
  if ((id1+id2) % 2 != 0) {
    return 0.0;    
  }

  // Flavour-dependent kinematics-dependent couplings.
  int idAbs1    = abs(id1);  
  int idAbs2    = abs(id2);  
  complex QuLL = (id1 == -id2) ? LqqZ[idAbs1] * OLpp/2.0 * propZ : 0.0;
  complex QtLL = (id1 == -id2) ? LqqZ[idAbs1] * ORpp/2.0 * propZ : 0.0;
  complex QuRR = (id1 == -id2) ? RqqZ[idAbs1] * ORpp/2.0 * propZ : 0.0;
  complex QtRR = (id1 == -id2) ? RqqZ[idAbs1] * OLpp/2.0 * propZ : 0.0;
  complex QuLR = 0.0;
  complex QtLR = 0.0;
  complex QuRL = 0.0;
  complex QtRL = 0.0;
  
  // Add t-channel squark flavour sums to QmXY couplings
  for (int jsq=1; jsq<=6; jsq++) {    
    int idsq=((jsq+2)/3)*1000000 + 2*((jsq-1) % 3) + (idAbs1+1) % 2 + 1;
    double msq2=pow(ParticleDataTable::m0(idsq),2);
    double usq     = uH - msq2;
    double tsq     = tH - msq2;
    QuLL += LsqXi[jsq][idAbs2]*conj(LsqXj[jsq][idAbs1])/usq;
    QtLL -= conj(LsqXi[jsq][idAbs1])*LsqXj[jsq][idAbs2]/tsq;
    QuRR += RsqXi[jsq][idAbs2]*conj(RsqXj[jsq][idAbs1])/usq;
    QtRR -= conj(RsqXi[jsq][idAbs1])*RsqXj[jsq][idAbs2]/tsq;
    QuLR += RsqXi[jsq][idAbs2]*conj(LsqXj[jsq][idAbs1])/usq;
    QtLR += conj(LsqXi[jsq][idAbs1])*RsqXj[jsq][idAbs2]/tsq;
    QuRL += LsqXi[jsq][idAbs2]*conj(RsqXj[jsq][idAbs1])/usq;
    QtRL += conj(RsqXi[jsq][idAbs1])*LsqXj[jsq][idAbs2]/tsq;
  }

  // Multiply by overall normalization
  QuLL *= 1.0/(sin2W * (1 - sin2W));
  QtLL *= 1.0/(sin2W * (1 - sin2W));
  QuRR *= 1.0/(sin2W * (1 - sin2W));
  QtRR *= 1.0/(sin2W * (1 - sin2W));
  QuLR *= 1.0/(sin2W * (1 - sin2W));
  QtLR *= 1.0/(sin2W * (1 - sin2W));
  QuRL *= 1.0/(sin2W * (1 - sin2W));
  QtRL *= 1.0/(sin2W * (1 - sin2W));

  // Compute matrix element weight
  double weight = 0;
  // Average over separate helicity contributions
  // LL (ha = -1, hb = +1) (divided by 4 for average)            
  weight += norm(QuLL) * ui * uj + norm(QtLL) * ti * tj
    + 2 * real(conj(QuLL) * QtLL) * m3 * m4 * sH;
  // RR (ha =  1, hb = -1) (divided by 4 for average)        
  weight += norm(QtRR) * ti * tj + norm(QuRR) * ui * uj  
    + 2 * real(conj(QuRR) * QtRR) * m3 * m4 * sH;
  // RL (ha =  1, hb =  1) (divided by 4 for average)        
  weight += norm(QuRL) * ui * uj + norm(QtRL) * ti * tj
    - real(conj(QuRL) * QtRL) * (uH * tH - s3 * s4);
  // LR (ha = -1, hb = -1) (divided by 4 for average)        
  weight += norm(QuLR) * ui * uj + norm(QtLR) * ti * tj
    - real(conj(QuLR) * QtLR) * (uH * tH - s3 * s4);

  // Cross section, including colour factor.
  double sigma = sigma0 * weight;
  if (idAbs1 < 9) sigma /= 3.;

  // Answer.
  return sigma;    

}

// neutralino-chargino 

double Sigma2qqbar2chi0char::sigmaHat() {

  // This cross section not fully debugged yet, skip. 
  return 0.0;

  // Only allow quark-antiquark incoming states
  if (id1*id2 >= 0) {
    return 0.0;    
  }
  
  // Only allow incoming states with sum(charge) = +/- 1
  if ((id1+id2) % 2 == 0) {
    return 0.0;    
  }

  // Flavour-dependent kinematics-dependent couplings.
  int idAbs1    = abs(id1);  
  int idAbs2    = abs(id2);  
  complex QuLL = (id1 == -id2) ? LqqZ[idAbs1] * OLpp/2.0 * propZ : 0.0;
  complex QtLL = (id1 == -id2) ? LqqZ[idAbs1] * ORpp/2.0 * propZ : 0.0;
  complex QuRR = (id1 == -id2) ? RqqZ[idAbs1] * ORpp/2.0 * propZ : 0.0;
  complex QtRR = (id1 == -id2) ? RqqZ[idAbs1] * OLpp/2.0 * propZ : 0.0;
  complex QuLR = 0.0;
  complex QtLR = 0.0;
  complex QuRL = 0.0;
  complex QtRL = 0.0;
  
  // Add t-channel squark flavour sums to QmXY couplings
  for (int jsq=1; jsq<=6; jsq++) {    
    int idsq=((jsq+2)/3)*1000000 + 2*((jsq-1) % 3) + (idAbs1+1) % 2 + 1;
    double msq2=pow(ParticleDataTable::m0(idsq),2);
    double usq     = uH - msq2;
    double tsq     = tH - msq2;
    QuLL += LsqXi[jsq][idAbs2]*conj(LsqXj[jsq][idAbs1])/usq;
    QtLL -= conj(LsqXi[jsq][idAbs1])*LsqXj[jsq][idAbs2]/tsq;
    QuRR += RsqXi[jsq][idAbs2]*conj(RsqXj[jsq][idAbs1])/usq;
    QtRR -= conj(RsqXi[jsq][idAbs1])*RsqXj[jsq][idAbs2]/tsq;
    QuLR += RsqXi[jsq][idAbs2]*conj(LsqXj[jsq][idAbs1])/usq;
    QtLR += conj(LsqXi[jsq][idAbs1])*RsqXj[jsq][idAbs2]/tsq;
    QuRL += LsqXi[jsq][idAbs2]*conj(RsqXj[jsq][idAbs1])/usq;
    QtRL += conj(RsqXi[jsq][idAbs1])*LsqXj[jsq][idAbs2]/tsq;
  }

  // Compute matrix element weight
  double weight = 0;
  // Average over separate helicity contributions
  // LL (ha = -1, hb = +1) (divided by 4 for average)            
  weight += norm(QuLL) * ui * uj + norm(QtLL) * ti * tj
    + 2 * real(conj(QuLL) * QtLL) * m3 * m4 * sH;
  // RR (ha =  1, hb = -1) (divided by 4 for average)        
  weight += norm(QtRR) * ti * tj + norm(QuRR) * ui * uj  
    + 2 * real(conj(QuRR) * QtRR) * m3 * m4 * sH;
  // RL (ha =  1, hb =  1) (divided by 4 for average)        
  weight += norm(QuRL) * ui * uj + norm(QtRL) * ti * tj
    - real(conj(QuRL) * QtRL) * (uH * tH - s3 * s4);
  // LR (ha = -1, hb = -1) (divided by 4 for average)        
  weight += norm(QuLR) * ui * uj + norm(QtLR) * ti * tj
    - real(conj(QuLR) * QtLR) * (uH * tH - s3 * s4);

  // Cross section, including colour factor.
  double sigma = sigma0 * weight;
  if (idAbs1 < 9) sigma /= 3.;

  // Answer.
  return sigma;    

}

// chargino-chargino 

double Sigma2qqbar2charchar::sigmaHat() {

  // This cross section not fully debugged yet, skip. 
  return 0.0;

  // Only allow quark-antiquark incoming states
  if (id1*id2 >= 0) {
    return 0.0;
  }
  
  // Only allow incoming states with sum(charge) = 0
  if ((id1+id2) % 2 != 0) {
    return 0.0;    
  }

  // Flavour-dependent kinematics-dependent couplings.
  int idAbs1    = abs(id1);  
  int idAbs2    = abs(id2);  
  
  // Add s-channel Z
  complex QuLL = (id1 == -id2) ? - LqqZ[idAbs1] * conj(OLp)/2.0 * propZ 
    / (sin2W * (1 - sin2W)) : 0.0;
  complex QtLL = (id1 == -id2) ? - LqqZ[idAbs1] * conj(ORp)/2.0 * propZ 
    / (sin2W * (1 - sin2W)) : 0.0;
  complex QuRR = (id1 == -id2) ? - RqqZ[idAbs1] * conj(ORp)/2.0 * propZ 
    / (sin2W * (1 - sin2W)) : 0.0;
  complex QtRR = (id1 == -id2) ? - RqqZ[idAbs1] * conj(OLp)/2.0 * propZ 
    / (sin2W * (1 - sin2W)) : 0.0;
  complex QuLR = 0.0;
  complex QtLR = 0.0;
  complex QuRL = 0.0;
  complex QtRL = 0.0;
  
  // Add s-channel photon
  if (idAbs1 == idAbs2 && abs(id3) == abs(id4)) {
    double e = -1.0/3.0;
    if (idAbs1 % 2 == 0) e = 2.0/3.0;
    if (idAbs1 > 10) e = -1.0;
    QuLL += e/sH;
    QtLL += e/sH;
    QuRR += e/sH;
    QtRR += e/sH;
  }

  // Add t-channel squark flavour sums to QmXY couplings
  for (int jsq=1; jsq<=6; jsq++) {    
    int idsq=((jsq+2)/3)*1000000 + 2*((jsq-1) % 3) + (idAbs1 % 2) + 1;
    double msq2=pow(ParticleDataTable::m0(idsq),2);
    double usq     = uH - msq2;
    double tsq     = tH - msq2;

    // up-type quarks get u-channel d-squark contributions
    if (idAbs1 % 2 == 0 ) {

      // Flip 1<->2 and i<->j if id1 is antiquark
      if (id1 > 0) {
	QuLL += LsqCi[jsq][idAbs2]*conj(LsqCj[jsq][idAbs1])/usq/2.0/sin2W;
	QuRR += RsqCi[jsq][idAbs2]*conj(RsqCj[jsq][idAbs1])/usq/2.0/sin2W;
	QuLR += RsqCi[jsq][idAbs2]*conj(LsqCj[jsq][idAbs1])/usq/2.0/sin2W;
	QuRL += LsqCi[jsq][idAbs2]*conj(RsqCj[jsq][idAbs1])/usq/2.0/sin2W;
      } else {
	QuLL += LsqCj[jsq][idAbs1]*conj(LsqCi[jsq][idAbs2])/usq/2.0/sin2W;
	QuRR += RsqCj[jsq][idAbs1]*conj(RsqCi[jsq][idAbs2])/usq/2.0/sin2W;
	QuLR += RsqCj[jsq][idAbs1]*conj(LsqCi[jsq][idAbs2])/usq/2.0/sin2W;
	QuRL += LsqCj[jsq][idAbs1]*conj(RsqCi[jsq][idAbs2])/usq/2.0/sin2W;
      } 
    }

    // down-type quarks get t-channel u-squark contributions
    else if (idAbs1 % 2 == 1) {
      // Flip 1<->2 and i<->j if id1 is antiquark
      if (id1 > 0) {
	QtLL -= LsqCi[jsq][idAbs1]*conj(LsqCj[jsq][idAbs2])/tsq/2.0/sin2W;
	QtRR -= RsqCi[jsq][idAbs1]*conj(RsqCj[jsq][idAbs2])/tsq/2.0/sin2W;
	QtLR += LsqCi[jsq][idAbs1]*conj(RsqCj[jsq][idAbs2])/tsq/2.0/sin2W;
	QtRL += RsqCi[jsq][idAbs1]*conj(LsqCj[jsq][idAbs2])/tsq/2.0/sin2W;
      } else {
	QtLL -= LsqCj[jsq][idAbs2]*conj(LsqCi[jsq][idAbs1])/tsq/2.0/sin2W;
	QtRR -= RsqCj[jsq][idAbs2]*conj(RsqCi[jsq][idAbs1])/tsq/2.0/sin2W;
	QtLR += LsqCj[jsq][idAbs2]*conj(RsqCi[jsq][idAbs1])/tsq/2.0/sin2W;
	QtRL += RsqCj[jsq][idAbs2]*conj(LsqCi[jsq][idAbs1])/tsq/2.0/sin2W;
      } 

    }
  }

  // Compute matrix element weight
  double weight = 0;
  // Average over separate helicity contributions
  // LL (ha = -1, hb = +1) (divided by 4 for average)            
  weight += norm(QuLL) * ui * uj + norm(QtLL) * ti * tj
    + 2 * real(conj(QuLL) * QtLL) * m3 * m4 * sH;
  // RR (ha =  1, hb = -1) (divided by 4 for average)        
  weight += norm(QtRR) * ti * tj + norm(QuRR) * ui * uj  
    + 2 * real(conj(QuRR) * QtRR) * m3 * m4 * sH;
  // RL (ha =  1, hb =  1) (divided by 4 for average)        
  weight += norm(QuRL) * ui * uj + norm(QtRL) * ti * tj
    - real(conj(QuRL) * QtRL) * (uH * tH - s3 * s4);
  // LR (ha = -1, hb = -1) (divided by 4 for average)        
  weight += norm(QuLR) * ui * uj + norm(QtLR) * ti * tj
    - real(conj(QuLR) * QtLR) * (uH * tH - s3 * s4);

  // Cross section, including colour factor.
  double sigma = sigma0 * weight;
  if (idAbs1 < 9) sigma /= 3.;

  // Answer.
  return sigma;    

}

//*********

// Select identity, colour and anticolour.

void Sigma2qqbar2gauginogaugino::setIdColAcol() {

  // Set flavours.

  if (nCharged == 0) {
    // Neutralino-Neutralino
    setId( id1, id2, id3, id4);

  } else if (nCharged == 1) {
    // Neutralino-Chargino: set charge according to in-states
    if ( abs(id1%1) == 1 && id1 > 0 ) {
      setId( id1, id2, id3, -id4);
    } else { 
      setId( id1, id2, id3, id4);
    }

  } else if (nCharged == 2) { 
    // Chargino-Chargino: charge already included in id3, id4
    setId( id1, id2, id3, id4);
  }
  
  // Colour flow topologies. Swap when antiquarks.
  if (abs(id1) < 9) setColAcol( 1, 0, 0, 1, 0, 0, 0, 0);
  else              setColAcol( 0, 0, 0, 0, 0, 0, 0, 0);
  if (id1 < 0) swapColAcol();

}

//**************************************************************************

} // end namespace Pythia8

