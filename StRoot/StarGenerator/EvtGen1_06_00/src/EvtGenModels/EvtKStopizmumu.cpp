//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package. If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//
// Module: EvtKStopizmumu.cc
//
// Description: Routine to implement KS -> pi0 mu mu; see JHEP08(1998)004.
//
// Modification history:
// 
// Veronika Chobanova, Diego Martinez Santos    November 25,2015  Module created
// With the assistance of Jeremy Dalseno
// LHCb collaboration
//
//------------------------------------------------------------------------

#include "EvtGenBase/EvtGenKine.hh"
#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtPatches.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtReport.hh"
#include "EvtGenBase/EvtConst.hh"
#include "EvtGenBase/EvtVector3R.hh"
#include "EvtGenBase/EvtVector4R.hh"
#include "EvtGenBase/EvtVector4C.hh"
#include "EvtGenBase/EvtDiracSpinor.hh"

#include "EvtGenModels/EvtKStopizmumu.hh"

void EvtKStopizmumu::init()
{
  // 5 arguments 
  checkNArg(5);

  // Only 3 daughters
  checkNDaug(3);

  // Spin-0 parent
  checkSpinParent(EvtSpinType::SCALAR);

  // Spin-0 daughters
  checkSpinDaughter(0,EvtSpinType::SCALAR);
  checkSpinDaughter(1,EvtSpinType::DIRAC);
  checkSpinDaughter(2,EvtSpinType::DIRAC);

  // KS parent
  const EvtId p = getParentId();
    
  if( p != EvtPDL::getId("K_S0"))
  {
    EvtGenReport(EVTGEN_ERROR,"EvtGen") << "EvtKStopizmumu: Parent must be K_S0" << std::endl;
    assert(0);
  }

  // Daughter types and ordering
  const EvtId d1 = getDaug(0);
  const EvtId d2 = getDaug(1);
  const EvtId d3 = getDaug(2);
  
  if( !(d1 == EvtPDL::getId("pi0") && 
        d2 == EvtPDL::getId("mu+") &&
        d3 == EvtPDL::getId("mu-")) )
  {
    EvtGenReport(EVTGEN_ERROR,"EvtGen") 
	<< "EvtKStopizmumu: Daughter sequence should be pi0, mu+, mu-" << std::endl;
    assert(0);
  }
}

void EvtKStopizmumu::decay( EvtParticle *p )
{
  const double Mpiz = EvtPDL::getMass(EvtPDL::getId("pi0")); 
  const double MKS = EvtPDL::getMass(EvtPDL::getId("K_S0"));
  const double MKS_Sq = MKS*MKS;
  const double rpisq = Mpiz*Mpiz/MKS_Sq;
  const double z0 = 1.0/3.0 + rpisq;

  // Generate 4-vectors in phase space
  p->initializePhaseSpace( getNDaug(), getDaugs() );

  // Daughter momenta
  const EvtVector4R p4piz = p->getDaug(0)->getP4();
  
  // Input parameters
  const double as = getArg(0);
  // bs value from G8/30GF in here: http://arxiv.org/pdf/hep-ph/0404136.pdf would be 0.017
  const double bs = getArg(1);
  const double b2 = getArg(2)*1e-8;
  const double d2 = getArg(3)*1e-8;
  const double rvsq = getArg(4);
  const double GF = EvtConst::Fermi;
  const double alpha_s = 4.0*b2/3.0;
  const double beta_s = -8.0*d2/3.0;

  // Calculate p4KS momentum
  EvtVector4R p4KS(MKS, 0.0, 0.0, 0.0);
  
  const EvtVector4R q = p4KS - p4piz;
  
  const double z = (q.get(0)*q.get(0) - q.get(1)*q.get(1) - q.get(2)*q.get(2) - q.get(3)*q.get(3))/MKS_Sq;
  
  // Calculate line shape
  const EvtComplex line_shape = GF*MKS_Sq*Wpol_z(z, as, bs) + Wpipi_z(z, alpha_s, beta_s, rvsq, rpisq, z0);
  
  // Calculate spin
  const EvtVector4R mom_sum = p4KS + p4piz;
  
  EvtVector4C l11,l12,l21,l22;

  l11 = EvtLeptonVCurrent(p->getDaug(1)->spParent(0),
                          p->getDaug(2)->spParent(0));
  
  l21 = EvtLeptonVCurrent(p->getDaug(1)->spParent(0),
                          p->getDaug(2)->spParent(1));
  
  l12 = EvtLeptonVCurrent(p->getDaug(1)->spParent(1),
                          p->getDaug(2)->spParent(0));
  
  l22 = EvtLeptonVCurrent(p->getDaug(1)->spParent(1),
                          p->getDaug(2)->spParent(1));

  vertex(0,0,mom_sum*l11*line_shape);
  vertex(0,1,mom_sum*l12*line_shape);
  vertex(1,0,mom_sum*l21*line_shape);
  vertex(1,1,mom_sum*l22*line_shape);
  
}

double EvtKStopizmumu::F_z( const double& z, const double& rvsq )
{
    double F_z = 1.0 + (z/rvsq);
    return F_z;
}

EvtComplex EvtKStopizmumu::G_z( const double& z )
{
    EvtComplex G_z;
    
    if (z <= 4.0) {
        G_z = sqrt((4.0/z) - 1.0)*asin(sqrt(z)/2.0);
    } else {
	double z4 = 4.0/z;
        G_z = -0.5*sqrt(1.0 - z4)*(log((1.0 - sqrt(1.0 - z4))/(1.0 + sqrt(1.0 + z4))) +
				   EvtComplex(0, EvtConst::pi));
    }

    return G_z;
}

EvtComplex EvtKStopizmumu::chi_z( const double& z, const double& rpisq )
{
    double z_prime = z/rpisq;
    EvtComplex chi = 4.0/9.0 - 4.0*rpisq/(3.0*z) - (1.0 - 4.0*rpisq/z)*G_z(z_prime)/3.0;
    return chi;
}

double EvtKStopizmumu::Wpol_z( const double& z, const double& as, const double& bs )
{
    double Wpol = (as + bs*z);
    return Wpol;
}

EvtComplex EvtKStopizmumu::Wpipi_z( const double& z, const double& alpha_s, const double& beta_s, 
				    const double& rvsq, const double& rpisq, const double& z0 )
{
    EvtComplex Wpipi = (alpha_s + beta_s*(z-z0)/rpisq)*F_z(z, rvsq)*chi_z(z, rpisq)/rpisq;
    return Wpipi;
}
