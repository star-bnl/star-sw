//--------------------------------------------------------------------------
//
// Environment:
//      This software is part of the EvtGen package developed jointly
//      for the BaBar and CLEO collaborations.  If you use all or part
//      of it, please give an appropriate acknowledgement.
//
// Copyright Information: See EvtGen/COPYRIGHT
//      Copyright (C) 1998      Caltech, UCSB
//
// Module: EvtGen/EvtDecayAmp.cc
//
// Description: Baseclass for models that calculates amplitudes
//
// Modification history:
//
//    DJL/RYD     August 11, 1998         Module created
//
//------------------------------------------------------------------------
#include "EvtGenBase/EvtPatches.hh"



#include "EvtGenBase/EvtDecayBase.hh"
#include "EvtGenBase/EvtDecayAmp.hh"
#include "EvtGenBase/EvtParticle.hh"
#include "EvtGenBase/EvtPDL.hh"
#include "EvtGenBase/EvtRandom.hh"
#include "EvtGenBase/EvtRadCorr.hh"
#include "EvtGenBase/EvtAmp.hh"
#include "EvtGenBase/EvtReport.hh"
using std::endl;


void EvtDecayAmp::makeDecay(EvtParticle* p, bool recursive){

  //original default value
  int ntimes=10000;
  
  int more;

  EvtSpinDensity rho;
  double prob,prob_max;

  _amp2.init(p->getId(),getNDaug(),getDaugs());

  do{

    _daugsDecayedByParentModel=false;
    _weight = 1.0;
    decay(p);

    rho=_amp2.getSpinDensity();

    prob=p->getSpinDensityForward().normalizedProb(rho);

    if (prob<0.0) {
      EvtGenReport(EVTGEN_ERROR,"EvtGen")<<"Negative prob:"<<p->getId().getId()
			    <<" "<<p->getChannel()<<endl;

      EvtGenReport(EVTGEN_ERROR,"EvtGen") << "rho_forward:"<<endl;
      EvtGenReport(EVTGEN_ERROR,"EvtGen") << p->getSpinDensityForward();
      EvtGenReport(EVTGEN_ERROR,"EvtGen") << "rho decay:"<<endl;
      EvtGenReport(EVTGEN_ERROR,"EvtGen") << rho <<endl;
    }

    if (prob!=prob) {

      EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "Forward density matrix:"<<endl;
      EvtGenReport(EVTGEN_DEBUG,"EvtGen") << p->getSpinDensityForward();

      EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "Decay density matrix:"<<endl;
      EvtGenReport(EVTGEN_DEBUG,"EvtGen") << rho;

      EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "prob:"<<prob<<endl;
      
      EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "Particle:"
			     <<EvtPDL::name(p->getId()).c_str()<<endl;
      EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "channel        :"<<p->getChannel()<<endl;
      EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "Momentum:" << p->getP4() << " " << p->mass() << endl;
      if( p->getParent()!=0){
	EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "parent:"
			       <<EvtPDL::name(
				p->getParent()->getId()).c_str()<<endl;
	EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "parent channel        :"
			       <<p->getParent()->getChannel()<<endl;

        size_t i;
	EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "parent daughters  :";
        for (i=0;i<p->getParent()->getNDaug();i++){
	  EvtGenReport(EVTGEN_DEBUG,"") << EvtPDL::name(
			    p->getParent()->getDaug(i)->getId()).c_str()
				 << " ";
        }
	EvtGenReport(EVTGEN_DEBUG,"") << endl;

	EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "daughters  :";
        for (size_t i=0;i<p->getNDaug();i++){
	  EvtGenReport(EVTGEN_DEBUG,"") << EvtPDL::name(
			    p->getDaug(i)->getId()).c_str()
				 << " ";
        }
	EvtGenReport(EVTGEN_DEBUG,"") << endl;

	EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "daughter momenta  :" << endl;;
        for (size_t i=0;i<p->getNDaug();i++){
	  EvtGenReport(EVTGEN_DEBUG,"") << p->getDaug(i)->getP4() << " " << p->getDaug(i)->mass();
	  EvtGenReport(EVTGEN_DEBUG,"") << endl;
        }

      }
    }


    prob/=_weight;

    prob_max = getProbMax(prob);
    p->setDecayProb(prob/prob_max);

    more=prob<EvtRandom::Flat(prob_max);
    
    ntimes--;

  }while(ntimes&&more);

  if (ntimes==0){
    EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "Tried accept/reject: 10000" 
			   <<" times, and rejected all the times!"<<endl;
   
    EvtGenReport(EVTGEN_DEBUG,"EvtGen")<<p->getSpinDensityForward()<<endl;
    EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "Is therefore accepting the last event!"<<endl;
    EvtGenReport(EVTGEN_DEBUG,"EvtGen") << "Decay of particle:"<<
      EvtPDL::name(p->getId()).c_str()<<"(channel:"<<
      p->getChannel()<<") with mass "<<p->mass()<<endl;
    
    for(size_t ii=0;ii<p->getNDaug();ii++){
      EvtGenReport(EVTGEN_DEBUG,"EvtGen") <<"Daughter "<<ii<<":"<<
	EvtPDL::name(p->getDaug(ii)->getId()).c_str()<<" with mass "<<
	p->getDaug(ii)->mass()<<endl;
    }				   
  }

  EvtSpinDensity rho_list[10];

  rho_list[0]=p->getSpinDensityForward();

  EvtAmp ampcont;

  if (_amp2._pstates!=1){
    ampcont=_amp2.contract(0,p->getSpinDensityForward());
  }
  else{
    ampcont=_amp2;
  }


  // it may be that the parent decay model has already
  // done the decay - this should be rare and the
  // model better know what it is doing..
  
  if ( !daugsDecayedByParentModel() ){

    if(recursive) {   
    
      for(size_t i=0;i<p->getNDaug();i++){

	rho.setDim(_amp2.dstates[i]);
      
	if (_amp2.dstates[i]==1) {
	  rho.set(0,0,EvtComplex(1.0,0.0));
	}
	else{
	  rho=ampcont.contract(_amp2._dnontrivial[i],_amp2);
	}
	
	if (!rho.check()) {
	  
	  EvtGenReport(EVTGEN_ERROR,"EvtGen") << "-------start error-------"<<endl;
	  EvtGenReport(EVTGEN_ERROR,"EvtGen")<<"forward rho failed Check:"<<
	    EvtPDL::name(p->getId()).c_str()<<" "<<p->getChannel()<<" "<<i<<endl;
	  
	  p->printTree();

	  for (size_t idaug = 0; idaug < p->getNDaug(); idaug++) {
	    EvtParticle* daughter = p->getDaug(idaug);
	    if (daughter != 0) {daughter->printTree();}
	  }

	  EvtParticle* pParent = p->getParent();
	  if (pParent != 0) {
	    EvtGenReport(EVTGEN_ERROR,"EvtGen")<<"Parent:"<<EvtPDL::name(pParent->getId()).c_str()<<endl;

	    EvtParticle* grandParent = pParent->getParent();

	    if (grandParent != 0) {
	      EvtGenReport(EVTGEN_ERROR,"EvtGen")<<"GrandParent:"<<EvtPDL::name(grandParent->getId()).c_str()<<endl;
	    }
	  }

	  EvtGenReport(EVTGEN_ERROR,"EvtGen") << " EvtSpinDensity rho: " << rho;
	  
	  _amp2.dump();

	  for(size_t ii=0;ii<i+1;ii++){
	    EvtGenReport(EVTGEN_ERROR,"EvtGen") << "rho_list[" << ii << "] = " << rho_list[ii];
	  }

	  EvtGenReport(EVTGEN_ERROR,"EvtGen") << "-------Done with error-------"<<endl;  

	}
      
	p->getDaug(i)->setSpinDensityForward(rho);
	p->getDaug(i)->decay();
	
	rho_list[i+1]=p->getDaug(i)->getSpinDensityBackward();
	
	if (_amp2.dstates[i]!=1){
	ampcont=ampcont.contract(_amp2._dnontrivial[i],rho_list[i+1]);
	}
      
	
      }
      
      p->setSpinDensityBackward(_amp2.getBackwardSpinDensity(rho_list));
      
      
      if (!p->getSpinDensityBackward().check()) {
	
	EvtGenReport(EVTGEN_ERROR,"EvtGen")<<"rho_backward failed Check"<<
	  p->getId().getId()<<" "<<p->getChannel()<<endl;
      
	EvtGenReport(EVTGEN_ERROR,"EvtGen") << p->getSpinDensityBackward();
      
      }
    }    
  }


  if (getPHOTOS() || EvtRadCorr::alwaysRadCorr()) {
    int n_daug_orig=p->getNDaug();
    EvtRadCorr::doRadCorr(p);
    int n_daug_new=p->getNDaug();
    for (int i=n_daug_orig;i<n_daug_new;i++){
      p->getDaug(i)->decay();
    }
  }

}










