/*!
 * \class StPmdDiscriminatorNN
 * \author
 */
/******************************************************************
 *
 * $Id: StPmdDiscriminatorNN.h,v 1.1 2003/05/29 13:20:53 subhasis Exp $
 *
 * Author: Subhasis Chattopadhyay
 *
 ******************************************************************
 *
 * Description: 
 ******************************************************************
 *
 * $Log: StPmdDiscriminatorNN.h,v $
 * Revision 1.1  2003/05/29 13:20:53  subhasis
 * NN discriminator
 *
 ******************************************************************/

#ifndef STAR_StPmdDiscriminatorNN
#define STAR_StPmdDiscriminatorNN
#include <TH2.h>
#include <TH1.h>
#include <TCanvas.h>
//For CC5 compatibility
#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif
//
#ifdef ST_NO_TEMPLATE_DEF_ARGS
//// Syntax currently required by Solaris compiler
#define StVector(T) vector<T, allocator<T> >
#else
#define StVector(T) vector<T>
#endif
//

class StPmdDetector;
class StPmdCluster;
class StPhmdCluster;
class StPmdNeuNet;
class StNNCluster;
typedef StVector(StNNCluster*) StPmdCl;

class StPmdDiscriminatorNN{

  private:
	  Int_t mApplyFlagNN;
	  StPmdCl mClContainer;
	  StPmdDetector* m_PmdDet;
	  StPmdDetector* m_CpvDet;
	  StPmdNeuNet* m_name1;
	  StPmdNeuNet* m_input1;
	  StPmdNeuNet* m_name2;
	  StPmdNeuNet* m_output1;
	  StPmdDiscriminatorMaker* m_commedeppmd;
	  StPmdDiscriminatorMaker* m_commedepcpv;
	  StPmdDiscriminatorMaker* m_DiscMaker;
  protected:
 TH1F * m_NNedep_ph; 
 TH1F * m_NNncell_ph; 
 TH1F * m_NNsigma_ph; 
 TH1F * m_NNedep_cpv_ph; 
 TH1F * m_NNedep_had; 
 TH1F * m_NNncell_had; 
 TH1F * m_NNsigma_had; 
 TH1F * m_NNedep_cpv_had; 

  public: 
  StPmdDiscriminatorNN(StPmdCl); 
  StPmdDiscriminatorNN(StPmdDetector*, StPmdDetector*); 
  StPmdDiscriminatorNN(StPmdDiscriminatorMaker*, StPmdDiscriminatorMaker*); 
  virtual ~StPmdDiscriminatorNN();

  void  Input(StPmdNeuNet*);  
  void  bookHistograms();
  void  Browse(TBrowser* b); 
  void fill();	
  void forward();
  void setFormula();
  void Discriminate();
  void setApplyFlag(Int_t);
  void setDisMaker(StPmdDiscriminatorMaker*);
  Float_t InputRange(Float_t,Float_t,Float_t&);


  
  ClassDef(StPmdDiscriminatorNN, 1) 
};

inline void StPmdDiscriminatorNN::setApplyFlag(Int_t flag){mApplyFlagNN=flag;}
inline void StPmdDiscriminatorNN::setDisMaker(StPmdDiscriminatorMaker* disc){m_DiscMaker=disc;}


#endif

















