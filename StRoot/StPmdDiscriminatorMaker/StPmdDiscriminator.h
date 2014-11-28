/*!
 * \class StPmdDiscriminator
 * \author
 */
/******************************************************************
 *
 * $Id: StPmdDiscriminator.h,v 1.2 2003/05/29 13:12:51 subhasis Exp $
 *
 * Author: Subhasis Chattopadhyay
 *
 ******************************************************************
 *
 * Description: 
 ******************************************************************
 *
 * $Log: StPmdDiscriminator.h,v $
 * Revision 1.2  2003/05/29 13:12:51  subhasis
 * several changes to include NN
 *
 ******************************************************************/

#ifndef STAR_StPmdDiscriminator
#define STAR_StPmdDiscriminator
#include <TH2.h>
#include <TH1.h>
#include <TCanvas.h>


class StPmdDetector;
class StPmdDiscriminator{

  private:
  Int_t m_photonlike;
	  Float_t mEdepThreshold;
	  Float_t mEdepcut;

	  StPmdDetector* m_PmdDet;
	  StPmdDetector* m_CpvDet;
  protected:
  
   public: 
  StPmdDiscriminator(Float_t, StPmdDetector*, StPmdDetector*); 
  virtual ~StPmdDiscriminator();

  virtual void Discriminate();
  virtual void Print();
  // virtual void Matching();
  virtual void SetEdepcut(Float_t);

  void getClusterPID();
  void  bookHistograms();
  void  Browse(TBrowser* b); 

  
  ClassDef(StPmdDiscriminator, 1) 
};

inline void StPmdDiscriminator::SetEdepcut(Float_t cut){mEdepcut=cut;}


#endif

















