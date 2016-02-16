//----------------------------------------------------------------------------
// Implementation of the KFParticle class
// .
// @author  M.Zyzak
// @version 1.0
// @since   06.01.16
// 
// 
//  -= Copyright &copy ALICE HLT and CBM L1 Groups =-
//____________________________________________________________________________

#include "KFPHistogramSet.h"
#include "KFPartEfficiencies.h"

KFPHistogramSet::KFPHistogramSet(int iPart)
{
  KFPartEfficiencies fParteff;
  std::string parName[NHisto1D] = {"M","p","p_{t}","y","DecayL","c#tau","chi2ndf","prob","#theta","phi","X","Y","Z","R", "L", "l/dl","Multiplicity"};
#ifdef CBM
  int nBins[NHisto1D] = {1000, // M
                          100, // p
                          100, // pt
                          100, // y
                          100, // DecayL
                          100, // ctau
                          100, // chi2/ndf
                          100, // prob
                          100, // theta
                          100, // phi
                          200, // X
                          200, // Y
                          360, // Z
                          200, // R
                          200, // L
                          200, // L/dL
                        fParteff.partMaxMult[iPart]+1};
  float xMin[NHisto1D] = { fParteff.partMHistoMin[iPart], // M
                            0.f, // p
                            0.f, // pt
                            0.f, // y
                            -5.f, // DecayL
                            0.f, // ctau
                            0.f, // chi2/ndf
                            0.f, // prob
                            -2.f, // theta
                            -2.f, // phi
                          -50.f, // X
                          -50.f, // Y
                          -10.f, // Z
                            0.f, // R
                            0.f, // L
                            -1.f, // L/dL
                            -0.5f };
  float xMax[NHisto1D] = { fParteff.partMHistoMax[iPart], // M
                            10.f, // p
                              3.f, // pt
                              6.f, // y
                            55.f, // DecayL
                            30.f, // ctau
                            20.f, // chi2/ndf
                              1.f, // prob
                              2.f, // theta
                              2.f, // phi
                            50.f, // X
                            50.f, // Y
                            80.f, // Z
                            50.f, // R
                            100.f, // L
                            35.f, // L/dL
                          float(fParteff.partMaxMult[iPart])+0.5f};
#else
  int nBins[NHisto1D] = {1000, // M
                          100, // p
                          100, // pt
                          100, // y
                          100, // DecayL
                          100, // ctau
                          100, // chi2/ndf
                          100, // prob
                          100, // theta
                          100, // phi
                          100, // X
                        1000, // Y
                        1000, // Z
                        1000, // R
                        1000, // L
                        1000, // L/dL
                        fParteff.partMaxMult[iPart]+1};
  float xMin[NHisto1D] = { fParteff.partMHistoMin[iPart], // M
                            0.f, // p
                            0.f, // pt
                            -6.f, // y
                            -5.f, // DecayL
                            0.f, // ctau
                            0.f, // chi2/ndf
                            0.f, // prob
                            -2.f, // theta
                            -2.f, // phi
                          -200.f, // X
                          -200.f, // Y
                          -200.f, // Z
                            0.f, // R
                            0.f, // L
                            -1.f, // L/dL
                            -0.5f };
  float xMax[NHisto1D] = { fParteff.partMHistoMax[iPart], // M
                            10.f, // p
                              3.f, // pt
                              6.f, // y
                            55.f, // DecayL
                            30.f, // ctau
                            20.f, // chi2/ndf
                              1.f, // prob
                              2.f, // theta
                              2.f, // phi
                            200.f, // X
                            200.f, // Y
                            200.f, // Z
                            200.f, // R
                            400.f, // L
                            35.f, // L/dL
                          float(fParteff.partMaxMult[iPart])+0.5f};
#endif
  for(int iHisto=0; iHisto<NHisto1D; iHisto++)
    fKFPHistogram1D[iHisto] = KFPHistogram1D(parName[iHisto], nBins[iHisto], xMin[iHisto], xMax[iHisto]);
}

void KFPHistogramSet::SetHistogramMemory(int* pointer)
{
  for(int i=0; i<NHisto1D; i++)
  {
    fKFPHistogram1D[i].SetHistogramMemory(pointer);
    pointer += fKFPHistogram1D[i].DataSize();
  }
}

void KFPHistogramSet::Fill(const KFParticle& particle)
{  
  float mass = 0, p=0, pt=0, err = 0;
  particle.GetMass(mass, err);
  particle.GetMomentum(p, err);
  particle.GetPt(pt, err);
  float rapidity = particle.GetRapidity();
  float chi2ndf  = particle.GetChi2()/float(particle.GetNDF());
  float r        = sqrt(particle.X()*particle.X() + particle.Y()*particle.Y());
  float l        = sqrt(particle.X()*particle.X() + particle.Y()*particle.Y() + particle.Z()*particle.Z());
  
  fKFPHistogram1D[0].Fill(mass);
  fKFPHistogram1D[1].Fill(p);
  fKFPHistogram1D[2].Fill(pt);
  fKFPHistogram1D[3].Fill(rapidity);
  fKFPHistogram1D[6].Fill(chi2ndf);
  fKFPHistogram1D[10].Fill(particle.X());
  fKFPHistogram1D[11].Fill(particle.Y());
  fKFPHistogram1D[12].Fill(particle.Z());
  fKFPHistogram1D[13].Fill(r);
  fKFPHistogram1D[14].Fill(l);
}
