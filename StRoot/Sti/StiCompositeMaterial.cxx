#include "StiCompositeMaterial.h"

StiCompositeMaterial::StiCompositeMaterial():m_dMassTotal(0),m_dNumberTotal(0){
} // StiCompositeMaterial

StiCompositeMaterial::~StiCompositeMaterial(){
} // ~StiCompositeMaterial

void StiCompositeMaterial::addByNumber(StiMaterial *pMaterial, 
                                       double dNumberWeight){

  double dMassWeight = dNumberWeight * pMaterial->getA();
  add(pMaterial, dMassWeight, dNumberWeight);

} // addByNumber

void StiCompositeMaterial::addByMass(StiMaterial *pMaterial, 
                                     double dMassWeight){

  double dNumberWeight = dMassWeight / pMaterial->getA();
  add(pMaterial, dMassWeight, dNumberWeight);

} // addByMass

void StiCompositeMaterial::add(StiMaterial *pMaterial, double dMassWeight,
                               double dNumberWeight){
  
  Weight_t weight(dNumberWeight, dMassWeight);
  WeightedMaterial_t material(pMaterial, weight);
  m_vMaterials.push_back(material);
  
  m_dMassTotal   += dMassWeight;
  m_dNumberTotal += dNumberWeight;
  
  update();

} // add

void StiCompositeMaterial::update(){

  _density    = 0.;   // will invert later
  _radLength  = 0.;   // will invert later
  _a          = 0.;
  _z          = 0.;
  _ionization = 0.;

  vWeightedMaterial_t::iterator pMaterialIterator;
  StiMaterial *pMaterial = 0;
  Weight_t weight;
  double dNumberFraction, dMassFraction;
  for(pMaterialIterator  = m_vMaterials.begin(); 
      pMaterialIterator != m_vMaterials.end(); 
      pMaterialIterator++){
    
    pMaterial = pMaterialIterator->first;
    weight    = pMaterialIterator->second;
    dNumberFraction = weight.first / m_dNumberTotal;  // n_i/N
    dMassFraction   = weight.second / m_dMassTotal;   // m_i/M

    _density    += dMassFraction / pMaterial->getDensity();
    _radLength  += dMassFraction / pMaterial->getRadLength();
    _a          += dNumberFraction * pMaterial->getA();
    _z          += dNumberFraction * pMaterial->getZ();
    _ionization += dNumberFraction * pMaterial->getIonization();
  }

  // fix inverted averages
  _density   = 1./_density;
  _radLength = 1./_radLength;

} // update
