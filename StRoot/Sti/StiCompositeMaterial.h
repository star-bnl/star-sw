/*! \class StiCompositeMaterial
  A the StiCompositeMaterial provides a simple way of using heterogeneous
  materials.  The density, radiation length, A, Z, etc. are averaged
  correctly given the weight of the individual component materials.

  Note:  One must be careful in interpreting the ionization potential
  average.  The average will be meaningless if individual atomic components
  of chemical compounds are added using StiCompositeMaterial::add.  One
  must enter complete compounds as a single entry (with the correct
  ionization potential) for averaging to be accurate.  Obviously, the
  bonding energy of each chemical compound will make the total ionization
  energy different than the simple average of the component atoms.  I.e.,
  if you want the right ionization potential for P10 gas, you must enter
  Methane and Argon as components, not Carbon, Hydrogen, and Argon.  The
  former will allow correct averaging of the ionization potential, where the
  latter will not.

  Similarly, the radiation length & density will not be correct for
  composite materials which are chemically bonded.  Effective A & Z will
  be correct, however.  Fortunately, ionization potential, density, and 
  radiation length are readily avaliable in tables for common chemical
  compounds.

  \author Ben Norman
  \date   14 Nov 02
*/

#ifndef STI_COMPOSITE_MATERIAL_H
#define STI_COMPOSITE_MATERIAL_H

#include <vector>
using std::vector;
#include <utility>
using std::pair;

#include "StiMaterial.h"

class StiCompositeMaterial: public StiMaterial {

public:

    /// First weight is by number proportion,
    /// second is by mass proportion
    typedef pair<double, double>         Weight_t;
    typedef pair<StiMaterial*, Weight_t> WeightedMaterial_t;
    typedef vector<WeightedMaterial_t>   vWeightedMaterial_t;

    /// default constructor
    StiCompositeMaterial();
    
    /// destructor
    ~StiCompositeMaterial();

    /// Add a new material with the given weight.  The sum of the weights
    /// is arbitrary; it does not have to be 1.  
    /// Weighting is either done by number or by mass,
    /// depending on the method called.  After adding the
    /// new component material, the composite's characteristics are
    /// recomputed.
    void addByNumber(StiMaterial *pMaterial, double dNumberWeight);
    void addByMass(StiMaterial *pMaterial, double dMassWeight);

protected:

    /// Updates the values of the composite based on the components.
    void update();
   
    /// Adds a material once the number & mass weights are known.
    void add(StiMaterial *pMaterial, double dMassWeight, double dNumberWeight);

    /// total mass weight
    double m_dMassTotal;
    /// total number weight
    double m_dNumberTotal;
    
    vWeightedMaterial_t m_vMaterials;
};

#endif
