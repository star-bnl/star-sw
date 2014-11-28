#ifndef __SpinCutsIU_h__
#define __SpinCutsIU_h__
#include "TObject.h" 
#include "StEEmcIUPair.h" 
class SpinCutsIU : public TObject
{
    public:
	SpinCutsIU();
	~SpinCutsIU(){ /* nada */ } 
	void setVertexCut(Float_t min,Float_t max); 
	void setZggCut(Float_t min,Float_t max); 
	void setTowerCut( Float_t minEt ); 
	//void setTowerFiducial( Float_t deta, Float_t dphi ); 
	Float_t z_vertex_min;
	Float_t z_vertex_max; 
	Float_t zgg_min;
	Float_t zgg_max; 
	Int_t   adc_cut; 
	Float_t tower_et_cut;
	//Float_t tower_deta;
	//Float_t tower_dphi; 
	Float_t eta_min;
	Float_t eta_max; 
	Bool_t operator()( StEEmcIUPair &pair );  
    private:
    protected:
	ClassDef(SpinCutsIU,1); 
};
inline void SpinCutsIU::setVertexCut(Float_t min,Float_t max){ z_vertex_min=min; z_vertex_max=max; } 
inline void SpinCutsIU::setZggCut(Float_t min,Float_t max){ zgg_min=min; zgg_max=max; } 
inline void SpinCutsIU::setTowerCut( Float_t minEt ){ tower_et_cut=minEt; } 
//inline void SpinCutsIU::setTowerFiducial( Float_t deta, Float_t dphi ){ tower_deta=deta; tower_dphi=dphi; } 
#endif
