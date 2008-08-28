/*******************************************************************
 *
 * $Id: StTofSimParam.h,v 1.2 2008/08/28 18:43:20 dongx Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Simulation Paramers for TOFp/TOFr/TOF
 *
 *****************************************************************
 *
 * $Log: StTofSimParam.h,v $
 * Revision 1.2  2008/08/28 18:43:20  dongx
 * Added MRPC-TOF simulation parameters
 * Added TOF/VPD fast simulation parameters
 *
 * Revision 1.1  2001/09/28 19:09:40  llope
 * first version
 *
 *******************************************************************/
#ifndef STTOFSIMPARAM_H
#define STTOFSIMPARAM

class StTofSimParam{
 private:
 /// TOFp parameters
  float m_adc_overflow       ;//! adc overflow                                      fg. not used
  float m_amax_noise         ;//! Maximum noise amplitude                           fg. not used
  float m_amin_noise         ;//! Minimum noise amplitude                           fg. not used
  float m_attlen             ;//! Attenuation length				
  float m_cath_eff           ;//! Cathode efficiency				
  float m_cath_surf          ;//! Ratio scintillator/pm surfaces 		
  float m_delay              ;//! scintillation light propagation delay		
  float m_elec_noise         ;//! Fraction slats producing electronic noise         fg. not used
  float m_gate_t0            ;//! Time to start integration gate                    fg. not used
  float m_gate_width         ;//! Time width integration gate of ADC                fg. not used
  float m_geo_from_geant     ;//! =1 gets geo from geant, otherwise geometry dbase  fg. not used
  float m_GeV_2_n_photons    ;//! Conversion factor from GeV to photons
  float m_nphe_to_adc        ;//! Nphe to ADC conversion
  float m_phys_noise         ;//! Fraction slats producing physics noise            fg. not used
  float m_position_tolerance ;//! NOT NEEDED IF GEOMETRY IS CORRECT                 fg. not used
  float m_slat_para          ;//! =0 for exponential attenuation, =1 for response table
  float m_start_res          ;//! Start time resolution
  float m_surf_loss          ;//! Surface losses
  float m_tdc_overflow       ;//! tdc overflow                                      fg. not used
  float m_time_res           ;//! Slat assembly time resolution

/// MRPC-TOF parameters
  int   m_n_gap              ;//! number of gaps
  int   m_nmax_clus          ;//! max number of clusters
  float m_d_in               ;//! Inner glass thickness
  float m_d_out              ;//! Outer glass thickness
  float m_d_gap              ;//! Gas gap thickness
  float m_alpha_dg           ;//! alpha * m_d_gap
  float m_er                 ;//! yupsilon
  float m_clus_par[4]        ;//! parameters used for number of cluster calculation
  float m_vd_mean            ;//! mean drift v calculation
  float m_nmean_e            ;//! mean number of electrons in a primary cluster
  float m_nmax_e             ;//! saturation number of electrons
  float m_dt                 ;//! delta t interval for slow simuation
  int   m_ndt                ;//! number of dt bin in slow simulation
  float m_toffset            ;//! A relative time for slow simulation
  float m_adc_thre           ;//! ADC threshold
  float m_xtalk_dy           ;//! x-talk boundary
  float m_res_fee            ;//! electron resolution in slow simulation

/// fast simulation parameters  
  float m_res_tof            ;//! total stop resolution in fast simulation
  float m_thre_tof           ;//! dE threshold in fast simulation for stop side
  float m_res_vpd            ;//! vpd res per channel in fast simulation
  float m_thre_vpd           ;//! dE threshold in fast simulation for vpd
  
/// efficiency
  float m_eff_tof[120][192]  ;//! efficiency for each MRPC channel
  float m_eff_vpd[50]        ;//! efficiency for each vpd tube
  
  /// simulator switches
  bool  mSimuTofp            ;//! Tofp simulator
  bool  mSimuVpd             ;//! VPD simulator
  bool  mSimuTofFast         ;//! Fast simulator for MRPC-TOF
  bool  mSimuTofSlow         ;//! Slow simulator for MRPC-TOF
    
 public:
  StTofSimParam();
  ~StTofSimParam();
  void init();
  void print();

  float adc_overflow()       const;
  float attlen()             const;
  float cath_eff()           const;
  float cath_surf()          const;
  float delay()              const;
  float elec_noise()         const;
  float gate_t0()            const;
  float gate_width()         const;
  float geo_from_geant()     const;
  float GeV_2_n_photons()    const;
  float nphe_to_adc()        const;
  float phys_noise()         const;
  float position_tolerance() const;
  float slat_para()          const;
  float start_res()          const;
  float surf_loss()          const;
  float tdc_overflow()       const;
  float time_res()           const;
  
  int   ngap()               const;
  int   nmaxclus()           const;
  float din()                const;
  float dout()               const;
  float dg()                 const;
  float alpha()              const;
  float er()                 const;
  float ka()                 const;
  float nclus(float beta)    const;
  float vd_mean()            const;
  float nmeane()             const;
  float nmaxe()              const;
  float dt()                 const;
  int   ndt()                const;
  float toffset()            const;
  float adc_thre()           const;
  float dy_xtalk()           const;
  float timeres_fee()        const;
  
  float timeres_tof()        const;
  float thre_tof()           const;
  float timeres_vpd()        const;
  float thre_vpd()           const;
  
  float eff_tof(int itray, int imodule, int icell) const;
  float eff_vpd(int itray, int itube) const;
  
  bool  simuTofp()           const;
  bool  simuVpd()            const;
  bool  simuTofFast()        const;
  bool  simuTofSlow()        const;
  
  void  setSimuTofp(bool val)     ;
  void  setSimuVpd(bool val)      ;
  void  setSimuTofFast(bool val)  ;
  void  setSimuTofSlow(bool val)  ;
    
};

inline float StTofSimParam::adc_overflow()       const {return m_adc_overflow;}
inline float StTofSimParam::attlen()             const {return m_attlen;}
inline float StTofSimParam::cath_eff()           const {return m_cath_eff;}
inline float StTofSimParam::cath_surf()          const {return m_cath_surf;}
inline float StTofSimParam::delay()              const {return m_delay;}
inline float StTofSimParam::elec_noise()         const {return m_elec_noise;}
inline float StTofSimParam::gate_t0()            const {return m_gate_t0;}
inline float StTofSimParam::gate_width()         const {return m_gate_width;}
inline float StTofSimParam::geo_from_geant()     const {return m_geo_from_geant;}
inline float StTofSimParam::GeV_2_n_photons()    const {return m_GeV_2_n_photons;}
inline float StTofSimParam::nphe_to_adc()        const {return m_nphe_to_adc;}
inline float StTofSimParam::phys_noise()         const {return m_phys_noise;}
inline float StTofSimParam::position_tolerance() const {return m_position_tolerance;}
inline float StTofSimParam::slat_para()          const {return m_slat_para;}
inline float StTofSimParam::start_res()          const {return m_start_res;}
inline float StTofSimParam::surf_loss()          const {return m_surf_loss;}
inline float StTofSimParam::tdc_overflow()       const {return m_tdc_overflow;}
inline float StTofSimParam::time_res()           const {return m_time_res;}

inline int StTofSimParam::ngap()                 const {return m_n_gap;}
inline int StTofSimParam::nmaxclus()             const {return m_nmax_clus;}
inline float StTofSimParam::din()                const {return m_d_in;}
inline float StTofSimParam::dout()               const {return m_d_out;}
inline float StTofSimParam::dg()                 const {return m_d_gap;}
inline float StTofSimParam::alpha()              const {return m_alpha_dg/m_d_gap;}
inline float StTofSimParam::er()                 const {return m_er;}
inline float StTofSimParam::ka()                 const {return m_er*m_d_gap/(m_n_gap*m_er*m_d_gap+(m_n_gap-1)*m_d_in+2*m_d_out);}
inline float StTofSimParam::vd_mean()            const {return m_vd_mean;}
inline float StTofSimParam::nmeane()             const {return m_nmean_e;}
inline float StTofSimParam::nmaxe()              const {return m_nmax_e;}
inline float StTofSimParam::dt()                 const {return m_dt;}
inline int StTofSimParam::ndt()                  const {return m_ndt;}
inline float StTofSimParam::toffset()            const {return m_toffset;}
inline float StTofSimParam::adc_thre()           const {return m_adc_thre;}
inline float StTofSimParam::dy_xtalk()           const {return m_xtalk_dy;}
inline float StTofSimParam::timeres_fee()        const {return m_res_fee;}
inline float StTofSimParam::timeres_tof()        const {return m_res_tof;}
inline float StTofSimParam::thre_tof()           const {return m_thre_tof;}
inline float StTofSimParam::timeres_vpd()        const {return m_res_vpd;}
inline float StTofSimParam::thre_vpd()           const {return m_thre_vpd;}
inline float StTofSimParam::eff_tof(int itray, int imodule, int icell) const {return m_eff_tof[itray-1][(imodule-1)*6+(icell-1)];}
inline float StTofSimParam::eff_vpd(int itray, int itube) const {return m_eff_vpd[(itray-121)*19+itube-1];}

inline bool StTofSimParam::simuTofp()    const {return mSimuTofp;}
inline bool StTofSimParam::simuVpd()     const {return mSimuVpd;}
inline bool StTofSimParam::simuTofFast() const {return mSimuTofFast;}
inline bool StTofSimParam::simuTofSlow() const {return mSimuTofSlow;}

inline void StTofSimParam::setSimuTofp(bool val)    {mSimuTofp=val;}
inline void StTofSimParam::setSimuVpd(bool val)     {mSimuVpd=val;}
inline void StTofSimParam::setSimuTofFast(bool val) {mSimuTofFast=val;}
inline void StTofSimParam::setSimuTofSlow(bool val) {mSimuTofSlow=val;}

#endif
