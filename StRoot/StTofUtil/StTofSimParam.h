/*******************************************************************
 *
 * $Id: StTofSimParam.h,v 1.1 2001/09/28 19:09:40 llope Exp $
 *
 * Author: Frank Geurts
 *****************************************************************
 *
 * Description: Simulation Paramers for TOFp
 *
 *****************************************************************
 *
 * $Log: StTofSimParam.h,v $
 * Revision 1.1  2001/09/28 19:09:40  llope
 * first version
 *
 *******************************************************************/
#ifndef STTOFSIMPARAM_H
#define STTOFSIMPARAM

class StTofSimParam{
 private:
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
#endif
