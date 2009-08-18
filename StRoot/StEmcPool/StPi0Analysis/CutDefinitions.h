#define INCLUDE_ALL_CUTS
#ifdef INCLUDE_EVENT_CUTS
#undef INCLUDE_ALL_CUTS
#endif
#ifdef INCLUDE_POINT_CUTS
#undef INCLUDE_ALL_CUTS
#endif
#ifdef INCLUDE_CANDIDATE_CUTS
#undef INCLUDE_ALL_CUTS
#endif
#ifdef INCLUDE_GAMMA_CUTS
#undef INCLUDE_ALL_CUTS
#endif
#ifdef INCLUDE_PION_CUTS
#undef INCLUDE_ALL_CUTS
#endif
#ifdef INCLUDE_ALL_CUTS
#define INCLUDE_EVENT_CUTS
#define INCLUDE_POINT_CUTS
#define INCLUDE_CANDIDATE_CUTS
#define INCLUDE_GAMMA_CUTS
#define INCLUDE_PION_CUTS
#endif

#ifdef INCLUDE_EVENT_CUTS
// Event cuts definition
DEFINE_CUT(EVENT, VALID,                        event.isValid(), "Valid event")
DEFINE_CUT(EVENT, MC_VALID,                     event.simulatedParticle.isValid(), "Valid MC particle")
DEFINE_CUT(EVENT, MC_VALID_DECAY,               event.simulatedParticle.daughters == (2 + (2 << 4)), "Valid MC 2gamma decay")
DEFINE_CUT(EVENT, Z,                            (eventParameters.zUse >= cutParameters.zCutLow) && (eventParameters.zUse < cutParameters.zCutHigh), "Z_{vertex} cut")
DEFINE_CUT(EVENT, TPC_VERTEX,                   eventParameters.zTPC != 0.0, "TPC Vertex found")
DEFINE_CUT(EVENT, BBC_VERTEX,                   (event.bbcEarliestWest != 255) && (event.bbcEarliestEast != 255), "BBC present")
DEFINE_CUT(EVENT, CORRUPTION,                   !event.corruptedCrates, "No corrupted headers in event")
DEFINE_CUT(EVENT, BAD_RUNS,                     !isBadRun(cutParameters.isBadRun, event.runId, eventParameters.year, eventParameters.day, eventParameters.runDay), "Bad runs rejected")
DEFINE_CUT(EVENT, MINBIAS,                      eventParameters.isMB, "Event is MinBias")
DEFINE_CUT(EVENT, HIGHTOWER1,                   eventParameters.isHT1, "Event is HighTower-1")
DEFINE_CUT(EVENT, HIGHTOWER2,                   eventParameters.isHT2, "Event is HighTower-2")
DEFINE_CUT(EVENT, HIGHTOWER1_SIMULATED,         eventParameters.isHT1Simulated, "Events satisfies HighTower-1 requirements")
DEFINE_CUT(EVENT, HIGHTOWER2_SIMULATED,         eventParameters.isHT2Simulated, "Events satisfies HighTower-2 requirements")
DEFINE_CUT(EVENT, FTPC_REFMULT,                 (eventParameters.uncorrectedNumberOfFtpcPrimaries >= cutParameters.ftpcRefMultLow) && (eventParameters.uncorrectedNumberOfFtpcPrimaries < cutParameters.ftpcRefMultHigh), "FTPC RefMult cut")
DEFINE_CUT(EVENT, TPC_REFMULT,                  (event.uncorrectedNumberOfTpcPrimaries >= cutParameters.tpcRefMultLow) && (event.uncorrectedNumberOfTpcPrimaries < cutParameters.tpcRefMultHigh), "TPC multiplicity cut")
DEFINE_CUT(EVENT, CLUSTERS_PER_TRACK,           (eventParameters.clustersPerTrack >= cutParameters.clustersPerTrackLow) && (eventParameters.clustersPerTrack < cutParameters.clustersPerTrackHigh), "Cut on the number of tower clusters per primary track")
DEFINE_CUT(EVENT, PT_TO_ET,                     (eventParameters.TPCPtToEMCEt >= cutParameters.TPCPtToEMCEtLow) && (eventParameters.TPCPtToEMCEt < cutParameters.TPCPtToEMCEtHigh), "Cut on the ratio TPC p_T / EMC E_T")
DEFINE_CUT(EVENT, PT_VS_ET,                     ((cutParameters.TPCPt0VsEMCEt0) + (event.totalTPCPt * cutParameters.TPCPt1VsEMCEt0) + (event.totalBEMCPointsEt * cutParameters.TPCPt0VsEMCEt1) + (event.totalTPCPt * event.totalBEMCPointsEt * cutParameters.TPCPt1VsEMCEt1) + (event.totalTPCPt * event.totalTPCPt * cutParameters.TPCPt2VsEMCEt0) + (event.totalBEMCPointsEt * event.totalBEMCPointsEt * cutParameters.TPCPt0VsEMCEt2)) > 0, "Cut on the TPC p_T vs EMC E_T")
DEFINE_CUT(EVENT, BEAM_BG_RUNS,                 !isBadRun_beambg(event.runId, eventParameters.year, eventParameters.day, eventParameters.runDay), "Runs with high beam bg")
DEFINE_CUT(EVENT, BX7_BG_RUNS,                  !isBadRun_bunchCrossingId7bit(event.runId, eventParameters.year, eventParameters.day, eventParameters.runDay), "Runs with high beam bg (from analysing bx7)")
DEFINE_CUT(EVENT, ABORT_GAP,                    !isGoodBunchCrossingId7bitPlusOffset(event.runId, eventParameters.year, eventParameters.day, eventParameters.runDay, eventParameters.bunchCrossingId7bitPlusOffset, true, false), "Event is from the abort gaps")
DEFINE_CUT(EVENT, BUNCH_EMPTY,                  !isGoodBunchCrossingId7bitPlusOffset(event.runId, eventParameters.year, eventParameters.day, eventParameters.runDay, eventParameters.bunchCrossingId7bitPlusOffset, false, true), "Event is from the empty bunches")
DEFINE_CUT(EVENT, NOBADBUNCH,                   isGoodBunchCrossingId7bitPlusOffset(event.runId, eventParameters.year, eventParameters.day, eventParameters.runDay, eventParameters.bunchCrossingId7bitPlusOffset, true, true), "Event is not from the empty bunches or abort gaps")
DEFINE_CUT(EVENT, HIGHTOWER1_ET,                eventParameters.isHT1SimulatedEt, "Events satisfies HighTower-1 Et requirements")
DEFINE_CUT(EVENT, HIGHTOWER2_ET,                eventParameters.isHT2SimulatedEt, "Events satisfies HighTower-2 Et requirements")
DEFINE_CUT(EVENT, JET_ET,                       (TMath::Abs(event.jet.eT) >= cutParameters.jetEtLow), "Events contains a jet")
DEFINE_CUT(EVENT, PT_VS_ET_TPC_VERTEX,          (event.zTPC == 0) || (((cutParameters.TPCPt0VsEMCEt0) + (event.totalTPCPt * cutParameters.TPCPt1VsEMCEt0) + (event.totalBEMCPointsEt * cutParameters.TPCPt0VsEMCEt1) + (event.totalTPCPt * event.totalBEMCPointsEt * cutParameters.TPCPt1VsEMCEt1) + (event.totalTPCPt * event.totalTPCPt * cutParameters.TPCPt2VsEMCEt0) + (event.totalBEMCPointsEt * event.totalBEMCPointsEt * cutParameters.TPCPt0VsEMCEt2)) > 0), "Cut on the TPC p_T vs EMC E_T (if TPC vertex was found)")
DEFINE_CUT(EVENT, JET_FOUND,                    (event.jet.eT > 0), "Jet finder found a jet")
DEFINE_CUT(EVENT, HIGHEST_ADC,                  (event.triggerSimulatedFinal.highestAdcHit.adc >= cutParameters.highestAdcLow) && (event.triggerSimulatedFinal.highestAdcHit.adc < cutParameters.highestAdcHigh), "Highest ADC within a range")
DEFINE_CUT(EVENT, HIGHEST_ET,                   (eventParameters.highestEtHit.eT >= cutParameters.highestEtLow) && (eventParameters.highestEtHit.eT < cutParameters.highestEtHigh), "Highest E_T within a range")
//DEFINE_CUT(EVENT, HIGHEST_ADC_STUCK,            (event.triggerSimulatedFinal.highestAdcHit.adc & 7) == 4, "Highest ADC stuck bits")
DEFINE_CUT(EVENT, BAD_EVENTS,                   !isBadEvent(event.runId, event.eventId, cutParameters.badEventsListFilename), "Bad events rejected")
#endif

#ifdef INCLUDE_POINT_CUTS
// Point cuts definition
DEFINE_CUT(POINT, VALID,                        point.isValid(), "Valid point")
DEFINE_CUT(POINT, CPV,                          pointParameters.passedCPV, "TPC track veto within a cone")
DEFINE_CUT(POINT, TYPE_SMDE,                    point.clusterBSMDE.energy > 0, "SMDE cluster present")
DEFINE_CUT(POINT, TYPE_SMDP,                    point.clusterBSMDP.energy > 0, "SMDP cluster present")
DEFINE_CUT(POINT, TYPE_SMDANY,                  (point.clusterBSMDE.energy > 0) || (point.clusterBSMDP.energy > 0), "At least one SMD cluster present")
DEFINE_CUT(POINT, ASSOCIATED,                   (pointParameters.distGamma >= cutParameters.gammaDistCutLow) && ((pointParameters.distGamma < cutParameters.gammaDistCutHigh) || (cutParameters.gammaDistCutHigh < 0)), "Point is associated with the MC track")
DEFINE_CUT(POINT, ENERGY,                       (point.energy >= cutParameters.pointEnergyLow) && (point.energy < cutParameters.pointEnergyHigh), "Point energy cut")
DEFINE_CUT(POINT, ETA_COORD,                    (point.etaCoord >= cutParameters.etaCoordLow) && (point.etaCoord < cutParameters.etaCoordHigh), "Cut on the detector eta coordinate")
DEFINE_CUT(POINT, PHI_COORD,                    (point.phiCoord >= cutParameters.phiCoordLow) && (point.phiCoord < cutParameters.phiCoordHigh), "Cut on the phi coordinate")
DEFINE_CUT(POINT, SMDE_BADCLOSE,                !point.clusterBSMDE.badClose, "No dead strip near SMDE cluster")
DEFINE_CUT(POINT, SMDP_BADCLOSE,                !point.clusterBSMDP.badClose, "No dead strip near SMDP cluster")
DEFINE_CUT(POINT, SMDANY_BADCLOSE,              (!point.clusterBSMDE.badClose) || (!point.clusterBSMDP.badClose), "No bad strip near at least one SMD cluster")
DEFINE_CUT(POINT, SMDE_ENERGY,                  (point.clusterBSMDE.energy >= cutParameters.smdEnergyLow) && (point.clusterBSMDE.energy < cutParameters.smdEnergyHigh), "SMDE energy cut")
DEFINE_CUT(POINT, SMDP_ENERGY,                  (point.clusterBSMDP.energy >= cutParameters.smdEnergyLow) && (point.clusterBSMDP.energy < cutParameters.smdEnergyHigh), "SMDP energy cut")
DEFINE_CUT(POINT, SMDANY_ENERGY,                ((point.clusterBSMDE.energy >= cutParameters.smdEnergyLow) && (point.clusterBSMDE.energy < cutParameters.smdEnergyHigh)) || ((point.clusterBSMDP.energy >= cutParameters.smdEnergyLow) && (point.clusterBSMDP.energy < cutParameters.smdEnergyHigh)), "SMDE or SMDP energy cut")
DEFINE_CUT(POINT, SMDE_HIGHEST_HIT,             point.clusterBSMDE.highestEnergyHit.energy > (point.clusterBSMDE.energy * cutParameters.highestHitEnergyFractionSMDE), "SMDE cluster has one highest hit")
DEFINE_CUT(POINT, SMDP_HIGHEST_HIT,             point.clusterBSMDP.highestEnergyHit.energy > (point.clusterBSMDP.energy * cutParameters.highestHitEnergyFractionSMDP), "SMDP cluster has one highest hit")
DEFINE_CUT(POINT, TOWER_HIGHEST_HIT,            point.clusterBTOW.highestEnergyHit.energy > (point.clusterBTOW.energy * cutParameters.highestHitEnergyFractionBTOW), "BTOW cluster has one highest hit")
DEFINE_CUT(POINT, SMDE_SIZE,                    (point.clusterBSMDE.size >= cutParameters.smdSizeLow) && (point.clusterBSMDE.size < cutParameters.smdSizeHigh), "SMDE cluster size cut")
DEFINE_CUT(POINT, SMDP_SIZE,                    (point.clusterBSMDP.size >= cutParameters.smdSizeLow) && (point.clusterBSMDP.size < cutParameters.smdSizeHigh), "SMDP cluster size cut")
DEFINE_CUT(POINT, IN_JET,                       (pointParameters.distJet >= cutParameters.jetDistCutLow) && ((pointParameters.distJet < cutParameters.jetDistCutHigh) || (cutParameters.jetDistCutHigh < 0)), "Point in jet cone")
DEFINE_CUT(POINT, TRIGGERED_HT1,                pointParameters.triggeredHT1, "Point has triggered HighTower-1")
DEFINE_CUT(POINT, TRIGGERED_HT2,                pointParameters.triggeredHT2, "Point has triggered HighTower-2")
DEFINE_CUT(POINT, IN_JET_BACK,                  (pointParameters.distJetBack >= cutParameters.jetDistCutLow) && ((pointParameters.distJetBack < cutParameters.jetDistCutHigh) || (cutParameters.jetDistCutHigh < 0)), "Point in back jet cone")
DEFINE_CUT(POINT, SMDE_HIGHEST_ADC,             (point.clusterBSMDE.highestEnergyHit.adc >= cutParameters.highestAdcLow) && (point.clusterBSMDE.highestEnergyHit.adc < cutParameters.highestAdcHigh), "SMDE highest ADC cut")
DEFINE_CUT(POINT, SMDP_HIGHEST_ADC,             (point.clusterBSMDP.highestEnergyHit.adc >= cutParameters.highestAdcLow) && (point.clusterBSMDP.highestEnergyHit.adc < cutParameters.highestAdcHigh), "SMDP highest ADC cut")
DEFINE_CUT(POINT, ETA,                          (pointParameters.eta >= cutParameters.etaLow) && (pointParameters.eta < cutParameters.etaHigh), "Cut on the eta coordinate")
#endif

#ifdef INCLUDE_CANDIDATE_CUTS
// Pi0 candidate cuts definition
DEFINE_CUT(CANDIDATE, VALID,                    candidate.isValid() && candidate.point1.isValid() && candidate.point1.point.isValid() && candidate.point2.isValid() && candidate.point2.point.isValid(), "Valid candidate")
DEFINE_CUT(CANDIDATE, ASYMETRY,                 (candidateParameters.asymetry >= cutParameters.asymCutLow) && (candidateParameters.asymetry < cutParameters.asymCutHigh), "Asymmetry cut")
DEFINE_CUT(CANDIDATE, PTBIN,                    (cutParameters.ptBinStep == 0) || (Int_t((candidateParameters.pTRec - cutParameters.ptBinStart) / cutParameters.ptBinStep) == Int_t((candidate.point1.event.simulatedParticle.pT - cutParameters.ptBinStart) / cutParameters.ptBinStep)), "pi0 is reconstructed in the correct pT bin")
DEFINE_CUT(CANDIDATE, MASS,                     (candidateParameters.m >= candidateParameters.massRegionLeft) && (candidateParameters.m < candidateParameters.massRegionRight), "Invariant mass cut")
DEFINE_CUT(CANDIDATE, TRIGGERED_HT1,            point1Parameters.triggeredHT1 || point2Parameters.triggeredHT1, "Candidate has triggered HighTower-1")
DEFINE_CUT(CANDIDATE, TRIGGERED_HT2,            point1Parameters.triggeredHT2 || point2Parameters.triggeredHT2, "Candidate has triggered HighTower-2")
DEFINE_CUT(CANDIDATE, OPENANGLE_KINEMATIC,      candidateParameters.openangle >= ((getMinimumOpenangle(candidateParameters.m, candidateParameters.energy) * cutParameters.openAngleMinFraction) + cutParameters.openAngleMinOffset), "Opening angle cut (using reconstructed mass)")
DEFINE_CUT(CANDIDATE, OPENANGLE_KINEMATIC_TRUE, candidateParameters.openangle >= ((getMinimumOpenangle(truePionMass, candidateParameters.energy) * cutParameters.openAngleMinFraction) + cutParameters.openAngleMinOffset), "Opening angle cut (using true pi0 mass)")
DEFINE_CUT(CANDIDATE, PT,                       ((candidateParameters.pTRec >= cutParameters.ptLow) && (candidateParameters.pTRec < cutParameters.ptHigh)), "Reconstructed pi0 pT cut")
DEFINE_CUT(CANDIDATE, ETA_COORD,                (candidateParameters.etaCoord >= cutParameters.etaCoordLow) && (candidateParameters.etaCoord < cutParameters.etaCoordHigh), "Cut on the detector eta coordinate")
DEFINE_CUT(CANDIDATE, ETA,                      (candidateParameters.eta >= cutParameters.etaLow) && (candidateParameters.eta < cutParameters.etaHigh), "Cut on the eta coordinate")
DEFINE_CUT(CANDIDATE, TRIGGERED_HT1_ET,         point1Parameters.triggeredTowerHT1Et || point2Parameters.triggeredTowerHT1Et, "Candidate contains tower that Et-triggered HighTower-1")
DEFINE_CUT(CANDIDATE, TRIGGERED_HT2_ET,         point1Parameters.triggeredTowerHT2Et || point2Parameters.triggeredTowerHT2Et, "Candidate contains tower that Et-triggered HighTower-2")
DEFINE_CUT(CANDIDATE, POINTS_MATCHED_ENERGY,    candidate.candidate.pointsMatched & 1, "Points (from mixed events) are matched in energy")
DEFINE_CUT(CANDIDATE, POINTS_MATCHED_JETDIST,   candidate.candidate.pointsMatched & 2, "Points (from mixed events) are matched in distance to the jet axis")
DEFINE_CUT(CANDIDATE, POINTS_MATCHED_ENERGY_CLOSEST, candidate.candidate.pointsMatched & 4, "Points (from mixed events) are matched in energy (closest)")
DEFINE_CUT(CANDIDATE, POINTS_MATCHED_JETDIST_CLOSEST, candidate.candidate.pointsMatched & 8, "Points (from mixed events) are matched in distance to the jet axis (closest)")
DEFINE_CUT(CANDIDATE, IN_JET,                   (candidateParameters.jetDist >= cutParameters.jetDistCutLow) && ((candidateParameters.jetDist < cutParameters.jetDistCutHigh) || (cutParameters.jetDistCutHigh < 0)), "Candidate in jet cone")
DEFINE_CUT(CANDIDATE, CPV,                      point1Parameters.passedCPV && point2Parameters.passedCPV, "Charged particle veto on both points")
#endif

#ifdef INCLUDE_GAMMA_CUTS
// Simulated photon cuts
DEFINE_CUT(GAMMA, VALID,                        gamma.isValid(), "Valid MC photon")
DEFINE_CUT(GAMMA, CONVERSION_NOTZERO,           (gamma.stopRadius >= cutParameters.gammaConversionRadiusLow) && (gamma.stopRadius < cutParameters.gammaConversionRadiusHigh), "Converted at nonzero radius")
DEFINE_CUT(GAMMA, CONVERSION_ZERO,              gamma.stopRadius == 0, "Converted at zero radius")
DEFINE_CUT(GAMMA, CONVERSION,                   (gamma.stopRadius == 0) || ((gamma.stopRadius >= cutParameters.gammaConversionRadiusLow) && (gamma.stopRadius < cutParameters.gammaConversionRadiusHigh)), "MC photon has converted")
DEFINE_CUT(GAMMA, ASSOCIATED,                   (gammaParameters.distAssociated >= cutParameters.gammaDistCutLow) && ((gammaParameters.distAssociated < cutParameters.gammaDistCutHigh) || (cutParameters.gammaDistCutHigh < 0)), "MC photon has EMC point associated")
DEFINE_CUT(GAMMA, ETA_COORD,                    (gammaParameters.etaCoord >= cutParameters.etaCoordLow) && (gammaParameters.etaCoord < cutParameters.etaCoordHigh), "Cut on the detector eta coordinate")
DEFINE_CUT(GAMMA, PHI_COORD,                    (gammaParameters.phiCoord >= cutParameters.phiCoordLow) && (gammaParameters.phiCoord < cutParameters.phiCoordHigh), "Cut on the phi coordinate")
DEFINE_CUT(GAMMA, ETA,                          (gammaParameters.eta >= cutParameters.etaLow) && (gammaParameters.eta < cutParameters.etaHigh), "Cut on the eta coordinate")
#endif

#ifdef INCLUDE_PION_CUTS
// Simulated pion cuts
DEFINE_CUT(PION, VALID,                         pion.isValid(), "Valid MC particle")
DEFINE_CUT(PION, VALID_DECAY,                   pion.parent.summary.daughters == (2 + (2 << 4)), "Valid MC 2gamma decay")
DEFINE_CUT(PION, VALID_GAMMA,                   TMath::Abs(pionParameters.pT - pionParameters.pTgammas) < 0.1, "Valid MC daughter gammas")
DEFINE_CUT(PION, ASSOCIATION_NOTSAME,           (pion.daughter1.associatedPoint.etaCoord != pion.daughter2.associatedPoint.etaCoord) || (pion.daughter1.associatedPoint.phiCoord != pion.daughter2.associatedPoint.phiCoord), "Decay MC photons were associated with different EMC points")
DEFINE_CUT(PION, ETA_COORD,                     (pionParameters.etaCoord >= cutParameters.etaCoordLow) && (pionParameters.etaCoord < cutParameters.etaCoordHigh), "Cut on the detector eta coordinate")
DEFINE_CUT(PION, ETA,                           (pionParameters.eta >= cutParameters.etaLow) && (pionParameters.eta < cutParameters.etaHigh), "Cut on the eta coordinate")
#endif

#undef INCLUDE_ALL_CUTS
#undef INCLUDE_EVENT_CUTS
#undef INCLUDE_POINT_CUTS
#undef INCLUDE_CANDIDATE_CUTS
#undef INCLUDE_GAMMA_CUTS
#undef INCLUDE_PION_CUTS
