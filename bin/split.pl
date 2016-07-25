#!/usr/bin/env perl
use File::Basename;
use FileHandle;
my @topvolumes = (
		  {'sys' => 'HEAD', 'top' => '', 'comment' => '', 'blob' => ''},
		  {'sys' => 'MAIN', 'top' => '', 'comment' => '', 'blob' => ''},
		  {'sys' => 'Material', 'top' => '', 'comment' => '', 'blob' => ''},
		  {'sys' => 'Media', 'top' => '', 'comment' => '', 'blob' => ''},
		  {'sys' => 'Rotations', 'top' => '', 'comment' => '', 'blob' => ''},
		  {'sys' => 'hall', 'top' => 'HALL', 'comment' => '', 'blob' => ''},
		  {'sys' => 'cave', 'top' => 'CAVE', 'comment' => '', 'blob' => ''},
		  {'sys' => 'pipe', 'top' => 'PIPE', 'comment' => 'STAR beam pipe mother volume, the geometry  of the STAR beam pipe', 'blob' => ''},
		  {'sys' => 'upstream', 'top' => 'UPST', 'comment' => 'upstream mother volume in the STAR cave, the geometry  of the UPSTREAM AreA', 'blob' => ''},
		  {'sys' => 'svt',  'top' => 'SVTT', 'comment' => 'mother of all SVT volumes', 'blob' => ''},
		  {'sys' => 'scon', 'top' => 'SCON', 'comment' => 'Silicon tracker supporting cone mother volume, Support structures living before in SVTT moved into CAVE', 
		   'blob' => ''},
		  {'sys' => 'ssd',  'top' => 'SFMO', 'comment' => 'mother of all Silicon Strip Detector volumes, the Silicon Strip Detector with TUP modifications',
		   'blob' => ''},
		  {'sys' => 'tpc',  'top' => 'TPCE', 'comment' => 'TPC envelope', 'blob' => ''},
		  {'sys' => 'ftpc', 'top' => 'FTPC', 'comment' => 'the Forward TPC in STAR', 'blob' => ''},
		  {'sys' => 'support','top' => 'SUPO', 'comment' => 'FTPC support mother volume, the geometry of the Forward TPC supports in STAR', 'blob' => ''},
		  {'sys' => 'fstd', 'top' => 'FTMO', 'comment' => 'mother of the single FTPC RO barrel, the geometry of the readout structure of the FTPC', 
		   'blob' => ''},
		  {'sys' => 'fsmo', 'top' => 'FTCM', 'comment' => 'mother of one endcap of FSTD, the geometry of the forward silicon tracker pixel detector', 'blob' => ''},
		  {'sys' => 'btof', 'top' => 'BTOF', 'comment' => 'whole CTF system envelope,  the Geometry of Barrel Trigger / Time Of Flight system', 
		   'blob' => ''},
		  {'sys' => 'vpd',  'top' => 'VPDD', 'comment' => 'whole VPPD assembly, the StartDet and pipe support hardware', 'blob' => ''},
		  {'sys' => 'calb', 'top' => 'CALB', 'comment' => 'EMC Barrel envelope, the geometry of the Barrel EM Calorimeter', 'blob' => ''},
		  {'sys' => 'ecal', 'top' => 'ECAL', 'comment' => 'EMC EndCap wheel, the EM EndCap Calorimeter', 'blob' => ''},
		  {'sys' => 'bbc',  'top' => 'BBCM', 'comment' => 'BBC East or West module,  the Beam Beam Counter Modules GEOmetry', 'blob' => ''},
		  {'sys' => 'fpd',  'top' => 'FBOX', 'comment' => 'Pb-Glass fpd detector', 'blob' => ''},
		  {'sys' => 'zcal', 'top' => 'ZCAL', 'comment' => 'region between the DX and the D0 magnets, the geometry of the Zero deg. Quartz Calorimeter', 
		   'blob' => ''},
		  {'sys' => 'magp', 'top' => 'MAGP', 'comment' => 'magnet mother, the geometry of the STAR magnet', 'blob' => ''},

		  {'sys' => 'fgt',  'top' => 'FGMO', 'comment' => 'mother volume for the FGTD,  Forward Pion Detector Modules', 'blob' => ''},
		  {'sys' => 'phmd', 'top' => 'PHMD', 'comment' => 'the PMD box volume,  the geometry of photon multiplicity detector', 'blob' => ''},
#		  {'sys' => 'phmd', 'top' => 'PHMS','comment' => 'the PMD box volume,  the geometry of photon multiplicity detector 1/3rd of PHMD', 'blob' => ''},
		  {'sys' => 'pixel','top' => 'PXMO', 'comment' => 'mother of the pixel detector volumes, the geometry of the STAR pixel detector', 'blob' => ''},
		  {'sys' => 'rich', 'top' => 'RICH', 'comment' => 'just an aluminum box, defines Ring Image Cerenkov geometry', 'blob' => ''},
		  {'sys' => 'ist',  'top' => 'IBMO', 'comment' => 'mother of the ISTB detector,the geometry of the Inner Silicon Tracker', 'blob' => ''},
		  {'sys' => 'fgtd', 'top' => 'FGMO', 'comment' => 'mother volume for the FGTD, Forward GEM Tracking Detector Geometry', 'blob' => ''},
		  {'sys' => 'gemb', 'top' => 'GMBO', 'comment' => 'mother of the GEMB detector, the geometry of GEMs in front of the TPC', 'blob' => ''},
		  {'sys' => 'quad', 'top' => 'MGMT', 'comment' => 'magnet mother, the description of all the magnets upstream inclusive of D0', 'blob' => ''},
		  {'sys' => 'shield', 'top' => 'SHLD', 'comment' => 'shield mother volume in the STAR cave, the shielding', 'blob' => ''},
		  {'sys' => 'mutd', 'top' => 'MUTD', 'comment' => 'muon detector mother, the geometry of the STAR muon trigger system', 'blob' => ''},
		  {'sys' => 'igtd', 'top' => 'IGMO', 'comment' => 'mother volume for the IGTD, Inner GEM Tracking Detector Geometry', 'blob' => ''},
		  {'sys' => 'hpdt', 'top' => 'YPXM', 'comment' => 'mother of the pixel detector volumes, HPDTGEO geometry of the STAR pixel detector', 'blob' => ''},
#		  {'sys' => 'itsp', 'top' => 'ITSP', 'comment' => 'Inner Tracker SuPport cone mother volume, the Inner Tracker Support', 'blob' => ''},
#		  {'sys' => 'support', 'top' => 'ITSP', 'comment' => 'Inner Tracker SuPport cone mother volume, the Inner Tracker Support', 'blob' => ''},

#		  {'sys' => 'ibsh', 'top' => 'IBSH', 'comment' => '', 'blob' => ''},
#		  {'sys' => 'ibsg', 'top' => 'IBSG', 'comment' => '', 'blob' => ''},
#		  {'sys' => 'ibsf', 'top' => 'IBSF', 'comment' => '', 'blob' => ''},
#		  {'sys' => 'ibse', 'top' => 'IBSE', 'comment' => '', 'blob' => ''},
#		  {'sys' => 'ibsd', 'top' => 'IBSD', 'comment' => '', 'blob' => ''},
#		  {'sys' => 'ibsc', 'top' => 'IBSC', 'comment' => '', 'blob' => ''},
#		  {'sys' => 'ibsb', 'top' => 'IBSB', 'comment' => '', 'blob' => ''},
#		  {'sys' => 'ibsa', 'top' => 'IBSA', 'comment' => '', 'blob' => ''},
		  {'sys' => 'ibem', 'top' => 'IBEM', 'comment' => '', 'blob' => ''},
#		  {'sys' => 'ibeh', 'top' => 'IBEH', 'comment' => '', 'blob' => ''},
		  {'sys' => 'dumm', 'top' => 'DUMM', 'comment' => 'dummy object, a dummy object used to simulate material balance effects', 'blob' => ''}
		 );
my @blocks = (
	      {'vol' => 'AIRA', 'comment'=>'a detector made in air  ', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'ALMF', 'comment'=>'an Aluminum sheet', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'ALUM', 'comment'=>'an Aluminum sheet', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'ASTR', 'comment'=>'the  strip', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'BANG', 'comment'=>'an angled part of TPC cooling structure ( Aile )', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BARM', 'comment'=>'a TPC cooling structure arm             ( Material  Aluminium )', 'sys'=>'btof', 'module'=>'btofgeo1.g'},
	      {'vol' => 'BARM', 'comment'=>'a TPC cooling structure arm             ( Material  Aluminium )', 'sys'=>'btof', 'module'=>'btofgeo2.g'},
	      {'vol' => 'BARM', 'comment'=>'a TPC cooling structure arm             ( Material  Aluminium )', 'sys'=>'btof', 'module'=>'btofgeo3.g'},
	      {'vol' => 'BARM', 'comment'=>'a TPC cooling structure arm             ( Material  Aluminium )', 'sys'=>'btof', 'module'=>'btofgeo4.g'},
	      {'vol' => 'BARM', 'comment'=>'a TPC cooling structure arm             ( Material  Aluminium )', 'sys'=>'btof', 'module'=>'btofgeo5|6.g'},
	      {'vol' => 'BARR', 'comment'=>'e Barrette quarzo opaco', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'BASA', 'comment'=>'the G10 base plate', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'BASE', 'comment'=>'a bottom of TPC coolant structure      ', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BASS', 'comment'=>'a single TOF Slat Assembly (slat+PMT+base)', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BBCA', 'comment'=>'one BBC Annulus', 'sys'=>'bbc', 'module'=>'bbcmgeo.g'},
	      {'vol' => 'BBCM', 'comment'=>'one BBC East or West', 'sys'=>'bbc', 'module'=>'bbcmgeo.g'},
	      {'vol' => 'BCCV', 'comment'=>'a  Ctb optical ConVerter', 'sys'=>'btof', 'module'=>'btofgeo1:4.g'},
	      {'vol' => 'BCCV', 'comment'=>'a  Ctb optical ConVerter', 'sys'=>'btof', 'module'=>'btofgeo5:6.g'},
	      {'vol' => 'BCEB', 'comment'=>'a square G10 board in the CW Base for TOF', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BCEL', 'comment'=>'a circular G10 board in the CW Base for TOF', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BCEL', 'comment'=>'a G10 board in the CW Base for TOF', 'sys'=>'btof', 'module'=>'btofgeo1.g'},
	      {'vol' => 'BCON', 'comment'=>'a generic plastic block for various connectors, foam-support-angles, etc......', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BCOO', 'comment'=>'the cooling rails/loops', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BCOV', 'comment'=>'a whole TPC cooling channel', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BCPM', 'comment'=>'a PhotoMultiplier Tube (same for CTB and TOF)', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BCSB', 'comment'=>'the active trigger scintillator SLAB for tof', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BCSK', 'comment'=>'a CTB Linear Base tube', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BFEE', 'comment'=>'a G10 discriminator/CW control board for TOF', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BGMT', 'comment'=>'the mixture gas box in tray that change the hc box into slim', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BLEM', 'comment'=>'a Lemo connector on the FEE boards', 'sys'=>'btof', 'module'=>'btofgeo3:6.g'},
	      {'vol' => 'BMAA', 'comment'=>'a b1ox for a 4wide AND 5wide phi column of TOF Scintillators', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BMTC', 'comment'=>'the Main Tray Cavity filled with MANY details for CTB', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BMTD', 'comment'=>'a 5wide phi column of TOF Scintillators', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BMTD', 'comment'=>'a phi column of TOF Scintillators', 'sys'=>'btof', 'module'=>'btofgeo1.g'},
	      {'vol' => 'BMTM', 'comment'=>'the Main Tray cavity divisions Mother volume for TOF', 'sys'=>'btof', 'module'=>'btofgeo1.g'},
	      {'vol' => 'BORD', 'comment'=>'carbon', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'BPIP', 'comment'=>'the Long Pipe for the cooling loop', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BPIQ', 'comment'=>'the Short Pipe for the cooling loop', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BPLA', 'comment'=>'the plastic angle pieces that hold the upper foam supports...', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BPOL', 'comment'=>'one Bbc POLystyren active scintillator layer', 'sys'=>'bbc', 'module'=>'bbcmgeo.g'},
	      {'vol' => 'BRAI', 'comment'=>'the Rail for the cooling loop', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BRCB', 'comment'=>'the PCB in the TOFr', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BRDT', 'comment'=>'the middle part (including innner glass and gas)in the MRPC', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BRFE', 'comment'=>'the FEE of tofr (run-3)', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BRGR', 'comment'=>'the GRaphite in the TOFr', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BRHC', 'comment'=>'the HoneyComb in the TOFr', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BRIG', 'comment'=>'the Inner Glass in the TOFr', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BRMD', 'comment'=>'a six channel sys for TOFr', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BRMY', 'comment'=>'the MYlar in the TOFr', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BROG', 'comment'=>'the Outer Glass in the TOFr', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BRSG', 'comment'=>'the sensitive gas layer in the TOFr', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BRTC', 'comment'=>'the Main Tray Cavity filled with the details for TOFr (run3 or run4)', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BRWG', 'comment'=>'the WedGe(support) in the TOFr', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BSEC', 'comment'=>'a sector of CTB/TOF Trigger Barrel Scintillators', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BTFE', 'comment'=>'the TINO/TDIG boards', 'sys'=>'btof', 'module'=>'btofgeo6.g'},
	      {'vol' => 'BTFT', 'comment'=>'the Foot structure    ( Material  Aluminium )', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BTOF', 'comment'=>'the whole CTF system envelope', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BTOH', 'comment'=>'a half of trigger system (west-east)', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BTRA', 'comment'=>'one full tray plus supporting structure for CTB/TOF', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BTSK', 'comment'=>'the outer shell of a TOF CW Base', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BTTC', 'comment'=>'the Main Tray Cavity filled with MANY details for TOF', 'sys'=>'btof', 'module'=>'btofgeo1.g'},
	      {'vol' => 'BTTC', 'comment'=>'the Main Tray Cavity filled with the details for TOFp', 'sys'=>'btof', 'module'=>'btofgeo2:3.g'},
	      {'vol' => 'BTTC', 'comment'=>'the Main Tray Cavity filled with the details for TOFp', 'sys'=>'btof', 'module'=>'btofgeo4:6.g'},
	      {'vol' => 'BUND', 'comment'=>'Undercarriage support tray - same both for CTB and TOF', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BUPC', 'comment'=>'the up pcb cover of tofr', 'sys'=>'btof', 'module'=>'btofgeo2:6.g'},
	      {'vol' => 'BWAT', 'comment'=>'tPC cooling water', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BXSA', 'comment'=>'the active trigger scintillator SLAB for ctb', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BXTR', 'comment'=>'a Main TRay covering box for CTB or TOF', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'BZEL', 'comment'=>'a Ctb PM electronics', 'sys'=>'btof', 'module'=>'btofgeo1:6.g'},
	      {'vol' => 'CALB', 'comment'=>'EMC Barrel envelope', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CAVE', 'comment'=>'GSTAR cave with subsystem envelopes', 'sys'=>'cave', 'module'=>'cavegeo.g'},
	      {'vol' => 'CBTW', 'comment'=>'the  sys Front Back Plate', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CHLV', 'comment'=>'corresponds to double syss...', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CLAD', 'comment'=>'one CLADding of BPOL active region', 'sys'=>'bbc', 'module'=>'bbcmgeo.g'},
	      {'vol' => 'COIL', 'comment'=>'the main coil mother', 'sys'=>'magp', 'module'=>'magpgeo.g'},
	      {'vol' => 'CPHI', 'comment'=>'corresponds to a single', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CSCI', 'comment'=>'a scintillator layer.', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CSDA', 'comment'=>'Al with sensitive gas volume', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CSHI', 'comment'=>'a sensiteve Ar/CO2 box', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CSMB', 'comment'=>'the back first (last) Al rib', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CSMC', 'comment'=>'the front first (last) Al rib', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CSMD', 'comment'=>'the shower maximum detector envelope', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CSME', 'comment'=>'the part of CSDA Al box with Ar/CO2 sensiteve gas', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CSMG', 'comment'=>'G10 front back plate', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'CSUP', 'comment'=>'a super layer with few layers inside', 'sys'=>'calb', 'module'=>'calbgeo:2.g'},
	      {'vol' => 'DCON', 'comment'=>'the beam pipe Bell section at the end of DX', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'DUMM', 'comment'=>'the dummy object', 'sys'=>'dumm', 'module'=>'dummgeo.g'},
	      {'vol' => 'DVAC', 'comment'=>'its cavity', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'DXMG', 'comment'=>'the return yoke for the DX mAgnet', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'DZER', 'comment'=>'the D0 yoke', 'sys'=>'quad', 'module'=>'quadgeo.g'},
	      {'vol' => 'EAGA', 'comment'=>'half of wheel air volume for  the EndCap', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'EALP', 'comment'=>'ALuminium  Plate in calorimeter cell', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ECAL', 'comment'=>'one EMC EndCap wheel', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ECGH', 'comment'=>'air Gap between endcap Half wheels', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ECHC', 'comment'=>'steel EndCap Half Cover', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ECVO', 'comment'=>'one of EndCap Volume with megatiles and radiators', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'EFLP', 'comment'=>'First Aluminium plate', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'EHMS', 'comment'=>'sHower Max Strip', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ELED', 'comment'=>'lead absorber Plate', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'EMGT', 'comment'=>'a megatile EM section', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'EMOD', 'comment'=>'one sys  of the EM EndCap', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'EMSS', 'comment'=>'steel support of the EndCap', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'EPER', 'comment'=>'a EM subsection period (super layer)', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'EPSB', 'comment'=>'Projectile Stainless steel Bar', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ERAD', 'comment'=>'radiator', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ERCM', 'comment'=>'stainless steel tie Rod in CaloriMeter sections', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ERSM', 'comment'=>'stainless steel tie Rod in Shower Max', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ESCI', 'comment'=>'the active scintillator (polystyren) layer ', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ESEC', 'comment'=>'a single EM section', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ESHM', 'comment'=>'the SHower Max  section', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ESPL', 'comment'=>'one of the Shower max  PLanes', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ESSP', 'comment'=>'Stainless Steel  back Plate', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'ETAR', 'comment'=>'one CELL of scintillator, fiber and plastic', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'EXGT', 'comment'=>'the G10 layer in the Shower Max ', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'EXSG', 'comment'=>'the Shower max  Gap for scintillator strips', 'sys'=>'ecal', 'module'=>'ecalgeo.g'},
	      {'vol' => 'FALP', 'comment'=>'ALuminium  Plate in shower max', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FALU', 'comment'=>'Aluminium Base Cell', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FBAS', 'comment'=>'Steel Base Plate', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FBOX', 'comment'=>'one Pb-Glass fpd detector', 'sys'=>'fpd', 'module'=>'fpdmgeo1:3.g'},
	      {'vol' => 'FDMO', 'comment'=>'the mother of an individual two-layer disk assembly (wafers and cooling)', 'sys'=>'fstd', 'module'=>'fstdgeo.g'},
	      {'vol' => 'FDMS', 'comment'=>'a division within an individual disk', 'sys'=>'fstd', 'module'=>'fstdgeo.g'},
	      {'vol' => 'FDMW', 'comment'=>'the mother wedge, housing plate, sensor  and chips', 'sys'=>'fstd', 'module'=>'fstdgeo.g'},
	      {'vol' => 'FDSC', 'comment'=>'the readout Chip', 'sys'=>'fstd', 'module'=>'fstdgeo.g'},
	      {'vol' => 'FDSW', 'comment'=>'the Silicon Wafer (all active)', 'sys'=>'fstd', 'module'=>'fstdgeo.g'},
	      {'vol' => 'FDTP', 'comment'=>'the AlN Thermal Plate', 'sys'=>'fstd', 'module'=>'fstdgeo.g'},
	      {'vol' => 'FDWD', 'comment'=>'the water duct made of carbon composite', 'sys'=>'fstd', 'module'=>'fstdgeo.g'},
	      {'vol' => 'FDWW', 'comment'=>'the water inside the carbon duct', 'sys'=>'fstd', 'module'=>'fstdgeo.g'},
	      {'vol' => 'FEAC', 'comment'=>'Steel Enclosure', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FEBC', 'comment'=>'Air square hole', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FECC', 'comment'=>'Steel distancer', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FEDC', 'comment'=>'Steel Enclosure part on south', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FEEC', 'comment'=>'Steel Enclosure part on north', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FENC', 'comment'=>'Steel Enclosure', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FERC', 'comment'=>'Air Enclosure part', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FESC', 'comment'=>'Air Enclosure part', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FETC', 'comment'=>'Air Enclosure part', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FFCE', 'comment'=>'the Fildcage Enhanced Support Structure', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FFLP', 'comment'=>'First Aluminium plate', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FFRA', 'comment'=>'outermost FC Ring', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FFSL', 'comment'=>'ceramic holder for fieldcage rings', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FGAM', 'comment'=>'the mother of the Argon volume', 'sys'=>'fgtd', 'module'=>'fgtdgeo.g'},
	      {'vol' => 'FGAS', 'comment'=>'the FTPC gas volume', 'sys'=>'ftpc', 'module'=>'ftpcgeo:2.g'},
	      {'vol' => 'FGFO', 'comment'=>'the GEM foils', 'sys'=>'fgtd', 'module'=>'fgtdgeo1.g'},
	      {'vol' => 'FGGT', 'comment'=>'the G10 plate', 'sys'=>'fgtd', 'module'=>'fgtdgeo.g'},
	      {'vol' => 'FGIS', 'comment'=>'the inner support or spacer', 'sys'=>'fgtd', 'module'=>'fgtdgeo1.g'},
	      {'vol' => 'FGKP', 'comment'=>'the Kapton layer', 'sys'=>'fgtd', 'module'=>'fgtdgeo.g'},
	      {'vol' => 'FGMO', 'comment'=>'the mother of the Forward GEM detector', 'sys'=>'fgtd', 'module'=>'fgtdge:2.g'},
	      {'vol' => 'FGRL', 'comment'=>'the readout layer', 'sys'=>'fgtd', 'module'=>'fgtdgeo1.g'},
	      {'vol' => 'FGRN', 'comment'=>'a Ring in the GEM detector', 'sys'=>'fgtd', 'module'=>'fgtdgeo.g'},
	      {'vol' => 'FGSC', 'comment'=>'the sensitive area', 'sys'=>'fgtd', 'module'=>'fgtdgeo:2.g'},
	      {'vol' => 'FHMS', 'comment'=>'sHower Max Strip', 'sys'=>'fpd', 'module'=>'fpdmgeo:3.g'},
	      {'vol' => 'FIAL', 'comment'=>'the inner AL-tube of the FTPC', 'sys'=>'ftpc', 'module'=>'ftpcgeo:2.g'},
	      {'vol' => 'FIFR', 'comment'=>'the Al inner flange ring', 'sys'=>'ftpc', 'module'=>'ftpcgeo.g'},
	      {'vol' => 'FKWI', 'comment'=>'the double Kapton window', 'sys'=>'ftpc', 'module'=>'ftpcgeo1.g'},
	      {'vol' => 'FKWI', 'comment'=>'the double Kapton window', 'sys'=>'ftpc', 'module'=>'ftpcgeo.g'},
	      {'vol' => 'FLED', 'comment'=>'lead absorber Plate', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FLGD', 'comment'=>'one Pb-Glass fpd detector', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FLGF', 'comment'=>'Lead Glass detector', 'sys'=>'fpd', 'module'=>'fpdmgeo2.g'},
	      {'vol' => 'FLGR', 'comment'=>'Lead Glass detector', 'sys'=>'fpd', 'module'=>'fpdmgeo:3.g'},
	      {'vol' => 'FLGT', 'comment'=>'one PbG Tower', 'sys'=>'fpd', 'module'=>'fpdmgeo:2.g'},
	      {'vol' => 'FLXF', 'comment'=>'Lead Glass detector', 'sys'=>'fpd', 'module'=>'fpdmgeo3.g'},
	      {'vol' => 'FMOD', 'comment'=>'one sys  of the EM EndCap', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FMPT', 'comment'=>'the insulating plastic tube of the drift-electrode', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FMSS', 'comment'=>'steel support of the EndCap', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FOAL', 'comment'=>'the Al drift-electrode', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FPAD', 'comment'=>'the Pad plane of the FTPC', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FPCT', 'comment'=>'Photo Cathode', 'sys'=>'fpd', 'module'=>'fpdmgeo:3.g'},
	      {'vol' => 'FPDM', 'comment'=>'one FPD West volume', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FPER', 'comment'=>'a EM sesection period (super layer)', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FPRB', 'comment'=>'silicon rubber', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FPSE', 'comment'=>'a single EM section', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FRCC', 'comment'=>'the Copper Cooling Layer of the Readout Chamber (Middle)', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FRCE', 'comment'=>'the Copper Cooling Layer of the Readout Chamber (Ends)', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FREL', 'comment'=>'the Electronics Layer of the Readout Chamber', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FREO', 'comment'=>'FREON', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'FROB', 'comment'=>'the Box Holes in the Readout Chamber', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FROE', 'comment'=>'the End Box Holes in the Readout Chamber', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FROL', 'comment'=>'the Length side Box Holes in the Readout Chamber', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FROM', 'comment'=>'one sys of the Readout Chamber', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FROP', 'comment'=>'the Polygon part of the support bar', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FROS', 'comment'=>'one Ring of Readout syss in the support Structure ', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FROT', 'comment'=>'the Trapezoid part of the support bar', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSBA', 'comment'=>'the Stabilizer for the  inner Support Ring', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSCI', 'comment'=>'the active scintillator (polystyren) layer ', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FSEC', 'comment'=>'a sensitive gas sector', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSEN', 'comment'=>'the sensitive gas volume', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSER', 'comment'=>'the Support End Ring', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSHL', 'comment'=>'a hole the beam of the strut', 'sys'=>'fsmo', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FSHM', 'comment'=>'the SHower Max  section', 'sys'=>'fpd', 'module'=>'fpdmgeo:1.g'},
	      {'vol' => 'FSHM', 'comment'=>'the SHower Max  section', 'sys'=>'fpd', 'module'=>'fpdmgeo:3.g'},
	      {'vol' => 'FSMO', 'comment'=>'the mother of one endcap of FSTD', 'sys'=>'fsmo', 'module'=>'fstdge:o.g'},
	      {'vol' => 'FSPG', 'comment'=>'the inner Support End Ring and the outer support Rings', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSPI', 'comment'=>'the Hole of the inner Support End Ring', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSRA', 'comment'=>'the outer Support End Ring', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSRB', 'comment'=>'the medium Support End Ring', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSRI', 'comment'=>'the inner Support Ring', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSSM', 'comment'=>'the main Support Stucture', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FSSP', 'comment'=>'stainless steel  Plate', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FSTC', 'comment'=>'the central beam of the strut', 'sys'=>'fsmo', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FSTL', 'comment'=>'the flat part of the strut', 'sys'=>'fsmo', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTAR', 'comment'=>'one CELL of scintillator, fiber and laminated lead', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FTCD', 'comment'=>'the division of the FTCM', 'sys'=>'fsmo', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTCM', 'comment'=>'the mother of the core struts and PCBs', 'sys'=>'fsmo', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTIF', 'comment'=>'the inner flange', 'sys'=>'fstd', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTMO', 'comment'=>'the mother of the single FTPC RO barrel', 'sys'=>'fstd', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTOF', 'comment'=>'the outer flange', 'sys'=>'fstd', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTOH', 'comment'=>'a hole the outer flange', 'sys'=>'fstd', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTOW', 'comment'=>'one PbG Tower', 'sys'=>'fpd', 'module'=>'fpdmgeo:3.g'},
	      {'vol' => 'FTPC', 'comment'=>'the Forward TPC mother (needed for standalong test only)', 'sys'=>'ftpc', 'module'=>'ftpcgeo:1.g'},
	      {'vol' => 'FTPI', 'comment'=>'the inner PCB', 'sys'=>'fsmo', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTPL', 'comment'=>'the plank covering the strut', 'sys'=>'fsmo', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTPO', 'comment'=>'the outer PCB', 'sys'=>'fsmo', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTRM', 'comment'=>'the rim connected to the inner flange', 'sys'=>'fstd', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FTSH', 'comment'=>'the protective shell', 'sys'=>'fsmo', 'module'=>'ftrogeo.g'},
	      {'vol' => 'FUMT', 'comment'=>'mu metal', 'sys'=>'fpd', 'module'=>'fpdmgeo:3.g'},
	      {'vol' => 'FWAL', 'comment'=>'almunum wrapper', 'sys'=>'fpd', 'module'=>'fpdmgeo:3.g'},
	      {'vol' => 'FXFP', 'comment'=>'SS laminated plate', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'FXGT', 'comment'=>'the G10 layer in the SMax', 'sys'=>'fpd', 'module'=>'fpdmgeo:3.g'},
	      {'vol' => 'FXSG', 'comment'=>'the Shower max  Gap for scintillator strips', 'sys'=>'fpd', 'module'=>'fpdmgeo.g'},
	      {'vol' => 'GMBO', 'comment'=>'the mother of the GEMB detector', 'sys'=>'gemb', 'module'=>'gembgeo.g'},
	      {'vol' => 'GMDI', 'comment'=>'the sensitive volume of the GEM strip detector, layer 1', 'sys'=>'gemb', 'module'=>'gembgeo.g'},
	      {'vol' => 'GMGI', 'comment'=>'a GEM strip detector filled with gas, layer 1', 'sys'=>'gemb', 'module'=>'gembgeo.g'},
	      {'vol' => 'GMMI', 'comment'=>'a GEM strip detector, mylar staff, layer 1', 'sys'=>'gemb', 'module'=>'gembgeo.g'},
	      {'vol' => 'GMVI', 'comment'=>'a GEM strip detector, layer 1', 'sys'=>'gemb', 'module'=>'gembgeo.g'},
	      {'vol' => 'HALL', 'comment'=>'GSTAR building', 'sys'=>'hall', 'module'=>'cavegeo.g'},
	      {'vol' => 'HOLE', 'comment'=>'an oppening in the aluminum frame', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'HONE', 'comment'=>'a CARBONIO', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'IBAA', 'comment'=>'the vertical post on the balcony (Aluminum)', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBAB', 'comment'=>'the diagonal post from the balcony (Aluminum)', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBAC', 'comment'=>'vertical parts of the cross post below the I-Beam (Aluminum)', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBAM', 'comment'=>'the mother of the whole long ladder', 'sys'=>'ist', 'module'=>'istbgeo:6:00.g'},
	      {'vol' => 'IBBC', 'comment'=>'the horizontal part of the cross post below the I-Beam (Aluminum)', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBCC', 'comment'=>'the end caps on the cross post below the I-Beam (Aluminum)', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBEH', 'comment'=>'a horizontal IBeam plate', 'sys'=>'ibem', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'IBEM', 'comment'=>'the IBeam structure beneath the Bell reducer cone', 'sys'=>'ibem', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'IBEV', 'comment'=>'a vertical IBeam plate', 'sys'=>'ibem', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'IBEW', 'comment'=>'the first part of the IBeam plate', 'sys'=>'ibem', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'IBLM', 'comment'=>'the mother of the sensor assembly', 'sys'=>'ist', 'module'=>'istbgeo:6:00.g'},
	      {'vol' => 'IBME', 'comment'=>'the mother of the interconnect of the IST on the WEST side', 'sys'=>'ibem', 'module'=>'istbgeo1,2,5,6.g'},
	      {'vol' => 'IBMO', 'comment'=>'the mother of the ISTB detector', 'sys'=>'ist', 'module'=>'istbgeo:6:00.g'},
	      {'vol' => 'IBMW', 'comment'=>'the mother of the interconnect of the IST on the EAST side', 'sys'=>'ist', 'module'=>'istbgeo1-2:5-6.g'},
	      {'vol' => 'IBMY', 'comment'=>'the mother of the inner IST layer', 'sys'=>'ist', 'module'=>'istbgeo*.g'},
	      {'vol' => 'IBMZ', 'comment'=>'the mother of the outer IST layer', 'sys'=>'ist', 'module'=>'istbgeo1.g'},
	      {'vol' => 'IBSA', 'comment'=>'the vertical post on the balcony (Envelope)', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBSB', 'comment'=>'the diagonal post from the balcony (Envelope)', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBSC', 'comment'=>'the cross post below the I-Beam (Envelope)', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBSD', 'comment'=>'the horizontal plates that hold the pipe-support brackets (Aluminum)', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBSE', 'comment'=>'the vertical parts of the pipe-support brackets (Aluminum)', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBSF', 'comment'=>'the long threaded rods for X-support of the I-beam', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBSG', 'comment'=>'the vertical bolts to the pipe-support brackets', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBSH', 'comment'=>'the cross-bolts from the pipe-support brackets to the pipe', 'sys'=>'ibem', 'module'=>'vpddgeo2.g'},
	      {'vol' => 'IBSP', 'comment'=>'the Silicon Sensor Passive Area', 'sys'=>'ist', 'module'=>'istbgeo00.g'},
	      {'vol' => 'IBSS', 'comment'=>'the Silicon Sensor Active Area', 'sys'=>'ibem', 'module'=>'istbgeo*.g'},
	      {'vol' => 'IGAL', 'comment'=>'the active area', 'sys'=>'igtd', 'module'=>'igtdgeo.g'},
	      {'vol' => 'IGDO', 'comment'=>'the mother volume of the individual GEM disks', 'sys'=>'igtd', 'module'=>'igtdgeo.g'},
	      {'vol' => 'IGFO', 'comment'=>'the GEM foils', 'sys'=>'igtd', 'module'=>'igtdgeo.g'},
	      {'vol' => 'IGIS', 'comment'=>'the inner support or spacer', 'sys'=>'igtd', 'module'=>'igtdgeo.g'},
	      {'vol' => 'IGMO', 'comment'=>'the mother volume for the IGTD', 'sys'=>'igtd', 'module'=>'igtdgeo.g'},
	      {'vol' => 'IGOS', 'comment'=>'the outer support or spacer', 'sys'=>'igtd', 'module'=>'igtdgeo.g'},
	      {'vol' => 'IGRL', 'comment'=>'the readout layer', 'sys'=>'igtd', 'module'=>'igtdgeo.g'},
	      {'vol' => 'ISCL', 'comment'=>'the Copper layer on the Kapton hybrid', 'sys'=>'ist', 'module'=>'istbgeo1.g'},
	      {'vol' => 'ISCL', 'comment'=>'the Copper layer on the Kapton hybrid', 'sys'=>'ist', 'module'=>'istbgeo*.g'},
	      {'vol' => 'ISCO', 'comment'=>'the interconnect between the support rings', 'sys'=>'ist', 'module'=>'istbgeo*.g'},
	      {'vol' => 'ISKH', 'comment'=>'the Kapton hybrid', 'sys'=>'ist', 'module'=>'istbgeo*.g'},
	      {'vol' => 'ISLA', 'comment'=>'the large glue layer between Carbon foam and sensor (and sensor and hybrid)', 'sys'=>'ist', 'module'=>'istbgeo1.g'},
	      {'vol' => 'ISLB', 'comment'=>'the small glue layer between hybrid and chip', 'sys'=>'ist', 'module'=>'istbgeo*.g'},
	      {'vol' => 'ISRI', 'comment'=>'the support ring for the layers', 'sys'=>'ist', 'module'=>'istbgeo*.g'},
	      {'vol' => 'ISSC', 'comment'=>'the readout Chip', 'sys'=>'ist', 'module'=>'istbgeo*.g'},
	      {'vol' => 'ISTP', 'comment'=>'the AlN Thermal Plate', 'sys'=>'ist', 'module'=>'istbgeo.g'},
	      {'vol' => 'ISTP', 'comment'=>'the Carbon Foam', 'sys'=>'ist', 'module'=>'istbgeo*.g'},
	      {'vol' => 'ISVD', 'comment'=>'the water inside the carbon duct', 'sys'=>'ist', 'module'=>'istbgeo.g'},
	      {'vol' => 'ISWD', 'comment'=>'the water duct made of carbon composite', 'sys'=>'ist', 'module'=>'istbgeo.g'},
	      {'vol' => 'ISXA', 'comment'=>'the large glue layer between Carbon foam and sensor (and sensor and hybrid)', 'sys'=>'ist', 'module'=>'istbgeo*.g'},
	      {'vol' => 'ITSP', 'comment'=>'the Inner Tracker SuPport cone mother volume', 'sys'=>'scon', 'module'=>'itspgeo.g'},
	      {'vol' => 'MAGP', 'comment'=>'the magnet mother', 'sys'=>'magp', 'module'=>'magpgeo.g'},
	      {'vol' => 'MBAR', 'comment'=>'a single return yoke bar', 'sys'=>'magp', 'module'=>'magpgeo.g'},
	      {'vol' => 'MCSE', 'comment'=>'a single barrel coil', 'sys'=>'magp', 'module'=>'magpgeo.g'},
	      {'vol' => 'MGMT', 'comment'=>'the magnet mother', 'sys'=>'quad', 'module'=>'quadgeo.g'},
	      {'vol' => 'MMRP', 'comment'=>'a Main TRay covering box for MRPC', 'sys'=>'mutd', 'module'=>'mutdgeo:3.g'},
	      {'vol' => 'MMTC', 'comment'=>'the Main Tray Cavity filled with the details for CTB', 'sys'=>'mutd', 'module'=>'mutdgeo:3.g'},
	      {'vol' => 'MPCV', 'comment'=>'the coil cavity in the pole-tip (filled with cables ?)', 'sys'=>'magp', 'module'=>'magpgeo.g'},
	      {'vol' => 'MPMT', 'comment'=>'a Main TRay covering box for PMT', 'sys'=>'mutd', 'module'=>'mutdgeo:3.g'},
	      {'vol' => 'MPTV', 'comment'=>'the magnet pole-tip volume', 'sys'=>'magp', 'module'=>'magpgeo.g'},
	      {'vol' => 'MRET', 'comment'=>'Magnet RETurn Yoke', 'sys'=>'magp', 'module'=>'magpgeo.g'},
	      {'vol' => 'MRGV', 'comment'=>'the Magnet Return rinG', 'sys'=>'magp', 'module'=>'magpgeo.g'},
	      {'vol' => 'MSEC', 'comment'=>'a sector containing a single retun bar', 'sys'=>'magp', 'module'=>'magpgeo.g'},
	      {'vol' => 'MTCL', 'comment'=>'tRIM COIL Volume (filled with aluminum)', 'sys'=>'magp', 'module'=>'magpgeo.g'},
	      {'vol' => 'MTRA', 'comment'=>'one full tray plus supporting structure for CTB/TOF', 'sys'=>'mutd', 'module'=>'mutdgeo:3.g'},
	      {'vol' => 'MUSC', 'comment'=>'a sector of MUON Trigger Barrel Scintillators', 'sys'=>'mutd', 'module'=>'mutdgeo:3.g'},
	      {'vol' => 'MUTD', 'comment'=>'the muon detector mother', 'sys'=>'mutd', 'module'=>'mutdgeo:3.g'},
	      {'vol' => 'MXSA', 'comment'=>'the active trigger scintillator SLAB for ctb', 'sys'=>'mutd', 'module'=>'mutdgeo:3.g'},
	      {'vol' => 'MXTR', 'comment'=>'a Main TRay covering box for CTB or TOF', 'sys'=>'mutd', 'module'=>'mutdgeo:3.g'},
	      {'vol' => 'OQUA', 'comment'=>'e me scelto per labellare il quarzo opaco', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'OQUF', 'comment'=>'FRAME QUARZO OPACO', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'PBPT', 'comment'=>'pb plate', 'sys'=>'fpd', 'module'=>'fpdmgeo1:3.g'},
	      {'vol' => 'PCBA', 'comment'=>'the chamber PCB', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PDCU', 'comment'=>'The outer cell in the PMD', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PDGS', 'comment'=>'The inner cell in the PMD', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PFEA', 'comment'=>'the iron plates for different syss', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PFLO', 'comment'=>'the 1st set of flanges at ~3.9 m from IR', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PFLT', 'comment'=>'the 2nd set of flanges at ~4.2 m from IR', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PHCA', 'comment'=>'the detector made in air', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PHMD', 'comment'=>'the PMD box volume and fill with air', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PHMS', 'comment'=>'the PMD sector volume - 1/3rd of PHMD', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PHSR', 'comment'=>'a detector box made in air', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PIPB', 'comment'=>'the beam pipe Bell reducing section', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PIPC', 'comment'=>'the Central Beam PIPe Volume', 'sys'=>'pipe', 'module'=>'pipegeo00.g'},
	      {'vol' => 'PIPC', 'comment'=>'the Central Beam PIPe Volume', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PIPE', 'comment'=>'the STAR beam pipe mother volume', 'sys'=>'pipe', 'module'=>'pipegeo*00.g'},
	      {'vol' => 'PIPH', 'comment'=>'the Large diameter Pipe before the beam pipes split', 'sys'=>'zcal', 'module'=>'zcalgeo.g'},
	      {'vol' => 'PIPI', 'comment'=>'Steel pipe of the Bellow section', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PIPJ', 'comment'=>'the final beam Pipes', 'sys'=>'zcal', 'module'=>'zcalgeo.g'},
	      {'vol' => 'PIPO', 'comment'=>'Steel pipe from Be to 1st flanges', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PIPS', 'comment'=>'5 inch OD steel beam pipe starting ~4.5 m from IR', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PIPT', 'comment'=>'short Steel pipe of the transition section', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PLAC', 'comment'=>'the active layer of the ladder', 'sys'=>'pixel', 'module'=>'pixlgeo*.g'},
	      {'vol' => 'PLAT', 'comment'=>'the End Plate of the large dia. Pipe', 'sys'=>'zcal', 'module'=>'zcalgeo.g'},
	      {'vol' => 'PLMI', 'comment'=>'the mother of a silicon ladder', 'sys'=>'pixel', 'module'=>'pixlgeo00.g'},
	      {'vol' => 'PLMO', 'comment'=>'the mother of the silicon ladder', 'sys'=>'pixel', 'module'=>'pixlgeo*.g'},
	      {'vol' => 'PLPS', 'comment'=>'the passive layer of the ladder', 'sys'=>'pixel', 'module'=>'pixlgeo00.g'},
	      {'vol' => 'PLPS', 'comment'=>'the passive layer of the ladder', 'sys'=>'pixel', 'module'=>'pixlgeo*.g'},
	      {'vol' => 'PLVA', 'comment'=>'the Vacuum Volume of the beam pipe holes in the end plate', 'sys'=>'zcal', 'module'=>'zcalgeo.g'},
	      {'vol' => 'PMDA', 'comment'=>'a detector box made in aluminium', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PPBA', 'comment'=>'The lead plates for different syss', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PRIB', 'comment'=>'a Rib of Steel Bellows', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PRID', 'comment'=>'a Rib section', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PRIS', 'comment'=>'the Bellow Steel Rib Set', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PSEC', 'comment'=>'a group of ladders', 'sys'=>'pixel', 'module'=>'pixlgeo*.g'},
	      {'vol' => 'PSLD', 'comment'=>'the svt beampipe shield', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PSTR', 'comment'=>'one pseudo-cell', 'sys'=>'phmd', 'module'=>'phmdgeo.g'},
	      {'vol' => 'PUPD', 'comment'=>'the Beam PIPe before the DX magnet', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'PUPE', 'comment'=>'the Beam PIPe through the DX mAgnet Volume', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'PUPF', 'comment'=>'the Outer PIPe through the DX mAgnet Volume', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'PUPG', 'comment'=>'the Beam PIPe After the DX magnet Volume', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'PVAB', 'comment'=>'its cavity', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PVAC', 'comment'=>'the Vacuum Volume of Be section of pipe', 'sys'=>'pipe', 'module'=>'pipegeo*.g'},
	      {'vol' => 'PVAD', 'comment'=>'the Vacuum Volume of the PIPe before the DX magnet', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'PVAE', 'comment'=>'the Vacuum Volume of DX mAgnet pipe', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'PVAG', 'comment'=>'the Vacuum Volume of the pipe after the DX magnet', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'PVAH', 'comment'=>'the Vacuum Volume of the large diameter pipe', 'sys'=>'zcal', 'module'=>'zcalgeo.g'},
	      {'vol' => 'PVAI', 'comment'=>'its cavity', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PVAJ', 'comment'=>'the Vacuum Volume of the final beam pipes', 'sys'=>'zcal', 'module'=>'zcalgeo.g'},
	      {'vol' => 'PVAO', 'comment'=>'its cavity', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PVAS', 'comment'=>'its cavity', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PVAT', 'comment'=>'its cavity', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PWRP', 'comment'=>'the beampipe wrap of Kapton and aluminum', 'sys'=>'pipe', 'module'=>'pipegeo.g'},
	      {'vol' => 'PXBX', 'comment'=>'the exoskeleton of the beampipe', 'sys'=>'pixel', 'module'=>'pixlgeo*.g'},
	      {'vol' => 'PXLA', 'comment'=>'the mother of a layer', 'sys'=>'pixel', 'module'=>'pixlgeo00.g'},
	      {'vol' => 'PXMO', 'comment'=>'the mother of the pixel detector volumes', 'sys'=>'pixel', 'module'=>'pixlgeo0*.g'},
	      {'vol' => 'QCAL', 'comment'=>'the Zero degree calorimeter', 'sys'=>'zcal', 'module'=>'zcalgeo.g'},
	      {'vol' => 'QDIV', 'comment'=>'one section/layer of the Quartz Calorimeter', 'sys'=>'zcal', 'module'=>'zcalgeo.g'},
	      {'vol' => 'QONE', 'comment'=>'the Q1 yoke', 'sys'=>'quad', 'module'=>'quadgeo.g'},
	      {'vol' => 'QSCI', 'comment'=>'a sensitive Fiber layer', 'sys'=>'zcal', 'module'=>'zcalgeo.g'},
	      {'vol' => 'QTHR', 'comment'=>'the Q3 yoke', 'sys'=>'quad', 'module'=>'quadgeo.g'},
	      {'vol' => 'QTWO', 'comment'=>'the Q2 yoke', 'sys'=>'quad', 'module'=>'quadgeo.g'},
	      {'vol' => 'QUAR', 'comment'=>'da me scelto per labellare il quarzo', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'RCSI', 'comment'=>'Cesium Iodide', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'RGAP', 'comment'=>'METANOL gap', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'RICH', 'comment'=>'just an aluminum box', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'SAGP', 'comment'=>'the Silver-Palladium layer', 'sys'=>'svt', 'module'=>'svttgeo*.g'},
	      {'vol' => 'SAKM', 'comment'=>'the beampipe support aluminum kinematic mount', 'sys'=>'svt', 'module'=>'scongeo*.g'},
	      {'vol' => 'SALM', 'comment'=>'the aluminum shield mesh', 'sys'=>'svt', 'module'=>'svttgeo*.g'},
	      {'vol' => 'SAPC', 'comment'=>'the core (Epoxy) of the adc board appendice', 'sys'=>'ssd', 'module'=>'sisdgeo*.g'},
	      {'vol' => 'SAPP', 'comment'=>'the mother volume of the adc board appendice', 'sys'=>'ssd', 'module'=>'sisdgeo*.g'},
	      {'vol' => 'SAPS', 'comment'=>'the side shell (Carbon) of the adc board appendice', 'sys'=>'ssd', 'module'=>'sisdgeo*.g'},
	      {'vol' => 'SAPT', 'comment'=>'the top-bottom shell (Carbon) of the adc board appendice', 'sys'=>'ssd', 'module'=>'sisdgeo*.g'},
	      {'vol' => 'SBCH', 'comment'=>'the horizontal branch of the DELRIN cross', 'sys'=>'ssd', 'module'=>'sisdgeo*.g'},
	      {'vol' => 'SBCR', 'comment'=>'the cutout in the beampipe support G10 ring', 'sys'=>'svt', 'module'=>'svttgeo1*.g'},
	      {'vol' => 'SBCV', 'comment'=>'the vertical branch of the DELRIN cross', 'sys'=>'ssd', 'module'=>'sisdgeo*.g'},
	      {'vol' => 'SBER', 'comment'=>'the Berillium wafer carrier rails', 'sys'=>'svt', 'module'=>'svttgeo*.g'},
	      {'vol' => 'SBFH', 'comment'=>'the horizontal branch of the Alumimium cross', 'sys'=>'ssd', 'module'=>'sisdgeo3:6.g'},
	      {'vol' => 'SBFV', 'comment'=>'the vertical branch of the Alumimium cross', 'sys'=>'ssd', 'module'=>'sisdgeo3:6.g'},
	      {'vol' => 'SBMI', 'comment'=>'the inner beampipe support mounting block', 'sys'=>'svt', 'module'=>'scongeo:6.g'},
	      {'vol' => 'SBMM', 'comment'=>'the beampipe support mounting mother volume', 'sys'=>'svt', 'module'=>'scongeo:6.g'},
	      {'vol' => 'SBMO', 'comment'=>'the outer beampipe support mounting block', 'sys'=>'svt', 'module'=>'scongeo:6.g'},
	      {'vol' => 'SBOI', 'comment'=>'the Berillia layer', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SBRG', 'comment'=>'the bracket joining the end rings', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SBRI', 'comment'=>'the bracket which joins the rings', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SBRL', 'comment'=>'the ceramic roller supporting the beampipe', 'sys'=>'svt', 'module'=>'scongeo:6.g'},
	      {'vol' => 'SBRM', 'comment'=>'a the mother of a single bracket joining the end rings', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SBRX', 'comment'=>'the stainless steel roller axis', 'sys'=>'svt', 'module'=>'scongeo:6.g'},
	      {'vol' => 'SBSP', 'comment'=>'the beampipe support mother volume', 'sys'=>'svt', 'module'=>'scongeo:6.g'},
	      {'vol' => 'SBSR', 'comment'=>'the beampipe support G10 ring', 'sys'=>'svt', 'module'=>'scongeo:6.g'},
	      {'vol' => 'SBWC', 'comment'=>'the bracket connecting the water manifold to the cone', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SCAL', 'comment'=>'the aluminization on the mylar wrap around the support cone', 'sys'=>'scon', 'module'=>'itspgeo:6.g'},
	      {'vol' => 'SCBL', 'comment'=>'the bundle of cables going from PCBs to manifolds', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SCBM', 'comment'=>'the mother for the bundle of cables going from PCBs', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SCKM', 'comment'=>'the cutout in the aluminum kinematic mount', 'sys'=>'svt', 'module'=>'scongeo:6.g'},
	      {'vol' => 'SCMP', 'comment'=>'the mounting plate inserted in the cones.', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SCMY', 'comment'=>'a mylar wrap around the support cone', 'sys'=>'scon', 'module'=>'itspgeo:6.g'},
	      {'vol' => 'SCON', 'comment'=>'the Silicon tracker supporting cone mother volume', 'sys'=>'scon', 'module'=>'scongeo:6.g'},
	      {'vol' => 'SCRW', 'comment'=>'the screw which attaches the end ring to the end ring bracket', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SCVB', 'comment'=>'the base plate of the V-shape piece', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SCVM', 'comment'=>'the mother volume of the V-shape piece', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SCVS', 'comment'=>'the side plate of the V-shape piece', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SDYE', 'comment'=>'the ic chip on the hybrid', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SECA', 'comment'=>'the cable on the electronics carrier', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SELE', 'comment'=>'the electronics mother volume', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SFAA', 'comment'=>'the A128C chip', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFAB', 'comment'=>'the big volume of the adc board', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFAM', 'comment'=>'the mother volume of the adc board', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFAS', 'comment'=>'the small volume of the adc board', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFCB', 'comment'=>'the big part of the second connection board', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFCF', 'comment'=>'the carbon fiber structure container', 'sys'=>'ssd', 'module'=>'sisdgeo:6.g'},
	      {'vol' => 'SFCM', 'comment'=>'the mother volume of the second connection board', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFCO', 'comment'=>'the connection board (rectangular with Hirose connectors)', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFCP', 'comment'=>'the cooling pipe', 'sys'=>'ssd', 'module'=>'sisdgeo:6.g'},
	      {'vol' => 'SFCS', 'comment'=>'the big part of the second connection board', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFCT', 'comment'=>'the carbon fiber tube', 'sys'=>'ssd', 'module'=>'sisdgeo:6.g'},
	      {'vol' => 'SFCW', 'comment'=>'the water cylinder in the cooling pipe', 'sys'=>'ssd', 'module'=>'sisdgeo:6.g'},
	      {'vol' => 'SFCX', 'comment'=>'the carbon fiber tube', 'sys'=>'ssd', 'module'=>'sisdgeo:6.g'},
	      {'vol' => 'SFDM', 'comment'=>'the mother of the detectors', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFEB', 'comment'=>'the big bus elbow', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFED', 'comment'=>'the watrer in the bundle of pipes going to manifolds', 'sys'=>'svt', 'module'=>'svttgeo4:6.g'},
	      {'vol' => 'SFES', 'comment'=>'the small bus elbow', 'sys'=>'ssd', 'module'=>'sisdgeo2:6.g'},
	      {'vol' => 'SFFK', 'comment'=>'horizontal part of the ladder skeleton carbon base', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFFL', 'comment'=>'titled part of the ladder skeleton carbon base', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFFX', 'comment'=>'the flex', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFKF', 'comment'=>'the first part of the kapton flex circuit', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFKK', 'comment'=>'horizontal part of the kapton film under the ladder base', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFKL', 'comment'=>'titled part of the kpaton film under the ladder base', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFKS', 'comment'=>'the second part of the kapton flex circuit', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFLA', 'comment'=>'the long part of the bus cable linking the syss to the connection board', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFLB', 'comment'=>'the part of the long bus cable on the connection board', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFLC', 'comment'=>'the part of the long bus cable on the connection board up to the connector', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFLM', 'comment'=>'the mother of the ladder', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFLR', 'comment'=>'the floor', 'sys'=>'shield', 'module'=>'shldgeo.g'},
	      {'vol' => 'SFLT', 'comment'=>'(half) the top corner of the triangular ladder skeleton', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFLU', 'comment'=>'(half) the side corner of the triangular ladder skeleton', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFMO', 'comment'=>'the mother of all Silicon Strip Detector volumes', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFPB', 'comment'=>'the ladder end outside mechanical part (rectangle with g10)', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFPI', 'comment'=>'the pions', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFPJ', 'comment'=>'the base of the pions', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFPR', 'comment'=>'the ladder end inside mechanical part (prism with g10 with half density)', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFRA', 'comment'=>'the hybrid stiffneer', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFRS', 'comment'=>'two supports of the hybrid stiffneer (piece of it)', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFSD', 'comment'=>'the strip detector', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SFSL', 'comment'=>'the Silicon of the strip detector', 'sys'=>'ssd', 'module'=>'sisdgeo5.g'},
	      {'vol' => 'SFSM', 'comment'=>'the mother of the ladder struct.', 'sys'=>'ssd', 'module'=>'sisdgeo:6.g'},
	      {'vol' => 'SFSS', 'comment'=>'the subvolume of the mother struct.', 'sys'=>'ssd', 'module'=>'sisdgeo.g'},
	      {'vol' => 'SFSW', 'comment'=>'a single wafer container', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SGLA', 'comment'=>'the insulating glass layer', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SGRA', 'comment'=>'the graphite/epoxy support cone', 'sys'=>'scon', 'module'=>'itspgeo:6.g'},
	      {'vol' => 'SHBI', 'comment'=>'the back iron slab', 'sys'=>'shield', 'module'=>'shldgeo.g'},
	      {'vol' => 'SHBK', 'comment'=>'the upper FTPC support top block', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SHBK', 'comment'=>'the upper FTPC support top block', 'sys'=>'support', 'module'=>'supogeo.g'},
	      {'vol' => 'SHBR', 'comment'=>'the upper FTPC support top bar', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SHBS', 'comment'=>'the shield base', 'sys'=>'shield', 'module'=>'shldgeo.g'},
	      {'vol' => 'SHFI', 'comment'=>'the forward iron slab', 'sys'=>'shield', 'module'=>'shldgeo.g'},
	      {'vol' => 'SHFX', 'comment'=>'the upper FTPC support fixture plate', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SHLA', 'comment'=>'the water hose layer for cone 3 (closer to vertex)', 'sys'=>'scon', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SHLB', 'comment'=>'the water hose layer cone 4 (further from vertex)', 'sys'=>'scon', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SHLD', 'comment'=>'the shield mother volume in the STAR cave', 'sys'=>'shield', 'module'=>'shldgeo.g'},
	      {'vol' => 'SHLS', 'comment'=>'the lateral slab', 'sys'=>'shield', 'module'=>'shldgeo.g'},
	      {'vol' => 'SHMA', 'comment'=>'a single mother volume for a water hose on the cone 3', 'sys'=>'scon', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SHMB', 'comment'=>'a single mother volume for a water hose on the cone 4', 'sys'=>'scon', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SHOL', 'comment'=>'the hole in the forward iron slab', 'sys'=>'shield', 'module'=>'shldgeo.g'},
	      {'vol' => 'SHPT', 'comment'=>'the upper FTPC support main plate', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SHRL', 'comment'=>'the upper FTPC support rail', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SHST', 'comment'=>'the upper FTPC support stabilizers', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SHWA', 'comment'=>'the water in the hose', 'sys'=>'scon', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SHXT', 'comment'=>'one Single HeXagonal Tile', 'sys'=>'bbc', 'module'=>'bbcmgeo.g'},
	      {'vol' => 'SIES', 'comment'=>'the volume to hold inner endring screws', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SIRP', 'comment'=>'the SVT inner end ring polycone (overlaps tube)', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SIRT', 'comment'=>'the SVT inner end ring tube', 'sys'=>'svt', 'module'=>'svttgeo11:6.g'},
	      {'vol' => 'SISH', 'comment'=>'the inner shield cylinder', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SISM', 'comment'=>'the mother volume division for the inner end ring screws', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SITB', 'comment'=>'the inner transition board (small r)', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SLBL', 'comment'=>'the lower FTPC support bolt', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SLDI', 'comment'=>'a ladder volume', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SLEN', 'comment'=>'the lower FTPC support end block', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SLFX', 'comment'=>'the lower FTPC support fixture plate', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SLHD', 'comment'=>'the lower FTPC support head plate (mounted to TPC)', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SLRL', 'comment'=>'the lower FTPC support rail', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SLSD', 'comment'=>'a single ladder mother (sector of tube)', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SLWL', 'comment'=>'the lower FTPC support side wall', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SLXW', 'comment'=>'the lower FTPC support cross wall', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SLYD', 'comment'=>'a single SVT layer ', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SMRD', 'comment'=>'the aluminum rod carrying the beampipe support', 'sys'=>'svt', 'module'=>'scongeo:6.g'},
	      {'vol' => 'SMRD', 'comment'=>'the aluminum rod carrying the beampipe support', 'sys'=>'svt', 'module'=>'svttgeo.g'},
	      {'vol' => 'SOER', 'comment'=>'the SVT outer end ring', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SOES', 'comment'=>'the volume to hold outer endring screws', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SOSH', 'comment'=>'the separation shield cylinder', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SOSM', 'comment'=>'the mother volume division for the outer end ring screws', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SOTB', 'comment'=>'the outer transition board (large r)', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SOUM', 'comment'=>'the mother of the array of the outer shileding support tubes', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SOUR', 'comment'=>'the outer shileding support tubes (rods)', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SPAC', 'comment'=>'e Spacers (quarz cylinders)', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'SPCB', 'comment'=>'the G10 PCB', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SPLS', 'comment'=>'the plastic walls of the bundle of pipes going to manifolds', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SRHC', 'comment'=>'the roha cell wafer support', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SRIC', 'comment'=>'sensitive part of the whole RICH ... full of Methane', 'sys'=>'rich', 'module'=>'richgeo.g'},
	      {'vol' => 'SROD', 'comment'=>'the SVT Be support rod', 'sys'=>'svt', 'module'=>'svttgeo1:6.g'},
	      {'vol' => 'SSBB', 'comment'=>'the Big part of the aluminium plate linking the ladder to the sector', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SSBS', 'comment'=>'the small part of the aluminium plate linking the ladder to the sector', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SSID', 'comment'=>'a non-sensitive left-right border of the wafer', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SSIR', 'comment'=>'a non-sensitive up-down border of the wafer', 'sys'=>'svt', 'module'=>'svttgeo6.g'},
	      {'vol' => 'SSLB', 'comment'=>'the linking (sector to the cone) box', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SSLT', 'comment'=>'the linking (sector to the cone) tube', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SSRS', 'comment'=>'the side of the small rib', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SSRT', 'comment'=>'the top of the side rib', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SSSH', 'comment'=>'the separation shield cylinder', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SSSS', 'comment'=>'the side of the small sector', 'sys'=>'ssd', 'module'=>'sisdgeo1:6.g'},
	      {'vol' => 'SSST', 'comment'=>'the top of the small sector', 'sys'=>'ssd', 'module'=>'sisdgeo6.g'},
	      {'vol' => 'STAB', 'comment'=>'the Berillium wafer carrier end tabs', 'sys'=>'svt', 'module'=>'svttgeo1:6.g'},
	      {'vol' => 'STAC', 'comment'=>'the copper part of the twin-ax cable layer', 'sys'=>'scon', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'STAP', 'comment'=>'the plastic part of the twin-ax cable layer (guess polyethylene)', 'sys'=>'scon', 'module'=>'itspgeo:6.g'},
	      {'vol' => 'STLI', 'comment'=>'the wafer pack container', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'STRA', 'comment'=>'a trapezoid of triangular shape', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'STRU', 'comment'=>'the Berillium struts between the wafer carrier rails', 'sys'=>'svt', 'module'=>'svttgeo3:6.g'},
	      {'vol' => 'STSI', 'comment'=>'a single waver container', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SUPH', 'comment'=>'the upper FTPC support mother volume', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SUPL', 'comment'=>'the lower FTPC support mother volume', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SUPO', 'comment'=>'the FTPC support mother volume', 'sys'=>'support', 'module'=>'supogeo1.g'},
	      {'vol' => 'SVTD', 'comment'=>'an active wafer volume', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SVTT', 'comment'=>'the mother of all SVT volumes', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWCH', 'comment'=>'the Be top and bottom of the water channel', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWCM', 'comment'=>'a single bracket mother between mani and cone', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWCS', 'comment'=>'the Be side of the water channel', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWCW', 'comment'=>'the water channel water (probably Evian?)', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWHO', 'comment'=>'a water hose', 'sys'=>'scon', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWMB', 'comment'=>'the water manifold bottom piece (small r)', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWMM', 'comment'=>'the water manifold mother', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWMS', 'comment'=>'the water manifold side pieces', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWMT', 'comment'=>'the water manifold top piece (big r)', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWMW', 'comment'=>'the water in the water manifold', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SWRP', 'comment'=>'an approximation of water in the circular pipe, a rectangular one', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SXAI', 'comment'=>'a first piece (A) of the bracket between mani and cone (X)', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SXBI', 'comment'=>'a second piece (B) of the bracket between mani and cone (X)', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SXRL', 'comment'=>'the mother of the circular water pipes', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'SYRU', 'comment'=>'the  wall of the water pipe', 'sys'=>'svt', 'module'=>'svttgeo10:6.g'},
	      {'vol' => 'TAEC', 'comment'=>'part of the heat shield for the PC boards', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TALC', 'comment'=>'part of the heat shield for the PC boards', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TCEX', 'comment'=>'part of the G10 for the PC boards', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TCRX', 'comment'=>'part of the G10 for the PC boards', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TESS', 'comment'=>'a division of endcap volume corresponding to one supersector', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'THLA', 'comment'=>'part of a hole for PC boards', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'THOL', 'comment'=>'part of a hole for PC boards', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'THRA', 'comment'=>'part of a hole for PC boards', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'THXM', 'comment'=>'on Triple HeXagonal', 'sys'=>'bbc', 'module'=>'bbcmgeo.g'},
	      {'vol' => 'TIAG', 'comment'=>'an air gap in inner sector aluminum structure', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TIAL', 'comment'=>'the inner Aluminum cylinder', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TIFC', 'comment'=>'the Inner Field Cage placed in TPC', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TIKA', 'comment'=>'the kapton film of the inner field cage', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TINX', 'comment'=>'the inner nomex structure', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TMAN', 'comment'=>'aluminum water manifold', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TMEA', 'comment'=>'a double sensitive layer around gating grid', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TMSE', 'comment'=>'a single sensitive volume', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TMWC', 'comment'=>'a wire chamber volume with both gated and sensitive volumes', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TOAD', 'comment'=>'Adhesive layer', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TOAE', 'comment'=>'extra aluminum supports in the air opennings', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TOFC', 'comment'=>'outer field cage - fill it with insulating gas already', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TOFC', 'comment'=>'outer field cage - fill it with insulating gas already', 'sys'=>'tpc', 'module'=>'tpcegeo2.g'},
	      {'vol' => 'TOFS', 'comment'=>'the Outer Field Cage structure', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TOHA', 'comment'=>'Honeycomb/Adhesive mixture', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TOKA', 'comment'=>'KAPTON layer', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TONX', 'comment'=>'Nomex support', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TOST', 'comment'=>'the Outer Field Cage Support', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TPAD', 'comment'=>'a real padrow with dimensions defined at positioning time', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TPCE', 'comment'=>'the TPC envelope', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TPCM', 'comment'=>'the Central Membrane placed in TPC', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TPCW', 'comment'=>'the TPC supporting endcap Wheel', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TPEA', 'comment'=>'one endcap placed in TPC', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TPGV', 'comment'=>'the Gas Volume placed in TPC', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TPIP', 'comment'=>'a water pipe there are lots per sect', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TPSS', 'comment'=>'a division of gas volume corresponding to a supersectors', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TRDC', 'comment'=>'an RDO Card', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TRDS', 'comment'=>'a division of rdo board volume corresponding to one supersector', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TRDV', 'comment'=>'the RDO board volume', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TSEC', 'comment'=>'a supersector containing Al sector, PCB and MWC volume', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TWGB', 'comment'=>'added alum blocksin Wheel', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TWGC', 'comment'=>'the larger Inner air gap in Wheel', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TWGI', 'comment'=>'the Inner air gap in Wheel', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'TWSS', 'comment'=>'a division of wheel corresponding to supersectors', 'sys'=>'tpc', 'module'=>'tpcegeo1.g'},
	      {'vol' => 'UPST', 'comment'=>'the upstream mother volume in the STAR cave', 'sys' => 'upstream', 'module'=>'upstgeo.g'},
	      {'vol' => 'VCNV', 'comment'=>'converter layer (radiator included)', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VDET', 'comment'=>'a single detector (Radiator+converter and PMT+electroncs)', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VDTI', 'comment'=>'inner part of the single detector', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VFEE', 'comment'=>'the FEE inside the box', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VLEM', 'comment'=>'a Lemo connector on the FEE boards', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPBA', 'comment'=>'the part of the hook that mounts to the front/back plate', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPBB', 'comment'=>'the part of the hook that mounts to the base plate', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPBI', 'comment'=>'the empty space inside of the FEE box', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPBO', 'comment'=>'container for the hook', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPBP', 'comment'=>'the Base Plate', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPBX', 'comment'=>'the FEE box', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPCF', 'comment'=>'the front plate of the boat clamp', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPCH', 'comment'=>'the horizontal plate of the boat clamp', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPCL', 'comment'=>'the boat clamp', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPCV', 'comment'=>'the vertical plate of the boat clamp', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPDD', 'comment'=>'the whole VPPD assembly', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPFA', 'comment'=>'the central upper part of the frontplate', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPFB', 'comment'=>'the middle upper part of the frontplate', 'sys'=>'vpd', 'module'=>'vpddgeo1.g'},
	      {'vol' => 'VPFC', 'comment'=>'the outer upper part of the frontplate', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VPFP', 'comment'=>'a single rectangular piece of the frontpanel', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VPIP', 'comment'=>'the Long Pipe', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VPMT', 'comment'=>'the PMT inner volume', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VPSA', 'comment'=>'part of a strut clamp that holds to the frontplate', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VPSB', 'comment'=>'part of a strut clamp that holds to the strut', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VPSC', 'comment'=>'a clamp that holds the strut', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VPST', 'comment'=>'the strut  volume', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VPSV', 'comment'=>'the actual strut between front and backplates', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VPSW', 'comment'=>'a tiny piece of aluminium that belongs to the strut', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VRAD', 'comment'=>'Cerenkov Radiator layer', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VRNG', 'comment'=>'a single pVPD Ring or the entire upVPD', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VSEC', 'comment'=>'one pVPD sector with all stuff inside', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'VXST', 'comment'=>'PMT output cables (just to look nicer)', 'sys'=>'vpd', 'module'=>'vpddgeo:2.g'},
	      {'vol' => 'YPLA', 'comment'=>'the active layer of the pixel', 'sys'=>'hpdt', 'module'=>'hpdtgeo.g'},
	      {'vol' => 'YPLM', 'comment'=>'the mother of the pixel ladder', 'sys'=>'hpdt', 'module'=>'hpdtgeo.g'},
	      {'vol' => 'YPLP', 'comment'=>'the mother of the support', 'sys'=>'hpdt', 'module'=>'hpdtgeo.g'},
	      {'vol' => 'YPWP', 'comment'=>'the all silicon layer active and readout ', 'sys'=>'hpdt', 'module'=>'hpdtgeo.g'},
	      {'vol' => 'YPWR', 'comment'=>'the other support', 'sys'=>'hpdt', 'module'=>'hpdtgeo.g'},
	      {'vol' => 'YPXM', 'comment'=>'the mother of the pixel detector volumes  !!define each object', 'sys'=>'hpdt', 'module'=>'hpdtgeo.g'},
	      {'vol' => 'ZCAL', 'comment'=>'the region between the DX and the D0 magnets', 'sys'=>'zcal', 'module'=>'zcalgeo.g'}
	     );
if ($#ARGV != 0) { 
  print "Usage $0 vers.h\n";
  print "For example: $0 y2005.h\n";
  exit 0;
}
foreach my $s (@blocks) {
  my $found = 0;
  foreach my $p (@topvolumes) {
    if ($p->{sys} eq $s->{sys}) {
      $found = 1;
    }
  }
  if (! $found) {
    print "for volume $s->{vol} system $s->{sys} has not been found\n";
  }
}

my @tops = ();
foreach my $p (@topvolumes) {
  next if (! $p->{top});
  print "package\t $p->{sys}\ttop\t$p->{top}\tcomment\t$p->{comment}\n";
  push @tops, $p->{top};
}
my $tops = join '|', @tops;
print "tops = $tops\n";
my $fileIn = $ARGV[0];
my ($vers) = split '\.', File::Basename::basename($fileIn);
print "version = $fileIn => $vers\n";
open(In,"$fileIn") or die "Can't open file $fileIn";
my $fileOut = $vers . ".h";
my $line;
my $case = 0;
my $P = 0; # pointer to system P->{blob}
my $pHead = 0;
my $pMain = 0;
my $pRotations = 0;
my $PMaterial = 0;
my $PMedia = 0;
my $pRotations = 0;
my $Rblob = "";
foreach my $p (@topvolumes) {
  if    ($p->{top} ne '') {next;}
  if    ($p->{sys} eq 'HEAD') {$pHead = $p;}
  elsif ($p->{sys} eq 'MAIN') {$pMain = $p;}
  elsif ($p->{sys} eq 'Material') {$pMaterial = $p;}
  elsif ($p->{sys} eq 'Media') {$pMedia = $p;}
  elsif ($p->{sys} eq 'Rotations') {$pRotations = $p;}
}
die if (!$pHead || ! $pMain || ! $pMaterial || ! $pMedia || ! $pRotations);
$pHead->{blob}  = "#define GeometryVersion  " . $vers ."\n";
$pHead->{blob} .= "#include \"GeometryConfiguration.h\"\n";
$pHead->{blob} .= "#include \"CreateGeometry.h\"\n";
$pHead->{blob} .= "#include \"Rotations.h\"\n";
$pMain->{blob}  = "TGeoRotation *rot;\n";
$pMain->{blob} .= "void geometry(){\n";
$pMain->{blob} .= "  gSystem->Load(\"libGeom\");\n";
$pMain->{blob} .= "  TGeoManager *star = new TGeoManager(\"star\",\"star\");\n";
$pMain->{blob} .= "  Rotations();\n";
while ($line = <In>) {
  next if ($line =~ /void/);
  if ($line =~ /^\/\/$/) {next;}
  if ($line =~ /^TGeoRotation \*rot;/ or
      $line =~ /^TGeoNode \*Node, \*Node1;/) {next;}
  if ($line =~ /Int_t imed = 0/) {next;}
  if ($line =~ /new TGeoManager/ or
      $line =~ /gSystem/ or $line =~ /^}/) { next;}
  if ($line =~ /CloseGeometry/) { next;}
  if ($line =~ /List of/) {
    if ($line =~ /List of Materials and Mixtures/) {
      $case = 1;
      $P = $pMaterial;
#      $pHead->{blob} .= "#include \"" . $P->{sys} . ".h\"\n";
      $pMain->{blob} .= "  "          . $P->{sys} . "();\n";
      $P->{blob} = "void Material() {\n";
    }
    if ($line =~ /List of Tracking Media/) {
      $case = 2;
      $P = $pMedia;
#      $pHead->{blob} .= "#include \"" . $P->{sys} . ".h\"\n";
#      $pMain->{blob} .= "  "          . $P->{sys} . "();\n";
      $P->{blob}  = "void Media() {\n";
      $P->{blob} .= "  Int_t imed = 0;\n";
    }
    if ($line =~ /List of Rotation matrices/) {
      $case = 3;
#      $pHead->{blob} .= "#include \"" . $P->{sys} . ".h\"\n";
      $pMain->{blob} .= "  "          . $P->{sys} . "();\n";
      $P = $pRotations;
    }
    if ($line =~ /List of Volumes/) {
      $case = 4;
      $P = 0;
    }
    if ($line =~ /List of Nodes/) {
      $case = 5;
      $P = 0;
    }
    next;
  }
  if ($line =~ /^Int_t imed = 0;/) {next;}
  if ($line =~ /^\/\//) {next;}
  if ($case < 4) {$P->{blob} .= $line; next;}
  if ($case == 4 or $case == 5) {
    if ($line =~ /SetTopVolume/) {
      $pMain->{blob} .= "  " . $line;
#      print $pMain->{blob};
      next;
    }
    my $volume;
    my $dummy;
    my $bvol = 0;
    if ($line =~ /TGeoVolume/) {
      ($dummy,$volume,$dummy) = split '"', $line;
    } elsif ($line =~ /AddNode/) {
      my ($dum,$bum) = split '\(', $line;
      my ($vol,$dum) = split',', $bum;
      $vol =~ s/ //g;
      $vol =~ s/_.*//;
      $vol =~ s/-.*//;
      $volume = $vol;
      my $tvol = $line;
      chomp($tvol);
      $tvol =~ s/->AddNode.*//;
      $tvol =~ s/^\ *//;
      if ($tvol =~ $tops and $volume =~ $tops and $volume !~ /^FBOX/  and $volume !~ /^IBEM/) {
	$pMain->{blob} .= $line;
	next;
      }
    }
    foreach my $v (@blocks) {
      if ($volume eq $v->{vol}) {
	$bvol = $v;
	last;
      }
    }
    if ($case == 4) {
      $line =~ s/SetTitle\(\".*\"\)/SetTitle\(\"$bvol->{comment}\"\)/;
    }
    if ($bvol->{sys} ne '') {
      my $found = 0;
      foreach my $p (@topvolumes) {
	if ($bvol->{sys} eq $p->{sys}) {
	  $found = 1;
	  $P = $p;
	  if (! $P->{blob}) {
	    $pHead->{blob} .= "#ifdef " . $P->{sys} . "Config\n";
	    $pHead->{blob} .= "#include \"" . $P->{sys} . ".h\"\t/* $p->{comment}  */\n";
	    $pMain->{blob} .= "#ifdef " . $P->{sys} . "Config\n";
	    if ($P->{sys} eq 'fpd' or $P->{sys} eq 'ibem') {
	      $pMain->{blob} .= "  " . $P->{sys} . "(CAVE);\n";
	      $P->{blob} .= "void " . $P->{sys} . "(TGeoVolume *CAVE) {\n";
	    } else {
	      $pMain->{blob} .= "  TGeoVolume *" . $P->{top} . " = " . $P->{sys} . "();\n";
	      $P->{blob} .= "TGeoVolume *" . $P->{sys} . "() {\n";
	    }
	    $pHead->{blob} .= "#endif \t/* " . $P->{sys} . "Config */\n";
	    $pMain->{blob} .= "#endif \t/* " . $P->{sys} . "Config */\n";
	    last;
	  }
	}
      }
      if (! $found) {
	print $line;
	die "$volume  $bvol->{vol} $bvol->{sys} not found\n";
      }
    }
  }
  if ($line =~ /TGeoRotation\(\"next\"/) {
    $Rblob .= $line;
    next;
  }
  if ($Rblob) {$P->{blob} .= $Rblob; $Rblob = "";}
  $P->{blob} .= $line;
}
close In;
foreach my $p (@topvolumes) {
  my $f = $fileOut;
  next if ($p->{blob} eq '');
  next if ($p->{sys} eq 'Rotations');
#  print "$p->{sys} : $p->{blob}";
  if ($p->{sys} eq 'HEAD') {
    $f = "geometry" . "." . $vers . ".h";
    open(Out,">$f") or die "Can't open file $f";
  } elsif ($p->{sys} eq 'MAIN') {
    $f = "geometry" . "." . $vers . ".h";
    $p->{blob} .= "}\n";
    open(Out,">>$f") or die "Can't open file $f";
  } else {
    if ($p->{sys} ne 'fpd' and $p->{sys} ne 'ibem') {
      if ($p->{top}) {$p->{blob} .= "  return $p->{top};\n";}
    }
    $p->{blob} .= "}\n";
    $f = $p->{sys} . "." . $vers . ".h";
    open(Out,">$f") or die "Can't open file $f";
  }
  print Out $p->{blob};
  close (Out);
}
  
