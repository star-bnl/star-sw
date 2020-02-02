#cd /net/l401/data/scratch2/kehw/reco/2019/14GeV_2019_TFG19e
#foreach d (`123 144`)
cd /net/l401/data/scratch2/kehw/reco/2019/19GeV_2019
foreach d (`ls -1d 0??`)
  mount.pl gpf01
  dirsync ${d} /direct/gpfs01/star/pwg/fisyak/Pico/14GeV_2019_TFG19e/${d}
end
