# Additional Notes specific for runing the code with simulation


## *** Setup code (from https://github.com/jdbrice/star-sw-1/blob/fwd-tracking/StRoot/StFwdTrackMaker/macro/README.md)

Perform a sparse checkout and hop onto the `fwd-tracking` branch
```sh
git clone --no-checkout git@github.com:jdbrice/star-sw-1.git
cd star-sw-1
git config core.sparseCheckout true

touch .git/info/sparse-checkout
echo "StDb" >> .git/info/sparse-checkout
echo "StarDb" >> .git/info/sparse-checkout
echo "StRoot/RTS" >> .git/info/sparse-checkout
echo "StRoot/StBFChain" >> .git/info/sparse-checkout
echo "StRoot/StEvent" >> .git/info/sparse-checkout
echo "StRoot/StFst*" >> .git/info/sparse-checkout
echo "StRoot/StFtt*" >> .git/info/sparse-checkout
echo "StRoot/StFwd*" >> .git/info/sparse-checkout
echo "StRoot/StFcs*" >> .git/info/sparse-checkout

git checkout fwd-tracking
```

Setup the environment, build the code, and perform the one-time build of the root geometry
```sh
source StRoot/StFwdTrackMaker/macro/env.sh
cons
./StRoot/StFwdTrackMaker/macro/build_geom.C
```

## *** Running particle gun MC:

```bash
ln -s StRoot/StFwdTrackMaker/macro/sim sim
sim/run_batch_fast 1 3
root -b -q sim/plot.C'(0,3,1)'
sim/run_batch_fast 1 5
root -b -q sim/plot.C'(0,5,1)'
sim/run_batch_fast 1 8
root -b -q sim/plot.C'(0,8,1)'
```

## *** Running PYTHIA

```bash
cp sim/runPythia.C .
cp sim/runSimBfc.C .
roo4star -b -q runPythia.C'(10,1,"JPsi")'
roo4star -b -q runSimBfc.C'(10,1,"JPsi")'
root -b -q sim/plot.C'(1,"JPsi")'
root -b -q sim/plotDilep.C'(1,"JPsi")'
```

## *** Submit PYTHIA jobs

```bash
cp sim/runpythia .
mkdir fcs2022 (or ln -s <your fzd data disk> fcs2022)
mkdir hist    (or ln -s <your hist disk> hist)
(edit sim/submitpythia.pl, esp nogeant=0 for running GEANT, nogeant=1 for just running analysis)
sim/submitpythia  
more condor/submit_pythia.txt
sim/submitpythia submit
sim/merge_pythia.pl or merge_dybg.pl
root -b -q sim/plotDilep.C'(0,"JPsi")'
root -b -q sim/plot.C'(0,"JPsi")'
```

thank you Akio
