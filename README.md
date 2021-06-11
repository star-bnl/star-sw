# star-sw-1
Software libraries for STAR experiment


## Checkout the Geometry code only
```bash
git clone --no-checkout https://github.com/jdbrice/star-sw-1.git fstGeo
cd fstGeo
git config core.sparseCheckout true
echo "StarVMC/Geometry" > .git/info/sparse-checkout
echo "StarVMC/xgeometry" >> .git/info/sparse-checkout
echo "StarVMC/StarGeometry" >> .git/info/sparse-checkout
git checkout
```

After this you should have a `StarVMC` directory

## build
You can build with 
```bash
cons +StarVMC/Geometry
```

Next I will add a geometry export script to "export" the simplified FST geometry so that it can be tested in the tracking code.
