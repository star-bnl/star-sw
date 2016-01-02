##StPicoKFVertexFitter
Simple utility class that takes a picoDst event and returns a KFVertex.  
You can pass a `std::vector<int>` of track indices to remove from the fit.  

###Authors:  
	**Liang He(he202@purdue.edu)
	Mustafa Mustafa (mmustafa@lbl.gov)  

  **Code Maintainer

- - -
### Presentations:  
#### STAR Protected:  
- [Systematic study of KFVertex in peripheral events](https://drupal.star.bnl.gov/STAR/system/files/Vertex%20finder.pdf), Liang He, HF PWG update.

- - -
###How to use in your code?
```c++

  StPicoKFVertexFitter kfVertexFitter;

  StThreeVectorF kfVertex = kfVertexFitter.primaryVertexRefit(picoDst);

```
You will need load the following libraries:
```c++
  // KFVertexFitter dependancies
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("Sti");
  gSystem->Load("StiUtilities");
  gSystem->Load("StSsdDbMaker");
  gSystem->Load("StSvtDbMaker");
  gSystem->Load("StiMaker");
  gSystem->Load("StPicoKFVertexFitter");
  // ---
```
