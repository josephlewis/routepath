# 0.1.5 (17th October 2022)
* Amended calculate_distance to make sure route and known_route have same crs

# 0.1.4 (17th June 2022)
* Modified rescale_cs_global to allow for user-supplied transformations of transition values via the fun argument
* Added fun argument to  global_wlc
* Added change_tolerance function for ease-of-use when modifying/testing tolerance threshold values

# 0.1.3 (11th January 2022)
* Implemented Local and Global Weighted Linear Combination
  - rescale_cs_global
  - rescale_cs_local
  - local_weights_cs

# 0.1.2 (09th January 2022)
* Added Hausdorff distance for measuring deviation between simulated route and known route 

# 0.1.1 (14th December 2021)
* Fixed frechet_distance error by assigning known_route crs to simulated route

# 0.1.0 (10th December 2021)
* Implemented ABC_rejection when postdicting routes
* Implemented euclidean, path deviation index, and frechet distance for measuring deviation between simulated route and known route
