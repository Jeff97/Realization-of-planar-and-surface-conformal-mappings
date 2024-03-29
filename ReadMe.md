Here, we provide some supplementary files for the manuscript[1]. 

- Movie 1 shows the simulated growth process of different samples (2D, 3D and complex 3D cases). To download the movie, one can click the item, then click "View raw". 

- Each folder contains .off files of the parametric plane and target shape. One can open .off files using software [MeshLab](https://www.meshlab.net/). 

[1] Jiong wang, Zili Jin & Zhanfeng Li. (2023). [Realization of planar and surface conformal mappings through stress-free growth of hyperelastic plates.](https://www.researchgate.net/publication/372403776_Realization_of_planar_and_surface_conformal_mappings_through_stress-free_growth_of_hyperelastic_plates)

# Simulation(needs expertise)

Each folder also contains necessary files to run the simulation using ABAQUS. To run the simulation, please make sure 

- Visual Studio and Parallel Studio are linked to ABAQUS, such that it can be used for subroutine development. [Here](https://www.researchgate.net/publication/349991987_Linking_ABAQUS_20192020_and_Intel_oneAPI_Base_Toolkit_FORTRAN_Compiler) is a linking guide from the internet. 

- The path in UMAT subroutine (see line 84 to 112 in the .for file) has been modified to your own one, since it is set as absolute path. For example

  Before modification:

  ```fortran
          open(301,FILE='C:\Users\12872\Desktop\'//
       &  'Bunny\part1\E0.CSV',status="old")
          read(301,*) E0
          close(301)
  ```

  After modification:

  ```fortran
      open(301,FILE='C:\$YourPath$\'//
   &  'Bunny\part1\E0.CSV',status="old")
      read(301,*) E0
      close(301)
  ```

- Submit the job through ABAQUS COMMAND window, for example

  ```fortran
  C:
  cd C:\$YourPath$\Bunny\part1
  
  abaqus job=Bunny_Part1 user=Growth-Bunny-Part1.for cpus=6
  ```

  

# Simulated results

### Case 1: a human face

![Alex](https://github.com/Jeff97/Realization-of-planar-and-surface-conformal-mappings/blob/main/Alex.jpg)

### Case 2: a model car

![Car](https://github.com/Jeff97/Realization-of-planar-and-surface-conformal-mappings/blob/main/Car.jpg)

### Case 3: a bunny

![Bunny](https://github.com/Jeff97/Realization-of-planar-and-surface-conformal-mappings/blob/main/Bunny.jpg)