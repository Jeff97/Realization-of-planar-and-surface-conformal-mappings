Here, we provide some supplementary files for the manuscript. 

- Movie 1 shows the simulated growth process of different samples (2D, 3D and complex 3D cases). To download the movie, one can click the item, then click "View raw". 

- Each folder contains .off files of the parametric plane and target shape. One can open .off files using software [MeshLab](https://www.meshlab.net/). 



# Simulation(needs expertise)

Each folder also contains necessary files to run the simulation using ABAQUS. To run the .inp file, please make sure 

- Visual Studio and Parallel Studio are linked to ABAQUS, such that it can be used for subroutine development. [Here](https://www.researchgate.net/publication/349991987_Linking_ABAQUS_20192020_and_Intel_oneAPI_Base_Toolkit_FORTRAN_Compiler) is a linking guide from the internet. 

- The path in UMAT subroutine (see line 84 to 112 in the .for files) has been modified to your own one, since it is set as absolute path. For example

  Before modification:

  ```fortran
          open(301,FILE='C:\Users\12872\Desktop\'//
       &  'Bunny\part1\E0.CSV',status="old")
          read(301,*) E0
          close(301)
  ```

  After modification:

          open(301,FILE='C:\$YourPath$\'//
       &  'Bunny\part1\E0.CSV',status="old")
          read(301,*) E0
          close(301)

- Submit the job in ABAQUS COMMAND window.

# Simulated results

![Alex](https://github.com/Jeff97/Realization-of-planar-and-surface-conformal-mappings/blob/main/Alex.jpg)

![Car](https://github.com/Jeff97/Realization-of-planar-and-surface-conformal-mappings/blob/main/Car.jpg)

![Bunny](https://github.com/Jeff97/Realization-of-planar-and-surface-conformal-mappings/blob/main/Bunny.jpg)