The code uses finite differences on an adaptive grid to approximate the two-dimensional advection equation.  The adaptive grid is dynamically generated using multiresolution analysis, with the computational grid composed of a series of nested grids, each dyadically finer than the previous one.  The nested grids are stored using a dynamic tree, updated at each time step.


main.f90 - Contains the main script.  Creates the inital tree (adaptive gird), evolves the initial data, and updates the grid.

parameters.f90 - Contains various parameters used throughout the simulation.

GENESIS.f90 - Contains routines to initiate the tree.

MR.f90 - Contains routines to update tree

INTERPOLATION.f90 - Contains interpolation routines for tree