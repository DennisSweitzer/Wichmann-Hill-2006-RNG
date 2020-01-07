# Wichmann-Hill-2006-RNG
R code for the Wichmann-Hill 2006 random number generator. It uses 4 32-bit state variables for a period of about 2^120 (1.329x10^36), and has a method for spinning off new seeds for independent parallel sequences of random numbers (with no overlap for >2.3x10^18 values)
